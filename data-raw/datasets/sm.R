# Global Seamount Database from Kim, S.-S., and P. Wessel (2011), Geophys. J. Int., in revision.
#
# This file contains 24,646 potential seamounts from the entire ocean basins.
#
# The columns are:
# - Longitude: (-180/+180). Center of each seamount (in degrees)
# - Latitude: (-90/+90). Center of each seamount (in degrees)
# - Azimuth: Estimated azimuth of the basal ellipse (in degree)
# - Major: Estimated major axis of the basal ellipse of each seamount (in km)
# - Minor: Estimated minor axis of the basal ellipse of each seamount (in km)
# - Height: Seamount height obtained from the predicted bathymetry TOPO V12 (in m)
# - FAA: Maximum amplitude of the Free-Air Gravity Anomaly (in mGal)
# - VGG: Maximum amplitude of the Vertical Gravity Gradient Anomaly (in Eotvos)
# - Depth: Regional depth of each seamount (in m)
# - CrustAge: Age of underlying seafloor from the AGE 3.2 grid (in Myr)
# - ID: for each seamount (plate_###)

librarian::shelf(
  devtools, dplyr, glue, here, janitor, readr, sf, terra, tidyr)
load_all()

dir_seamounts <- "/Users/bbest/My Drive/projects/offhab/data/seamounts (Kim & Wessel, 2011)"
seamounts_csv <- glue("{dir_seamounts}/KWSMTSv01.csv")
seamounts_geo <- here("data-raw/mdls_sm_seamounts.geojson")

d <- read_csv(seamounts_csv) %>%
  clean_names()
# chr (2): Longitude, ID
# dbl (9): Latitude, Azimuth, Major, Minor, Height, FAA, VGG, Depth, CrustAge
# Rows: 24666 Columns: 11
colnames(d)
# [1] "longitude" "latitude"  "azimuth"   "major"     "minor"     "height"
# [7] "faa"       "vgg"       "depth"     "crust_age" "id"
d2 <- d %>%
  mutate_at(setdiff(colnames(d), "id"), as.numeric)
# Problem while computing `longitude = .Primitive("as.double")(longitude)`.
# ℹ NAs introduced by coercion

# skip regions listed in first column (longitude) ----
idx_na <- which(is.na(d2$longitude))
d_hdr <- tibble(
  idx = idx_na,
  hdr = d$longitude[idx_na])
d_hdr$dif = diff(c(0,idx_na)) - 1
d_hdr
d$longitude[idx_na]
# > AF 3888 seamounts # from *.txt
#  [1] "> AN 4837 seamounts" "> AO 2855 seamounts" "> AR 47 seamounts"
#  [4] "> CI 149 seamounts"  "> CO 185 seamounts"  "> CP 1 seamounts"
#  [7] "> EA 9 seamounts"    "> EU 532 seamounts"  "> GP 2 seamounts"
# [10] "> IN 356 seamounts"  "> JF 21 seamounts"   "> JZ 40 seamounts"
# [13] "> NA 705 seamounts"  "> NZ 1112 seamounts" "> OA 7 seamounts"
# [16] "> PA 6863 seamounts" "> PSM 697 seamounts" "> RI 30 seamounts"
# [19] "> SA 1133 seamounts" "> SC 134 seamounts"  "> SD 80 seamounts"
# [22] "> SO 924 seamounts"  "> SW 36 seamounts"

d <- d2 %>%
  filter(!is.na(longitude))

# create points ----
pts <- d %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    remove = F,
    crs    = st_crs(4326))

# identify to OffHab zones ----

con <- oh_pg_con()
oh_zones <- st_read(con, "oh_zones")
# mapview::mapView(oh_zones)

pts <- pts %>%
  st_join(
    oh_zones)
table(pts$zone_key, useNA = "ifany")
# table(pts$zone_name, useNA = "ifany")
# cec   noa   noc   soc   wao  <NA>
#   3     4     4    14    18 24600

pts <- pts %>%
  filter(!is.na(zone_id))
# mapview::mapView(pts)

# create circle polygons based on basal area (with major & minor axes of ellipse) ----
ply <- pts %>%
  st_transform(3857) %>%
  mutate(
    area_km   = pi*major*minor,
    radius_km = sqrt(area_km/pi),
    geometry  = st_buffer(geometry, radius_km*1000))
# mapview::mapView(ply)

# calculate depth level per diff't biogeographic properties ----
brks <- c(0, 200, 800, Inf)
ply <- ply %>%
  mutate(
    summit_depth       = (-1 * depth) - height,
    summit_depth_class = cut(summit_depth, brks))
table(ply$summit_depth_class, useNA = "ifany")
# (0,200] (200,800] (800,Inf]
#       0         0        43
ply %>%
  st_transform(4326) %>%
  write_sf(seamounts_geo, delete_dsn=T)

# project to OffHab raster ----
ply <- read_sf(seamounts_geo) %>%
  st_transform(3857)
r_cid <- oh_rast("cell_id")
# sum(duplicated(values(r_cid, na.rm=T))) # 0
r_sm  <- rasterize(ply, r_cid, 1)
r     <- c(r_cid, r_sm)
# sum(duplicated(values(r$cell_id, na.rm=T))) # 0

# write to db ----
con <- oh_pg_con()

tbl_sm_model <- tibble(
  mdl_id      = 1,
  name_short  = "Seamounts",
  year        = 2011,
  name_long   = "Seamount Census from Altimetry-Derived Gravity Data",
  description = '',
  source      = "http://www.soest.hawaii.edu/PT/SMTS/main.html",
  citation    = "Kim S-S, Wessel P (2011) New global seamount census from
  altimetry-derived gravity data: New global seamount census. Geophysical
  Journal International 186:615–631",
  value_type  = "presence")
dbWriteTable(con, "sm_model", tbl_sm_model, overwrite = T)

d <- r %>%
  values(dataframe=T, na.rm=T) %>%
  tibble() %>%
  mutate(
    tbl    = "sm_model",
    mdl_id = 1) %>%
  select(tbl, mdl_id, cell_id, value = layer)
# sum(duplicated(d$cell_id)) # 0
# 56,701 × 4
dbAppendTable(con, "oh_cells_rast", d)
# dbGetQuery(con, "SELECT COUNT(*) AS n FROM oh_cells_rast WHERE tbl = 'sm_model'")


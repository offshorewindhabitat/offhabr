# Hydrothermal Vents
#
# * [Beaulieu, SE; Szafrański, KM (2020): InterRidge Global Database of Active
#    Submarine Hydrothermal Vent Fields Version 3.4](
#    https://doi.pangaea.de/10.1594/PANGAEA.917894)
#
# year        = 2020,
# name_long   = "InterRidge Global Database of Active Submarine Hydrothermal Vent Fields",
# source      = "http://vents-data.interridge.org",
# citation    = "Beaulieu, S.E., Szafranski, K. (2020) InterRidge Global
#   Database of Active Submarine Hydrothermal Vent Fields, Version 3.4. World Wide
#   Web electronic publication available from http://vents-data.interridge.org
#   Accessed YYYY-MM-DD.",
librarian::shelf(
  devtools, glue, here, janitor, readr, sf, terra)
load_all()

dir_vents <- "/Users/bbest/My Drive/projects/offhab/data/derived/vents (Interridge Vent Database v3.4)/"
vents_csv <- glue("{dir_vents}/vent_fields_all_20200325cleansorted.csv")
vents_geo <- here("data-raw/datasets/ve.geojson")

pts <- read_csv(vents_csv) %>%
  # chr (14): Name.ID, Name.Alias.es., Vent.Sites, Activity, Max.Temperature.Category, Ocean, ...
  # dbl  (7): ...1, Maximum.Temperature, Latitude, Longitude, Maximum.or.Single.Reported.Depth...
  clean_names() %>%
  st_as_sf(
    coords = c("longitude", "latitude"), crs = 4326, remove=F)
# 721 features and 21 fields

# mapview::mapView(pts)

# identify to OffHab zones ----
con <- oh_pg_con()
oh_zones <- st_read(con, "oh_zones")
# mapview::mapView(oh_zones)

pts <- pts %>%
  st_join(
    oh_zones)
table(pts$zone_key, useNA = "ifany")
table(pts$zone_name, useNA = "ifany")
# noc  wao <NA>
#   3    4  714

pts <- pts %>%
  filter(!is.na(zone_key))

# add zone of biogeographic influence to points ----
# split difference for zone of influence from literature:
# - 100 km2 (Haymon et al., 1990).
# - 200 km2 (Van Dover et al., 2002)
radius_km <- round(sqrt(mean(c(200,100)))) # 12 km
ply <- pts %>%
  st_transform(3857) %>%
  mutate(
    geometry  = st_buffer(geometry, radius_km*1000))
# mapview::mapView(ply)
ply %>%
  st_transform(4326) %>%
  write_sf(vents_geo, delete_dsn=T)

# project to OffHab raster ----
ply <- read_sf(vents_geo) %>%
  st_transform(3857)
r_cid <- oh_rast("cell_id")
# sum(duplicated(values(r_cid, na.rm=T))) # 0
r_ve  <- rasterize(ply, r_cid, 1)
r     <- c(r_cid, r_ve)

# write to db ----
con <- oh_pg_con()

tbl_ve_model <- tibble(
  mdl_id      = 1,
  name_short  = "Hydrothermal Vents",
  year        = 2020,
  name_long   = "InterRidge Global Database of Active Submarine Hydrothermal Vent Fields",
  source      = "http://vents-data.interridge.org",
  citation    = "Beaulieu, S.E., Szafranski, K. (2020) InterRidge Global
  Database of Active Submarine Hydrothermal Vent Fields, Version 3.4. World Wide
  Web electronic publication available from http://vents-data.interridge.org
  Accessed YYYY-MM-DD.",
  value_type  = "presence")
dbWriteTable(con, "ve_model", tbl_ve_model, overwrite = T)

d <- r %>%
  values(dataframe=T, na.rm=T) %>%
  tibble() %>%
  mutate(
    tbl    = "ve_model",
    mdl_id = 1) %>%
  select(tbl, mdl_id, cell_id, value = layer)
# sum(duplicated(d$cell_id)) # 0
# 12,948 × 4
dbSendQuery(con, "DELETE FROM oh_cells_rast WHERE tbl = 've_model'")
dbAppendTable(con, "oh_cells_rast", d)
# dbGetQuery(con, "SELECT COUNT(*) AS n FROM oh_cells_rast WHERE tbl = 've_model'")

# redo rast ----
librarian::shelf(
  here, readr)
options(readr.show_col_types = F)

dir_lyrs_tif <- "/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_tif"
lyrs_csv     <- here("data-raw/layers.csv")
ds_key       <- "ve"
r_tif        <- glue("{dir_lyrs_tif}/{ds_key}.tif")

# read geojson, project to web Mercator
ply <- read_sf(vents_geo) %>%
  st_transform(3857)
# mapView(ply)

# rasterize to OH
r_na <- oh_rast("NA")
r_ve <- rasterize(ply, r_na, 1)
write_rast(r_ve, r_tif)
# plet(r_ve, tiles="Esri.NatGeoWorldMap")

# clear old layers of same dataset
con <- oh_con(read_only = F)
dbSendQuery(con, glue("DELETE FROM lyrs WHERE ds_key = '{ds_key}'"))

# set data for new layer
# tbl(con, "lyrs") |>
#   colnames() |> paste(collapse=' = " ",\n') |> cat()
d_lyr <- tibble(
  ds_key      = ds_key,
  lyr_key     = ds_key,
  aphia_id    = NA,
  val_min     = 1,
  val_max     = 1,
  rescale_min = 1,
  rescale_max = 1)

# insert new layer
cols <- colnames(d_lyr) |> paste(collapse = ", ")
qs   <- rep("?", ncol(d_lyr)) |> paste(collapse = ", ")
dbExecute(
  con,
  glue("INSERT INTO lyrs ({cols}) VALUES ({qs})"),
  d_lyr |> as.list() |> unname())
# tbl(con, "lyrs") |>
#   filter(ds_key == !!ds_key)

# disconnect db
dbDisconnect(con, shutdown=T)


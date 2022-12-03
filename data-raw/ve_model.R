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

dir_vents <- "/Users/bbest/My Drive/projects/offhab/data/vents (Interridge Vent Database v3.4)/"
vents_csv <- glue("{dir_vents}/vent_fields_all_20200325cleansorted.csv")
vents_geo <- here("data-raw/ve_vents.geojson")

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


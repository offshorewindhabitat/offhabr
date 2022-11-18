# code to prepare Offshore Habitat Zones (`oh_zones`) based on:
# - BOEM “2019-2024 Draft Proposed Program Areas”
# - for lower-48 (excluding Alaska & Hawaii)
# - clipped to US EEZ

librarian::shelf(
  dplyr, geojsonsf, glue, mapview, mregions, rgdal,
  rnaturalearth, sf, stringr, units)
select = dplyr::select
mapviewOptions(fgb = F)

# BOEM program areas ----

# BOEM: “2019-2024 Draft Proposed Program Areas”
dir_data   <- "/Users/bbest/My Drive/projects/offhab/data"
dir_bareas <- glue("{dir_data}/marinecadastre.gov/2019-2024 Draft Proposed Program Areas")

ak  <- read_sf(glue("{dir_bareas}/ak-5yr-2019-2024_dir/2019-2024DPPAreas_AK.shp"))
atl <- read_sf(glue("{dir_bareas}/atl-5yr-2019-2024_dir/2019-2024DPPAreas_Atlantic.shp"))
gom <- read_sf(glue("{dir_bareas}/Gom_5yr_2019_2024_dir/2019-2024DPPAreas_GOM.shp"))
pac <- read_sf(glue("{dir_bareas}/pac-5yr-2019-2024_dir/2019-2024DPPAreas_Pac.shp"))

# librarian::shelf(
#   ggplot2,
#   # ggrepel, rnaturalearth, rnaturalearthdata,
#   # ropensci/rnaturalearthhires,
#   purrr, stringr, tidyr)

sf::sf_use_s2(FALSE) # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
bareas <- rbind(
  atl,
  gom %>%
    select(-(setdiff(names(gom), names(atl)))),
  pac) %>%
  st_transform(4326)
# mapview(bareas, col.regions = "blue")

# EEZ ----
us_ids <- mr_names("MarineRegions:eez") %>%
  filter(str_detect(geoname, "United States")) %>%
  select(id, geoname)
# us_ids
# 1 eez.265 Joint regime area United States / Russia
# 2 eez.271 United States Exclusive Economic Zone (Hawaii)
# 3 eez.281 United States Exclusive Economic Zone
# 4 eez.276 United States Exclusive Economic Zone (Alaska)

us_eez <- mr_features_get(
  type      = "MarineRegions:eez",
  featureID = "eez.281") %>%
  geojson_sf()
# mapview(us_eez, col.regions = "brown") +
#   mapview(bareas, col.regions = "blue")

# oh_zones: bareas - us_eez
oh_zones <- bareas %>%
  mutate(
    zone_key  = tolower(MMS_PLAN_A),
    zone_name = RESA_summa,
    region    = recode(
      MMS_REGION,
      A = "Atlantic",
      G = "Gulf of Mexico",
      P = "Pacific"),
    area_km2  = st_area(geometry) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  select(
    zone_key, zone_name, region, area_km2, geom = geometry) %>%
  st_intersection(
    us_eez %>%
      select(geometry))
# mapview(oh_zones)

usethis::use_data(oh_zones, overwrite = TRUE)

# write to pg db
devtools::load_all()
con <- oh_pg_con()
st_write(
  oh_zones, con, "oh_zones", delete_layer=T)

# add zone_id for referencing rasters ----
oh_zones <- st_read(con, "oh_zones") %>%
  arrange(zone_key) %>%
  tibble::rowid_to_column("zone_id")
st_write(
  oh_zones, con, "oh_zones", delete_layer=T)
create_index(con, "oh_zones", "geom", geom=T)
create_index(con, "oh_zones", "zone_id", unique=T)
create_index(con, "oh_zones", "zone_key", unique=T)
usethis::use_data(oh_zones, overwrite = TRUE)

# create simplified version for faster rendering of smaller output files
oh_zones_s1k <- oh_zones %>%
  sf::st_simplify(T, 1000)
usethis::use_data(oh_zones_s1k, overwrite = TRUE)

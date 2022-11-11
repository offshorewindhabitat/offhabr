# code to prepare BOEM Wind Energy Areas (`boem_blocks`) based on:
# - [Renewable Energy GIS Data | Bureau of Ocean Energy Management](https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data)
#   - BOEMWindLeases_6_30_2022: 3282
#   - BOEMWindPlanningAreaOutlines_7_26_2022: 5563

librarian::shelf(
  dplyr, geojsonsf, glue, mapview, mregions, rgdal,
  rnaturalearth, sf, stringr, units)
devtools::load_all()
select = dplyr::select
mapviewOptions(fgb = F)

dir_data <- "/Users/bbest/My Drive/projects/offhab/data"
gdb <- glue("{dir_data}/boem.gov/BOEMWindLayers_4Download.gdb")

lyrs <- rgdal::ogrListLayers(gdb)
# lyrs
# [1] "BOEM_MHKLeasesandPlanningAreas"
# [2] "BOEMWindLeases_6_30_2022"
# [3] "BOEMWindPlanningAreas_07_26_22"
# [4] "BOEMWindPlanningAreaOutlines_7_26_2022"
# [5] "BOEMWindLeaseOutlines_6_30_2022"
# mapview(read_sf(gdb, lyrs[1]), layer.name = lyrs[1]) +
#   mapview(read_sf(gdb, lyrs[2]), layer.name = lyrs[2]) +
#   mapview(read_sf(gdb, lyrs[3]), layer.name = lyrs[3]) +
#   mapview(read_sf(gdb, lyrs[4]), layer.name = lyrs[4]) +
#   mapview(read_sf(gdb, lyrs[5]), layer.name = lyrs[5])

lse <- read_sf(gdb, lyrs[2])
# table(lse$LEASE_TYPE)
# Commercial           Research Right-of-Way Grant
#       3282                  7                  1
lse <- lse %>%
  select(-LEASE_NUMBER_COMPANY) %>%
  filter(LEASE_TYPE == "Commercial")
# mapview(boem_lse)

pln <- read_sf(gdb, lyrs[3])

intersect(names(pln), names(lse))
# [1] "PROTRACTION_NUMBER" "BLOCK_NUMBER"       "BLOCK_LABEL"        "SUB_BLOCK"
# [5] "Shape_Length"       "Shape_Area"         "Shape"

flds_pln_not_lse <- setdiff(names(pln), names(lse))
# [1] "ADDITIONAL_INFORMATION" "CATEGORY1"              "CATEGORY2"
# [4] "URL1"                   "URL2"

flds_lse_not_pln <- setdiff(names(lse), names(pln))
# [1] "LEASE_NUMBER"     "LEASE_TYPE"       "RESOURCE"         "COMPANY"
# [5] "LEASE_DATE"       "LEASE_TERM"       "PROJECT_EASEMENT" "STATE"
# [9] "PROTRACTION_NAME" "LEASE_DOCUMENT1"  "LEASE_DOCUMENT2"

pln <- pln %>%
  rename_with(
    .fn   = ~ paste0("plan_", .x),
    .cols = all_of(flds_pln_not_lse))

lse <- lse %>%
  rename_with(
    .fn   = ~ paste0("lease_", .x),
    .cols = all_of(flds_lse_not_pln))

boem_blocks <- bind_rows(
  lse %>%
    mutate(
      block_type = "lease"),
  pln %>%
    mutate(
      block_type = "plan")) %>%
  select(-Shape_Length, -Shape_Area) %>%
  rename(
    geom = Shape) %>%
  filter(
    plan_CATEGORY1 != "Gulf of Mexico Call Area") %>%
  mutate(
    block_key = glue("{PROTRACTION_NUMBER}_{BLOCK_NUMBER}"),
    area_km2  = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  st_join(
    oh_zones %>%
      select(zone_key, geom))
# mapview(boem_blocks, zcol = "zone_key")

boem_blocks <- boem_blocks %>%
  relocate(block_key, block_type, zone_key) %>%
  relocate(
    all_of(colnames(boem_blocks) %>% str_subset("^lease_")),
    .after = last_col()) %>%
  relocate(
    all_of(colnames(boem_blocks) %>% str_subset("^plan_")),
    .after = last_col()) %>%
  relocate(
    area_km2, geom,
    .after = last_col())
# colnames(boem_blocks)

# mapview(boem_blocks)
usethis::use_data(boem_blocks, overwrite = TRUE)

# write to pg db
devtools::load_all()
con <- oh_pg_con()
st_write(
  boem_blocks, con, "boem_blocks",
  layer_options = c(
    # https://gdal.org/drivers/vector/pg.html#layer-creation-options
    "OVERWRITE=yes", "LAUNDER=true"))
dbSendQuery(
  con,
  "CREATE INDEX IF NOT EXISTS boem_blocks_geom_idx ON boem_blocks USING GIST (geom);")




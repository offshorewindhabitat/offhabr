# code to prepare Offshore Habitat Zones (`oh_zones`) based on:
# - BOEM “2019-2024 Draft Proposed Program Areas”
# - for lower-48 (excluding Alaska & Hawaii)
# - clipped to US EEZ
librarian::shelf(
  devtools, dplyr, geojsonsf, glue, here, mapview, mregions, rgdal,
  rnaturalearth, sf, stringr, tibble, units)
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

# intersect with OceanAdapt Regions, so in vs out per Zone  ----
load_all()
oa_rgns_geo <- here("data-raw/datasets/oa_regions.geojson")
con         <- oh_pg_con()

# mapView(oh_zones) +
#   mapView(oa_rgns)

# merge seasons
# sort(oa_rgns$oa_region)
# "Northeast": "Northeast US Fall", "Northeast US Spring"
# "Southeast": "Southeast US Fall", "Southeast US Spring", "Southeast US Summer"

oa_rgns <- read_sf(oa_rgns_geo) %>%
  filter(
    oa_region %>% str_detect("Northeast", negate=T),
    oa_region %>% str_detect("Southeast", negate=T)) %>%
  mutate(
    oa_region = recode(
      oa_region,
      "West Coast Annual 2003-Present" = "West Coast"),
    oa_rgn = recode(
      oa_rgn,
      "wca" = "wc",
      "gmx" = "gm")) %>%
  bind_rows(
    oa_rgns %>%
      filter(
        oa_region %>% str_detect("Northeast")) %>%
      summarize(
        oa_region_rds = paste(oa_region_rds, collapse = "; ")) %>%
      mutate(
        oa_rgn    = "ne",
        oa_region = "Northeast"),
    oa_rgns %>%
      filter(
        oa_region %>% str_detect("Southeast")) %>%
      summarize(
        oa_region_rds = paste(oa_region_rds, collapse = "; ")) %>%
      mutate(
        oa_rgn    = "se",
        oa_region = "Southeast") )

zr_i <- st_intersection(oh_zones, oa_rgns)
zr_d <- st_difference(oh_zones, st_union(oa_rgns))

oh_zones <- bind_rows(zr_i, zr_d) %>%
  rename(
    oa_key    = oa_rgn,
    boem_key  = zone_key,
    boem_area = zone_name) %>%
  mutate(
    zone_key  = glue("{boem_key}-{ifelse(is.na(oa_key), 'na', oa_key)}"),
    zone_name = glue("{boem_area} - {ifelse(is.na(oa_region), 'Not Available', oa_region)}"),
    area_km2 = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units()) %>%
  relocate(
    zone_key, zone_name, region, ) %>%
  relocate(
    area_km2, .before = geom) %>%
  arrange(zone_key) %>%
  select(-zone_id) %>%
  rowid_to_column("zone_id")
# mapView(zr_id, zcol="zone_key")

# versions for with and without OA ----
load_all()
# mapView(offhabr::oh_zones)
# offhabr::oh_zones %>%
#   colnames() %>% paste(collapse = ", ")
# zone_id, zone_key, zone_name, region, boem_key, boem_area, oa_key, oa_region, oa_region_rds, area_km2, geom
# offhabr::oh_zones %>%
#   st_drop_geometry() %>%
#   View()

oh_zones_v1 <- offhabr::oh_zones %>%
  group_by(region, boem_key, boem_area) %>%
  summarize(.groups = "drop") %>%
  mutate(
    zone_version = 1,
    zone_key     = boem_key,
    zone_name    = boem_area) %>%
  arrange(zone_key) %>%
  select(zone_version, zone_key, zone_name, region, boem_key, boem_area)
# mapView(oh_zones_v1)
oh_zones_v2 <- offhabr::oh_zones %>%
  filter(!is.na(oa_key)) %>%
  mutate(
    zone_version = 2) %>%
  relocate(zone_version) %>%
  select(-zone_id)
oh_zones <- bind_rows(
  oh_zones_v1,
  oh_zones_v2) %>%
  rowid_to_column("zone_id") %>%
  mutate(
    geom     = st_make_valid(geom),
    area_km2 = st_area(geom) %>%
      set_units(km^2) %>%
      drop_units())

# write to database and R package ----
# st_write(
#   oh_zones, con, "oh_zones", delete_layer=T)
# create_index(con, "oh_zones", "geom", geom=T)
# create_index(con, "oh_zones", "zone_id", unique=T)
# create_index(con, "oh_zones", "zone_key", unique=T)
usethis::use_data(oh_zones, overwrite = TRUE)

# document in R/data.R:
#   colnames(oh_zones) %>% paste(collapse="}{}\n#'   \\item{") %>% cat()

# create simplified version for faster rendering of smaller output files
oh_zones_s1k <- oh_zones %>%
  sf::st_simplify(T, 1000)
usethis::use_data(oh_zones_s1k, overwrite = TRUE)


# colors for zones and regions ----
librarian::shelf(
  dplyr, here, RColorBrewer, readr, scales, sf)
devtools::load_all()
# three regions (A,B,C), with a start(0), end(1) and color separation between (|)
#  0, A, 1, |, 0, B, 1, |, 0, C, 1
#  1, 2, 3, 4, 5, 6, 7, 9,10,11,12
cols_all <- scales::colour_ramp(brewer.pal(11, "Spectral"))(seq(0,1, length=12))
# show_col(cols_all)

rgns <- oh_zones_s1k |>
  filter(zone_version == 1) |>
  pull(region) |> sort() |> unique()

cols_rgns <- cols_all[c(2,6,11)]
names(cols_rgns) <- rgns
# show_col(cols_rgns)

oh_zns <- oh_zones_s1k |>
  filter(zone_version == 1)

zone_colors_csv <- here("data-raw/zone_colors.csv")
if (!file.exists(zone_colors_csv)){
  oh_zns |>
    st_drop_geometry() |>
    arrange(region, zone_name) |>
    select(region, zone_name) |>
    write_csv(zone_colors_csv)
  # manually add color_rank column from NAtlantic down to EGomex to SCal to WA
}
d_zon_col_rnk <-  read_csv(zone_colors_csv)

d_cols_zons <- d_zon_col_rnk |>
  mutate(
    color = map2_chr(
      region, zone_name,
      function(rgn, zon){

        # get rank of color for zone within region
        rnk <- d_zon_col_rnk |>
          filter(zone_name == zon) |>
          pull(color_rank)

        # get color ranks of all zones within region
        rnks_rgn <- d_zon_col_rnk |>
          filter(region == rgn) |>
          pull(color_rank)

        # get index of region color to get start(0) and end(1) colors mentioned above
        i_col_rgn <- which(cols_rgns[rgn] == cols_all)

        # get color ramp based on region's start(0) and end(1) colors
        ramp_rgn <- colour_ramp(
          cols_all[(i_col_rgn - 1):(i_col_rgn + 1)])

        # get colors for all zones in region
        cols_rgn <- ramp_rgn(seq(0, 1, length = length(rnks_rgn)))

        # get color for specified zone
        cols_rgn[rnk] }))

cols_zons <- setNames(d_cols_zons$color, d_cols_zons$zone_name)

oh_colors <- c(cols_rgns, cols_zons)
usethis::use_data(oh_colors, overwrite = TRUE)

# make oh_regions.tif ----
rgns_tif <- here("inst/oh_regions.tif")

v_rgns <- c("Atlantic","Gulf of Mexico", "Pacific")
d_z2r <- oh_zones_s1k |>
  st_drop_geometry() |>
  filter(zone_version == 1) |>
  mutate(
    rgn_id = match(region, v_rgns)) |>
  select(zone_id, rgn_id)
r_z <- oh_rast("zone_id")
r_rgns <- classify(r_z, d_z2r)
levels(r_rgns) <- tibble(
  id     = 1:length(v_rgns),
  region = v_rgns)
# plot(r_rgn)
names(r_rgns) <- "region"
# tmp_tif <- tempfile(fileext = ".tif")
# writeRaster(
#   r_rgns, tmp_tif,
#   datatype  = "INT1U", overwrite = T)
# file_move(tmp_tif, rgns_tif)
writeRaster(
  r_rgns, rgns_tif,
  datatype  = "INT1U", overwrite = T)

# try
# r_rgns <- rast(rgns_tif)
# plot(r_rgns)


# make oh_regions_web.tif ----
rgns_web_tif <- here("inst/oh_regions_web.tif")
r_z <- oh_rast("zone_id", web_version=T)
r_rgns_web <- classify(r_z, d_z2r)
levels(r_rgns_web) <- tibble(
  id     = 1:length(v_rgns),
  region = v_rgns)
# plot(r_rgns)
names(r_rgns_web) <- "region"
# tmp_tif <- tempfile(fileext = ".tif")
# writeRaster(
#   r_rgns, tmp_tif,
#   datatype  = "INT1U", overwrite = T)
# file_move(tmp_tif, rgns_tif)
write_rast(
  r_rgns_web, rgns_web_tif,
  datatype="INT1U", overwrite=T,
  use_gdal_cog = T)

# create study area mask feature class
oh_study_v1 <- oh_zones |>
  filter(zone_version == 1) |>
  select(geom) |>
  st_union()
# mapview::mapview(oh_study_v1)
oh_study_v1 |>
  sf::write_sf(here("inst/oh_study_v1.geojson"))
oh_study_v1 |>
  sf::write_sf(here("inst/oh_study_v1.shp"))
usethis::use_data(oh_study_v1)

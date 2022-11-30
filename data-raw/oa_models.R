# OceanAdapt models (Pinsky et al) of bottom trawl data
# starting with https://github.com/pinskylab/OceanAdapt
#
# that was ported to [NOAA DisMap portal](https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html)
#
# ported from script for old BOEM regions in offhab-scripts/interpolate_bottom-trawl.Rmd

# * libraries ----
librarian::shelf(
  arcpullr, devtools, dplyr, dplyr, fs, glue, here, ggplot2, gstat, leaflet,
  mapview, multidplyr, purrr, raster, readr, sf, stringr, terra, tibble)
select = dplyr::select
options(readr.show_col_types = F)

# * paths ----
dm_server     <-"https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/"
dm_lyr           <- "DisMAP_Regions_20220516/FeatureServer/0"
dir_tif       <- "~/My Drive/projects/offhab/data/oceanadapt.rutgers.edu/data_tif"
dir_tmp       <- "~/My Drive/projects/offhab/data/oceanadapt.rutgers.edu/tmp"
dir_oa        <- "~/Github/ecoquants/OceanAdapt"
d_spp_csv     <- path(dir_tif, "_spp.csv")
d_spp_rgn_csv <- path(dir_tif, "_spp_rgn.csv")

# get OceanAdapt (OA) regions from DisMap (DM) ArcGIS server ----
librarian::shelf(
  )
load_all()

oa_rgns_geo <- here("data-raw/oa_regions.geojson")

dm_lyr_url <- paste0(dm_server, dm_lyr)
d_lyr <- get_spatial_layer(dm_lyr_url) # table(d_lyr$OARegion)
table(d_lyr$OARegion)
#    Aleutian Islands             Eastern Bering Sea                 Gulf of Alaska                 Gulf of Mexico
#                 266                              7                          19415                              4
#   Northeast US Fall            Northeast US Spring              Southeast US Fall            Southeast US Spring
#                   1                              1                              1                              1
# Southeast US Summer West Coast Annual 2003-Present West Coast Triennial 1977-2004
#                   1                             35                              5

# skipping: Aleutian Islands, Eastern Bering Sea, Gulf of Alaska
oa_rgns <- c(
  gmx = "Gulf of Mexico",
  nef = "Northeast US Fall",
  nes = "Northeast US Spring",
  sef = "Southeast US Fall",
  ses = "Southeast US Spring",
  seu = "Southeast US Summer",
  wca = "West Coast Annual 2003-Present") # "West Coast Annual",
  # wct = "West Coast Triennial 1977-2004") # West Coast Triennial")
# region names used in stored rds
oa_rgns_rds <- c(
  oa_rgns[!names(oa_rgns) %in% c("wca","wct")],
  wca = "West Coast Annual")
  # wct = "West Coast Triennial") # TODO: skip since only 1977-2004

ply_oa_rgns <- d_lyr %>%
  rename(oa_region = OARegion) %>%
  filter(oa_region %in% oa_rgns) %>%
  group_by(oa_region) %>%
  summarize(
    geometry = st_union(geoms, is_coverage = T) %>%
      st_cast("POLYGON"),
    .groups = "drop") %>%
  st_as_sf(crs = 4326) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 1) %>% # 1 meter
  left_join(
    tibble(
      oa_rgn        = names(oa_rgns),
      oa_region     = oa_rgns,
      oa_region_rds = oa_rgns_rds[names(oa_rgns)]),
    by = "oa_region") %>%
  relocate(oa_rgn) %>%
  relocate(oa_region_rds, .after = oa_region)
stopifnot(length(setdiff(oa_rgns, ply_oa_rgns$oa_region))==0)

# mapview(ply_oa_rgns)

write_sf(ply_oa_rgns, oa_rgns_geo, delete_dsn = T)

# interpolate biomass data [over OA regions intersected with OffHab (OH) zones] ----

# * helper functions to process region - species - year ----
get_rgn_sp <- function(sp_key){
  d_sp <- d_spp_rgn %>%
    filter(
      region %in% ply_mer$oa_region_rds,
      sp_key == !!sp_key)
  # message(glue("  SPECIES {j} of {length(sp_keys)}: {sp_key} ({d_sp$sp_sci}) {d_sp$sp_cmn} ~ "), "\n")
  message(glue("  SPECIES {sp_key} ~ {Sys.time()}"), "\n")

  # ** species' most recent year ----
  yrs <- pts_mer %>%
    st_drop_geometry() %>%
    tibble() %>%
    filter(sp_key == !!sp_key) %>%
    arrange(year) %>%
    pull(year) %>%
    unique()

  if (length(yrs) == 0){
    return("SKIP: why zero yrs?!")
  }

  yr = max(yrs)
  message(glue("    YEAR: {yr}"))

  r_yrs_tif <- glue("{dir_tif}/{ply_mer$oa_rgn}_{sp_key}_{yr}.tif")

  if (!file.exists(r_yrs_tif)){
    message(glue("    TRYING: {basename(r_yrs_tif)}"))

    yrs <- (yr-2):(yr+2)
    wts <- 3 - (abs(yr - yrs)) # 1 2 3 2 1
    lst <- sapply(
      yrs, function(yr){
        get_sp_yr(sp_key, yr) })

    idx <- !is.na(lst)
    if (sum(idx) == 0){
      return("SKIP: no data?!")
    }
    message(glue("    WRITING: {basename(r_yrs_tif)}"))
    stk   <- raster::stack(lst[idx])
    r_yrs <- raster::weighted.mean(stk, wts[idx], na.rm=F)
    # mapview(r_yrs)
    raster:: writeRaster(r_yrs, r_yrs_tif)
    return(glue("WROTE: {basename(r_yrs_tif)}"))
  }
  return(glue("SKIP: exists {basename(r_yrs_tif)}"))
}
get_sp_yr <- function(sp_key, yr){
  # sp_key="ach-spi1"; yr=2019

  r_yr_tif <- glue("{dir_tmp}/{ply_mer$oa_rgn}_{sp_key}_{yr}-1yr.tif")

  if (!file.exists(r_yr_tif)){

    p <- pts_mer %>%
      filter(
        sp_key  == !!sp_key,
        year    == !!yr)

    if (nrow(p) == 0){
      message(glue("    unavailable: {ply_mer$oa_rgn}_{sp_key} for {yr}"))
      return(NA)
    }

    if (nrow(p %>% filter(wtcpue_cbrt > 0)) == 0){
      message(glue("    all zeros: {ply_mer$oa_rgn}_{sp_key} for {yr}"))
      return(NA)
    }

    message(glue("    writing: {basename(r_yr_tif)}"))

    mdl <- gstat::gstat(
      formula = wtcpue_cbrt ~ 1, locations = p,
      nmin = 10, nmax = 15, maxdist = 200000, set = list(idp = 1))

    r_yr <- raster::interpolate(r_cid, mdl) %>%
      raster::mask(r_cid) # mapview::mapview(r_yr)
    # plot(r_yr)

    raster::writeRaster(r_yr, r_yr_tif)
  }
  raster::raster(r_yr_tif)
}

# * get species list ----
if (any(!file.exists(d_spp_rgn_csv, d_spp_csv))){

  d_spp_rgn_0_csv <- here("data_clean/spplist.csv")

  d_spp_rgn <- read_csv(d_spp_rgn_0_csv) %>%
    rename(
      sp_sci = spp,
      sp_cmn = common)

  # * get unique species key `sp_key` ----
  sp_sci2key <- function(spp){
    gs <- str_split(spp, "\\W")[[1]][1:2]
    g <- str_sub(gs[1], end=3)
    s <- str_sub(gs[2], end=3)
    str_to_lower(glue("{g}-{s}"))
  }

  d_spp <- d_spp_rgn %>%
    group_by(sp_sci) %>%
    summarise(sp_cmn = first(sp_cmn)) %>%
    arrange(sp_sci, sp_cmn) %>%
    mutate(
      sp_key = map_chr(sp_sci, sp_sci2key))

  sp_keys_dup <- d_spp$sp_key[duplicated(d_spp$sp_key)]

  d_spp <- bind_rows(
    d_spp %>%
      filter(!sp_key %in% sp_keys_dup),
    d_spp %>%
      filter(sp_key %in% sp_keys_dup) %>%
      group_by(sp_key) %>%
      mutate(
        i = row_number(),
        sp_key2 = glue("{sp_key}{i}")) %>%
      select(
        sp_sci, sp_cmn, sp_key = sp_key2) ) %>%
    select(sp_key, sp_sci, sp_cmn) %>%
    arrange(sp_key, sp_sci)

  d_spp_rgn <- d_spp_rgn %>%
    left_join(
      d_spp %>%
        select(sp_sci, sp_key),
      by = "sp_sci")

  write_csv(d_spp, d_spp_csv)
  write_csv(d_spp_rgn, d_spp_rgn_csv)
}

d_spp     <- read_csv(d_spp_csv)
d_spp_rgn <- read_csv(d_spp_rgn_csv)

# iterate over OA regions ----
ply_oa_rgns <- read_sf(oa_rgns_geo)

for (i in 1:nrow(ply_oa_rgns)){ # i = 7
  ply <- slice(ply_oa_rgns, i)
  message(glue("REGION {i} of {nrow(ply_oa_rgns)}: {ply$oa_region} ({ply$oa_rgn}) ~ {Sys.time()}"))

  pts_rds <- glue("{dir_oa}/data_clean/dat_exploded{ply$oa_region_rds}.rds")
  stopifnot(file_exists(pts_rds))
  pts <- readRDS(pts_rds) %>%
    tibble()  %>% # gmx: 12,921,639 × 11
    mutate(
      year        = as.integer(year),
      wtcpue_cbrt = wtcpue^(1/3)) %>% # take the cube root
    rename(
      sp_sci = spp) %>%
    left_join(
      d_spp %>%
        select(sp_sci, sp_key),
      by = "sp_sci") %>%
    st_as_sf(
      coords = c("lon", "lat"), remove = T, crs = 4326) %>%
    select(
      year, sp_key, wtcpue_cbrt)  %>%
    filter(!is.na(sp_key)) # gmx nrow: 12,921,639 -> 2,525,473

  # * project to mercator ----
  pts_mer <- st_transform(pts, 3857)
  ply_mer <- ply %>%
    st_transform(3857) %>%
    mutate(one = 1)
  rm(pts, ply) # save memory

  # * setup regional raster for interpolating into with OffHab cell_id ----
  rgn_cid_tif <- glue("{dir_tif}/{ply_mer$oa_rgn}_cell_id.tif")
  if (!file.exists(rgn_cid_tif)){
    r_cid <- oh_rast("cell_id")
    r_rgn <- rasterize(ply_mer, r_cid, "one") %>%
      mask(r_cid)
    r_cid <- r_cid %>%
      mask(r_rgn) %>%
      trim()
    writeRaster(r_cid, rgn_cid_tif)
  }
  r_cid <- raster::raster(rgn_cid_tif)
  # plot(r_cid)
  # mapView(r_cid)

  # * iterate over species ----
  sp_keys <- d_spp_rgn %>%
    filter(
      !flagged,
      sp_key != "na-na",
      region %in% ply_mer$oa_region_rds) %>%
    distinct(sp_key) %>%
    arrange(sp_key) %>%
    pull(sp_key)

  # sp_keys_0 <- sp_keys
  # sp_keys <- sp_keys_0[127:length(sp_keys)]

  cl <- new_cluster(parallel::detectCores() - 2)
  cluster_library(
    cl, c(
      "dplyr", "glue", "gstat", "purrr", "raster", "sf", "terra"))
  cluster_assign(
    cl,
    dir_tif    = dir_tif,
    dir_tmp    = dir_tmp,
    get_rgn_sp = get_rgn_sp,
    get_sp_yr  = get_sp_yr,
    d_spp_rgn  = d_spp_rgn,
    ply_mer    = ply_mer,
    pts_mer    = pts_mer,
    r_cid      = r_cid)

  d_rgn_spp <- tibble(
    oa_rgn = ply_mer$oa_rgn,
    sp_key = sp_keys) %>%
    group_by(oa_rgn, sp_key) %>%
    partition(cl) %>%
    mutate(
      status = map_chr(sp_key, get_rgn_sp))

}

# d_lst <- lapply(
#   dmrgns, function(dmrgn){ # dmrgn = dmrgns[1]
#     dmregion_rds <- dmregions_rds[[dmrgn]]
#
#     stopifnot(file_exists(path_rds))
#     d <- readRDS(path_rds) })
# d_dmrgn <- bind_rows(d_lst)


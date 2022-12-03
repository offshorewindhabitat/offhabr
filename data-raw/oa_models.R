# OceanAdapt models (Pinsky et al) of bottom trawl data
# starting with https://github.com/pinskylab/OceanAdapt
#
# that was ported to [NOAA DisMap portal](https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html)
#
# ported original [offhab-scripts: interpolate\_bottom-trawl.Rmd](https://github.com/ecoquants/offhab-scripts/blob/cd6b2e4ff667cd81d3399b2e9538dcf5494ac9c3/interpolate_bottom-trawl.Rmd?h=1) into `data-raw/oa_models.R` with `multidplyr` parallel processing to speed up raster generation of interpolated biomass survey points.

# libraries & paths ----
librarian::shelf(
  arcpullr, devtools, dplyr, dplyr, fs, glue, here, ggplot2, gstat, janitor,
  leaflet, mapview, multidplyr, purrr, raster, readr, sf, stringr, terra,
  tibble, worrms)
select = dplyr::select
options(readr.show_col_types = F)
load_all()
con <- oh_pg_con()

# paths
dm_server     <-"https://services2.arcgis.com/C8EMgrsFcRFL6LrL/ArcGIS/rest/services/"
dm_lyr        <- "DisMAP_Regions_20220516/FeatureServer/0"
oa_rgns_geo   <- here("data-raw/oa_regions.geojson")
dir_tif       <- "~/My Drive/projects/offhab/data/oceanadapt.rutgers.edu/tif"
dir_tif_1yr   <- "~/My Drive/projects/offhab/data/oceanadapt.rutgers.edu/tif_1yr"
dir_oa        <- "~/Github/ecoquants/OceanAdapt"
spp_csv       <- path(dir_tif, "_spp.csv")
spp_rgn_csv   <- path(dir_tif, "_spp_rgn.csv")
spp_wm_csv    <- path(dir_tif, "_spp_worms.csv")
oa_mdls_r_csv <- here("data-raw/oa_models_rast.csv")
log_csv       <- here("data-raw/oa_models_log.csv")

# get OceanAdapt (OA) regions from DisMap (DM) ArcGIS server ----
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
    writeRaster(r_yrs, r_yrs_tif)
    return(glue("WROTE: {basename(r_yrs_tif)}"))
  }
  return(glue("SKIP: exists {basename(r_yrs_tif)}"))
}
get_sp_yr <- function(sp_key, yr){
  # sp_key="ach-spi1"; yr=2019

  r_yr_tif <- glue("{dir_tif_1yr}/{ply_mer$oa_rgn}_{sp_key}_{yr}-1yr.tif")

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

sp_sci2key <- function(spp){
  gs <- str_split(spp, "\\W")[[1]][1:2]
  g <- str_sub(gs[1], end=3)
  s <- str_sub(gs[2], end=3)
  str_to_lower(glue("{g}-{s}"))
}

# * get species list ----
if (any(!file.exists(spp_rgn_csv, spp_csv))){

  d_spp_rgn_0_csv <- here("data_clean/spplist.csv")

  d_spp_rgn <- read_csv(d_spp_rgn_0_csv) %>%
    rename(
      sp_sci = spp,
      sp_cmn = common)

  # * get unique species key `sp_key` ----
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

  write_csv(d_spp, spp_csv)
  write_csv(d_spp_rgn, spp_rgn_csv)
}

d_spp     <- read_csv(spp_csv)
d_spp_rgn <- read_csv(spp_rgn_csv)

# * iterate over OA regions ----
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

  # ** project to mercator ----
  pts_mer <- st_transform(pts, 3857)
  ply_mer <- ply %>%
    st_transform(3857) %>%
    mutate(one = 1)
  rm(pts, ply) # save memory

  # ** setup regional raster for interpolating into with OH cell_id ----
  rgn_cid_tif <- glue("{dir_tif}/{ply_mer$oa_rgn}_cell_id.tif")
  if (!file.exists(rgn_cid_tif)){
    r_cid <- oh_rast("cell_id")
    r_rgn <- rasterize(ply_mer, r_cid, "one") %>%
      mask(r_cid)
    r_cid <- r_cid %>%
      mask(r_rgn) %>%
      trim()
    writeRaster(r_cid, rgn_cid_tif, datatype="INT4U", overwrite=T)
  }
  r_cid <- raster::raster(rgn_cid_tif)
  # plot(r_cid)
  # mapView(r_cid)

  # ** iterate over species in region using `multidplyr::*cluster*()` ----
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
    dir_tif     = dir_tif,
    dir_tif_1yr = dir_tif_1yr,
    get_rgn_sp  = get_rgn_sp,
    get_sp_yr   = get_sp_yr,
    d_spp_rgn   = d_spp_rgn,
    ply_mer     = ply_mer,
    pts_mer     = pts_mer,
    r_cid       = r_cid)

  d_rgn_spp <- tibble(
    oa_rgn = ply_mer$oa_rgn,
    sp_key = sp_keys) %>%
    group_by(oa_rgn, sp_key) %>%
    partition(cl) %>%
    mutate(
      status = map_chr(sp_key, get_rgn_sp))

}

# cleanup big memory objects
rm(pts_mer, d)
gc()

# load taxa and models into db ----

# db connection
con <- oh_pg_con()

# get previously fetched species info
#   see [offhab-scripts: interpolate_bottom-trawl.Rmd](https://github.com/ecoquants/offhab-scripts/blob/cd6b2e4ff667cd81d3399b2e9538dcf5494ac9c3/interpolate_bottom-trawl.Rmd?h=1)
d_spp    <- read_csv(spp_csv)
d_spp_wm <- read_csv(spp_wm_csv) %>%
  clean_names()

# inject unique taxa_wm not already in db
aphia_ids <- tbl(con, "taxa_wm") %>%
  pull(aphia_id)
tbl_taxa_wm <- d_spp_wm %>%
  select(-sp_key, -sp_sci, -sp_cmn)  %>%
  filter(
    !aphia_id %in% aphia_ids,
    !duplicated(aphia_id))
dbAppendTable(con, "taxa_wm", tbl_taxa_wm)

# TODO: deal with duplicate aphia_id in tifs?

# get all tifs
rx <- "(.+)_(.+)_(.+)\\.tif$"
d_mdls <- tibble(
  base_tif = setdiff(
    list.files(dir_tif, rx),
    list.files(dir_tif, "(.+)_cell_id\\.tif$")),
  path_tif = path_norm(path(dir_tif, base_tif))) %>%
  mutate(
    oa_rgn = str_replace(base_tif, rx, "\\1"),
    sp_key = str_replace(base_tif, rx, "\\2"),
    yr     = str_replace(base_tif, rx, "\\3") %>%
      as.integer()) %>% # nrow: 759
  left_join(
    d_spp_wm, by = "sp_key") %>%
  arrange(oa_rgn, sp_key, yr) %>%
  rowid_to_column("mdl_id")

# write overview table of models in dataset
tbl_oa_models <- d_mdls %>%
  select(mdl_id, aphia_id, oa_rgn, sp_key, sp_sci, sp_cmn, yr)

dbWriteTable(con, "oa_models", tbl_oa_models, overwrite = T)
create_index(con, "oa_models", "mdl_id", unique = T)
create_index(con, "oa_models", "aphia_id")

# get biomass values in rasters, plus OffHab cell_id
d_mdls_rast <- d_mdls %>%
  select(
    mdl_id, mdl_tif = path_tif, oa_rgn) %>%
  arrange(mdl_id) %>%
  mutate(
    cid_tif       = glue("{dir_tif}/{oa_rgn}_cell_id.tif"),
    d_cid_biomass = pmap(list(mdl_id, cid_tif, mdl_tif), ~{
      # message every 20 models
      if (..1 == 1 | ..1 %% 20 == 0)
        message(glue(
          "{..1} of {length(mdl_id)}: {basename(..3)} ~ {Sys.time()}", .trim=F))
      rast(c(..2, ..3)) %>%
        values(dataframe=T, na.rm=T) %>%
        tibble() }) )

# memory exhausted with unnest() for all, so inject models individually
for (i in 1:nrow(d_mdls_rast)){ # i = 1
  # message every 10 models
  if (i == 1 | i %% 10 == 0)
    message(glue("{i} of {nrow(d_mdls_rast)} ~ {Sys.time()}"))
  # 20 of 759 ~ 2022-11-30 10:55:59
  # 80 of 759 ~ 2022-11-30 10:58:56
  d_i <- d_mdls_rast %>%
    slice(i) %>%
    unnest(d_cid_biomass) %>%
    mutate(
      tbl = "oa_models") %>%
    select(
      tbl, row_id = mdl_id, cell_id, density = layer)

  dbAppendTable(con, "oh_cells_rast", d_i)
}

# remove duplicates from oh_cells_rast ----

# MOTIVATION...
# create_index(con, "oh_cells_rast", c("tbl", "row_id", "cell_id"), unique=T) # start
#    DOH! after291 secs ERROR: Key (tbl, row_id, cell_id)=(oa_models, 370, 76894464) is duplicated
# d <- tbl(con, "oh_cells_rast") %>%
#   filter(
#     tbl    == !!tbl,
#     row_id == !!row_id) %>%
#   collect()
# # before indexes: 279.786
# # after  indexes:   0.053
# nrow(d) #   63,077
# d_d <- d %>%
#   distinct(tbl, row_id, cell_id)
# nrow(d_d) # 1,330.827

# * alter/create indexes ----
dbSendQuery(con, "ALTER TABLE oh_cells_rast   ADD COLUMN id      SERIAL PRIMARY KEY")
dbSendQuery(con, "ALTER TABLE oh_cells_rast ALTER COLUMN cell_id TYPE INT4") # 4767.941 sec
dbSendQuery(con, "ALTER TABLE oh_cells_ply ALTER COLUMN cell_id TYPE INT4")
create_index(con, "oh_cells_rast", "tbl")
create_index(con, "oh_cells_rast", "row_id") # 1692.927 sec for tbl & row_id create_index()
create_index(con, "oh_cells_rast", "cell_id")

# * use multidplyr cluster with function per model (tbl, row_id) ----
if (!file.exists(oa_mdls_r_csv)){
  con <- oh_pg_con()
  d_mdls <- tbl(con, "oh_cells_rast") %>%
    group_by(tbl, row_id) %>%
    summarize(.groups = "drop") %>%
    collect() %>%
    rowid_to_column("id")
  write_csv(d_mdls, oa_models_rast_csv)
}
d_mdls <- read_csv(oa_mdls_r_csv)

if (file.exists(log_csv)){
  ids_completed <- read_csv(
    log_csv,
    col_names = c("time", "id", "tbl", "row_id")) %>%
    pull(id)

  d_mdls <- d_mdls %>%
    filter(!id %in% ids_completed)
}

rm_dupes_mdl_cells_rest <- function(tbl, row_id, id, log_csv=NULL){

  con <- oh_pg_con() # cannot pass via cluster_assign()
  DBI::dbSendQuery(
    con,
    glue::glue(
      "DELETE FROM oh_cells_rast
        WHERE id IN
        (SELECT id
          FROM
          (SELECT id,
            ROW_NUMBER() OVER (
              PARTITION BY tbl, row_id, cell_id
              ORDER BY     tbl, row_id, cell_id )
              AS row_num
          FROM oh_cells_rast
          WHERE
            tbl    = '{tbl}' AND
            row_id = {row_id} ) t
         WHERE t.row_num > 1 )")) # 0.409 sec after indexes
  dbDisconnect(con)

  if (!is.null(log_csv))
    tibble(
      time_completed = Sys.time(),
      id     = id,
      tbl    = tbl,
      row_id = row_id) %>%
      write_csv(log_csv, append=T)

  Sys.time() %>% as.character()
}

cl <- new_cluster(parallel::detectCores() - 1)
cluster_library(cl, c(
  "DBI", "dplyr", "glue", "offhabr", "purrr", "readr", "tibble"))
cluster_assign(
  cl,
  log_csv                 = log_csv,
  rm_dupes_mdl_cells_rest = rm_dupes_mdl_cells_rest)

d_mdls_cl <- d_mdls %>%
  partition(cl) %>%
  mutate(
    time_completed = pmap_chr(
      list(tbl=tbl, row_id=row_id, id=id),
      rm_dupes_mdl_cells_rest,
      log_csv=log_csv))

# * create unique index for oh_cells_rast and vacuum db ----
create_index(con, "oh_cells_rast", c("tbl", "row_id", "cell_id"), unique=T, show=T, exec=F)
dbSendQuery(con, "VACUUM FULL ANALYZE")

# test plotting a model output ----
# ORIGINAL ERROR: (tbl, row_id, cell_id)=(oa_models, 370, 76894464) is duplicated
# con <- oh_pg_con()
# oa_rgns_redo <- c('sef','ses','seu','wca')
# d_mdls <- tbl(con, "oa_models") %>%    # nrow: 759
#   filter(oa_rgn %in% oa_rgns_redo) %>% # nrow: 393
#   collect()
# View(d_mdls)
# system.time({
#   d1 <- tbl(con, "oh_cells_rast") %>%
#     filter(
#       tbl    == "oa_models",
#       mdl_id == as.integer(377)) %>%
#     collect()
# })
# # ses oa_rgn:
# #   d369 <- d1
# #   d370 <- d1
# r <- oh_rast ()
# r[d1$cell_id] <- d1$value
# r <- trim(r)
# plot(r)
# # mapView(r, maxpixels=ncell(r))
#
# oa_rgn <- "ses"
# rgn_cid_tif <- glue("{dir_tif}/{oa_rgn}_cell_id.tif")
# r_rgn <- rast(rgn_cid_tif)
# plot(r_rgn)
# v_rgn <- values(r_rgn, dataframe=T, na.rm=T)
# sum(duplicated(v_rgn)) # ses: 53,576

ply_oa_rgns <- read_sf(oa_rgns_geo)
ply_oa_rgns <- ply_oa_rgns %>%
  mutate(
    r = imap(oa_rgn, ~{
      tif <- glue("{dir_tif}/{.x}_cell_id.tif")
      message(glue("r ({.y}): {basename(.x)}"))
      rast(tif) }),
    r_ndupes = imap_int(r, ~{
      message(glue("r_ndupes ({.y})"))
      v <- values(.x, dataframe=T, na.rm=T)
      sum(duplicated(v)) }),
    r_ncell = map_int(r, ~ncell(.) %>% as.integer()))

# ply_oa_rgns %>%
#   st_drop_geometry() %>%
#   select(-r) %>%
#   View()

# redo {oa_rgn}_cell_id.tif so INT4U and no duplicates ----
# duplicates in 4:7: 'sef', 'ses', 'seu', 'wca'
#   where also missing other info

ply_oa_rgns <- read_sf(oa_rgns_geo)
message(glue("dir_tif: {dir_tif}"))
for (i in 1:nrow(ply_oa_rgns)){ # i = 4 # sef

  ply <- slice(ply_oa_rgns, i) %>%
    st_transform(3857) %>%
    mutate(one = 1)

  rgn_cid_tif <- glue("{dir_tif}/{ply$oa_rgn}_cell_id.tif")
  message(glue("{i}: {basename(rgn_cid_tif)}"))

  r_cid <- oh_rast("cell_id")
  # sum(duplicated(values(r_cid, na.rm=T))) # 0
  # range(r_cid) #  1001   105,428,344
  # INT4U max:           2,147,483,647

  r_rgn <- rasterize(ply, r_cid, "one") %>%
    mask(r_cid)
  r_cid <- r_cid %>%
    mask(r_rgn) %>%
    trim()
  # sum(duplicated(values(r_cid, na.rm=T)))  # 0
  # range(r_cid)  # 61,058,191 86,934,347

  writeRaster(r_cid, rgn_cid_tif, datatype="INT4U", overwrite=T)
  # r_cid2 <- rast(rgn_cid_tif)
  # sum(duplicated(values(r_cid2, na.rm=T))) # 0
}

# * redo biomass values from rasters, plus OffHab cell_id ----
# DONE: ∆ oh_cells_rast fld names (manually in DBeaver)
#  tbl     -> mdl_tbl
#  row_id  -> mdl_id
#  density -> value
con <- oh_pg_con()
oa_rgns_redo <- c('sef','ses','seu','wca')

d_mdls <- tbl(con, "oa_models") %>%    # nrow: 759
  filter(oa_rgn %in% oa_rgns_redo) %>% # nrow: 393
  collect()

d_mdls_r <- d_mdls %>%
  select(
    mdl_id, oa_rgn, sp_key, yr) %>%
  arrange(mdl_id) %>%
  mutate(
    mdl_tif   = glue("{dir_tif}/{oa_rgn}_{sp_key}_{yr}.tif"),
    cid_tif   = glue("{dir_tif}/{oa_rgn}_cell_id.tif"),
    d_mdl_cid = pmap(
      list(mdl_id, cid_tif, mdl_tif),
      function(mdl_id, cid_tif, mdl_tif){

        # message every 20 models
        if (mdl_id == d_mdls$mdl_id[1] | mdl_id %% 20 == 0)
          message(glue(
            "d_mdl_cid = ...mdl_id {mdl_id}: {basename(mdl_tif)} ~ {Sys.time()}"))

        # get raster cell_id & layer value, excluding existing cell_ids already set
        rast(c(cid_tif, mdl_tif)) %>%
          values(dataframe=T, na.rm=T) %>%
          tibble()  }))

# memory exhausted with unnest() for all, so inject models individually
insert_rast <- function(mdl_id, d_mdl_cid, log_csv=NULL){
  con <- oh_pg_con()

  # existing cellid's already in db
  in_db <- dbGetQuery(
    con, glue(
      "SELECT cell_id
        FROM oh_cells_rast
        WHERE
          tbl    = 'oa_models' AND
          mdl_id = {mdl_id}"))

  # prep df to inject
  d <- d_mdl_cid %>%
    mutate(
      mdl_id = mdl_id,
      tbl    = "oa_models") %>%
    select(
      tbl, mdl_id, cell_id, value = layer) %>%
    # remove existing cell_id's
    anti_join(
      in_db,
      by = "cell_id")

  if (nrow(d) > 0)
    dbAppendTable(con, "oh_cells_rast", d)
  dbDisconnect(con)

  if (!is.null(log_csv))
    tibble(
      done   = Sys.time(),
      tbl    = "oa_models",
      mdl_id = mdl_id,
      nrows  = nrow(d)) %>%
    write_csv(log_csv, append=T)
  T
}

cl <- new_cluster(parallel::detectCores() - 1)
cluster_library(cl, c(
  "DBI", "dplyr", "glue", "offhabr", "purrr", "readr", "tibble"))
cluster_assign(
  cl,
  log_csv     = log_csv,
  insert_rast = insert_rast)

d_mdls_cl <- d_mdls_r %>%
  select(mdl_id, d_mdl_cid) %>%
  partition(cl) %>%
  mutate(
    done = pmap_lgl(
      list(mdl_id=mdl_id, d_mdl_cid=d_mdl_cid),
      insert_rast,
      log_csv=log_csv))

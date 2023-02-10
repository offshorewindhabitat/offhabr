librarian::shelf(
  DBI, devtools, dplyr, fs, glue,
  multidplyr, parallel, readr, stringr, terra, tidyr)
# devtools::install_local(force = T)
load_all("~/Github/ecoquants/offhabr")

dir_derived <- "/Users/bbest/My Drive/projects/offhab/data/derived"
dir_lyrs    <- glue("{dir_derived}/lyrs_tif")
dir_lyrs_oh <- glue("{dir_derived}/lyrs_oh")

# get layer tifs ----
d_lyrs <- tibble(
  path    = dir_ls(dir_lyrs, glob = "*.tif"),
  lyr_key = path_ext_remove(path_file(path))) |>
  separate(
    lyr_key, c("ds_key", "aphia_id", "extra"), sep = "_",
    remove = F, convert = T, extra = "merge", fill = "right")
# d_lyrs |>
#   filter(lyr_key == "sm") |>
#   View()
table(d_lyrs$ds_key, useNA = "ifany")
#   am   du   gm   nc   oa   rl   sm   sw   ve   vg
# 9645   33   17   80  641  959    1   13    1    1

# write data-raw/datasets.csv to duckdb datasets ----
# use for tracking which dataset contributes to each cell

# con <- oh_con(read_only = F) # dbDisconnect(con, shutdown=T)
# d_ds <- read_csv(here("data-raw/datasets.csv"))
# dbWriteTable(con, "datasets", d_ds)
# dbDisconnect(con, shutdown=T)

# species X datasets (rows X cols), n_datasets ----
con <- oh_con() # dbDisconnect(con, shutdown=T)
d_ds <- tbl(con, "datasets") |>
  filter(active) |>   # IMPORTANT: drop oa_* layers
  select(ds_id, ds_key) |>
  collect()

d_lyrs_ds_lng <- d_lyrs |>
  filter(!is.na(aphia_id)) |>
  left_join(
    d_ds, by = "ds_key") |>
  filter(!is.na(ds_id)) |>  # IMPORTANT: drop oa_* layers
  select(aphia_id, path, ds_key)
table(d_lyrs_ds_lng$ds_key)
#   am   du   gm   nc   oa   rl   sw
# 9645   33   17   79  641  959   13
# since filtering out inactive datasets, ie oa:
#   am   du   gm   nc        rl   sw
# 9645   33   17   79       959   13

d_lyrs_ds <- d_lyrs_ds_lng |>
  pivot_wider(
    names_from = ds_key,
    values_from = path) |>
  rowwise() |>
  mutate(
    n_datasets = sum(!is.na(c_across(where(is.character))))) |>
  ungroup()
table(d_lyrs_ds$n_datasets)
#    1    2    3    4    5
# 9238  935   76    9    3
# after filter out oa:
#    1    2    3    4    5
# 9422  608   19    9    3

# process species with greater than 1 dataset ----
d_lyrs_ds_gt1 <- d_lyrs_ds |>
  # filter(aphia_id == 104286)
  filter(n_datasets > 1)

# quick fix: rm oa lyrs
# aphia_oa_redo <- d_oh_ds |>
#   filter(ds_id == 1) |>
#   pull(aphia_id) |>
#   unique()
# d_lyrs_ds_gt1 <- d_lyrs_ds_gt1 |>
#   filter(aphia_id %in% aphia_oa_redo)

# d2 <- d_lyrs |>
#   filter(aphia_id == 100832) |>
#   select(19:22, path)
# rast(d2$path[1]) |> trim() |>
#   plet(tiles="Esri.NatGeoWorldMap")
# rast(d2$path[2]) |> trim() |>
#   plet(tiles="Esri.NatGeoWorldMap")

# tbl(con, "taxa_wm") |>
#   filter(aphia_id == 104286)
# https://www.marinespecies.org/aphia.php?p=taxdetails&id=104286#distributions


get_val_ds_rasters <- function(i, redo=F){
  d_i <- d_lyrs_ds_gt1 |>
    # filter(aphia_id == 100832)
    slice(i)
  stopifnot(nrow(d_i) == 1)
  aphia_id <- d_i$aphia_id
  r_val_tif <- glue("{dir_lyrs_oh}/oh_{aphia_id}.tif")
  # r_ds_tif  <- glue("{dir_lyrs_oh}/oh_{aphia_id}_dsid.tif")
  r_ds_csv  <- glue("{dir_lyrs_oh}/oh_{aphia_id}_dsid.csv")
  # message(glue("r_val_tif: {basename(r_val_tif)} ~ {Sys.time()}"))
  if (file.exists(r_val_tif) & !redo)
    return(T)

  # aphia_id = 137098;  Risso's dolphin (Grampus griseus)
  # offhab-report/figures/mosaic_lyrs_137098RissosDolphin_1.png
  d_sp <- d_i |>
    select(-n_datasets) |>
    pivot_longer(
      !aphia_id,
      names_to  = "ds_key",
      values_to = "path") |>
    filter(!is.na(path)) |>
    left_join(
      d_ds,
      by = "ds_key") |>
    arrange(ds_id)

  # TODO: if only one layer:
  # - val: mask to r_rgns and set 0 = NA
  # - dsid: set to all val

  # get stack of dataset rasters
  stk <- rast(as.character(d_sp$path))
  names(stk) <- d_sp$ds_key
  stk_dsid <- (not.na(stk) * d_sp$ds_id) |> mask(stk)

  # stk[["am"]] |> trim() |> plet(tiles="Esri.NatGeoWorldMap")
  # stk[["rl"]] |> trim() |> plet(tiles="Esri.NatGeoWorldMap")
  # stk_dsid[["am"]] |> trim() |> plet(tiles="Esri.NatGeoWorldMap")
  # stk_dsid[["rl"]] |> trim() |> plet(tiles="Esri.NatGeoWorldMap")

  # get regions for masking and dataset detection
  r_rgns <- oh_rast("region")
  # trim(r_rgns) |> plet(tiles="Esri.NatGeoWorldMap")

  # get minimum of stack of dataset rasters;
  #   presumes dataset id (ds_id) is ordered, lowest = preferred
  r_ds_min <- min(stk_dsid, na.rm = T) |>
    mask(r_rgns)
  # trim(r_ds_min) |> plet(tiles="Esri.NatGeoWorldMap")

  # assign dataset as minimum per zone
  d_rgn_ds <- zonal(r_ds_min, r_rgns, "min", na.rm=T) |>
    tibble() |>
    left_join(
      levels(r_rgns)[[1]],
      by = "region")

  # write csv for later ingesting into db
  d_rgn_ds |>
    filter(!is.na(min)) |>
    select(region, ds_id = min) |>
    mutate(
      lyr_key = glue("oh_{aphia_id}")) |>
    write_csv(r_ds_csv)

  # setup data frame for mosaicking values across datasets
  d_rgn_ds <- d_rgn_ds |>
    select(rgn_id = value, ds_id = min)

  # populate raster of dataset to use
  r_ds_rgn <- classify(r_rgns, d_rgn_ds)
  names(r_ds_rgn) <- "ds_id"
  levels(r_ds_rgn) <- d_sp |>
    select(id = ds_id, ds_key)

  idx <- function(ds_key, stk_names, ...){
    # subset dataframe of rasters per column
    #   into single column with value of preferred dataset (ds_key)
    stk_cols        <- list(...)
    d <- data.frame(stk_cols)
    d$na <- rep(NA, nrow(d)) |> as.integer()
    na_idx <- length(stk_names) + 1
    names(d) <- c(stk_names, "na")
    ds2col <- 1:length(stk_names) |> setNames(stk_names)
    col_idx <- ifelse(is.na(ds_key), na_idx, ds2col[as.character(ds_key)])
    m <- cbind(1:length(col_idx), col_idx)
    v <- d[m]
    v[v==0] <- NA
    v
  }

  r_val <- lapp(c(r_ds_rgn, stk), idx, stk_names = names(stk))
  names(r_val) <- glue("oh_{aphia_id}_val") # plot(r_val)
  # trim(r_val) |> plet(tiles="Esri.NatGeoWorldMap")

  # r_ds <- r_ds_rgn |>
  #   mask(r_val)
  # names(r_ds) <- glue("oh_{aphia_id}_dsid")
  # null <- write_rast(r_ds, r_ds_tif, method = "nearest")

  write_rast(r_val, r_val_tif)
  T
}

# * setup cluster to parallelize ----
cl <- new_cluster(detectCores() - 1) # eg my MacBook Air has 8 CPUs, so 7 cores used
cluster_library(
  cl, c(
    "dplyr", "fs", "glue", "offhabr", "purrr", "readr", "terra", "tibble", "tidyr"))
cluster_assign(
  cl,
  dir_lyrs_oh        = dir_lyrs_oh,
  d_ds               = d_ds,
  d_lyrs_ds_gt1      = d_lyrs_ds_gt1,
  get_val_ds_rasters = get_val_ds_rasters)

Sys.time()
tibble(
  i = 1:nrow(d_lyrs_ds_gt1)) |>
  # i = 2:3) |>
  # i = 200:nrow(d_lyrs_ds_gt1)) |>
  partition(cl) |>
  mutate(
    do = map_lgl(i, get_val_ds_rasters, redo = T)) |>
  collect()
# 2 rasters: 92 sec
# TODO: recreate oh_#_dsid.tifs <= 2023-02-01 12:07pm
#   since `write_rast(r_ds, r_ds_tif)` needed `, method = "nearest"`

# estimate eta ---
d_lyrs_oh <- dir_info(dir_lyrs_oh, glob = "*.tif")
n      <- nrow(d_lyrs_ds_gt1)
n_done <- nrow(d_lyrs_oh)
n_todo <- n - n_done

t0 <- as.POSIXlt("2023-02-01 10:41:00", "America/Los_Angeles")
d_eta <- d_lyrs_oh |>
  filter(birth_time > t0)
t1 <- as.POSIXlt(Sys.time(), "America/Los_Angeles")
te <- difftime(t1, t0, units = "secs") / nrow(d_eta)
te
eta <- t1 + (te * n_todo)
dhrs <- difftime(eta, t1, units = "hours")
message(glue("
  {n_todo} of {n} to go ~ {Sys.time()}
    each: {round(te, 2)} sec
    eta: {eta} PST ({round(dhrs,1)} hrs)"))
# 802 of 1023 to go
#   each: 25.69 sec
#   eta: 2023-02-01 17:24:43 PST
# 771 of 1023 to go ~ 2023-02-01 11:53:11
#   each: 24.56 sec
#   eta: 2023-02-01 17:07:03 PST (5.3 hrs)

# 1016102: interesting RedList for GoMex, datasets 6 & 8 (RedList)
r_val <- rast("/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_oh/oh_1016102.tif")
plot(trim(r_val))
librarian::shelf(DBI, dplyr, offhabr)
con <- oh_con()
d_ds <- tbl(con, "datasets") |>
  select(ds_id, ds_key) |>
  collect()

# check r_val, r_ds ----
r_val <- rast(r_val_tif) # plot(r_val)
# plet(r_val, tiles="Esri.NatGeoWorldMap")

# writeRaster(
#   r_rgns, rgns_tif,
#   datatype  = "INT1U", overwrite = T)

# assign unique ds_key per aphia_id ----
d_lyrs_oh <- tibble(
  path_tif = dir_ls(dir_lyrs_oh, glob = "*.tif"),
  path_csv = glue("{path_ext_remove(path_tif)}_dsid.csv"),
  file     = path_file(path_tif) |> path_ext_remove()) |>
  separate(
    file, c("ds", "aphia_id", "extra"),
    sep = "_", remove = F, convert = T, extra = "merge", fill = "right") |>
  mutate(
    lyr_key = glue("oh_{aphia_id}"))
stopifnot(sum(!is.na(d_lyrs_oh$extra)) == 0)
stopifnot(sum(!fs::file_exists(d_lyrs_oh$path_tif)) == 0)
stopifnot(sum(!fs::file_exists(d_lyrs_oh$path_csv)) == 0)

# quick fix: remove old oh_* lyrs with oa, but now only one layer without
# d_lyrs_oh_ds1 <- d_lyrs_oh |>
#   left_join(
#     d_lyrs_ds,
#     by = "aphia_id") |>
#   filter(n_datasets == 1)
# file_delete(d_lyrs_oh_ds1$path_tif)
# file_delete(d_lyrs_oh_ds1$path_csv)


# r_rgn <- oh_rast("region")

d_lyrs_oh_ds <- d_lyrs_oh |>
  mutate(
    data = map(path_csv, read_csv, show_col_types=F))

d_oh_ds <- bind_rows(d_lyrs_oh_ds$data) |>
  filter(!is.na(ds_id)) |>
  relocate(lyr_key, region, ds_id) |>
  separate(
    lyr_key, c("ds_key", "aphia_id"),
    remove = F, convert = T) |>
  group_by(aphia_id) |>
  mutate(
    n = n()) |>
  arrange(aphia_id, ds_key) |>
  ungroup()

head(d_oh_ds)
# lyr_key    region         ds_id
# <chr>      <chr>          <dbl>
# 1 oh_100832  Atlantic           2
# 2 oh_100832  Gulf of Mexico     2
# 3 oh_100832  Pacific            5
# 4 oh_1016102 Atlantic           6
# 5 oh_1016102 Gulf of Mexico     8
# 6 oh_1026118 Atlantic           6

table(d_oh_ds$n)
# 1    2    3
# 371 1116  282
#   1    2    3
# 370 1118  282
#
# 1   2   3
# 158 808 231

table(d_oh_ds$ds_id)
#  2    3    5    6    8
# 48   11   12 1112   14

# * write lyr_rgn_ds ----
d_oh_ds_gt1 <- d_oh_ds |>
  # filter(n > 1) |>
  select(-ds_key) |>
  left_join(
    d_ds, by = "ds_id")

con <- oh_con(read_only = F) # dbDisconnect(con, shutdown=T)
# dbListTables(con)
dbWriteTable(con, "lyr_rgn_ds", d_oh_ds, overwrite = T)
create_index(con, "lyr_rgn_ds", c("lyr_key", "region"), unique = T, overwrite = T)
create_index(con, "lyr_rgn_ds", c("ds_id"), overwrite = T)
dbDisconnect(con, shutdown=T)

# * promote is_ds_prime for original dataset, drop oh_# with only one dataset ----
d_ds <- tbl(con, "datasets") |>
  collect()

d_oh_ds_n1 <- d_oh_ds |>
  filter(n == 1) |>
  select(aphia_id, ds_id) |>
  left_join(
    d_ds, by = "ds_id")

# confirm for first row, aphia_id: 105808
# r_am <- d_lyrs |>
#   filter(aphia_id == 105808) |>
#   left_join(
#     d_ds, by = "ds_key") |>
#   filter(ds_id == 6) |>
#   pull(path) |>
#   rast() |>
#   trim()
# plet(r_am, tiles = "Esri.NatGeoWorldMap")
# r_oh <- d_lyrs_oh |>
#   filter(aphia_id == 105808) |>
#   pull(path_tif) |>
#   rast() |>
#   trim()
# plet(r_oh, tiles = "Esri.NatGeoWorldMap")

# dbDisconnect(con, shutdown = T)
con <- oh_con(read_only = F)

for (i in 1:nrow(d_oh_ds_n1)){ # i = 1
  di <- d_oh_ds_n1 |>
    slice(i)
  message(glue("{i} of {nrow(d_oh_ds_n1)}: drop oh_{di$aphia_id}, promote {di$ds_key}_{di$aphia_id}"))

  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!di$aphia_id)
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
    SET is_ds_prime = FALSE
    WHERE
      aphia_id = {di$aphia_id}"))
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
    SET is_ds_prime = TRUE
    WHERE
      aphia_id = {di$aphia_id} AND
      ds_key = '{di$ds_key}'"))
  dbExecute(
    con,
    glue(
      "DELETE FROM lyrs
    WHERE
      aphia_id = {di$aphia_id} AND
      ds_key = 'oh'"))
  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!di$aphia_id)
  file_delete(glue("{dir_lyrs_oh}/oh_{di$aphia_id}{c('.tif','_dsid.csv')}"))

  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_rgn_ds
    WHERE
      lyr_key = 'oh_{di$aphia_id}'"))
  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_zone_stats
    WHERE
      lyr_key = 'oh_{di$aphia_id}'"))
}

# * promote original dataset for oh lyrs with same dataset across regions ----
d_oh_ds_gt1 <- d_oh_ds |>
  filter(n > 1) |>
  select(-ds_key) |>
  left_join(
    d_ds, by = "ds_id") |>
  group_by(aphia_id) |>
  mutate(n_unique = length(unique(ds_id))) |>
  arrange(aphia_id, ds_key)

d_oh_ds_gt1_nu1 <- d_oh_ds_gt1 |>
  filter(n_unique == 1) |>
  group_by(aphia_id, ds_key) |>
  summarize(n_rgns = n(), .groups = "drop")

for (i in 2:nrow(d_oh_ds_gt1_nu1)){ # i = 1

  di <- d_oh_ds_gt1_nu1 |>
    slice(i)
  aphia_id <- di$aphia_id
  ds_key <- di$ds_key
  message(glue("{i} of {nrow(d_oh_ds_gt1_nu1)}: drop oh_{aphia_id}, promote {ds_key}_{aphia_id} "))

  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!aphia_id)
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
      SET is_ds_prime = FALSE
      WHERE
        aphia_id = {aphia_id}"))
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
    SET is_ds_prime = TRUE
    WHERE
      aphia_id = {aphia_id} AND
      ds_key = '{ds_key}'"))
  dbExecute(
    con,
    glue(
      "DELETE FROM lyrs
    WHERE
      aphia_id = {di$aphia_id} AND
      ds_key = 'oh'"))
  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!di$aphia_id)

  files <- glue("{dir_lyrs_oh}/oh_{di$aphia_id}{c('.tif','_dsid.csv')}")
  for (f in files){
    if (file_exists(f))
      file_delete(f)
  }

  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_rgn_ds
    WHERE
      lyr_key = 'oh_{di$aphia_id}'"))
  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_zone_stats
    WHERE
      lyr_key = 'oh_{di$aphia_id}'"))
}

# * promote is_ds_prime for oh lyrs with more than 1 unique dataset across regions ----
d_oh_ds_gt1_nugt1 <- d_oh_ds_gt1 |>
  filter(n_unique > 1) |>
  arrange(aphia_id, ds_key) |>
  ungroup()

aphia_ids <- unique(d_oh_ds_gt1_nugt1$aphia_id)
for (i in 2:length(aphia_ids)){ # i = 1
  aphia_id <- aphia_ids[i]
  di <- d_oh_ds_gt1_nugt1 |>
    filter(aphia_id == !!aphia_id)
  message(glue("{i} of {length(aphia_ids)}: promote oh_{aphia_id} with more than one unique dataset across regions"))

  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!aphia_id)
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
    SET is_ds_prime = FALSE
    WHERE
      aphia_id = {aphia_id}"))
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
    SET is_ds_prime = TRUE
    WHERE
      aphia_id = {aphia_id} AND
      ds_key = 'oh'"))
  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!aphia_id)

  # tbl(con, "lyr_rgn_ds") |>
  #   filter(lyr_key == !!glue('oh_{aphia_id}'))
  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_rgn_ds
    WHERE
      lyr_key = 'oh_{aphia_id}'"))
  dbAppendTable(
    con,
    "lyr_rgn_ds",
    di |> select(lyr_key, region, ds_id))
  # tbl(con, "lyr_rgn_ds") |>
  #   filter(lyr_key == !!glue('oh_{aphia_id}'))

  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_zone_stats
    WHERE
      lyr_key = 'oh_{aphia_id}'"))
}


# * remove oh_* lyrs without tif, promote non-oa to is_ds_prime ----

d_lyrs_oh <- tibble(
  path_tif = dir_ls(dir_lyrs_oh, glob = "*.tif"),
  path_csv = glue("{path_ext_remove(path_tif)}_dsid.csv"),
  file     = path_file(path_tif) |> path_ext_remove()) |>
  separate(
    file, c("ds", "aphia_id", "extra"),
    sep = "_", remove = F, convert = T, extra = "merge", fill = "right") |>
  mutate(
    lyr_key = glue("oh_{aphia_id}"))
stopifnot(sum(!is.na(d_lyrs_oh$extra)) == 0)
stopifnot(sum(!fs::file_exists(d_lyrs_oh$path_tif)) == 0)
stopifnot(sum(!fs::file_exists(d_lyrs_oh$path_csv)) == 0)

d_lyrs_ohnot <- tbl(con, "lyrs") |>
  collect() |>
  filter(
    ds_key == "oh",
    !aphia_id %in% d_lyrs_oh$aphia_id) |>
  arrange(aphia_id)
aphia_ids <- d_lyrs_ohnot$aphia_id

for (i in 1:length(aphia_ids)){ # i = 1
  aphia_id <- aphia_ids[i]

  di <- tbl(con, "lyrs") |>
    filter(
      aphia_id == !!aphia_id,
      ds_key != "oa",
      ds_key != "oh") |>
    collect()
  ds_key <- di$ds_key
  stopifnot(nrow(di) == 1)

  message(glue("{i} of {length(aphia_ids)}: drop oh_{aphia_id}, promote {ds_key}_{aphia_id}"))

  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!di$aphia_id)
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
    SET is_ds_prime = FALSE
    WHERE
      aphia_id = {aphia_id}"))
  dbExecute(
    con,
    glue(
      "UPDATE lyrs
    SET is_ds_prime = TRUE
    WHERE
      aphia_id = {aphia_id} AND
      ds_key = '{ds_key}'"))
  dbExecute(
    con,
    glue(
      "DELETE FROM lyrs
    WHERE
      aphia_id = {aphia_id} AND
      ds_key = 'oh'"))
  # tbl(con, "lyrs") |>
  #   filter(aphia_id == !!di$aphia_id)

  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_rgn_ds
    WHERE
      lyr_key = 'oh_{aphia_id}'"))
  dbExecute(
    con,
    glue(
      "DELETE FROM lyr_zone_stats
    WHERE
      lyr_key = 'oh_{aphia_id}'"))

}

# * rm oh_*_dsid.csv ----
file_delete(d_lyrs_oh$path_csv)

# * update oh paths in lyrs tbl ----
dbDisconnect(con, shutdown=T)
con <- oh_con(read_only = F)
# dbListTables(con)

d_lyrs <- tbl(con, "lyrs") |>
  collect() |>
  mutate(
    path_tif = ifelse(
      file.exists(path_tif),
      path_tif,
      str_replace(path_tif, "_val", "")))
dbWriteTable(con, "lyrs", d_lyrs, overwrite=T)

dbDisconnect(con, shutdown=T)

# * recreate lyrs tbl in db after accidental delete ----

dbAppendTable(
  con,
  "datasets",
  tibble(
    ds_id      = 0,
    ds_key     = "oh",
    active     = T,
    name_short = "OffHab",
    name_long  = "Offshore Habitat composite"))

d_lyrs <- tibble(
  path_tif = c(
    dir_ls(dir_lyrs, glob = "*.tif"),
    dir_ls(dir_lyrs_oh, glob = "*.tif")),
  lyr_key  = path_ext_remove(path_file(path_tif))) |>
  separate(
    lyr_key, c("ds_key", "aphia_id"), sep = "_",
    remove = F, convert = T, fill = "right") |>
  filter(
    ds_key != "oa") |>
  left_join(
    read_csv(here::here("data-raw/layers.csv")) |>
      select(lyr_key, val_min, val_max, rescale_min, rescale_max),
    by = "lyr_key") |>
  left_join(
    tbl(con, "datasets") |>
      select(ds_key, ds_id) |>
      collect(),
    by = "ds_key") |>
  arrange(
    aphia_id, ds_id) |>
  group_by(
    aphia_id) |>
  mutate(
    is_ds_prime = if_else(row_number() == 1, T, F))

table(d_lyrs$is_ds_prime)
# FALSE   TRUE
#   729 10,062

con <- oh_con(read_only = F) # dbDisconnect(con, shutdown=T)
dbWriteTable(con, "lyrs", d_lyrs, overwrite=T)
create_index(con, "lyrs", "lyr_key", unique = T, overwrite = T)
create_index(con, "lyrs", "ds_key", overwrite = T)
create_index(con, "lyrs", "aphia_id", overwrite = T)
dbDisconnect(con, shutdown=T)

# get single set of layers unique by aphia_id ----
con <- oh_con() # dbDisconnect(con, shutdown=T)

d_lyrs <- tbl(con, "lyrs") |>
  filter(is_ds_prime == T) |>
  collect()
stopifnot(sum(duplicated(d_lyrs$aphia_id)) == 0)

# test stack and weight ----

stk <- d_lyrs |>
  filter(
    is_ds_prime,
    !is.na(aphia_id)) |>
  slice(1:3) |>
  pull(path_tif) |>
  rast()
summary(stk)
# Mean   : 2.03   Mean   : 2.8    Mean   : 1.7
stk_x <- stk * (1:3)^2
summary(stk_x)
# Mean   :  2.05   Mean   : 11.3   Mean   : 14.93

# yay, works!

# transfer taxa_rl, taxa_rl_catscores ----
con_dk <- oh_con(read_only = F) # dbDisconnect(con, shutdown = T)
con_pg <- oh_pg_con()

# taxa_rl pg to dk
d_taxa_rl <- tbl(con_pg, "taxa_rl") |> collect()
dbWriteTable(con_dk, "taxa_rl", d_taxa_rl)
create_index(con_dk, "taxa_rl", "aphia_id", unique = T, overwrite = T)
create_index(con_dk, "taxa_rl", "category", overwrite = T)

# taxa_rl_catscores pg to dk
d_taxa_rl_catscores <- tbl(con_pg, "taxa_rl_catscores") |> collect()
dbWriteTable(con_dk, "taxa_rl_catscores", d_taxa_rl_catscores)
create_index(con_dk, "taxa_rl_catscores", "category", overwrite = T)

dbDisconnect(con_dk, shutdown = T)

# calculate extinction risk ----
er_csv <- glue("{dir_derived}/er.csv")
er_tif <- glue("{dir_derived}/er.tif")

con <- oh_con()

d_er <- tbl(con, "lyrs") |>
  filter(
    is_ds_prime,
    !is.na(aphia_id)) |>
  select(aphia_id, ds_key, lyr_key, path_tif) |>
  left_join(
    tbl(con, "taxa_rl") |>
      select(aphia_id, category),
    by = "aphia_id") |>
  left_join(
    tbl(con, "taxa_rl_catscores") |>
      select(category, rl_score),
    by = "category")

d_er_cats <- d_er |>
  group_by(category, rl_score) |>
  summarize(
    n = n()) |>
  arrange(rl_score) |>
  collect()
d_er_cats
#   category rl_score     n
#   <chr>       <int> <dbl>
# 1 DD             NA    158
# 2 NA             NA  7,359
# 3 LC              0  2,336
# 4 LR/lc           0      1
# 5 NT              1     57
# 6 VU              2     88
# 7 EN              3     36
# 8 CR              4     26

d_er <- d_er |>
  filter(
    !is.na(rl_score),
    rl_score > 0) |>
  left_join(
    tbl(con, "taxa_wm"),
    by = "aphia_id") |>
  collect()
nrow(d_er) # 207
write_csv(d_er, er_csv)

system.time({
  s_er <- rast(d_er$path_tif) * d_er$rl_score
}) # 12.6 min
system.time({
  r_er <- sum(s_er, na.rm = T)
}) # 3 min

r_rgn <- oh_rast("region")
r_er <- r_er |>
  mask(r_rgn)

write_rast(r_er, er_tif, datatype = "INT2U")
# plet(r_er, tiles="Esri.NatGeoWorldMap")
# global(r_er, "max", na.rm=T) # 22,585

# upload to gcs for mapping high res ----
librarian::shelf(googleCloudStorageR)


Sys.setenv(
  "GCS_DEFAULT_BUCKET" = "offhab_lyrs",
  "GCS_AUTH_FILE"      = "/Users/bbest/My Drive/private/offhab-google-service-account_09e7228ac965.json")
gcs_global_bucket("offhab_lyrs")

tif <- er_tif
tif <- tbl(con, "lyrs") |>
  filter(lyr_key == "vg") |>
  pull(path_tif)

# upload
googleCloudStorageR::gcs_upload(
  file = tif,
  name = basename(tif))

# make publicly available
googleCloudStorageR::gcs_update_object_acl(
  basename(tif), entity_type = "allUsers")



# old ----

d_er$path_tif[1:2]
# TODO: skip VRT for rl_score == 1

d_er |>
  filter(rl_score == 4) |>
  slice(1) |>
  pull(path_tif)
# /Users/bbest/My Drive/projects/offhab/data/derived/lyrs_oh/oh_445374_val.tif
# oh_445374

tif_oh <- "/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_oh/oh_445374.tif"
tif_1 <- "/Users/bbest/My Drive/projects/offhab/data/derived/oh_445374_test1.tif"
file.copy(tif_0, tif_1, overwrite = T)
r_oh <- rast(tif_oh)
trim(r) |> plet(tiles="Esri.NatGeoWorldMap")

tbl(con, "lyrs") |>
  filter(aphia_id == 445374)
# 1 am     am_445374   445374      NA      NA          NA        NA /Users… FALSE
# 2 rl     rl_445374   445374      NA      NA          NA        NA /Users… FALSE
# 3 oh     oh_445374   445374      NA      NA          NA        NA /Users… TRUE

tif_am <- "/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_tif/am_445374.tif"
r_am <- rast(tif_am)
trim(r_am) |> plet(tiles="Esri.NatGeoWorldMap")


# so where


r_am <- rast(glue("{dir_lyrs}/am_445374.tif"))
r_rl <- rast(glue("{dir_lyrs}/rl_445374.tif"))
r_oh <- rast(glue("{dir_lyrs_oh}/oh_445374_val.tif"))
trim(r_am) |> plet(tiles="Esri.NatGeoWorldMap")
trim(r_rl) |> plet(tiles="Esri.NatGeoWorldMap")

tbl(con, "lyr_rgn_ds") |>
  filter(lyr_key == "oh_445374")

dbListTables(con)


trim(r) |> plet(tiles="Esri.NatGeoWorldMap")
makeVRT(
  tif_1, scale = 4,
  lyrnms = "oh_445374_er",
  datatype = "INT1U", crs = crs(r),
  ncol = ncol(r), nrow = nrow(r),
  xres = xres(r), yres = yres(r),
  xmin = xmin(r), ymin = ymin(r))
vrt <- glue("{tif_1}.vrt")

terra::crs(r)
r_vrt <- rast(vrt)
trim(r_vrt) |> plet(tiles="Esri.NatGeoWorldMap")

v_vrt <- values(r_vrt, na.rm = T)
summary(v_vrt)
# oh_445374_test1.tif
# Min.   :   0.0000
# 1st Qu.:   0.0000
# Median :   0.0000
# Mean   :   0.6484
# 3rd Qu.:   0.0000
# Max.   :1020.0000


plet(r_vrt, tiles="Esri.NatGeoWorldMap")
v_r <- values(r, na.rm = T)
summary(v_r)


# so max 400

#' | INT1U | 	             0 |	          255 | Byte    |
#' | INT2S | 	       -32,767 |	       32,767 | Int16   |
#' | INT2U | 	             0 |	       65,534 | UInt16  |
#' | INT4S |  -2,147,483,647 |	2,147,483,647 | Int32   |
#' | INT4U |	             0 |  4,294,967,296 | UInt32  |





# multiply by extinction risk ----



# 3. lookup other traits for summation
# 4. write into report

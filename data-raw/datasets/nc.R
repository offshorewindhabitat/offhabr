# NCCOS Assessments Modeling At-Sea Density of Marine Birds for Atlantic and Pacific
# - [Atlantic](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0176682)
# - [Pacific](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0242882)
#
# ncei.noaa.gov - seabirds, atlantic/
# - 0176682/1.1/data/0-data/NCCOS-Atlantic-Birds_ArchiveDataPackage/NCCOS-Atlantic-Birds_ArchiveDataPackage/Documentation/
#   - Atlantic_bird_mapping_data_documentation.pdf
#   - atl_spp.xlsx
#
# ncei.noaa.gov - seabirds, pacific/
# - 0242882/1.1/data/1-data/4LFC6T_PacificBirds_NCCOS/
#   - DataDocumentation.pdf
#   - pac_spp.xlsx

# packages
librarian::shelf(
  devtools, dplyr, fs, here, janitor, mapview, purrr, readr, readxl, sf, stringr,
  terra, tibble, tidyr, units)
load_all()
options(readr.show_col_types = F)

# read layers ----
dir_g   <- "/Users/bbest/My Drive/projects/offhab/data/raw"
dir_atl <- glue("{dir_g}/ncei.noaa.gov - seabirds, atlantic/0176682/1.1/data/0-data/NCCOS-Atlantic-Birds_ArchiveDataPackage")
dir_pac <- glue("{dir_g}/ncei.noaa.gov - seabirds, pacific/0242882/1.1/data/0-data")

spp_atl_xls <- glue("{dir_atl}/Documentation/atl_spp.xlsx")
spp_pac_xls <- glue("{dir_pac}/4LFC6T_PacificBirds_NCCOS/pac_spp.xlsx")

# * get Atlantic rasters of density ----
dir_atl_tif <- glue("{dir_atl}/Data/model_output_predictions")
rx_atl  <- "/(.+)/(.+)/(.+)"
d_atl <- tibble(
  path_tif = list.files(dir_atl_tif, "*.density_bootstrap_QUANT_50.tif$", full.names = T, recursive = T)) %>%
  mutate(
    region  = "Atlantic",
    rel_tif = str_replace(path_tif, dir_atl_tif, ""),
    season  = str_replace(rel_tif, rx_atl, "\\1"),
    sp_code = str_replace(rel_tif, rx_atl, "\\2"),
    tif     = str_replace(rel_tif, rx_atl, "\\3"))
# d_atl %>%
#   select(-path_tif, -rel_tif) %>%
#   View()

# * get Pacific rasters of density ----
dir_pac_tif <- glue("{dir_pac}/model_output_predictions")
rx_pac  <- "/(.+)_(.+)_predicted_density.tif"
d_pac <- tibble(
  path_tif = list.files(dir_pac_tif, "*.predicted_density.tif$", full.names = T, recursive = T)) %>%
  mutate(
    region  = "Pacific",
    rel_tif = str_replace(path_tif, dir_pac_tif, ""),
    sp_code = str_replace(rel_tif, rx_pac, "\\1"),
    season  = str_replace(rel_tif, rx_pac, "\\2"),
    tif     = basename(rel_tif))
# d_pac %>%
#   select(-path_tif, -rel_tif) %>%
#   View()

# * combine Atlantic and Pacific ----
d <- bind_rows(
  d_atl,
  d_pac)
# d %>%
#   select(-path_tif, -rel_tif) %>%
#   View()

d <- d %>%
  select(region, sp_code, season, path_tif) %>%
  group_by(region, sp_code) %>%
  nest() %>%
  mutate(
    seasons   = map_chr(data, ~paste0(.$season, collapse=";")),
    n_seasons = map_int(data, nrow),
    r         = map(data, function(data){
      terra::rast(data$path_tif) %>%
        terra::mean(na.rm=T)
    }))

# inspect ----
which.max(d$region == "Atlantic") #  1
which.max(d$region == "Pacific")  # 48

# d$r[[1]]
# plot(d$r[[1]])
# mapView(d$r[[1]])
# d$r[[48]]
# d$r[[49]]
# terra::compareGeom(d$r[[141]], d$r[[142]])
# plot(d$r[[141]])
# mapView(d$r[[141]])
# different projections whether Atlantic or Pacific

# get projections for Atlantic and Pacific
r_atl_om1 <- d$r[[1]]           # plot(r_atl_om1)
crs(r_atl_om1, proj=T)
# +proj=omerc +lat_0=35 +lonc=-75 +alpha=40 +gamma=40 +k=0.9996 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
r_pac_om2 <- d$r[[48]]         # plot(r_pac_om2)  # WGS_1984_Albers
crs(r_pac_om2, proj=T)
# +proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +gamma=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
r_cid_mer <- oh_rast("cell_id") # plot(r_zid_mer)  # WGS 84 / Pseudo-Mercator
r_zid_mer <- oh_rast("zone_id") # plot(r_zid_mer)  # WGS 84 / Pseudo-Mercator

p_cid_mer <- as.points(r_cid_mer)
p_cid_om1 <- terra::project(p_cid_mer, r_atl_om1)
p_cid_om2 <- terra::project(p_cid_mer, r_pac_om2)

# extract values for OffHab cells ----
extract_oh_cells <- function(r_om, i=0, n=0){
  # r_om <- d$r[[1]]
  message(glue("{i} of {n}: extract_oh_cells() ~ {Sys.time()}"))
  if (compareGeom(r_om, r_atl_om1, stopOnError=F)){
    p_cid <- p_cid_om1
  } else{
    if (compareGeom(r_om, r_pac_om2, stopOnError=F)){
      p_cid <- p_cid_om2
    } else {
      stop("WHOAH! mismatch reference raster")
    }
  }
  r_om %>%
    terra::extract(p_cid, bind=T) %>%
    as_tibble() %>%
    filter(!is.na(mean)) %>%
    rename(density = mean)
}

# check single raster
# x <- extract_oh_cells(r_pac_om2)
# r <- oh_rast("NA")
# r[x$cell_id] <- x$density
# r <- trim(r)
# plot(r)
# mapView(r, maxpixels = ncell(r))

#
d <- d %>%
  mutate(
    d_oh_cells = imap(r, ~extract_oh_cells(r_om=.x, i=.y), n=nrow(d)))
# 33 min for 93 rows
#   1  of 93: extract_oh_cells() ~ 2022-11-27 16:57:21
# [93] of 93: extract_oh_cells() ~ 2022-11-27 17:25:35

d <- d %>%
  mutate(
    tifs = map_chr(
      data, ~paste(basename(.$path_tif), collapse = ";"))) %>%
  ungroup() %>%
  arrange(sp_code, region) %>%
  relocate(sp_code) %>%
  rowid_to_column("row_id")

# write oh_cells_rast to db ----
con <- oh_pg_con()

# too big to unnest at once, so run individually
#   Error: vector memory exhausted (limit reached?)
for (i in 1:nrow(d)){ # i = 1
  message(glue("{i} of {nrow(d)} ~ {Sys.time()}"))
  d_i <- d %>%
    slice(i) %>%
    select(row_id, d_oh_cells) %>%
    unnest(d_oh_cells) %>%
    mutate(
      tbl = "nc_density") %>%
    select(tbl, row_id, cell_id, density)

  dbAppendTable(con, "oh_cells_rast", d_i)
}

# temporarily write metadata of dataset to csv before sorting species
m_csv <- here("data-raw/mdls_nc_density_meta.csv")
m <- d %>%
  select(row_id, sp_code, region, seasons, n_seasons, tifs)
write_csv(m, m_csv)

# match sp_code to scientific_name ----
message("stripping ambiguous scientific_names for WoRMS matching:")
spp <- bind_rows(
  read_excel(spp_atl_xls) %>%
    mutate(
      region = "Atlantic"),
  read_excel(spp_pac_xls) %>%
    mutate(
      region = "Pacific")) %>%
  janitor::clean_names() %>%
  mutate(
    scientific_name = scientific_name %>%
      str_replace_all("\\r\\n", " ") %>%
      str_replace_all(" spp.", ""),
    scientific_name = map_chr(scientific_name, ~{
      sn <- .
      if (str_detect(sn, "/")){
        sn_new <- str_replace(sn, "(\\w) (.*)", "\\1")
        message(glue("  '{sn}' -> '{sn_new}'"))
        # 'Sterna hirundo/paradisaea'                       -> 'Sterna'
        # 'Larus argentatus/glaucoides'                     -> 'Larus'
        # 'Stercorarius pomarinus/parasiticus/ longicaudus' -> 'Stercorarius'
        # 'Stercorarius parasiticus/longicaudus'            -> 'Stercorarius'
        # 'Thalasseus maximus/elegans'                      -> 'Thalasseus'
        # 'Synthliboramphus scrippsi/hypoleucus/craveri'    -> 'Synthliboramphus'
        # 'Ardenna tenuirostris/grisea/carneipes'           -> 'Ardenna'
        # 'Aechmophorus occidentalis/clarkii'               -> 'Aechmophorus'
        # 'Larus occidentalis/glaucescens'                  -> 'Larus'
        sn <- sn_new
      }
      sn
    }),
    species_code = recode(
      species_code,
      `COTE- ARTE`          = "COTE-ARTE",
      `STTS-\r\nSOSH- FFSH` = "STTS-SOSH-FFSH")) %>%
  arrange(species_code, region)
# View(spp)

spp <- spp %>%
  group_by(species_code) %>%
  summarize(
    common_name     = first(common_name),
    scientific_name = first(scientific_name),
    .groups = "drop")

spp_wm <- wm_add_aphia_id(
  spp, scientific_name,
  tbl_str = "nc_density", fld_str = "scientific_name")

# View(spp_wm)

spp_x_csv <- here("data-raw/mdls_nc_density_spp_missing-aphia_id.csv")
if (!file.exists(spp_x_csv)){
  spp_wm %>%
    filter(is.na(aphia_id)) %>%
    select(species_code, common_name, scientific_name_orig = scientific_name) %>%
    mutate(
      scientific_name = NA,
      aphia_id = NA) %>%
    write_csv(spp_x_csv)
  # then manually set new scientific_name googling and trying at https://www.marinespecies.org/
}
spp_x <- read_csv(spp_x_csv) %>%
  select(species_code, common_name, scientific_name)

spp2 <- spp %>%
  anti_join(
    spp_x,
    by = "species_code") %>%
  bind_rows(
    spp_x) %>%
  arrange(species_code)

spp_wm <- wm_add_aphia_id(
  spp2, scientific_name,
  tbl_str = "nc_density", fld_str = "scientific_name")

spp_x2 <- spp_wm %>%
  filter(is.na(aphia_id)) %>%
  select(species_code, common_name, scientific_name)
stopifnot(nrow(spp_x2) == 0)

# debug: for editing wm_add_aphia_id()
# dbSendQuery(
#   con,
#   glue("DELETE FROM taxa WHERE tbl = 'nc_density'"))


# write nc_density to db ----
nc_density <- read_csv(m_csv) %>%
  rename(species_code = sp_code) %>%
  left_join(
    spp_wm, by = "species_code") %>%
  select(
    row_id, aphia_id, species_code, common_name, scientific_name,
    region, seasons, n_seasons, tifs)
dbWriteTable(con, "nc_density", nc_density)

# redo rast ----
librarian::shelf(
  here, readr)
options(readr.show_col_types = F)

dir_lyrs_tif <- "/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_tif"
lyrs_csv     <- here("data-raw/layers.csv")
ds_key       <- "nc"

con <- oh_pg_con()

# join aphia_id to data
intersect(colnames(d), colnames(d_taxa))
# [1] "region"    "seasons"   "n_seasons"
setdiff(colnames(d), colnames(d_taxa))
# [1] "sp_code" "data"    "r"
setdiff(colnames(d_taxa), colnames(d))
# [1] "row_id"          "aphia_id"        "species_code"
# [4] "common_name"     "scientific_name" "tifs"
d <- d |>
  left_join(
    tbl(con, "ds_nc") |>
      collect() |>
      rename(sp_code = species_code),
    by = c("sp_code", "region", "seasons", "n_seasons"))

# * fill in missing aphia_ids manually ----
d |>
  filter(is.na(aphia_id)) |>
  pull(sp_code)
# [1] "HERG-ICGU"      "ROYT-ELTE"      "SCMU-GUMU-CRMU" "WEGR-CLGR"
# [5] "WEGU-WGWH-GWGU"
d_sp_lookups <- tribble(
  ~sp_code, ~aphia_id, ~sp_lookup, ~scientific_name, ~common_name,
  "SCMU-GUMU-CRMU", 343632, "Synthliboramphus", "Synthliboramphus scrippsi/hypoleucus/craveri", "Scripps’s/Guadalupe/Craveri’s Murrelet",
  "HERG-ICGU",  137043, "Larus", "Larus argentatus/glaucoides ", "Herring/Iceland Gull",
  "ROYT-ELTE", 137047, "Thalasseus", "Thalasseus maximus/elegans","Royal/Elegant Tern",
  "WEGR-CLGR", 366616, "Aechmophorus", "Aechmophorus occidentalis/clarkii", "Western/Clark’s Grebe",
  "WEGU-WGWH-GWGU", 137043, "Larus", "Larus occidentalis/glaucescens", "Western/Glaucous-winged Gull")
d <- d |>
  filter(!is.na(aphia_id)) |>
  rbind(
    d |>
      filter(is.na(aphia_id)) |>
      select(-aphia_id, -scientific_name, -common_name) |>
      left_join(
        d_sp_lookups |>
          select(sp_code, aphia_id, scientific_name, common_name),
        by = "sp_code"))

# * iterate over aphia_ids ----
r_na <- oh_rast("NA")
aphia_ids <- unique(d$aphia_id)
for (i in 2:length(aphia_ids)){ # i = 1

  aphia_id <- aphia_ids[i]
  lyr_key  <- glue("{ds_key}_{aphia_id}")
  r_tif    <- glue("{dir_lyrs_tif}/{lyr_key}.tif")
  message(glue("{i} of {length(aphia_ids)}: {basename(r_tif)} ~ {Sys.time()}"))

  d_sp <- d |>
    filter(aphia_id == !!aphia_id)

  stk <- list()
  for (region in d_sp$region){ # region = d_sp$region[1]
    message(glue("  region: {region} ~ {Sys.time()}"))

    r <- d_sp |>
      filter(region == !!region) |>
      pull(r) %>%
      .[[1]]
    # plet(r, tiles="Esri.NatGeoWorldMap")
    stk[[region]] <- terra::project(r, r_na)
    # plet(stk[[region]], tiles="Esri.NatGeoWorldMap")
  }
  r <- rast(stk) |>
    app(fun = "mean", na.rm=T)
  # plet(r, tiles="Esri.NatGeoWorldMap")

  # rescale 0 to 100
  (vr <- range(values(r, na.rm = T)))
  r <- setValues(
    r,
    scales::rescale(
      values(r),
      to = c(0, 100)) )
  # 0 -> NA
  r[r==0] <- NA
  # plet(r, tiles="Esri.NatGeoWorldMap")

  # write raster
  write_rast(r, r_tif)

  # write min, max metadata to layers.csv
  d_m <- read_csv(lyrs_csv) |>
    filter(
      lyr_key != !!lyr_key) |>
    bind_rows(
      tibble(
        lyr_key     = lyr_key,
        ds_key      = ds_key,
        aphia_id    = aphia_id,
        val_min	    = vr[1],
        val_max     = vr[2],
        rescale_min	= 0,
        rescale_max = 100) )
  message(glue("  nrow(lyrs_csv): {nrow(d)}"))
  write_csv(d_m, lyrs_csv)
}

# write data-raw/layers.csv to duckdb ----

# ls() %>% stringr::str_subset("con")
# DBI::dbDisconnect(con, shutdown = T)
con <- oh_con(read_only = F)
# dbListTables(con)

lyrs <- read_csv(here("../offhabr/data-raw/layers.csv"))
# head(lyrs)
# ds_key lyr_key   aphia_id       val_min  val_max rescale_min rescale_max
# <chr>  <chr>        <dbl>         <dbl>    <dbl>       <dbl>       <dbl>
# 1 vg     vg              NA 217.          6175.              1         100
# 2 du     du_136986   136986   0             15.2             0         100
# 3 du     du_137017   137017   0.000000173   77.5             0         100
# 4 du     du_137034   137034   0.000000837   15.6             0         100
# 5 du     du_137087   137087   0              1.71            0         100
# 6 du     du_137088   137088   0              0.694           0         100
DBI::dbWriteTable(con, "lyrs", lyrs)

DBI::dbDisconnect(con, shutdown = T)

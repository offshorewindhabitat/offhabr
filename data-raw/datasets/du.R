# Duke University
# [Habitat-based Marine Mammal Density Models for the U.S. Atlantic: Latest Versions](https://seamap.env.duke.edu/models/Duke/EC/)
#
# Density is expressed as the number of individual animals per 100 square km.
#
# Skipping uncertainty metrics for now:
# - *_standard_error*.img
# - *_cv*.img
# - *_5_percent*.img
# - *_95_percent*.img

# packages
librarian::shelf(
  devtools, dplyr, fs, janitor, mapview, purrr, sf, stringr, terra, tibble, tidyr,
  units)
load_all()

# read layers ----
dir_g <- "/Users/bbest/My Drive/projects/offhab/data/seamap.env.duke.edu - cetaceans"

# * get East Coast (EC) rasters (*.img) of density ----
rx_ec  <- "/(.+)/(.+)_v(.+)/Rasters[/]?(.+)?/(.+)"
d_ec <- tibble(
  path_img = list.files(dir_g, "*density.*\\.img$", full.names = T, recursive = T)) %>%
  mutate(
    rel_img = str_replace(path_img, dir_g, ""),
    rgn     = str_replace(rel_img, rx_ec, "\\1"),
    sp      = str_replace(rel_img, rx_ec, "\\2") %>%
      str_replace_all("_", " "),
    ver     = str_replace(rel_img, rx_ec, "\\3"),
    yrs     = str_replace(rel_img, rx_ec, "\\4"),
    img     = str_replace(rel_img, rx_ec, "\\5"))
# d_ec %>%
#   # filter(yrs != "") %>%
#   select(-path_img, -rel_img) %>%
#   View()
d_ec %>%
  filter(yrs != "") %>%
  select(sp, yrs) %>%
  table()
#                             yrs
# sp                           2002-2008 2002-2019 2003-2009 2003-2019 2009-2019 2010-2019
# Humpback whale                    12        12         0         0        12         0
# North Atlantic right whale         0         0        12        12         0        12
# Choosing:
# - Humpback whale:             2002-2019
# - North Atlantic right whale: 2003-2019
d_ec <- d_ec %>%
  filter(
    (sp  == "Humpback whale"
     & yrs == "2002-2019") |
      (sp  == "North Atlantic right whale"
       & yrs == "2003-2019") ) %>%
  bind_rows(
    d_ec %>%
      filter(
        !sp %in% c(
          "Humpback whale",
          "North Atlantic right whale")) )

# * get Gulf of Mexico (EC) rasters (*.img) of abundance ----
rx_gom <- "/(.+)/GOM_(.+)_v(.+)/(.+)"
# rel_img <- "/GOM/GOM_Atlantic_spotted_dolphin_v4.3/GOM_Atlantic_spotted_dolphin_abundance.img"
d_gom <- tibble(
  path_img = list.files(dir_g, "*abundance.*\\.img$", full.names = T, recursive = T)) %>%
  mutate(
    rel_img = str_replace(path_img, dir_g, ""),
    rgn     = str_replace(rel_img, rx_gom, "\\1"),
    sp      = str_replace(rel_img, rx_gom, "\\2") %>%
      str_replace_all("_", " "),
    ver     = str_replace(rel_img, rx_gom, "\\3"),
    img     = str_replace(rel_img, rx_gom, "\\4"))
# d_gom %>%
#   # filter(yrs != "") %>%
#   select(-path_img, -rel_img) %>%
#   View()

d <- bind_rows(
  d_ec,
  d_gom)

d_ck <- d %>%
  select(rgn, sp, img) %>%
  group_by(rgn, sp) %>%
  nest() %>%
  mutate(
    n_img = map_int(data, nrow)) %>%
  arrange(desc(n_img), sp, rgn)
# View(d_ck)

d <- d %>%
  select(rgn, sp, ver, yrs, path_img) %>%
  nest(d_paths = path_img) %>%
  mutate(
    n_imgs = map_int(d_paths, nrow),
    r      = map(d_paths, function(d_paths){
      terra::rast(d_paths$path_img) %>%
        terra::mean(na.rm=T)
    }))

# plot(d$r[[1]])
# mapView(d$r[[1]])
# plot(d$r[[32]])
# mapView(d$r[[32]])

r_ec_alb  <- d$r[[1]]           # plot(r_ec_alb)   # WGS_1984_Albers
r_gom_alb <- d$r[[32]]          # plot(r_gom_alb)  # WGS_1984_Albers
r_cid_mer <- oh_rast("cell_id") # plot(r_cid_mer)  # WGS 84 / Pseudo-Mercator
r_zid_mer <- oh_rast("zone_id") # plot(r_zid_mer)  # WGS 84 / Pseudo-Mercator

p_cid_mer <- as.points(r_cid_mer)
p_cid_alb <- terra::project(p_cid_mer, r_ec_alb)

extract_oh_cells <- function(r_alb, i=0, n=0){
  message(glue("{i} of {n}: extract_oh_cells() ~ {Sys.time()}"))
  r_alb %>%
    terra::extract(p_cid_alb, bind=T) %>%
    as_tibble() %>%
    filter(!is.na(mean)) %>%
    rename(density = mean)
}
# check single raster
# x <- extract_oh_cells(r_gom_alb)
# r <- oh_rast("NA")
# r[x$cell_id] <- x$density
# r <- trim(r)
# plot(r)
# mapView(r, maxpixels = ncell(r))

d <- d %>%
  mutate(
    d_oh_cells = imap(r, extract_oh_cells, nrow(d)))
# 15 min for 50 rows
#  1 of 50: extract_oh_cells() ~ 2022-11-27 12:13:53
# 50 of 50: extract_oh_cells() ~ 2022-11-27 12:28:27

d <- d %>%
  mutate(
    sp   = str_trim(sp),
    imgs = map_chr(
      d_paths, ~paste(basename(.$path_img), collapse = ";")),
    yrs  = ifelse(yrs == "", NA, yrs)) %>%
  arrange(sp, rgn) %>%
  relocate(sp, rgn) %>%
  rowid_to_column("row_id")

du_density <- d %>%
  select(sp, rgn, ver, yrs, imgs, n_imgs)

# match taxa ----

# * common to scientific ----
sp_wm <- wm_rest(du_density, sp, "AphiaRecordsByVernacular")
# sp_wm[,1:5] %>% View()

# supplement with Googling
sp_wm %>%
  filter(is.na(scientificname)) %>%
  pull(sp) %>% paste0(collapse = '","",\n"') %>% cat()
sp_sci <- tribble(
                             ~sp,                   ~taxa,
  "Atlantic white sided dolphin", "Lagenorhynchus acutus",
  "Brydes whale"                , "Balaenoptera brydei",
  "Cuviers beaked whale"        , "Ziphius cavirostris",
  "Dwarf and pygmy sperm whales", "Kogia",
  "Frasers dolphin"             , "Lagenodelphis hosei",
  "Melon headed whale"          , "Peponocephala electra",
  "Mesoplodont beaked whales"   , "Mesoplodon",
  "Rissos dolphin"              , "Grampus griseus",
  "Rough toothed dolphin"       , "Steno bredanensis",
  "Seals"                       , "Pinnipedia",
  "Short beaked common dolphin" , "Delphinus delphis",
  "Short finned pilot whale"    , "Globicephala macrorhynchus",
  "Unidentified beaked whales"  , "Hyperoodontidae",
  "White beaked dolphin"        , "Lagenorhynchus albirostris")

sp_taxa <- sp_wm %>%
  select(sp, taxa = scientificname) %>%
  filter(!is.na(taxa)) %>%
  bind_rows(
    sp_sci)
# View(sp_taxa)

du_density <- du_density %>%
  left_join(
    sp_taxa,
    by = "sp") %>%
  relocate(taxa, .after=row_id)

# * scientific to WoRMS aphia_id ----
du_density <- wm_add_aphia_id(du_density, taxa)

# check not missing any
du_density %>%
  filter(is.na(aphia_id))

# write cells and du_density to db ----

oh_cells_rast <- d %>%
  select(row_id, d_oh_cells) %>%
  unnest(d_oh_cells) %>%
  mutate(
    tbl = "du_density") %>%
  select(tbl, row_id, cell_id, density)

con_oh <- oh_pg_con()
dbWriteTable(con_oh, "oh_cells_rast", oh_cells_rast)
dbWriteTable(con_oh, "du_density", du_density)

# write to rast lyrs ----
librarian::shelf(
  here, readr)
options(readr.show_col_types = F)

dir_lyrs_tif <- "/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_tif"
lyrs_csv     <- here("data-raw/layers.csv")
ds_key       <- "du"

con_oh <- oh_pg_con()

ds_du <- tbl(con_oh, "ds_du") |> collect() |>
  arrange(aphia_id, row_id) |>
  filter(!duplicated(row_id))

# * mosaic EC & GOM ----
aphia_ids <- unique(ds_du$aphia_id)
for (i in 1:length(aphia_ids) ){ # i = 1
  aphia_id <- aphia_ids[i]
  lyr_key <- glue("{ds_key}_{aphia_id}")
  r_tif <- glue("{dir_lyrs_tif}/{lyr_key}.tif")
  message(glue("{i} of {length(aphia_ids)}: {basename(r_tif)} ~ {Sys.time()}"))

  ds_sp <- ds_du |>
    filter(aphia_id == !!aphia_id)

  stk <- list()
  for (rgn in ds_sp$rgn){ # rgn = ds_sp$rgn[1]
    message(glue("  rgn: {rgn} ~ {Sys.time()}"))

    mdl_id <- ds_sp |> filter(rgn == !!rgn) |> pull(row_id)
    d_v <- tbl(con_oh, "cells_ds_rast") %>%
      filter(
        tbl    == "du_density",
        mdl_id == !!mdl_id) %>%
      select(cell_id, val) |>
      collect()

    r <- oh_rast("NA")
    r[d_v$cell_id] <- d_v$val
    # plet(r, tiles="Esri.NatGeoWorldMap")

    stk[[rgn]] <- r
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

  # write min, max to layers.csv
  d <- read_csv(lyrs_csv) |>
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
  write_csv(d,lyrs_csv)
}

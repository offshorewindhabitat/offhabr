# consolidate regional species into same raster, save in new smaller format
# for whole OH study area

librarian::shelf(
  fs, readr, terra, tidyr)
devtools::load_all()

dir_tif_in  <- "/Users/bbest/My Drive/projects/offhab/data/oceanadapt.rutgers.edu/tif"
dir_tif_out <- "/Users/bbest/My Drive/projects/offhab/data/_oh_rast0to100/oa"
spp_csv     <- glue("{dir_tif_in}/_spp.csv")
spp_rgn_csv <- glue("{dir_tif_in}/_spp_rgn.csv")
spp_wm_csv  <- glue("{dir_tif_in}/_spp_worms.csv")
taxa_csv    <- glue("{dir_tif_out}/_taxa.csv")

# read all tifs ----

tifs <- list.files(dir_tif_in, ".*\\.tif$", full.names = T)

d_tifs <- tibble(
  tif_path = tifs) %>%
  mutate(
    tif = path_ext_remove(basename(tif_path))) %>%
  separate(tif, c("rgn", "sp_key", "yr"), "_") # d_tifs
# 766 × 4

# get tifs across all regions for each sp_key
D_tifs <- d_tifs %>%
  group_by(sp_key) %>%
  nest() # D_tifs
# 657 × 2

# read spp ----
spp     <- read_csv(spp_csv)      # spp
spp_rgn <- read_csv(spp_rgn_csv)  # spp_rgn
# TODO: figure out if need to exclude spp_rgn.flagged=TRUE?
spp_wm  <- read_csv(spp_wm_csv)   # spp_wm

# TODO: handle duplicate aphia_ids ----

aphia_id_dupes <- spp_wm %>%
  filter(duplicated(AphiaID)) %>%
  pull(AphiaID)

spp_wm %>%
  filter(AphiaID %in% aphia_id_dupes) %>%
  arrange(AphiaID, sp_sci) %>%
  select(1:4, 6)
# A tibble: 8 × 5
#   sp_key  sp_sci                 sp_cmn              AphiaID scientificname
#    <chr>   <chr>                  <chr>                 <dbl> <chr>
# 1 str-na  Strongylocentrotus     NA                   123390 Strongylocentrotus
# 2 str-cf  Strongylocentrotus cf. NA                   123390 Strongylocentrotus
# 3 seb-na  Sebastes               NA                   126175 Sebastes
# 4 seb-sp  Sebastes sp.           NA                   126175 Sebastes
# 5 aur-na  Aurelia                NA                   135263 Aurelia
# 6 aur-spp Aurelia spp.           moon jellies         135263 Aurelia
# 7 pep-tra Peprilus tracanthus    butterfish           159828 Peprilus triacanthus
# 8 pep-tri Peprilus triacanthus   American butterfish  159828 Peprilus triacanthus

sp_key_dupes <- spp_wm %>%
  filter(AphiaID %in% aphia_id_dupes) %>%
  pull(sp_key)

d_tifs %>%
  filter(sp_key %in% sp_key_dupes) %>%
  arrange(sp_key, rgn, yr) %>%
  select(sp_key, rgn, yr)
# A tibble: 10 × 3
#    sp_key  rgn   yr
#    <chr>   <chr> <chr>
#  1 aur-na  wca   2018
#  2 aur-spp gmx   2019 # aur-*: diff rgns & yrs -> mosaic only
#  3 pep-tra nef   2019
#  4 pep-tra nes   2020
#  5 pep-tri nef   2019
#  6 pep-tri nes   2020 # pep-*: same rgns & yrs -> average (or sum?)
#  7 seb-na  wca   2018
#  8 seb-sp  wca   2018 # pep-*: same rgns & yrs -> average (or sum?)
#  9 str-cf  wca   2018
# 10 str-na  wca   2018 # str-*: same rgns & yrs -> average (or sum?)

# iterate over species ----
aphia_ids <- spp_wm %>%
  distinct(AphiaID) %>%
  pull(AphiaID) %>%
  sort()

# template raster
r_na <- oh_rast()

for (i in 1:length(aphia_ids)){ # i = 1
# for (i in 1:5){ # i = 1

  aphia_id <- aphia_ids[i]
  lyr      <- glue("oa_{aphia_id}")
  tif_out  <- glue("{dir_tif_out}/{lyr}.tif")
  sp_keys  <- spp_wm %>%
    filter(AphiaID == aphia_id) %>%
    pull(sp_key)

  d_i <- D_tifs %>%
    filter(sp_key %in% sp_keys) %>%
    unnest(data)
  i_tifs <- d_i$tif_path

  message(glue("{i}: {basename(tif_out)} ~ {Sys.time()}"))
  if (length(i_tifs) > 1)
    message(glue("  n_tifs: {length(i_tifs)}"))
  if (length(sp_keys) > 1)
    message(glue("  n_sp_keys: {length(sp_keys)}"))

  if (length(i_tifs) == 1){
    r <- terra::rast(i_tifs) %>%
      terra::extend(r_na)
  } else {
    # TODO: NOTE duplicate sp_keys get summed
    rs <- purrr::map(i_tifs, function(tif){
      terra::rast(tif) %>%
        terra::extend(r_na) })
    r <- rast(rs) %>%
      sum(na.rm = T)
    # plot(r)
  }

  # rescale
  v_max <- terra::global(r, max, na.rm=T) %>% as.numeric()
  r <- r / v_max * 100
  names(r) <- lyr
  # plot(r)
  # TODO: resolve non-linearity with cube-root before rescaling?

  # record rescaling and input tifs
  if (file.exists(taxa_csv))
    read_csv(taxa_csv) %>%
      filter(!(aphia_id == !!aphia_id)) %>%
      write_csv(taxa_csv)
  tibble(
    aphia_id = aphia_id,
    tifs     = paste0(basename(i_tifs), collapse = "; "),
    v_max    = v_max) %>%
    write_csv(taxa_csv, append = T)

  # write efficient raster
  offhabr::write_rast(r, tif_out)
}



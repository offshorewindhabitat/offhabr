worker <- 6

librarian::shelf(
  # raquamaps/aquamapsdata,
  # raquamaps/raquamaps,
  # caret,
  # devtools,
  glue,
  # janitor,
  DBI, dplyr,
  # librarian, mapview,
  purrr,
  readr, sf,
  # stringr,
  terra)
  # zeallot)

devtools::load_all()

dir_data_rast = "/Users/bbest/My Drive/projects/offhab/data/_oh_rast0to100"
dir_tif <- glue("{dir_data_rast}/am")

# con_am <- am_pg_con()
con_oh <- offhabr::oh_pg_con()

spp <- tbl(con_oh, "ds_am_spp") %>%
  collect()
# sum(duplicated(spp$aphia_id)) # 134

r_na <- oh_rast()

write_sp_rast_am <- function(i, worker = 1){

  ds_key   <- "am"
  sp_id    <- spp$species_id[i]
  aphia_id <- spp$aphia_id[i]
  taxa     <- spp$taxa[i]
  lyr      <- glue::glue("{ds_key}_{aphia_id}")
  tif      <- glue::glue("{dir_tif}/{lyr}.tif")
  log_txt  <- glue("{dirname(dir_tif)}/am_log_{worker}.txt")

  readr::write_lines(
    glue::glue("{i}/{nrow(spp)}: {aphia_id} ({taxa}) ~ {Sys.time()}"),
    log_txt,
    append = T)

  g_gcs <- sf::st_read(
    con_oh,
    query = glue::glue(
      "WITH
      s_c AS (
        SELECT hcaf_id, probability
        FROM ds_am_spp_cells
        WHERE species_id = '{sp_id}'),
      c AS (
        SELECT hcaf_id, geom
        FROM ds_am_cells
      )
      SELECT probability, geom
      FROM s_c
      LEFT JOIN c USING (hcaf_id)"))
 # DBI::dbDisconnect(con_oh)

  g_mer <- sf::st_transform(g_gcs, 3857)
  r <- terra::rasterize(g_mer, r_na, "probability")

  if (sum(!is.na(values(r))) == 0){
    readr::write_lines(
      "  all NA -- SKIPPING!",
      log_txt,
      append = T)
    return(F)
  }

  r <- round(r*100)
  # range(r)
  # plot(r)
  # mapview::mapView(g_gcs)

  names(r) <- lyr
  offhabr::write_rast(r, tif)

  return(T)
}
# 1/9780: 392274 (Leiogalathea laevirostris) ~ 2022-12-31 10:40:15
# 45/9780: 280354 (Coelorinchus ventrilux) ~ 2022-12-31 10:44:40

# cl <- new_cluster(parallel::detectCores() - 1)
# cluster_library(
#   cl, c(
#     "DBI", "dplyr", "glue", "offhabr", "readr", "purrr", "sf", "terra"))
# cluster_assign(
#   cl,
#   write_sp_rast_am = write_sp_rast_am,
#   spp              = spp,
#   dir_tif          = dir_tif,
#   r_na             = r_na)

# librarian::shelf(furrr, tictoc)
# plan(multisession, workers = parallel::detectCores() - 1)

# partition(100:nrow(spp)
i_dif <- round(nrow(spp)/7)
d <- tibble(
  i = (worker*i_dif + 1):((worker+1)*i_dif)) %>%
  # partition(cl) %>%
  mutate(
    # status = map_lgl(i, write_sp_rast_am, spp, dir_tif, r_na))
    # status = furrr::future_map_lgl(i, write_sp_rast_am, spp, dir_tif, r_na))
    status = map_lgl(i, write_sp_rast_am, worker = worker))


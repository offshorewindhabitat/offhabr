# after running rl.R, pull ranges into rasters

# packages, paths, con ----
librarian::shelf(
  DBI, devtools, dplyr, duckdb, fs, glue, janitor, mapview, purrr, sf, stringr,
  terra, tibble, tidyr, units, worrms)
load_all()
options("duckdb.enable_rstudio_connection_pane"=TRUE)

# connect to databases
con_dk <- dbConnect(duckdb(dbdir = "offhab.duckdb"))
con_rl <- rl_pg_con()

# TODO: investigate duplicates ----
aphia_ids <- tbl(con_rl, "rl_range") %>%
  filter(!is.na(aphia_id)) %>%
  pull("aphia_id")
dupes <- aphia_ids[duplicated(aphia_ids)] %>% unique()
length(dupes) # 232
dupes[1] # 445367
tbl(con_rl, "rl_range") %>% # colnames()
  select(-geom) %>%
  filter(aphia_id == 445367) %>%
  # collect() %>% View()
  select(aphia_id, taxa, binomial, subspecies, subpop) %>%
  collect()
#   aphia_id taxa               binomial           subspecies subpop
#      <int> <chr>              <chr>              <chr>      <chr>
# 1   445367 Haliotis marmorata Haliotis marmorata NA         NA
# 2   445367 Haliotis virginea  Haliotis virginea  NA         NA
# WHOAH! Different species, yet same aphia_id?!

bb <- st_bbox(oh_zones) %>% st_as_sfc() %>% st_as_text(EWKT = T)
aphia_ids <- dbGetQuery(
  con_rl,
  glue("WITH
    z AS (
      SELECT '{bb}'::geometry AS geom),
    r AS (
      SELECT aphia_id, geom
      FROM rl_range
      WHERE
        presence IN (1, 2, 3, 4))
    SELECT aphia_id
    FROM r
    JOIN z ON ST_Intersects(r.geom, z.geom)")) %>%
  pull(aphia_id)
length(aphia_ids) # 1012
table(duplicated(aphia_ids))
# FALSE  TRUE
#   961    51
aphia_ids <- aphia_ids[!duplicated(aphia_ids)]
length(aphia_ids) # 961

dir_data_rast = "/Users/bbest/My Drive/projects/offhab/data/_oh_rast0to100"

dir_tif <- glue("{dir_data_rast}/rl")
# TODO: function

ds_key <- "rl"
aphia_id <- aphia_ids[2]

aphia_ids <- na.omit(aphia_ids) %>% sort()
length(aphia_ids) # 960
for (i in 1:length(aphia_ids)){ # i = 1
  aphia_id  <- aphia_ids[i]

  # S Africa: 445301 Haliotis alfredensis https://www.iucnredlist.org/species/78748407/78772388

  a <- tbl(con_rl, "rl_range") %>% # colnames()
    select(-geom) %>%
    filter(aphia_id == !!aphia_id) %>%
    # collect() %>% View()
    select(aphia_id, taxa, binomial, subspecies, subpop) %>%
    collect()
  message(glue("{i}/{length(aphia_ids)}: {a$aphia_id} ({a$taxa}) ~ {Sys.time()}"))

  lyr <- glue("{ds_key}_{aphia_id}")
  tif <- glue("{dir_tif}/{lyr}.tif")

  g_gcs <- read_sf(
    con_rl,
    query = glue(
      "SELECT geom FROM rl_range WHERE aphia_id = {aphia_id}"))

  oh_bbox_mer <- st_bbox(oh_zones) %>% st_as_sfc()
  oh_bbox_gcs <- st_transform(oh_bbox_mer, 4326)

  # TODO: check filter for intersecting geoms
  # g_gcs <- g_gcs %>%
  #   filter(st_intersects(g_gcs, oh_bbox_gcs, sparse = F)[1, ])

  g_mer <- st_transform(g_gcs, 3857)
  r_na <- oh_rast()
  r <- rasterize(g_mer, r_na)

  if (sum(!is.na(values(r))) == 0){
    message("  all NA -- skipping")
    next()
  }

  r[r == 1] <- 50

  names(r) <- lyr
  write_rast(r, tif)
}

tifs <- list.files(dir_tif, ".*\\.tif$", full.names = T)
stk <- rast(tifs)
# Error: [rast] extents do not match
ext_oh <- ext(oh_rast())
d <- tibble(
  tif = tifs) %>%
  mutate(
    ext_match = map_lgl(tif, ~ ext(rast(.)) == ext_oh))
bad_tif <- d %>% filter(!ext_match) %>% pull(tif)
basename(bad_tif) # rl_445301.tif
unlink(bad_tif)
tifs_oh <- d %>% filter(ext_match) %>% pull(tif)
stk <- rast(tifs_oh)
names(stk)

write_rast(stk, glue("{dir_data_rast}/rl.tif"))

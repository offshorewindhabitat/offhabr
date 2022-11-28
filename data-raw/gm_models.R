#
# [Cetacean and sea turtle spatial density model outputs from visual observations using line-transect survey methods aboard NOAA vessel and aircraft platforms in the Gulf of Mexico from 2003-06-12 to 2019-07-31 (NCEI Accession 0256800)](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:256800)
#
# additional metadata: https://www.fisheries.noaa.gov/inport/item/67830
#
# ncei.noaa.gov - GoMex cetacean & sea turtle SDMs/
# -
#

# packages
librarian::shelf(
  devtools, dplyr, fs, glue, here, janitor, mapview, purrr, readr, readxl, sf, stringr,
  tibble, tidyr, units)
load_all()
options(readr.show_col_types = F)

# helper functions ----
get_annual_density <- function(d_sf, i=0){
  # i = 12; d_sf <- d$sf[[i]]
  message(glue("{i} of {nrow(d)} in get_annual_density() ~ {Sys.time()}"))

  # get geometries
  geoms <- d_sf %>%
    select(hexid = HEXID, geom = geometry)

  # get *_n abundance (#/40km2) per month and calculate annual average density (#/km2)
  mos_n <- glue("{month.abb}_n")
  attrs <- d_sf %>%
    st_drop_geometry() %>%
    select(hexid = HEXID, any_of(mos_n)) %>%
    pivot_longer(-hexid, names_to = "mo_n", values_to = "n") %>%
    filter(n != -9999) %>%
    group_by(hexid) %>%
    summarize(
      n = mean(n),
      .groups = "drop") %>%
    mutate(
      density = n / 40) %>%
    select(hexid, density) # individuals / km2

  # return geometries joined with summarized attributes
  d_sum <- geoms %>%
    left_join(
      attrs, by = "hexid")
  d_sum
}

# read layers ----
dir_shp   <- "/Users/bbest/My Drive/projects/offhab/data/ncei.noaa.gov - GoMex cetacean & sea turtle SDMs/0256800/2.2/data/0-data/NOAA_SEFSC_Cetacean_SeaTurtle_SDM_shapefiles"
taxa_xls <- glue("{dir_shp}/../spp_gmx.xlsx")

taxa <- read_excel(taxa_xls)

d <- tibble(
  path_shp = list.files(dir_shp, "shp$", full.names=T)) %>%
    mutate(
      base_shp = basename(path_shp) %>% path_ext_remove(),
      taxa_shp = str_replace(base_shp, "(.*)_Monthly_.*", "\\1")) %>%
  left_join(
    taxa %>%
      select(taxa_shp, taxa_sci, taxa_doc),
    by = "taxa_shp") %>%
  rowid_to_column("mdl_id") %>%
  mutate(
    sf        = map(path_shp, read_sf),
    d_density = imap(sf, get_annual_density),
    months_lst = map(sf, ~{
      intersect(
        colnames(.),
        glue("{month.abb}_n")) %>%
        str_replace("_n", "")  }),
    n_months = map_int(months_lst, length),
    months   = map_chr(months_lst, paste, collapse=";"))

# mapView(di, zcol="n")
# mapView(di, zcol="area_km2")
# # mapView(di, zcol="density0")
# mapView(di, zcol="density")
range(di$n)        # 10,752.92  400,910.00
range(di$density)  #    268.82   10,022.75
range(di$area_km2)   # 5.208291e-05 4.000006e+01
# range(di$density0) # 2.688238e+02 3.092492e+09

gm_models <- d %>%
  select(mdl_id, taxa_sci, taxa_doc, months, n_months)

mdl_hex <- d %>%
  select(
    mdl_id, d_density) %>%
  unnest(d_density)

gm_model_hexagons <- mdl_hex %>%
  group_by(hexid, geom) %>%
  summarize(.groups="drop") %>%
  st_as_sf()

gm_model_hexagon_densities<- mdl_hex %>%
  st_drop_geometry() %>%
  select(mdl_id, hexid, density) %>%
  filter(!is.na(density))


# add aphia_id ----
gm_models <- wm_add_aphia_id(gm_models, taxa_sci)

# TODO?: combine Oceanic_* & Shelf_* for AtlanticSpotted_Dolphin + CommonBottlenose_Dolphin

# write to database ----
con <- oh_pg_con()
dbWriteTable(con, "gm_models", gm_models, overwrite=T)
st_write(gm_model_hexagons, con, "gm_model_hexagons", delete_layer=T)
dbWriteTable(con, "gm_model_hexagon_densities", gm_model_hexagon_densities, overwrite=T)

create_index(con, "gm_models", "mdl_id", unique = T)
create_index(con, "gm_model_hexagons", "hexid", unique = T)
create_index(con, "gm_model_hexagon_densities", c("mdl_id", "hexid"), unique = T)

# *_ply x cell_id ----
tbl(con, "gm_model_hexagons")
oh_cells_ply <- dbGetQuery(
  con,
  "WITH
  c  AS (
    SELECT
      cell_id, geom
    FROM oh_cells),
  d AS (
    SELECT
      hexid AS ply_id, ST_Transform(geom, 4326) AS geom
    FROM gm_model_hexagons)
  SELECT DISTINCT ON (tbl, ply_id, cell_id)
    'gm_model_hexagons' AS tbl,
    d.ply_id,
    c.cell_id
  FROM c JOIN
    d ON ST_Covers(d.geom, c.geom)") %>%
  tibble()
oh_cells_ply
dbSendQuery(con, "DELETE FROM oh_cells_ply WHERE tbl = 'gm_model_hexagons'")
dbAppendTable(con, "oh_cells_ply", oh_cells_ply)

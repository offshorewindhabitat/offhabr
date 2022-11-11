# transfer AquaMaps from sqlite to postgres database

# https://raquamaps.github.io/aquamapsdata/articles/intro.html#data-scope-and-content-1

# The database is incomplete in terms of species mapped (26,399 /33,518). It is based on AquaMaps’ conservative rule of generating envelopes and predictions for species with >=10 ‘good cells’ and excludes records of data-poor species (i.e. endemic and/or rare species). Please contact the AquaMaps team directly if you want access to the complete dataset.
#
# The map data provided (hcaf_species_native table) give computer-generated predictions. Please contact the AquaMaps team directly if you need to access the latest reviewed/improved species maps.
#
# Map data showing future species distributions for 2050 and 2100 (under different RCP scenarios) are excluded. Please contact the AquaMaps team directly if you are interested in these datasets.

# dependency for aquamapsdata:
#  - Terminal: brew install gnupg
#  - R: install.packages("rcrypt")
librarian::shelf(
  raquamaps/aquamapsdata,
  raquamaps/raquamaps,
  devtools, glue,
  DBI, dplyr, librarian, mapview, purrr,
  readr, sf, stringr, terra, zeallot)

devtools::load_all()
con_pg <- oh_pg_con()

# make database spatial
dbSendQuery(
  con_pg, "CREATE EXTENSION postgis;")

# downloads about 2 GB of data, approx 10 GB when unpacked
# download_db()

# data(package = "raquamaps")

con_sl <- aquamapsdata::default_db("sqlite") # /Users/bbest/Library/Application Support/aquamaps/am.db
# dbListTables(con_sl)
# [1] "fts"                 "fts_config"
# [3] "fts_content"         "fts_data"
# [5] "fts_docsize"         "fts_idx"
# [7] "hcaf_r"              "hcaf_species_native"
# [9] "hspen_r"             "occurrencecells_r"
# [11] "speciesoccursum_r"

tibble(
  tbl_am = dbListTables(con_sl) %>%
    str_subset("^(?!fts).*"),
  tbl_pg = glue("am_{am_tbl}")) %>%
  write_csv("data-raw/aquamaps_tbls.csv")

# manually copied and updated tbl_pg column
d_tbls <- read_csv("data-raw/aquamaps_tbls_renamed.csv") %>%
  arrange(tbl_pg)
# d_tbls
#   tbl_am              tbl_pg
# 1 hcaf_r              am_cells
# 2 speciesoccursum_r   am_spp
# 3 hcaf_species_native am_spp_cells
# 4 occurrencecells_r   am_spp_occs
# 5 hspen_r             am_spp_prefs

# write from sqlite to postgresql database
for (i in 1:nrow(d_tbls)){
  with(d_tbls[i,],{
    message(glue("{tbl_am} -> {tbl_pg}"))
    d <- dbReadTable(con_sl, tbl_am)
    dbWriteTable(con_pg, tbl_pg, d)
  })
}

# write metadata
dbWriteTable(
  con_pg, "am_meta", aquamapsdata::am_meta)

# reduce am_spp_cells from 9.4G to 224M (per DBeaver)
dbSendQuery(
  con_pg,
  'DELETE FROM am_spp_cells
   WHERE "CsquareCode" NOT IN (
    SELECT "CsquareCode" FROM am_cells_ply);')

# tbl(con_pg, "am_spp_cells")
# tbl(con_pg, "am_cells_ply")

plot(am_cells_ply["ID"])
# create indexes
dbSendQuery(
  con_pg,
  'CREATE INDEX IF NOT EXISTS am_spp_cells_CenterLong_idx ON am_spp_cells("CenterLong")')
dbSendQuery(
  con_pg,
  'CREATE INDEX IF NOT EXISTS am_spp_cells_CenterLat_idx ON am_spp_cells("CenterLat")')
dbSendQuery(
  con_pg,
  'CREATE INDEX IF NOT EXISTS am_spp_cells_CsquareCode_idx ON am_spp_cells("CsquareCode")')

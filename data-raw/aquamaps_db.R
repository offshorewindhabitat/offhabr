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
  devtools, glue, janitor,
  DBI, dplyr, librarian, mapview, purrr,
  readr, sf, stringr, terra, zeallot)

devtools::load_all()
oh_pg <- oh_pg_con()
am_pg <- am_pg_con()

# make databases spatial
dbSendQuery(
  oh_pg, "CREATE EXTENSION IF NOT EXISTS postgis;")
dbSendQuery(
  am_pg, "CREATE EXTENSION IF NOT EXISTS postgis;")

# enable cross-database querying
dbSendQuery(
  oh_pg, "CREATE EXTENSION IF NOT EXISTS postgres_fdw")
dbSendQuery(
  oh_pg,
  "CREATE SERVER aquamaps_server FOREIGN DATA WRAPPER postgres_fdw
   OPTIONS (host 'localhost', port '5432', dbname 'aquamaps')")
dbSendQuery(
  oh_pg,
  "CREATE USER MAPPING FOR bbest SERVER aquamaps_server OPTIONS (user 'bbest')")
dbSendQuery(
  oh_pg,
  "CREATE SCHEMA aquamaps")
# does NOT work:
#   tbl(oh_pg, "aquamaps.cells")
# does work:
#   dbGetQuery(oh_pg, "SELECT * FROM aquamaps.cells LIMIT 10")

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
  tbl_pg = glue("am_{tbl_am}")) %>%
  write_csv("data-raw/aquamaps_tbls.csv")

# manually copied and updated tbl_pg column
d_tbls <- read_csv("data-raw/aquamaps_tbls_renamed.csv") %>%
  arrange(tbl_pg)
d_flds <- read_csv("data-raw/aquamaps_flds_renamed.csv") %>%
  arrange(tbl_am, fld_old)

# d_tbls
#   tbl_am              tbl_pg
#   <chr>               <chr>
# 1 hcaf_r              cells
# 2 speciesoccursum_r   spp
# 3 hcaf_species_native spp_cells
# 4 occurrencecells_r   spp_occs
# 5 hspen_r             spp_prefs

# write from sqlite to postgresql database
#   and change columns to lowercase so don't have to quote columns in postgres queries
for (i in 1:nrow(d_tbls)){ # i=1
  with(d_tbls[i,],{
    # attach(d_tbls[i,])
    # detach(d_tbls[i,])
    message(glue("{tbl_am} -> {tbl_pg}"))

    # d <- dbGetQuery(con_sl, glue("SELECT * FROM {tbl_am} LIMIT 10"))
    d <- dbReadTable(con_sl, tbl_am)

    d_flds_tbl_0 <- tibble(
      tbl_am  = !!tbl_am,
      fld_old = colnames(d),
      fld_new = "",
      fld_jan = d %>%
        clean_names() %>%
        colnames())
    if (i == 1){
      write_csv(d_flds_tbl_0, "data-raw/aquamaps_flds.csv")
    } else {
      write_csv(d_flds_tbl_0, "data-raw/aquamaps_flds.csv", append=T)
    }

    d_flds_tbl <- d_flds %>%
      filter(tbl_am == !!tbl_am)
    d <- d %>%
      rename_with(
        .fn = ~d_flds_tbl %>%
          filter(fld_old == .x) %>%
          pull(fld_new),
        .cols = all_of(d_flds_tbl$fld_old)) %>%
        janitor::clean_names()

    dbWriteTable(am_pg, tbl_pg, d, overwrite = T)
  })
}

# write metadata
flds_key <- with(d_flds, setNames(fld_new, fld_old))
tbls_key <- with(d_tbls, setNames(tbl_pg, tbl_am))
m <- aquamapsdata::am_meta %>%
  rename(
    table_orig = table,
    field_orig = field) %>%
  mutate(
    table  = recode(table_orig, !!!tbls_key),
    field2 = recode(field_orig, !!!flds_key)) %>%
  group_by(table) %>%
  mutate(
    field = make_clean_names(field2)) %>%
  select(-field2) %>%
  relocate(table, .before=table_orig) %>%
  relocate(field, .after=table_orig)
dbWriteTable(
  am_pg, "meta", m, overwrite = T)

# TODO: add prefix am_
am_m <- m %>%
  mutate(
    table = glue("am_{table}"))
dbWriteTable(
  oh_pg, "am_meta", am_m, overwrite = T)

# refresh cross-database schema
dbSendQuery(
  oh_pg,
  "DROP SCHEMA IF EXISTS aquamaps CASCADE")
dbSendQuery(
  oh_pg,
  "CREATE SCHEMA aquamaps")
dbSendQuery(
  oh_pg,
  "IMPORT FOREIGN SCHEMA public from SERVER aquamaps_server into aquamaps")

# get reduced set of rows specific to OffHab study area
#  for portablility (ie xfer to server) and speed (faster querying)
csq_oh     <- unique(am_cells_ply$CsquareCode)
csq_oh_sql <- paste(csq_oh, collapse = "','")

# reduce am_spp_cells from 9.4G to 224M (per DBeaver)
dbSendQuery(oh_pg, "DROP TABLE IF EXISTS am_spp_cells")
dbSendQuery(
  oh_pg,
  glue("CREATE TABLE am_spp_cells AS
   SELECT * FROM aquamaps.spp_cells
   WHERE csquare_code IN ('{csq_oh_sql}')"))
dbSendQuery(oh_pg, "DROP TABLE IF EXISTS am_spp")
dbSendQuery(
  oh_pg,
  "CREATE TABLE am_spp AS
   SELECT * FROM aquamaps.spp
   WHERE species_id IN (
     SELECT species_id FROM am_spp_cells GROUP BY species_id)")
dbSendQuery(oh_pg, "DROP TABLE IF EXISTS am_spp_prefs")
dbSendQuery(
  oh_pg,
  "CREATE TABLE am_spp_prefs AS
   SELECT * FROM aquamaps.spp_prefs
   WHERE species_id IN (
     SELECT species_id FROM am_spp)")
dbSendQuery(oh_pg, "DROP TABLE IF EXISTS am_cells")
dbSendQuery(
  oh_pg,
  "CREATE TABLE am_cells AS
   SELECT * FROM aquamaps.cells
   WHERE csquare_code IN (
    SELECT csquare_code FROM am_spp_cells GROUP BY csquare_code)")
# NOTE: skipping aquamaps.spp_occs since occurrences used
#   to model spp_cells not so relevant for now

# cleanup storage to reflect new reduced size
dbSendQuery(
  oh_pg,
  'VACUUM (FULL, ANALYZE)')
dbSendQuery(
  am_pg,
  'VACUUM (FULL, ANALYZE)')

# create indexes
dbSendQuery(
  oh_pg,
  'CREATE INDEX IF NOT EXISTS am_spp_cells_CenterLong_idx ON am_spp_cells("CenterLong")')
dbSendQuery(
  oh_pg,
  'CREATE INDEX IF NOT EXISTS am_spp_cells_CenterLat_idx ON am_spp_cells("CenterLat")')
dbSendQuery(
  oh_pg,
  'CREATE INDEX IF NOT EXISTS am_spp_cells_CsquareCode_idx ON am_spp_cells("CsquareCode")')

dbSendQuery(oh_pg, 'ALTER TABLE am_cells RENAME COLUMN "ID" TO hcaf_id')
dbSendQuery(
  oh_pg,
  'CREATE INDEX IF NOT EXISTS am_cells_hcaf_id_idx ON am_cells("hcaf_id")')

am_spp_cells <- tbl(oh_pg, "am_spp_cells") %>%
  left_join(
    tbl(oh_pg, "am_cells") %>%
      select(CsquareCode, hcaf_id),
    by = "CsquareCode") %>%
  collect()
dbWriteTable(oh_pg, "am_spp_cells", am_spp_cells, overwrite=T)
dbSendQuery(
  oh_pg,
  'CREATE INDEX IF NOT EXISTS am_spp_cells_hcaf_id_idx ON am_spp_cells("hcaf_id")')

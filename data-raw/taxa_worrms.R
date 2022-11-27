# TODO: wrap below into generic function for other taxa_tbl & taxa_fld sources
librarian::shelf(
  dplyr, glue, httr2, purrr, stringr, tidyr,
  worrms)
devtools::load_all()

# paging limit max request lookup for wm_rest() function ----

wm_rest_params <- tibble::tribble(
         ~type ,                 ~operation, ~max_req,              ~param,
  "sci_fuzzy"  , "AphiaRecordsByMatchNames",       NA, "scientificnames[]",
  # "sci_exact"  ,      "AphiaRecordsByNames",      500, "scientificnames[]",
  # IUCN RL LOBSTERS.gpkg (n=207) <error/httr2_http_414> Error in `resp_abort()`: ! HTTP 414 URI Too Long.
  # IUCN RL LOBSTERS.gpkg 12056 characters is too long
  # IUCN RL REEF_FORMING_CORALS_PART2_valids.gpkg get_df(resp, 1) 8823 characters is too long
  "sci_exact"  ,      "AphiaRecordsByNames",      100, "scientificnames[]",
  "aphia_exact",   "AphiaRecordsByAphiaIDs",       50,        "aphiaids[]")
usethis::use_data(wm_rest_params, overwrite = T)


# get taxa with non-exact matching aphia_id
# taxa_matchother <- tbl(con, "taxa") %>%
#   select(aphia_id, taxa) %>%
#   left_join(
#     tbl(con, "taxa_wm") %>%
#       select(aphia_id, scientificname),
#     by = "aphia_id") %>%
#   filter(taxa != scientificname) %>%
#   collect()
# View(taxa_matchother)

# from db table `am_spp` -> `taxa` ----

con <- oh_pg_con()

# add taxa to first column of am_spp since single field not already referencable
am_spp <- tbl(con, "am_spp") %>%
  collect() %>%
  mutate(
    taxa = glue("{genus} {species}")) %>%
  relocate(taxa)
dbWriteTable(con, "am_spp", am_spp, overwrite=T)

taxa <- am_spp
  select(taxa) %>%
  mutate(
    tbl = "am_spp",
    fld = "taxa") %>%
  relocate(tbl, fld, taxa)

# wm_records_names() ----

# page through results (more than ~100 at a time bonks)
d <- tibble(
  i_beg = seq(1, nrow(taxa), by=200),
  i_end = c(seq(100, nrow(taxa), by=200)))
if (max(d$i_end) < nrow(taxa)){
  d <- d %>%
    bind_rows(
      tibble(
        i_beg = d$i_end[nrow(d)],
        i_end = nrow(taxa)))
}

# look for exact matching names
message(glue("Paging (100 ea) through {nrow(taxa)} taxa via worrms::wm_records_names()"))
wm_n <- d %>%
  mutate(
    wm = map2(i_beg, i_end, ~{
      message(glue("  {.x}:{.y}"))
      wm_records_names(name = taxa$taxa[.x:.y]) %>%
        bind_rows()
    })) %>%
  unnest(wm) %>%
  select(-i_beg, -i_end) %>%
  filter(!duplicated(scientificname))

# get taxa exactly matched
taxa_wm_e <- taxa %>%
  inner_join(
    wm_n,
    by = c(taxa = "scientificname"),
    keep = T)

# get taxa missing to still match
taxa_na <- taxa %>%
  anti_join(
    wm_n,
    by = c(taxa_orig = "scientificname"))

# wm_records_taxamatch() ----
#   match still missing taxa using fuzzy matching
message(glue(
  "Matched {nrow(taxa_wm_e)} taxa with exact match,
  but still missing {nrow(taxa_na)} taxa,
  so using individual fuzzy match a la worrms::wm_records_taxamatch()
  taking ~ 1 sec per taxa request"))

get_req <- function(sci){
  request("https://www.marinespecies.org/rest") %>%
    req_url_path_append("AphiaRecordsByMatchNames") %>%
    req_url_query(
      `scientificnames[]` = sci)
}

get_df <- function(resp, i=0){
  # imap(resp, get_df) sets i for debugging problematic row
  # message(glue("i: {i}"))
  # if (i == 44)
  #   browser()
  if (resp$status_code == 204) # No Content
    return(NA)

  resp %>%
    resp_body_json(simplifyVector = T, flatten = T) %>%
    lapply(tibble::as_tibble) %>%
    bind_rows()
}

# fuzzy match taxa
taxa_wm_f <- taxa_na %>%
  mutate(
    req  = map(taxa_orig, get_req),
    resp = multi_req_perform(req), # 58 min/4,868 reqs; 0.7 sec/req
    df   = map(resp, get_df)) %>%
  select(-req, -resp) %>%
  unnest(df)

# remove duplicates
taxa_dupes <- taxa_wm_f %>% filter(duplicated(taxa_orig)) %>% pull(taxa_orig)
if (length(taxa_dupes) > 0){

  taxa_wm_fd <- taxa_wm_f %>%
    filter(
      taxa_orig %in% taxa_dupes) %>%
    arrange(taxa_orig, scientificname) %>%
    filter(status == "accepted") %>%
    group_by(taxa_orig) %>%
    arrange(desc(modified)) %>%
    mutate(
      i = row_number(modified)) %>%
    filter(i == 1) %>%
    select(-i)

  taxa_wm_f <- taxa_wm_f %>%
    filter(
      !taxa_orig %in% taxa_dupes) %>%
    bind_rows(
      wm_td) %>%
    arrange(taxa_orig, scientificname)
}

# join: wm_records_names() & wm_records_taxamatch() ----
taxa_wm <- bind_rows(
  taxa_wm_e,
  taxa_wm_f) %>%
  relocate(taxa_tbl, taxa_fld, taxa_key) %>%
  arrange(taxa_tbl, taxa_fld, taxa_key) %>%
  janitor::clean_names()

dbWriteTable(con, "taxa_wm", taxa_wm, overwrite = T)
# taxa_wm <- dbReadTable(con, "taxa_wm") %>%
#   janitor::clean_names()
# dbWriteTable(con, "taxa_wm", taxa_wm, overwrite = T)

# post-hoc table fixes for am_spp, taxa_wm -> taxa

am_spp <- tbl(con, "am_spp") %>%
  collect() %>%
  left_join(
    taxa %>%
      select(taxa, aphia_id),
    by = "taxa") %>%
  relocate(aphia_id, taxa)
dbWriteTable(
  con, "am_spp", am_spp, overwrite = T)
create_index(con, "am_spp", "aphia_id", unique = T)

taxa <- taxa %>%
  select(tbl, fld, taxa, aphia_id)
dbWriteTable(
  con, "taxa", taxa, overwrite = T)
create_index(con, "taxa", c("tbl", "aphia_id"), unique = T)

# recreating taxa_wm from taxa with aphia_id ----

# recode taxa.fld = "species_id" to "taxa"
taxa <- dbReadTable(con, "taxa") %>%
  mutate(
    fld = recode(fld, species_id = "taxa"))
dbWriteTable(
  con, "am_spp", am_spp, overwrite = T)


# move columns to front: aphia_id, taxa
am_spp <- dbReadTable(
  con, "am_spp") %>%
  relocate(aphia_id, taxa)
dbWriteTable(
    con, "am_spp", am_spp, overwrite = T)

aphias <- tbl(con, "taxa") %>%
  pull(aphia_id)
# length(aphias)          # 9,766
# sum(duplicated(aphias)) #    24

# am_spp <- tbl(con, "am_spp") %>%
#   collect()
# nrow(am_spp) # 9780

# page through results, since wm_record() only does max 50 at a time
d <- tibble(
  i_beg = seq(1, nrow(taxa), by=50),
  i_end = c(seq(50, nrow(taxa), by=50), nrow(taxa)))
taxa_wm <- d %>%
  mutate(
    wm = map2(i_beg, i_end, ~{
      message(glue("  {.x}:{.y} ~ {Sys.time()}"))
      worrms::wm_record(id = aphias[.x:.y]) %>%
        bind_rows()
    })) %>%
  unnest(wm) %>%
  select(-i_beg, -i_end) %>%
  janitor::clean_names() %>%
  filter(!duplicated(aphia_id))
dbWriteTable(
    con, "taxa_wm", taxa_wm, overwrite = T)
create_index(con, "taxa_wm", "aphia_id", unique = T, overwrite = T)

# ∆ *.aphia_id to valid_aphia_id in am_spp, taxa_wm, taxa ----
taxa_wm_x <- tbl(con, "taxa_wm") %>%
  filter(aphia_id != valid_aphia_id) %>%
  select(aphia_id, valid_aphia_id) %>%
  collect()

am_spp <- tbl(con, "am_spp") %>%
  collect() %>%
  left_join(
    taxa_wm_x, by = "aphia_id") %>%
  mutate(
    aphia_id = if_else(
      !is.na(valid_aphia_id),
      valid_aphia_id,
      aphia_id)) %>%
  select(-valid_aphia_id)
dbWriteTable(
  con, "am_spp", am_spp, overwrite = T)

taxa <- tbl(con, "taxa") %>%
  collect() %>%
  left_join(
    taxa_wm_x, by = "aphia_id") %>%
  mutate(
    aphia_id = if_else(
      !is.na(valid_aphia_id),
      valid_aphia_id,
      aphia_id)) %>%
  select(-valid_aphia_id)
dbWriteTable(
  con, "taxa", taxa, overwrite = T)

taxa <- tbl(con, "taxa") %>%
  collect() %>%
  mutate(
    fld = recode(fld, species_id="taxa"))
dbWriteTable(
  con, "taxa", taxa, overwrite = T)
create_index(con, "taxa", c("tbl", "fld", "taxa"), unique = T, overwrite = T)

dbSendQuery(
  con,
  "DELETE FROM taxa_wm WHERE aphia_id != valid_aphia_id")
taxa_wm_aphia_ids <- tbl(con, "taxa_wm") %>% pull(aphia_id)
taxa_wm_x <- taxa_wm_x %>%                       # nrow: 1,054
  filter(!valid_aphia_id %in% taxa_wm_aphia_ids) # nrow:   966
# TODO: implications of same aphia_id for different am_spp?
taxa_wm_y <- wm_aphia_records(taxa_wm_x$valid_aphia_id, verbose = T)
dbAppendTable(
  con,
  "taxa_wm",
  taxa_wm_y)


# taxa_wm_attr: wm_classification_() ----
# TODO: possibly page through results since slow

taxa_wm <- dbReadTable(con, "taxa_wm")

taxa_wm_class <- wm_classification_(
  id = taxa_wm$AphiaID[1:10])
dbWriteTable(con, "taxa_wm_class", taxa_wm_class, overwrite = T)

# taxa_wm_attr ----
# TODO: possibly page through results since slow
taxa_wm_attr <- wm_attr_data_(
  id = taxa_wm$AphiaID[1:10], include_inherited = T)
# View(taxa_wm_attr) # 55 × 11
with(taxa_wm_attr, AphiaID != AphiaID_Inherited) %>% sum()
# 47

taxa_wm_attrF <- wm_attr_data_(
  id = taxa_wm$AphiaID[1:10], include_inherited = F)
# View(taxa_wm_attrF) # 9 x 11
# AphiaID == AphiaID_Inherited
with(taxa_wm_attrF, AphiaID != AphiaID_Inherited) %>% sum()
# 1

# TODO: handle children column
dbWriteTable(con, "taxa_wm_attr", taxa_wm_attr, overwrite = T)


# test ----
tbl(con, "taxa") %>%
  group_by(tbl) %>%
  summarize(n = n()) %>%
  collect()



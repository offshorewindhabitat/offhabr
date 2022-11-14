# TODO: wrap below into generic function for other taxa_tbl & taxa_fld sources
librarian::shelf(
  dplyr, glue, httr2, purrr, stringr, tidyr,
  worrms)
devtools::load_all()

# from db table `am_spp` -> `taxa` ----

con <- oh_pg_con()

taxa <- tbl(con, "am_spp") %>%
  arrange(genus, species) %>%
  collect() %>%
  select(taxa_key = species_id, genus, species) %>%
  mutate(
    taxa_tbl  = "am_spp",
    taxa_fld  = "species_id",
    taxa_orig = glue("{genus} {species}") %>%
      as.character(),
    taxa_rank = "species") %>%
  relocate(taxa_tbl, taxa_fld, taxa_key) %>%
  select(-genus, -species)

# wm_records_names() ----

# page through results (more than ~100 at a time bonks)
d <- tibble(
  i_beg = seq(1, nrow(spp), by=200),
  i_end = c(seq(100, nrow(spp), by=200)))
if (max(d$i_end) < nrow(spp)){
  d <- d %>%
    bind_rows(
      tibble(
        i_beg = d$i_end[nrow(d)],
        i_end = nrow(spp)))
}

# look for exact matching names
message(glue("Paging (100 ea) through {nrow(taxa)} taxa via worrms::wm_records_names()"))
wm_n <- d %>%
  mutate(
    wm = map2(i_beg, i_end, ~{
      message(glue("  {.x}:{.y}"))
      wm_records_names(name = taxa$taxa_orig[.x:.y]) %>%
        bind_rows()
    })) %>%
  unnest(wm) %>%
  select(-i_beg, -i_end) %>%
  filter(!duplicated(scientificname))

# get taxa exactly matched
taxa_wm_e <- taxa %>%
  inner_join(
    wm_n,
    by = c(taxa_orig = "scientificname"),
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
  arrange(taxa_tbl, taxa_fld, taxa_key)

dbWriteTable(con, "taxa_wm", taxa_wm, overwrite = T)
# TODO: append for future data sources

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





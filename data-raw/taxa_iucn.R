librarian::shelf(
  dplyr, glue,
  rredlist,
  stringr)
devtools::load_all()
con <- oh_pg_con()

Sys.setenv(
  IUCN_REDLIST_KEY = readLines("~/My Drive/private/iucnredlist.org_api_token.txt"))
token <- Sys.getenv("IUCN_REDLIST_KEY")

# IUCN RL API: country/getspecies/USA  ----
country_iso2 <- "US"
req <- request("https://apiv3.iucnredlist.org/api/v3") %>%
  req_url_path_append("country/getspecies") %>%
  req_url_path_append(country_iso2) %>%
  req_url_query(
    token = token)
resp <- req %>%
  req_perform() %>%
  resp_body_json(simplifyDataFrame = T, flatten =T) %>%
  .$result %>%
  as_tibble()
readr::write_csv(resp, "data-raw/taxa_iucn_us.csv")
resp <- readr::read_csv("data-raw/taxa_iucn_us.csv")

dbWriteTable(con, "taxa_rl_us", resp, overwrite = T)

# IUCN RL API: species/* taxa from wm_spp  ----

# alo-sap | Alosa sapidissima | American shad | 158670
# https://apiv3.iucnredlist.org/api/v3/species/Alosa%20sapidissima?token=fb963555580923eb0f0b2778060e5bcbd0cca50c0b21c416ed7ff68da9c25c11
# https://apiv3.iucnredlist.org/api/v3/species/id/191206?token=fb963555580923eb0f0b2778060e5bcbd0cca50c0b21c416ed7ff68da9c25c11

taxa <- dbReadTable(con, "taxa_wm") %>%
  as_tibble() %>%
  mutate(
    taxa_orig = ifelse(
      is.na(valid_name),
      taxa_orig,
      valid_name))

get_req <- function(sp, token){
  req <- request("https://apiv3.iucnredlist.org/api/v3") %>%
    req_url_path_append("species") %>%
    req_url_path_append(URLencode(sp)) %>%
    req_url_query(
      token = token)
}

system.time({
  taxa_rl <- tibble(
    taxa_orig = taxa$taxa_orig) %>%
    mutate(
      req  = map(taxa_orig, get_req, token = token),
      resp = multi_req_perform(req)) # 58 min/4,868 reqs; 0.7 sec/req
  # df   = map(resp, get_df)) %>%
  # select(-req, -resp) %>%
  # unnest(df)
}) # 26 sec / 100 reqs = 0.26 sec / req;
# 4690.629 / 60 = 78.2 min; 4690.629 / 9766 = 0.5 sec / req

get_df <- function(resp, i=0){
  # imap(resp, get_df) sets i for debugging problematic row
  # message(glue("i: {i}"))
  # if (i == 44)
  #   browser()
  if (resp$status_code == 204) # No Content
    return(NA)
  x <- resp %>%
    resp_body_json(simplifyVector = T, flatten = T)
  if (length(x$result) == 0)
    return(NA)
  x$result
}

# taxa_rl_0 <- taxa_rl
# saveRDS(taxa_rl, "data-raw/_tmp_taxa_rl.rds")

taxa_rl <- taxa_rl %>%
  mutate(
    rl = imap(resp, get_df)) %>%
  select(-req, -resp) %>%
  filter(
    !is.na(rl)) %>%
  unnest(rl) %>%
  filter(
    !duplicated(taxa_orig)) # 2,733 × 31

dbWriteTable(con, "taxa_rl", taxa_rl, overwrite = T)


# evaluate taxa_rl ----

# taxa in US species list not in
taxa_rl %>%
  taxa_wm

table(taxa_rl$category)
# CR    DD    EN    LC LR/lc    NT    VU
# 26   163    39  2357     1    57    90
#
# DD: NA - Data Deficient
# LC: 0 - Least Concern
# LR/lc: 0 - Lower Risk, least concern (1994 category)
# NT: 1 - Near Threatened
# LR/cd: 1 - Lower Risk, conservation dependent (1994 category)
# LR/nt: 1 - Lower Risk, near threatened (1994 category)
# VU: 2 - VUlnerable
# EN: 3 - ENdangered
# CR: 4 - CRitically endangered
# EW: NA (*not 5) - Extinct in the Wild (n=0)
# EX: NA (*not 5) - EXtinct (n=1)

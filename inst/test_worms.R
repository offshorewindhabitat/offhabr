librarian::shelf(devtools, dplyr)
document()
load_all()
con <- oh_pg_con()
# taxa_rl_us100 <- tbl(con, "taxa_rl_us") %>% collect() %>% slice(1:100)
# taxa_rl_us10  <- tbl(con, "taxa_rl_us") %>% collect() %>% slice(1:10)
# taxa_rl_us10a <- taxa_rl_us10 %>% wm_add_aphia_id(scientific_name)
taxa_rl10  <- tbl(con, "taxa_rl") %>% collect() %>% slice(1:10)
taxa_rl10a <- taxa_rl10 %>% wm_add_aphia_id(taxa_orig)

load_all()
tibble(aphia_id = 254978) %>%
  wm_rest(aphia_id, operation = "AphiaRecordsByAphiaIDs")

tibble(aphia_id = 254978) %>%
  wm_rest(aphia_id, operation = "AphiaRecordsByAphiaIDs")

load_all()
x <- tibble(
  taxa = "Delphinus capensis") %>%
  wm_rest(taxa,operation = "AphiaRecordsByNames")

dbSendQuery(con, "DELETE FROM taxa WHERE aphia_id = 254978")
dbSendQuery(con, "DELETE FROM taxa_wm WHERE aphia_id = 254978")

# https://www.marinespecies.org/rest/AphiaRecordsByAphiaID?aphiaids%5B%5D=254978
# https://www.marinespecies.org/rest/AphiaRecordsByAphiaIDs?aphiaids%5B%5D=254978


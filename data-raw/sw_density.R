# SouthWest Fisheries Science Center Density Surface Models of Marine Mammals (Becker et al, 2020)
#
# [Predictive Models of Cetacean Densities in the California Current Ecosystem, 2020b | InPort](https://www.fisheries.noaa.gov/inport/item/64349)
#
#    aphia_id lyr                                      species_2                      n
#       <int> <chr>                                    <chr>                      <int>
#  1   137087 Minke_whale_summer_fall                  Balaenoptera acutorostrata 12257
#  2   137090 Blue_whale_summer_fall                   Balaenoptera musculus      12257
#  3   137091 Fin_whale_summer_fall                    Balaenoptera physalus      12257
#  4   137092 Humpback_whale_summer_fall               Megaptera novaeangliae     12257
#  5   137094 Short_beaked_common_dolphin_summer_fall  Delphinus delphis          12257
#  6   137098 Rissos_dolphin_summer_fall               Grampus griseus            12257
#  7   137107 Striped_dolphin_summer_fall              Stenella coeruleoalba      12257
#  8   137111 Bottlenose_dolphin_summer_fall           Tursiops truncatus         12257
#  9   242608 Bairds_beaked_whale_summer_fall          Berardius bairdii          12257
# 10   254975 Northern_right_whale_dolphin_summer_fall Lissodelphis borealis      12257
# 11   254978 Long_beaked_common_dolphin_summer_fall   Delphinus capensis         12257
# 12   254987 Dalls_porpoise_summer_fall               Phocoenoides dalli         12257
# 13  1571909 Pacific_white_sided_dolphin_summer_fall  Lagenorhynchus obliquidens 12257

# packages
librarian::shelf(
  devtools, dplyr, janitor, mapview, purrr, sf, tibble, tidyr, worrms)
load_all()

# geodatabase
gdb <- "/Users/bbest/My Drive/projects/offhab/data/noaa.maps.arcgis.com/swfsc_cce_becker_et_al_2020b.gdb"

# read layers
lyrs <- st_layers(gdb) %>% .$name
sw_density <- tibble(
  lyr = lyrs) %>%
  mutate(
    sf = map(lyr, ~ read_sf(gdb, layer = .))) %>%
  unnest(sf) %>%
  clean_names() %>%
  st_as_sf() %>%
  rename(
    geom = shape)

# prepend aphia_id from WoRMS matching and update tables taxa, taxa_wm
sw_density <- sw_density %>%
  wm_add_aphia_id(species_2)

# write to database
con <- oh_pg_con() # dbListTables(con)
st_write(sw_density, con, "sw_density")

# priority for taxa.aphia_id
# density > RES > presence (expert)

# dataset_zones
# tbl | zone_key | pct_overlap
tbl = "sw_density"
area_km2


tbl(con, "taxa") %>%
  group_by(tbl) %>%

# get dataset overlap with oh_zones
sw_density_bnd <- sw_density %>%
  filter(lyr == lyrs[1]) %>%
  st_union()
# mapview(sw_density_bnd)
oh_x <- dbGetQuery(
  con,
  " "
)

"WITH
       b AS (
         SELECT block_key, block_type, zone_key, geom FROM boem_blocks), -- WHERE zone_key = 'cgm'
       zc AS (
         SELECT zc_id, hcaf_id, csquare_code, geom FROM am_cell_zones)   -- WHERE zone_key = 'cgm'
     SELECT
       block_key, block_type, zone_key,
       zc_id, hcaf_id, csquare_code,
       CASE
         WHEN ST_CoveredBy(b.geom, zc.geom)
         THEN b.geom
         ELSE
          ST_Multi(
            ST_Intersection(b.geom, zc.geom))
         END AS geom
     FROM b
       INNER JOIN zc
       ON ST_INTERSECTS(b.geom, zc.geom)
          AND Not ST_Touches(b.geom, zc.geom)"


# extra... ----

# get summary (output above)
sw_density %>%
  st_drop_geometry() %>%
  group_by(
    aphia_id, lyr, species_2) %>%
  summarize(n = n())

# map density
# sw_density %>%
#   filter(lyr == "Blue_whale_summer_fall") %>%
#   mapView(zcol = "density")

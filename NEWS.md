# offhabr 0.6.0

-   \+ `oh_rast()` for fetching OffHab reference raster by `cell_id`, `zone_id` or all `NA` and optionally trimming to `zone_id`

- fixed `wm_add_aphia_id()` which wasn't properly including fuzzy matches and updated `wm_rest_params` with practical values to fetch all data (versus theoretical maxima which bonked when URL of request got too long)

- intersected AquaMaps with new OffHab reference raster in `data-raw/am_cells_ply.R`

- imported marine [IUCN range maps](https://www.iucnredlist.org/resources/spatial-data-download) into the OffHab database with `data-raw/rl_range.R` using new `rl_pg_con()` to import the full set of data before clipping to the OffHab zones into the OffHab database (`oh_pg_con()`).

- imported [Predictive Models of Cetacean Densities in the California Current Ecosystem, 2020b | InPort](https://www.fisheries.noaa.gov/inport/item/64349) with `data-raw/sw_density.R`

# offhabr 0.5.2

-   \+ `zone_id` column to `oh_zones` and `block_id` column to `boem_blocks` for integer referencing with rasters

# offhabr 0.5.1

-   update documentation for `wm_add_aphia_id()` (now with `Roxygen: list(markdown = TRUE)` turned on)

# offhabr 0.5.0

-   \+ `wm_add_aphia_id()` to match and prepend `aphia_id` from WoRMS REST services using new `wm_rest()` helper function, which uses new dataset `wm_rest_params`

-   \+ dataset `sw_density` table into db (see `data-raw/sw_density.R` on Github): [Predictive Models of Cetacean Densities in the California Current Ecosystem, 2020b \| InPort](https://www.fisheries.noaa.gov/inport/item/64349).

# offhabr 0.4.2

-   \+ `data-raw/taxa_worrms.R` for matching so far `am_spp.{genus} {species}` with "WoRMS - World Register of Marine Species" [MarineSpecies.org](https://www.marinespecies.org) using its API web service [marinespecies.org/rest](https://www.marinespecies.org/rest/) and the R package [worrms](https://docs.ropensci.org/worrms/articles/worrms.html).

# offhabr 0.4.1

-   `oh_map_ply()` -\> `oh_map()` + `oh_add_ply()` to accomodate adding multiple layers of polygons, eg blocks on top of zones.

# offhabr 0.4.0

-   \+ `oh_map_ply()` to map polygons, possibly with divergent midpoint in the color scheme using `div_mid` argument.

-   \+ `oh_zones_s1k`: simplified version of `oh_zones` to 1 km for faster rendering of smaller output files

# offhabr 0.3.0

-   \+ `am_cell_blocks` dataset from intersecting OffHab zone x AquaMaps cell x BOEM block; + corresponding raster with `get_am_cell_blocks_grd()`; + database helper function `create_index()`

# offhabr 0.2.0

-   Migrated entire AquaMaps DB into dedicated `aquamaps` postgres database (+ `am_pg_con()`; for calculating range size of each species) and used cross-database querying to extract subset of cells and species within `oh_zones` to into `offhab` postgres database with `am_*` prefix (see `data-raw/aquamaps_db.R`).
-   Cleaned up field names to be all lower-case, added table `am_meta` into database.

# offhabr 0.1.0

-   Added a `NEWS.md` file to track changes to the package.
-   Created initial R package with prepped spatial data bounds & db con containing functions: `get_am_cells_grd()`, `oh_pg_con()`; datasets: `am_cells_ply`, `boem_blocks`, `oh_zones`

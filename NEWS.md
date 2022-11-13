# offhabr 0.4.0

* \+ `oh_map_ply()` to map polygons, possibly with divergent midpoint in the color scheme using `div_mid` argument.
* \+ `oh_zones_s1k`: simplified version of `oh_zones` to 1 km for faster rendering of smaller output files

# offhabr 0.3.0

* \+ `am_cell_blocks` dataset from intersecting OffHab zone x AquaMaps cell x BOEM block; \+ corresponding raster with `get_am_cell_blocks_grd()`; \+ database helper function  `create_index()`

# offhabr 0.2.0

* Migrated entire AquaMaps DB into dedicated `aquamaps` postgres database (\+ `am_pg_con()`; for calculating range size of each species) and used cross-database querying to extract subset of cells and species within `oh_zones` to into `offhab` postgres database with `am_*` prefix (see `data-raw/aquamaps_db.R`).
* Cleaned up field names to be all lower-case, added table `am_meta` into database.

# offhabr 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Created initial R package with prepped spatial data bounds & db con containing functions: `get_am_cells_grd()`, `oh_pg_con()`; datasets: `am_cells_ply`, `boem_blocks`, `oh_zones`



# offhabr 0.13.0

- Added reporting functions (with concept report and new reference header) `oh_tbl_zone_score_dev()`, `dt_color_fld()`, `link_caption()` for use in generating table outputs in [report source](https://github.com/offshorewindhabitat/report).

- Modified `oh_map()` with additional arguments (defaulting to TRUE|FALSE according to `knitr::is_html_output()`) for zoomControl, attributionControl and fullScreenControl in case of screenshot captures into static report.docx.

# offhabr 0.12.1

- more package cleanup for shinyapps.io install

# offhabr 0.12.0

- Fixed all warnings with package loading and moved packages to Suggests for loading [offhab-scripts: sp_map/](https://github.com/ecoquants/offhab-scripts/tree/cbce3bd670d49deb571202d617bd64acd09620d6/sp_map) Shiny app into [ShinyApps.io](https://ShinyApps.io).

# offhabr 0.11.0

- Intersected `oh_zones` with `oa_rgns` to further differentiate portions of zones with and without bottom trawl models.
- Updated `oh_zones` to having two `zone_version` values for with (`2`) and without (`1`) OceanAdapt regions for using and skipping bottom trawl data.

# offhabr 0.10.0

- Added `oa_regions` ([Ocean Adapt regions](https://www.fisheries.noaa.gov/inport/item/67352)) to map limits of interpolated bottom trawl model results.

- Renamed:
    * data:  `boem_blocks` -> `oh_blocks`
    * table: `boem_blocks` -> `blocks`

# offhabr 0.9.0

- Added **hydrothermal vents** ([InterRidge Global Database of Active Submarine Hydrothermal Vent Fields Version 3.4](https://doi.pangaea.de/10.1594/PANGAEA.917894)) with `data-raw/mdls_ve.R`, 
which for US are only in Northern California (n=3) and Washington/Oregon (n=4) 
with a 12 km radius zone of influence based on splitting the difference between
studies showing biogeographic influence out to 100 km^2 (Haymon et al., 1990) and 200 km^2 (Van Dover et al., 2002) [`round(sqrt(mean(c(200,100)))) = 12`].

- Added **seamounts** ([Global Seamount Database](http://www.soest.hawaii.edu/PT/SMTS)) with `data-raw/mdls_sm.R`, which for US are in Central California (n=3), North Atlantic (n=4), Northern California (n=4), Southern California (n=14), and Washington/Oregon (n=18). Created circles of influence based on area (calculated
from major and minor axes of ellipse). All summit depths were greater than 800 m,
the third class of biogeographic distinction (Clark et al, 2011).

- Added **productivity** ([Ocean Productivity from Oregon State](http://sites.science.oregonstate.edu/ocean.productivity)) with `data-raw/mdls_vg.R` for the entire US OffHab study area. Averaged the 12 
monthly products using the Vertically Generalized Production Model (VGPM) algorithm (Behrenfeld & Falkowski, 1997a).

- Fixed missing data from earlier ingestion of **bottom trawl** (`data-raw/mdls_oa.R`) when cell_id was not saving raster in format, creating duplicates.

# offhabr 0.8.0

- Ported original [offhab-scripts: interpolate\_bottom-trawl.Rmd](https://github.com/ecoquants/offhab-scripts/blob/cd6b2e4ff667cd81d3399b2e9538dcf5494ac9c3/interpolate_bottom-trawl.Rmd?h=1) into `data-raw/mdls_oa.R` with `multidplyr` parallel processing to speed up raster generation of interpolated biomass survey points. These are the **bottom trawl** 
models from [OceanAdapt](https://oceanadapt.rutgers.edu) that have been ported to
[NOAA Distribution Mapping and Analysis Portal (DisMAP)](https://apps-st.fisheries.noaa.gov/dismap/DisMAP.html).

# offhabr 0.7.1

- Loaded [**cetacean and sea turtle** spatial density model outputs ... in the Gulf of Mexico](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:256800) into database with `data-raw/mdls_gm.R`

# offhabr 0.7.0

- Added searching for non-marine taxa with `wm_add_aphia_id()` and allowance for 
extra query parameters with `wm_rest()`, e.g. `wm_rest(tbl, fld, operation = "AphiaRecordsByNames", marine_only = F)`

- Added **seabird** "NCCOS Assessments Modeling At-Sea Density of Marine Birds" for [Atlantic](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0176682) and [Pacific](https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0242882) with `data-raw/mdls_nc.R`

# offhabr 0.6.1

- Added [Habitat-based **Marine Mammal** Density Models for the U.S. Atlantic: Latest Versions](https://seamap.env.duke.edu/models/Duke/EC/) with `data-raw/mdsl_du.R`

- Added `operation="AphiaRecordsByVernacular"` option to `wm_rest()` for searching common names.

# offhabr 0.6.0

- Added `oh_rast()` for fetching OffHab reference raster by `cell_id`, `zone_id` or all `NA` and optionally trimming to `zone_id`

- Fixed `wm_add_aphia_id()` which wasn't properly including fuzzy matches and updated `wm_rest_params` with practical values to fetch all data (versus theoretical maxima which bonked when URL of request got too long)

- Intersected AquaMaps with new OffHab reference raster in `data-raw/am_cells_ply.R`

- Imported marine [IUCN **range** maps](https://www.iucnredlist.org/resources/spatial-data-download) into the OffHab database with `data-raw/mdls_rl.R` using new `rl_pg_con()` to import the full set of data before clipping to the OffHab zones into the OffHab database (`oh_pg_con()`).

- Imported [Predictive Models of **Cetacean** Densities in the California Current Ecosystem, 2020b | InPort](https://www.fisheries.noaa.gov/inport/item/64349) with `data-raw/mdsl_sw.R`

# offhabr 0.5.2

- Added `zone_id` column to `oh_zones` and `block_id` column to `boem_blocks` for integer referencing with rasters

# offhabr 0.5.1

- Updated documentation for `wm_add_aphia_id()` (now with `Roxygen: list(markdown = TRUE)` turned on)

# offhabr 0.5.0

- Added `wm_add_aphia_id()` to match and prepend `aphia_id` from WoRMS REST services using new `wm_rest()` helper function, which uses new dataset `wm_rest_params`

- Added dataset `mdls_sw` table into db (see `data-raw/mdls_sw.R` on Github): [Predictive Models of **Cetacean** Densities in the California Current Ecosystem, 2020b \| InPort](https://www.fisheries.noaa.gov/inport/item/64349).

# offhabr 0.4.2

- Added `data-raw/taxa_worrms.R` for matching so far `am_spp.{genus} {species}` with "WoRMS - World Register of Marine Species" [MarineSpecies.org](https://www.marinespecies.org) using its API web service [marinespecies.org/rest](https://www.marinespecies.org/rest/) and the R package [worrms](https://docs.ropensci.org/worrms/articles/worrms.html).

# offhabr 0.4.1

- Renamed `oh_map_ply()` to `oh_map()` and added `oh_add_ply()` to accomodate adding multiple layers of polygons, eg blocks on top of zones.

# offhabr 0.4.0

- Added `oh_map_ply()` to map polygons, possibly with divergent midpoint in the color scheme using `div_mid` argument.

- Added `oh_zones_s1k`: simplified version of `oh_zones` to 1 km for faster rendering of smaller output files

# offhabr 0.3.0

- Added `am_cell_blocks` dataset from intersecting OffHab zone x AquaMaps cell x BOEM block; + corresponding raster with `get_am_cell_blocks_grd()`; + database helper function `create_index()`

# offhabr 0.2.0

- Migrated entire **AquaMaps** DB into dedicated `aquamaps` postgres database (+ `am_pg_con()`; for calculating range size of each species) and used cross-database querying to extract subset of cells and species within `oh_zones` to into `offhab` postgres database with `am_*` prefix (see `data-raw/aquamaps_db.R`).
- Cleaned up field names to be all lower-case, added table `am_meta` into database.

# offhabr 0.1.0

- Added a `NEWS.md` file to track changes to the package.
- Created initial R package with prepped spatial data bounds & db con containing functions: `get_am_cells_grd()`, `oh_pg_con()`; datasets: `am_cells_ply`, `boem_blocks`, `oh_zones`

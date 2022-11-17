# bathymetry, high res

# run before:
# - crm_bathy.R
# - gebco_bathy.R

# tifs <- list.files(dir_tif, "tif$", full.names=T)
# stk <- stack(tifs)

# packages ----
librarian::shelf(
  devtools, dplyr, glue, here, mapview, ncdf4, purrr, terra,
  rlang, sf, tibble, tidyr)
load_all()

# paths ----
dir_crm_tif    <- "~/My Drive/projects/offhab/data/ngdc.noaa.gov - bathymetry (CRM)"
crm_tif        <- glue("{dir_crm_tif}/crm_zones.tif")

dir_gebco      <- "/Users/bbest/My Drive/projects/offhab/data/gebco.net"
gebco_zone_tif <- glue("{dir_g}/gebco_zones.tif")

crm_tifs <- list.files(dir_crm_tif, "tif$", full.names=T)

crm_rs <- map(crm_tifs, rast)
crm_r <- mosaic(sprc(crm_rs))
writeRaster(crm_r, crm_tif)
crm_rs[[1]]
crm_r


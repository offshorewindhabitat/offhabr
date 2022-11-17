# GEBCO - General Bathymetric Chart of the Oceans
# download: https://www.gebco.net/data_and_products/gridded_bathymetry_data/#global

# packages ----
librarian::shelf(
  devtools, dplyr, glue, here, mapview, ncdf4, purrr, raster,
  rlang, sf, tibble, tidyr)
load_all()

dir_gebco      <- "/Users/bbest/My Drive/projects/offhab/data/gebco.net"
gebco_zone_tif <- glue("{dir_g}/gebco_zones.tif")

# offhab zones union and bounding box ----
zones_u <- oh_zones %>%
  st_union()
# mapview(zones_u)
b <- st_bbox(zones_u)

# GEBCO read ----
gebco_nc <- "/Users/bbest/big/gebco_2022_sub_ice_topo/GEBCO_2022_sub_ice_topo.nc"

d <- nc_open(gebco_nc)
names(d$var) # crs, elevation
names(d$dim) # lat, lon
x <- ncvar_get(d, "lon")
y <- ncvar_get(d, "lat")
ix <- tibble(
  x = x) %>%
  rowid_to_column("i") %>%
  filter(
    x >= b["xmin"],
    x <= b["xmax"]) %>%
  pull(i)
iy <- tibble(
  y = y) %>%
  rowid_to_column("i") %>%
  filter(
    y >= b["ymin"],
    y <= b["ymax"]) %>%
  pull(i)

elev <- ncvar_get(
  d, "elevation",
  start = c(min(ix), min(iy)),
  count = c(length(ix), length(iy)))
r <- raster(
  list(x = x[ix], y = y[iy], z = elev),
  crs = 4326)
r <- mask(r, as_Spatial(zones_u))
# mapview(r) +
#   mapview(zones_u)
writeRaster(r, gebco_zone_tif)

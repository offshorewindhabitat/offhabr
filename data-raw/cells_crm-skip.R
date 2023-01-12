# Setup finescale pixels as base analytical units using

# NOAA's Coastal Relief Model, backfilled to EEZ with GEBCO
# - [CRM](https://www.ngdc.noaa.gov/mgg/coastal/crm.html)
# - [GEBCO](https://www.gebco.net/data_and_products/gridded_bathymetry_data/#global)

# - [_crm - Google Sheets](https://docs.google.com/spreadsheets/d/1XxGlWV0c8G9yWNsAYHpYwrUrVnsmI9VJqiQ3ImDRvgc/edit#gid=0)

# packages ----
librarian::shelf(
  devtools, dplyr, glue, here, mapview, ncdf4, purrr, raster,
  rlang, sf, tibble, tidyr)
load_all()

dir_crm_tif <- "~/My Drive/projects/offhab/data/ngdc.noaa.gov - bathymetry (CRM)"

# zone bounding boxes ----
zones_bbox <- oh_zones %>%
  mutate(
    bbox = map(geom, ~ st_bbox(.) %>% st_as_sfc()) %>%
      reduce(rbind) %>% .[,1]) %>%
  st_drop_geometry() %>%
  select(zone_key, bbox) %>%
  st_as_sf(crs = 4326)
# mapview(oh_zones, zcol="zone_key") +
# mapview(zones_bbox, zcol="zone_key")

# crm volume bounding boxes ----
crm_vol_bbox <- function(vol){
  # vol = 1
  message(glue("vol: {vol}"))
  u <- glue("https://www.ngdc.noaa.gov/thredds/dodsC/crm/crm_vol{vol}.nc")
  d <- nc_open(u)
  x <- ncvar_get(d, "x")
  y <- ncvar_get(d, "y")

  st_bbox(
    c(xmin = min(x),
      xmax = max(x),
      ymin = min(y),
      ymax = max(y)),
    crs = st_crs(4326)) %>%
    st_as_sfc() %>%
    st_as_sf()
}

crm <- tibble(
  vol = 1:8) %>%
  mutate(
    bbox = map(vol, crm_vol_bbox) %>%
      reduce(rbind) %>% .$x) %>%
  st_as_sf(crs = 4326)
# mapview(zones_bbox, zcol="zone_key") +
#   mapview(crm, zcol="vol")

# crm rasters per zone -----
crm_vol_raster <- function(vol, ply){ # vol = 3; ply = z
  url <- glue("https://www.ngdc.noaa.gov/thredds/dodsC/crm/crm_vol{vol}.nc")
  d <- nc_open(url)

  b <- st_bbox(ply)
  x <- ncvar_get(d, "x")
  y <- ncvar_get(d, "y")
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

  if (length(ix) == 0 | length(iy) == 0 )
    return(NA)

  z <- ncvar_get(
    d, "z",
    start = c(min(ix), min(iy)),
    count = c(length(ix), length(iy)))
  r <- raster(
    list(x = x[ix], y = y[iy], z = z),
    crs = 4326) %>%
    mask(ply)
  r
}

# iterate over zones ----
for (i_z in 1:nrow(oh_zones)){ # i_z = 1
  zon <- oh_zones %>% slice(i_z)

  message(glue("zone: {zone_name} ({zone_key})"))

  tif <- glue("{dir_crm_tif}/crm_zone-{zon$zone_key}.tif")
  if (file.exists(tif))
    next()

  # get CRM volumes that intersect with OH zone
  x_vols <- st_intersects(zon, crm)[[1]]
  # mapview(zon) +
  #   mapview(crm %>% filter(vol %in% x_vols))

  # accumulate list of rasters
  rs <- list()
  for (i in seq_along(x_vols)){ # vol = x_vols[1]
    vol = x_vols[i]
    message(glue("  crm_vol_raster({vol}, zon) ~ {Sys.time()}"))
    rs[[i]] <- crm_vol_raster(vol, zon)
  }

  message(glue("  mosaic ~ {Sys.time()}"))
  rs <- rs[!is.na(rs)]
  if (length(rs) > 1){
    r <- exec("mosaic", !!!rs, fun = mean)
  } else {
    r <- rs[[1]]
  }

  message(glue("  write"))
  mapview(r)
  writeRaster(r, normalizePath(tif), overwrite = T)
}

# tifs <- list.files(dir_tif, "tif$", full.names=T)
# stk <- stack(tifs)

# 22M crm_zone-cec.tif
# 79M crm_zone-cgm.tif
# 78M crm_zone-egm.tif
# 13M crm_zone-fls.tif
# 42M crm_zone-mda.tif
# 78M crm_zone-noa.tif
# 21M crm_zone-noc.tif
# 42M crm_zone-soa.tif
# 46M crm_zone-soc.tif
# 54M crm_zone-wao.tif
# 34M crm_zone-wgm.tif

# Ocean Productivity from Oregon State
# http://sites.science.oregonstate.edu/ocean.productivity
#
# calculated mean from 2021 monthlies
#
# Community guidance for developing this website was to provide a single
# productivity product as a Standard product. For this, we initially chose the
# original Vertically Generalized Production Model (VGPM) (Behrenfeld and
# Falkowski, 1997a) as the standard algorithm. The VGPM is a "chlorophyll-based"
# model that estimate net primary production from chlorophyll using a
# temperature-dependent description of chlorophyll-specific photosynthetic
# efficiency. For the VGPM, net primary production is a function of chlorophyll,
# available light, and the photosynthetic efficiency. Shown below is an example
# of VGPM-based global ocean net primary production for July of 2016.
#
# Standard products are based on chlorophyll, temperature and PAR data from
# SeaWiFS, MODIS and VIIRS satellites, along with estimates of euphotic zone
# depth from a model developed by Morel and Berthon (1989) and based on
# chlorophyll concentration. Monthly global ocean production for July 2016 was
# 4.43626 Pg (1 Pg = 10**15 g.)
#
# vgpm.m.2021.tar (48 MB) downloaded into dir_vgpm from: * [Ocean Productivity:
# Online VGPM Data](
# http://orca.science.oregonstate.edu/1080.by.2160.monthly.hdf.vgpm.m.chl.m.sst.php)
#
# See [Ocean Productivity: Frequently Asked
# Questions](http://orca.science.oregonstate.edu/faq01.php)

librarian::shelf(
  DBI, devtools, fs, glue, R.utils, sf, terra)
load_all()

dir_vgpm       <- "/Users/bbest/My Drive/projects/offhab/data/raw/oregonstate.edu - productivity/vgpm.m.2021"
vgpm_tif       <- glue("{dir_vgpm}/vgpm_2021_oh.tif")

# unzip from *.gz to *.hdf ----
gzs <- list.files(dir_vgpm, "\\.gz$", full.names=T)
lapply(gzs, gunzip)

# convert from *.hdf to *.nc ----
#   download [h4tonccf_nc4](https://www.hdfeos.org/software/h4cflib.php)
#     and add to directory; chmod o+x h4tonccf_nc4; allow to run in OS Settings
hdfs   <- list.files(dir_vgpm, ".hdf", full.names=T)
ncs    <- path_ext_set(hdfs, "nc")
idx_nc <- !file.exists(ncs)
cmds   <- glue("cd '{dir_vgpm}'; ./h4tonccf_nc4 {basename(hdfs[idx_nc])}")
sapply(cmds, system)
sapply(hdfs, file_delete)

# convert nc to tif, projected and clipped to OffHab raster ----

# * get OffHab bounding box for clipping global raster ----
r_cid <- oh_rast("cell_id")
bb_oh_b1dd_gcs <- r_cid %>%
  ext() %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_set_crs(3857) %>%
  st_transform(4326) %>%
  st_set_crs(NA) %>%
  st_buffer(1) %>%
  st_set_crs(4326)
# mapview::mapView(bb_oh_b1dd_gcs)

nc_to_oh_tif <- function(nc, i=0){
  # nc = ncs[1]; i=0
  tif <- ext_to_oh_tif(nc)
  message(glue("{i}: {basename(nc)} -> {basename(tif)}"))

  # read netcdf as raster
  #   set extent and coordinate reference system and NA
  r <- rast(nc)
  ext(r) <- c(-180, 180, -90, 90)
  crs(r) <- "EPSG:4326"
  r[r==-9999] <- NA

  r <- r %>%
    crop(bb_oh_b1dd_gcs) %>%
    project(
      r_cid, method="bilinear", align=T) %>%
    crop(r_cid) %>%
    mask(r_cid)
  # plot(r)

  writeRaster(r, tif, overwrite=T)
}

ncs  <- list.files(dir_vgpm, "\\.nc$", full.names=T)
ext_to_oh_tif <- function(nc){ glue("{path_ext_remove(nc)}_oh.tif") }
tifs <- ext_to_oh_tif(ncs)

# ncs[!file.exists(tifs)] %>%
ncs %>%
  iwalk(nc_to_oh_tif)

# calculate annual mean from monthly tifs ----
tifs <- list.files(dir_vgpm, "\\.tif$", full.names=T)
r_ms <- rast(tifs)
r <- mean(r_ms, na.rm=T)
writeRaster(r, vgpm_tif)

v <- values(r, na.rm=T)
range(v) # 217.1408 6174.7437
ncell(r) # 105,432,074
# plot(r)

# write to lyrs, db ----
dir_lyrs_tif <- "/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_tif"
vg_tif <- glue("{dir_lyrs_tif}/vg.tif")

r_vgpm <- rast(vgpm_tif)
# plet(r_vgpm, tiles="Esri.NatGeoWorldMap")

r_vg <- setValues(
  r_vgpm,
  scales::rescale(
    values(r_vgpm),
    to = c(1, 100)) )
# plet(r_vg, tiles="Esri.NatGeoWorldMap")

offhabr::write_rast(r_vg, vg_tif)
file.size(vgpm_tif) # 73,025,983
file.size(vg_tif)   #    439,834

# * test: revert to original values ----
(vr <- range(values(r_vgpm, na.rm = T)))
# 217.1408 6174.7437

# revert to original values
r_vg_s <- terra::setValues(
  r_vg,
  scales::rescale(
    values(r_vg),
    to = c(vr[1], vr[2])) )
# plet(r_vg_s, tiles="Esri.NatGeoWorldMap")

# wrote metadata to inst/datasets.csv
# wrote to inst/layers.csv:
#   ds_key  lyr_key aphia_id   val_min   val_max rescale_min rescale_max
#   vg      vg		            217.1408 6174.7437           1         100

con <- oh_con(read_only = F) # dbDisconnect(con, shutdown = T)



#' Drop raster layers without any data
#'
#' Drop `terra::rast()` layers without any data
#'
#' @param x raster layer
#'
#' @return raster stack
#' @importFrom terra global
#' @importFrom dplyr pull
#' @export
#' @concept rast
#'
#' @examples
drop_na_lyrs <- function(x){
  d_i <- terra::global(x, fun="min", na.rm=T)
  i <- d_i %>%
    dplyr::pull(min) %>%
    is.na(.) %>%
    !.
  x[[i]]
}

#' OffHab reference raster
#'
#' Get OffHab reference raster of cells by `cell_id`, `zone_id` or all `NA` and
#' optionally trim to `zone_id`.
#'
#' The OffHab reference raster is based on the GEBCO global bathymetry clipped to the
#' United States in the web Mercator projection (EPSG:3857) for readily using
#' with interactive maps (e.g. `leaflet::leaflet()`).
#'
#' @param type the type of raster, being one of: `"NA"` (default all `NA` values)
#'   `"cell_id"` with unique cell indices to set values of reference raster, or
#'   `"zone_id"` corresponding with the `zone_id` of `oh_zones`
#' @param zone_id the `zone_id` (integer) to trim the output raster, being one of the
#'   zones found in the `zone_id` field of `oh_zones` or `"ALL"` zones (the default)
#' @param zone_version the `zone_version` (integer) to choose, whether original BOEM
#' planning area clipped out to EEZ (`=1`) or more restricted to OceanAdapt regions
#' for bottom trawl data (`=2`)
#'
#' @return return a reference raster (from `terra::rast()`)
#' @export
#' @concept rast
#' @importFrom terra mask ncell rast trim values
#'
#' @examples
#'
#' # reference raster with all NA values
#' r_na <- oh_rast()
#' r_na
#'
#' # zone_id for all zones
#' r_zid <- oh_rast("zone_id")
#' r_zid
#' terra::plot(r_zid)
#'
#' # cell_id for all zones
#' r_cid <- oh_rast("cell_id")
#' r_cid
#' terra::plot(r_cid)
#'
#' # cell_id for Straits of Florida (zone_id: 4; zone_key: fls)
#' r_cid_fls <- oh_rast("cell_id", 4)
#' r_cid_fls
#' terra::plot(r_cid_fls)
oh_rast <- function(type = c("NA", "cell_id", "zone_id"), zone_id = "ALL", zone_version=1){
  stopifnot(type %in% c("NA", "cell_id", "zone_id"))
  stopifnot(zone_id == "ALL" | is.numeric(zone_id))
  type = type[1]

  tif <- system.file(glue::glue("oh_zones_v{zone_version}.tif"), package = "offhabr")
  r_z <- terra::rast(tif)
  # terra::plot(r_z)
  r <- switch(
    type,
    cell_id = terra::setValues(r_z, 1:(terra::ncell(r_z))) %>%
      mask(r_z),
    `NA`    = terra::setValues(r_z, NA),
    zone_id = r_z)
  names(r) <- type

  if (is.numeric(zone_id)){
    r <- r %>%
      terra::mask(r_z == zone_id, maskvalues=c(NA,0)) %>%
      terra::trim()
  }
  r
}

#' Write compact raster with compression, tiles and sparse encoding
#'
#' Use `terra::WriteRaster()` followed by `gdal_translate` for minimizing the output
#' file size with creation options for maximum compression
#' (`COMPRESS=DEFLATE ZLEVEL=9 PREDICTOR=2`), tiles (`TILED=YES`) and sparse
#' (`SPARSE_OK=TRUE`) encoding.
#'
#' The `datatype` corresponds with the following:
#' | terra |             min |            max | gdal    |
#' | :---- | --------------: | -------------: | :------ |
#' | INT1U | 	             0 |	          255 | Byte    |
#' | INT2S | 	       -32,767 |	       32,767 | Int16   |
#' | INT2U | 	             0 |	       65,534 | UInt16  |
#' | INT4S |  -2,147,483,647 |	2,147,483,647 | Int32   |
#' | INT4U |	             0 |  4,294,967,296 | UInt32  |
#' | FLT4S |	      -3.4e+38 |	      3.4e+38 | Float32 |
#' | FLT8S |	     -1.7e+308 |	     1.7e+308 | Float64 |
#'
#' @param r input raster layer or path to input GeoTIFF
#' @param tif output path to GeoTIFF
#' @param datatype datatype; one of: INT1U (default), INT2S, INT2U, INT4S, INT4U, FLT4S, FLT8S
#' @param overwrite defaults to TRUE
#' @param epsg projection number; default: Web Mercator (3857)
#'
#' @return returns nothing since only writing `tif`, i.e. a side-effect function
#' @importFrom terra writeRaster
#' @importFrom glue glue
#' @export
#' @concept rast
#'
#' @examples
write_rast <- function(
    r,
    tif,
    datatype  = "INT1U",
    overwrite = TRUE,
    epsg      = 3857){

  # ?terra::writeRaster
  #   datatype = # "INT1U", "INT2U", "INT2S", "INT4U", "INT4S", "FLT4S", "FLT8S"

  # https://search.r-project.org/CRAN/refmans/raster/html/dataType.html
  # Datatype definition	minimum possible value	maximum possible value
  # LOG1S	      FALSE(0) 	      TRUE(1)
  # INT1S	          -127	          127
  # INT1U	             0	          255  Byte
  # INT2S	       -32,767	       32,767  Int16
  # INT2U	             0	       65,534  UInt16
  # INT4S	-2,147,483,647	2,147,483,647  Int32
  # INT4U	             0  4,294,967,296  UInt32
  # FLT4S	      -3.4e+38	      3.4e+38  Float32
  # FLT8S	     -1.7e+308	     1.7e+308  Float64

  # https://gdal.org/programs/gdal_translate.html
  # -ot {Byte/Int8/Int16/UInt16/UInt32/Int32/UInt64/Int64/Float32/Float64/
  #      CInt16/CInt32/CFloat32/CFloat64}

  terra2gdal_datatypes <- c(      #    min            max
    "INT1U" = "Byte",    # 	             0	          255
    "INT2S" = "Int16",   #	       -32,767	       32,767
    "INT2U" = "UInt16",  #	             0	       65,534
    "INT4S" = "Int32",   #	-2,147,483,647	2,147,483,647
    "INT4U" = "UInt32",  #	             0  4,294,967,296
    "FLT4S" = "Float32", #	      -3.4e+38	      3.4e+38
    "FLT8S" = "Float64") #	     -1.7e+308	     1.7e+308

  # https://gdal.org/drivers/raster/gtiff.html
  # Internal nodata masks
  #   FILETYPE_MASK bit value is set on the TIFFTAG_SUBFILETYPE
  #   GDAL_TIFF_INTERNAL_MASK configuration option is set to YES
  # Sparse files
  #   SPARSE_OK creation option to YES
  # Creation Options
  #   NUM_THREADS=number_of_threads/ALL_CPUS
  #   BIGTIFF=YES

  if (!datatype %in% names(terra2gdal_datatypes))
    stop(glue::glue("Sorry, write_rast() needs one of the following datatype values:
         {paste0(names(terra2gdal_datatypes), collapse=', ')}"))
  if (fs::path_ext(tif) != "tif")
    stop("Sorry, write_rast only works with fs::path_ext(tif)=='tif' for now")

  if ("SpatRaster" %in% class(r)){
    tmp_tif <- tempfile(fileext = ".tif")
    terra::writeRaster(
      r,
      tmp_tif,
      datatype = datatype,
      overwrite = overwrite)

  } else {
    # assume path to tif
    tmp_tif <- r
  }

  gdal_datatype <- terra2gdal_datatypes[[datatype]]
  opt_byte     <- ifelse(datatype == "INT1U", "-a_nodata 255", "")
  opt_compress <- "-co COMPRESS=DEFLATE -co ZLEVEL=9 -co PREDICTOR=2"
  opt_tile     <- "-co TILED=YES"
  opt_sparse   <- "-co SPARSE_OK=TRUE"
  opt_epsg     <- glue("-a_srs EPSG:{epsg}")
  opts <- glue::glue("{opt_byte} {opt_epsg} -ot {gdal_datatype} {opt_compress} {opt_tile} {opt_sparse}")
  system(glue::glue("gdal_translate {opts} '{tmp_tif}' '{tif}'"))
  unlink(tmp_tif)
}

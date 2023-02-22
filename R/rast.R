
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
#'   `"cell_id"` with unique cell indices to set values of reference raster,
#'   `"region"` showing the 3 unique regions ("Atlantic","Gulf of Mexico" and "Pacific"),
#'   `"zone_id"` corresponding with the `zone_id` of `oh_zones`,
#'   `"block_id"` corresponding with the `block_id` of `oh_blocks`,
#'   `"elev_m"` for GEBCO elevation (meters), or `"area_m2"`
#'   for square meter area per cell
#' @param zone_id the `zone_id` (integer) to trim the output raster, being one of the
#'   zones found in the `zone_id` field of `oh_zones` or `"ALL"` zones (the default)
#' @param zone_version the `zone_version` (integer) to choose, whether original BOEM
#' planning area clipped out to EEZ (`=1`) or more restricted to OceanAdapt regions
#' for bottom trawl data (`=2`); applies to either `type` of `"zone_id"` or `"block_id"`
#' @param web_version use web-optimized cloud-optimized GeoTIFF version; default=FALSE
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
oh_rast <- function(
    type         = c("NA", "cell_id", "region", "zone_id", "block_id", "elev_m", "area_m2"),
    zone_id      = "ALL",
    zone_version = c(1, 2),
    web_version  = F){

  # type         = "block_id"
  # zone_id      = "ALL"
  # zone_version = 1
  # devtools::load_all()

  stopifnot(type %in% c("NA", "cell_id", "region", "zone_id", "block_id", "elev_m", "area_m2"))
  stopifnot(zone_id == "ALL" | is.numeric(zone_id))
  type         = type[1]
  zone_version = zone_version[1]
  tif_sfx      = ifelse(web_version, "_web", "")

  gx_tif <- dplyr::case_when(
    type == "region"   ~ as.character(glue::glue("oh_regions{tif_sfx}.tif")),
    type == "zone_id"  ~ as.character(glue::glue("oh_zones_v{zone_version}{tif_sfx}.tif")),
    type == "block_id" ~ as.character(glue::glue("oh_blocks_v{zone_version}{tif_sfx}.tif")),
    type == "elev_m"   ~ as.character(glue::glue("oh_elev_m{tif_sfx}.tif")),
    TRUE ~ as.character(glue::glue("oh_area_m2{tif_sfx}.tif")))

  tif <- system.file(glue::glue(gx_tif), package = "offhabr", mustWork = T)

  r_1 <- terra::rast(tif)  # terra::plot(r_z)
  r <- switch(
    type,
    cell_id  = terra::setValues(r_1, 1:(terra::ncell(r_1))) |>
      mask(r_1),
    region   = r_1,
    zone_id  = r_1,
    block_id = r_1,
    area_m2  = r_1,
    `NA`     = terra::setValues(r_1, NA) )
  names(r) <- dplyr::case_when(
    type %in% c("zone_id", "block_id") ~ glue::glue("{type}_v{zone_version}"),
    TRUE ~ type)

  if (is.numeric(zone_id)){
    gx_z  <- "oh_zones_v{zone_version}{tif_sfx}.tif"
    z_tif <- system.file(glue::glue(gx_z), package = "offhabr")
    r_z <- terra::rast(z_tif)
    r <- r |>
      terra::mask(r_z == zone_id, maskvalues=c(NA,0))  |>
      terra::trim()
  }
  r
}

#' Write compact raster with compression, tiles and sparse encoding
#'
#' Use `terra::WriteRaster()` followed by
#' [`rio cogeo create`](https://cogeotiff.github.io/rio-cogeo/CLI/) for
#' creating cloud-optimized GeoTIFF that minimizes the output file size with
#' creation options for maximum compression
#' (`COMPRESS=DEFLATE ZLEVEL=9 PREDICTOR=2`), tiles (`TILED=YES`) (and
#' optionally sparse (`SPARSE_OK=TRUE`) encoding).
#'
#' The `datatype` corresponds with the following:
#' | terra |             min |            max | gdal    | rio     |
#' | :---- | --------------: | -------------: | :------ |:------- |
#' | INT1U | 	             0 |	          255 | Byte    | uint8   |
#' | INT2S | 	       -32,767 |	       32,767 | Int16   | int16   |
#' | INT2U | 	             0 |	       65,534 | UInt16  | uint16  |
#' | INT4S |  -2,147,483,647 |	2,147,483,647 | Int32   | int32   |
#' | INT4U |	             0 |  4,294,967,296 | UInt32  | uint32  |
#' | FLT4S |	      -3.4e+38 |	      3.4e+38 | Float32 | float32 |
#' | FLT8S |	     -1.7e+308 |	     1.7e+308 | Float64 | float64 |
#'
#' @param r input raster layer or path to input GeoTIFF
#' @param tif output path to GeoTIFF
#' @param datatype datatype; one of: INT1U (default), INT2S, INT2U, INT4S, INT4U, FLT4S, FLT8S
#' @param method for internal overview generation; use `"average"` (default) for continous
#'   and `"nearest"` for categorical
#' @param overwrite defaults to TRUE
#' @param epsg projection number; default: Web Mercator (3857)
#' @param threads used by [`rio cogeo create`](https://cogeotiff.github.io/rio-cogeo/CLI/); defaults to `"ALL_CPUS"`; change to `1` if using in parallel
#' @param verbose show commands; defaults to FALSE
#' @param web_optimize create a web-tiling friendly COG per
#' [Advanced Topics - rio-cogeo](https://cogeotiff.github.io/rio-cogeo/Advanced/)
#' that aligns internal tiles with the web-mercator grid resulting in a larger
#' @param use_gdal_cog use GDAL COG driver (requires GDAL >= 3.1); does not change extent
#' file with different bounds and pixel sizes from original; defaults to FALSE
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
    datatype     = "INT1U",
    overwrite    = TRUE,
    method       = c("average", "nearest"),
    threads      = "ALL_CPUS",
    epsg         = 3857,
    verbose      = F,
    web_optimize = F,
    use_gdal_cog = F){

  method <- method[1]

  # datatype="INT1U"; overwrite=TRUE; method=c("average", "nearest"); threads="ALL_CPUS"; epsg=3857; verbose=F

  # rio cogeo create --help
  # --dtype [ubyte|uint8|uint16|int16|uint32|int32|float32|float64]
  terra2rio_dtypes <- c( #             min            max
    "INT1U" = "uint8",   # 	             0	          255
    "INT2S" = "int16",   #	       -32,767	       32,767
    "INT2U" = "uint16",  #	             0	       65,534
    "INT4S" = "int32",   #	-2,147,483,647	2,147,483,647
    "INT4U" = "uint32",  #	             0  4,294,967,296
    "FLT4S" = "float32", #	      -3.4e+38	      3.4e+38
    "FLT8S" = "float64") #	     -1.7e+308	     1.7e+308

  # https://gdal.org/drivers/raster/gtiff.html
  # Internal nodata masks
  #   FILETYPE_MASK bit value is set on the TIFFTAG_SUBFILETYPE
  #   GDAL_TIFF_INTERNAL_MASK configuration option is set to YES
  # Sparse files
  #   SPARSE_OK creation option to YES
  # Creation Options
  #   NUM_THREADS=number_of_threads/ALL_CPUS
  #   BIGTIFF=YES

  if (!datatype %in% names(terra2rio_dtypes))
    stop(glue::glue("Sorry, write_rast() needs one of the following datatype values:
         {paste0(names(terra2rio_dtypes), collapse=', ')}"))
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
    if (!is.character(r)) stop(
      "Expecting the input argument r to be a character path to a raster file
      (if not a terra::rast object)")
    # assume path to tif
    tmp_tif <- r
  }

  # https://cogeotiff.github.io/rio-cogeo/CLI/
  dtype        <- terra2rio_dtypes[[datatype]]
  opt_byte     <- ifelse(datatype == "INT1U", "--nodata 255", "")
  opt_compress <- "--cog-profile deflate --allow-intermediate-compression"
  opt_threads  <- "--threads ALL_CPUS"
  opt_web      <- ifelse(web_optimize, "--web-optimized", "")
    # https://cogeotiff.github.io/rio-cogeo/Advanced/ create a web-tiling friendly COG
    # bounds and internal tiles aligned with web-mercator grid; raw data and overviews resolution match mercator zoom level resolution.
    # it will certainly create a larger file (with padding tiles on the side of the file)
    # gives different cell sizes and dimensions:
    # rast(tif)
    # dimensions  : 5888, 11776, 1  (nrow, ncol, nlyr)
    # resolution  : 611.4962, 611.4962  (x, y)
  opt_resample <- glue("--resampling {method}")
  opt_driver   <- ifelse(use_gdal_cog, "--use-cog-driver", "")
  opts <- glue("--dtype {dtype} {opt_byte} {opt_compress} {opt_web} {opt_resample} {opt_driver}")
  cmd <- glue("rio cogeo create {opts} '{tmp_tif}' '{tif}'")
  if (verbose)
    message(cmd)
  system(cmd)

  if ("SpatRaster" %in% class(r))
    unlink(tmp_tif)

  return(T)

  # system(glue("rio cogeo validate '{rel_tif}'"))

  # tested COG tif with public URL uploaed to Google Cloud Storage via:
  #   https://api.cogeo.xyz/docs#/Cloud%20Optimized%20GeoTIFF/cog_validate_cog_validate_get
  # got COG warning:
  #   "The file is greater than 512xH or 512xW, it is recommended to include internal overviews"
  # see resolution:
  #   https://saxenasanket.medium.com/cog-overview-and-how-to-create-and-validate-a-cloud-optimised-geotiff-b39e671ff013
  # create internal over

  # cmd <- glue::glue("gdaladdo -r {method} '{tmp_tif}' 2 4 8 16 ")
  # message(cmd)
  # system(cmd)

  # gdal_datatype <- terra2gdal_datatypes[[datatype]]
  # opt_byte     <- ifelse(datatype == "INT1U", "-a_nodata 255", "")

  # OLD pre-COG ----
  # opt_compress <- "-co COMPRESS=DEFLATE -co ZLEVEL=9 -co PREDICTOR=2"
  # opt_tile     <- "-co TILED=YES"
  # NEW COG ----
  # https://trac.osgeo.org/gdal/wiki/CloudOptimizedGeoTIFF#HowtogenerateitwithGDAL
  # Given an input dataset in.tif with already generated internal or external overviews, a cloud optimized GeoTIFF (COG) can be generated with:
  #
  # gdal_translate in.tif out.tif -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW
  #
  # This will result in a images with tiles of dimension 256x256 pixel for main resolution, and 128x128 tiles for overviews.
  # opt_compress <- "-co COMPRESS=LZW"
  # opt_compress <- "-co COMPRESS=DEFLATE -co ZLEVEL=9 -co PREDICTOR=YES"
  # opt_compress <- "-co COMPRESS=DEFLATE"
  # opt_tile     <- "-co TILED=YES -co COPY_SRC_OVERVIEWS=YES" # for COG
  # since using `-of COG` getting messages:
    # Warning 1: General options of gdal_translate make the COPY_SRC_OVERVIEWS creation option ineffective as they hide the overviews
    # Warning 6: driver COG does not support creation option TILED
    # Warning 6: driver COG does not support creation option COPY_SRC_OVERVIEWS
  # NEW:
  # opt_tile     <- ""

  # opt_sparse   <- "-co SPARSE_OK=TRUE"
  # opt_sparse   <- ""
  # opt_compute  <- "-co NUM_THREADS=ALL_CPUS"
  # opt_epsg     <- glue("-a_srs EPSG:{epsg}")
  # opt_epsg     <- ""
  # opts <- glue::glue("-ot {gdal_datatype} {opt_byte} -of COG {opt_compress} {opt_tile} {opt_sparse} {opt_compute} {opt_epsg}")
  # cmd <- glue::glue("gdal_translate {opts} '{tmp_tif}' '{tif}'")
  # message(cmd)
  # system(cmd)
  # unlink(tmp_tif)
}

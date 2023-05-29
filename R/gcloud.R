#' Upload to Google Cloud Storage
#'
#' @param file usually a GeoTIFF
#' @param name defaults to `basename(file)`
#' @param make_public defaults to True
#' @param gcs_auth_json JSON for authorizing
#' @param gcs_bucket bucket (i.e. folder) to upload into on Google Cloud Storage
#'
#' @return empty, only side effect
#' @export
#' @concept gcs
upload_to_gcs <- function(
    file, name =  basename(file), make_public = T,
    gcs_auth_json = "/Users/bbest/My Drive/private/offhab-google-service-account_09e7228ac965.json",
    gcs_bucket = "offhab_lyrs"){
  # file = "/Users/bbest/My Drive/projects/offhab/data/derived/lyrs_aphia/aphia_100599.tif"
  # name =  basename(file); make_public = T; gcs_bucket = "offhab_lyrs"
  # gcs_auth_json = "/Users/bbest/My Drive/private/offhab-google-service-account_09e7228ac965.json"

  stopifnot(file.exists(gcs_auth_json))
  Sys.setenv(
    "GCS_DEFAULT_BUCKET" = gcs_bucket,
    "GCS_AUTH_FILE"      = gcs_auth_json)
  # librarian::shelf(googleCloudStorageR)

  # upload
  m <- googleCloudStorageR::gcs_upload(
    file = file,
    name = name)

  # make publicly available
  if (make_public)
    googleCloudStorageR::gcs_update_object_acl(
      name, entity_type = "allUsers")
}

#' Load into Google Earth Engine from Google Cloud Storage
#'
#' @param gcs_name Google Cloud Storage (GCS) file name
#' @param gee_name Google Earth Engine (GEE) file name (cannot have `.`);
#' defaults to `gcs_name` without the file extension
#' @param properties a list of properties or blank (default: `""`)
#' @param gcs_bucket GCS bucket (like a folder) for containing `gcs_name`; default: "offhab_lyrs"
#' @param gee_asset GEE asset (like a folder) for containing `gee_name`; default: "projects/ee-offhab/assets/lyrs_aphia_web"
#' @param pyramiding_policy GEE pyramiding; default: "MEAN" (but if categorical data want something else here)
#' @param missing_data GEE missing data; default: 255 (but need something esle if not INT1U)
#'
#' @return string of `{gee_asset}/{gee_name}` to reference image in GEE
#' @export
#' @concept gcs
gcs_to_gee <- function(
    gcs_name,
    gee_name          = fs::path_ext_remove(gcs_name),
    properties        = "",
    gcs_bucket        = "offhab_lyrs",
    gee_asset         = "projects/ee-offhab/assets/lyrs_aphia_web",
    pyramiding_policy = "MEAN",
    missing_data      = 255){

  f_json <- tempfile(fileext = ".json")
  properties_json <- jsonlite::toJSON(properties, pretty=T, auto_unbox=T)

  glue::glue(
    '{{
       "name": "{gee_asset}/{gee_name}",
       "tilesets":[{{"sources":[{{"uris":["gs://{gcs_bucket}/{gcs_name}"]}}]}}],
       "pyramidingPolicy":"MEAN",
       "properties": {properties_json},
       "missing_data":{{"values":[{missing_data}]}}
    }}') |>
    writeLines(f_json)
  # readLines(f_json) |> cat()

  cmd <- glue::glue("earthengine upload image --manifest '{f_json}'")
  system(cmd)
  return(glue("{gee_asset}/{gee_name}"))
}

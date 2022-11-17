#' Match taxa to WoRMS aphia_id and update database
#'
#' And optionally append to taxa* tables in database
#'
#' @param df data frame
#' @param fld unquoted field name containing taxonomic field, eg `taxa`
#' @param tbl_str the source table to be written into the database referenced in `taxa.tbl`
#' @param fld_str the source field to be written into the database referenced in `taxa.fld`
#'
#' @return the original data frame `df` with `aphia_id` column prepended
#' @export
#' @concept worms
#'
#' @examples
#' \dontrun{
#' tmp_test <- tibble::tribble(
#'         ~common,                  ~scientific,
#'   "Minke whale",  "Balaenoptera acutorostrata",
#'    "Blue whale",  "Balaenoptera musculus")
#' wm_add_aphia_id(df_test, scientific, tbl_str = "tmp_test")
#' }
wm_add_aphia_id <- function(
    df, fld,
    tbl_str = deparse(substitute(df)),
    fld_str = deparse(substitute(fld))){

  if ("." == tbl_str)
    stop(
      "The `tbl_str` argument is '.' meaning you need to pass the data frame
      directly into `wm_add_aphia_id()` or if piping (ie `%>%`) set the `tbl_str`.")

  # ensure aphia_id not already a field in df
  stopifnot(!"aphia_id" %in% colnames(df))

  con <- oh_pg_con()

  taxas <- df %>%
    sf::st_drop_geometry() %>%
    group_by({{ fld }}) %>%
    summarize(.groups = "drop") %>%
    pull({{ fld }})
  taxas_0 <- taxas # debug

  # match taxa already in db table taxa
  message(glue("DB: matching {length(taxas)} in table taxa."))
  taxa_y <- tbl(con, "taxa") %>%
    filter(taxa %in% taxas) %>%
    select(taxa, aphia_id) %>%
    collect() %>%
    filter(!duplicated(taxa))

  df2 <- df %>%
    left_join(
      taxa_y,
      by = setNames("taxa", fld_str)) %>%
    relocate(aphia_id)

  # look for exact names in WoRMS
  taxas <- setdiff(taxas, taxa_y$taxa)
  if (length(taxas) > 0){
    message(glue("WoRMS: exact matching {length(taxas)} taxa."))

    wm_e <- tibble(
      taxa = taxas) %>%
      wm_rest(taxa,operation = "AphiaRecordsByNames")

    wm_e <- wm_e %>%
      group_by(scientificname) %>%
      arrange(status, desc(modified)) %>%
      mutate(
        i = row_number(modified)) %>%
      filter(i == 1) %>%
      select(taxa = scientificname, valid_aphia_id)
    df2 <- df2 %>%
      left_join(
        wm_e,
        by = setNames("taxa", fld_str)) %>%
      mutate(
        aphia_id = ifelse(
          is.na(valid_aphia_id),
          aphia_id,
          valid_aphia_id)) %>%
      select(-valid_aphia_id)

    taxas <- setdiff(taxas, wm_e$taxa)
  }

  # fuzzy match taxa in WoRMS
  if (length(taxas) > 0){
    message(glue("WoRMS: fuzzy matching {length(taxas)} taxa."))

    wm_f <- tibble(
      taxa = taxas) %>%
      wm_rest(taxa, operation = "AphiaRecordsByMatchNames")

    wm_f <- wm_f %>%
      group_by(scientificname) %>%
      arrange(status, desc(modified)) %>%
      mutate(
        i = row_number(modified)) %>%
      filter(i == 1) %>%
      select(taxa = scientificname, valid_aphia_id)
    df2 <- df2 %>%
      left_join(
        wm_e,
        by = setNames("taxa", fld_str)) %>%
      mutate(
        aphia_id = ifelse(
          is.na(valid_aphia_id),
          aphia_id,
          valid_aphia_id)) %>%
      select(-valid_aphia_id)
  }

  # append_db...

  # taxa: origin table, field, taxa and WoRMS identifier (aphia_id)
  taxa <- df2 %>%
    st_drop_geometry() %>%
    group_by({{ fld }}, aphia_id) %>%
    summarize() %>%
    select(
      taxa = {{ fld }},
      aphia_id) %>%
    mutate(
      tbl  = tbl_str,
      fld  = fld_str) %>%
    relocate(tbl, fld)

  # append taxa reference, after deleting existing
  dbSendQuery(
    con,
    glue("DELETE FROM taxa WHERE tbl = '{tbl_str}' AND fld = '{fld_str}'"))
  message(glue("DB: appending {nrow(taxa)} records to table taxa."))
  dbAppendTable(
    con, "taxa", taxa)

  # get aphia_ids missing in taxa_wm
  aphias_x <- tbl(con, "taxa") %>%
    filter(
      tbl == !!tbl_str & fld == !!fld_str) %>%
    anti_join(
      tbl(con, "taxa_wm"),
      by = "aphia_id") %>%
    select(taxa, aphia_id) %>%
    collect()

  # fetch and append WoRMS records for missing aphia_ids
  if (nrow(aphias_x) > 0 ){
    message(glue("DB: appending {nrow(aphias_x)} records to table taxa_wm."))
    taxa_wm_x <- aphias_x %>%
      wm_rest(aphia_id, operation = "AphiaRecordsByAphiaIDs")
    dbAppendTable(
      con, "taxa_wm", taxa_wm_x)
  }

  df2
}

#' Query WoRMS REST API with multiple requests
#'
#' When trying to perform batch requests the WoRMS REST API unfortunately does
#' not return the requested field so it is not obvious which taxa requested
#' matches which response. Even when fetching records in batch by `aphia_id`
#' functions in the `worrms` R package do not page requests based on the record
#' limits of the WoRMS REST API. Finally, it is much preferred to use the
#' multiplexing capabilities of the latest `httr2` library to send multiple
#' requests in parallel, versus each one sequentially.
#'
#' @param df data frame to match
#' @param fld field in data frame to use with operation
#' @param operation operation name of WoRMS REST API; One of operations listed at [marinespecies.org/rest](https://www.marinespecies.org/rest), like
#' "AphiaRecordsByMatchNames" (non-paging), "AphiaRecordsByNames" (paging) or "AphiaRecordsByAphiaIDs" (paging);
#' default: "AphiaRecordsByMatchNames"
#' @param server URL of server REST endpoint; default: "https://www.marinespecies.org/rest"
#' @param ... other query parameters to pass to operation
#'
#' @return data frame of results from WoRMS API prepended with unique values from `fld`
#' @import dplyr httr2 purrr tidyr
#' @export
#' @concept worms
#'
#' @examples
#' \dontrun{
#' tmp_test <- tibble::tribble(
#'         ~common,                   ~scientific, aphia_id_0,
#'   "Minke whale",  "Balaenoptera acutorostrata", 137087,
#'    "Blue whale",       "Balaenoptera musculus", 137090)
#' wm_exact <- wm_rest(tmp_test, scientific, "AphiaRecordsByMatchNames")
#' wm_fuzzy <- wm_rest(tmp_test, scientific, "AphiaRecordsByNames")
#' wm_byid  <- wm_rest(tmp_test, aphia_id_0, "AphiaRecordsByAphiaIDs")
#' wm_add_aphia_id(df_test, scientific, tbl_str = "tmp_test")
#' }
wm_rest <- function(
    df, fld,
    operation = "AphiaRecordsByMatchNames",
    server    = "https://www.marinespecies.org/rest",
    verbose   = F,
    ...){
  # operation="AphiaRecordsByNames"; server="https://www.marinespecies.org/rest"

  fld_str   = deparse(substitute(fld))

  # get unique values from df.fld
  vals <- df %>%
    sf::st_drop_geometry() %>%
    group_by({{ fld }}) %>%
    summarize(.groups = "drop") %>%
    pull({{ fld }})
  # DEBUG:
  # vals <<- vals

  # helper function to formulate request
  get_req <- function(vals){
    q <- setNames(vals, rep(op$param, length(vals)))
    request(server) %>%
      req_url_path_append(operation) %>%
      req_url_query(!!!q)
  }

  # helper function to transform response to data frame
  get_df <- function(resp, i=0){
    # imap(resp, get_df) sets i for debugging problematic row
    # message(glue("i: {i}"))
    # if (i == 44)
    #   browser()
    if (resp$status_code == 204) # No Content
      return(NA)

    d <- resp %>%
      resp_body_json(simplifyVector = T, flatten = T)

    if(inherits(d, "list")){
      d <- d %>%
        lapply(tibble::as_tibble) %>%
        bind_rows()
    }
    d %>%
      janitor::clean_names()
  }

  # get operation parameters
  op <- wm_rest_params %>%
    filter(operation == !!operation)
  is_paging <- nrow(op) == 1 && !is.na(op$max_req)

  # formulate requests, based on whether paging operation
  if (is_paging){
    # page through requests based on operation

    nmax <- op$max_req

    # get pages
    i_last = ifelse(
      length(vals) %% nmax == 0,
      NULL,
      length(vals))
    v_end  = ifelse(
      length(vals) > nmax,
      c(seq(nmax, length(vals), by=nmax), i_last),
      length(vals))
    d <- tibble(
      i_beg = seq(1, length(vals), by=nmax),
      i_end = v_end)

    # fetch aphia records, bind results, clean column names, remove duplicats
    d <- d %>%
      mutate(
        req = map2(
          i_beg, i_end,
          ~get_req(vals = vals[.x:.y]))) %>%
      select(-i_beg, -i_end)

    # calculate eta
    eta <- Sys.time() + as.difftime(nrow(d) * 1, units="secs")
    eta_msg <- glue(
      "Given ~ 1 second per request and {nrow(d)} requests (max {nmax} values
      per request) with {length(vals)} unique values of `{fld_str}`,
       estimated time of completion: {eta}")
  } else {
    # do not page through requests
    d <- tibble(
      val = vals) %>%
      mutate(
        req  = map(val, get_req))

    # calculate eta
    eta <- Sys.time() + as.difftime(length(vals) * 1, units="secs")
    eta_msg <- glue(
      "Given ~ 1 second per request and {length(vals)} unique values of `{fld_str}`,
       estimated time of completion: {eta}")
  }
  message(eta_msg)

  # perform requests and convert to data frame
  d <- d %>%
    mutate(
      resp = multi_req_perform(req),
      df   = map(resp, get_df)) %>%
    select(-req, -resp) %>%
    unnest(df)

  # if not paging, rename field vals to original input fld
  if (!is_paging){
    fld_val <- fld_str
    if (fld_val %in% names(d))
      fld_val <- glue("_{fld_val}")
    flds_rnm <- setNames("val", fld_val)
    d <- d %>%
      rename(!!!flds_rnm)
  }

  d
}

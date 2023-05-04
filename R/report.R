#' Add extra link to caption
#'
#' Add link to caption for static reports (i.e. docx or pdf).
#'
#' @param txt text of caption to always include
#' @param txt_link text to additionally include if condition_skip not TRUE
#' @param condition_skip condition of when to skip including extra `txt_link`
#' @param url_pfx link to prefix the output
#' @concept report
#'
#' @return text
#' @export
link_caption <- function(
    txt,
    txt_link = ". Please also see the [online interactive figure]",
    condition_skip = knitr::is_html_output(),
    url_pfx = "https://offshorewindhabitat.info/report"){
  if (condition_skip)
    return(txt)

  file  = knitr::current_input() |>
    stringr::str_replace(".rmarkdown", "")
  chunk = knitr::opts_current$get("label")

  glue("{txt}{txt_link}({url_pfx}/{file}.html#{chunk}).")
}

#' Colorize field in a datatable
#'
#' Apply a spectral color based on range of values in a field of a `DT::datatable()`.
#'
#' @param dt interactive `DT::datatable()`
#' @param fld field name in data table
#' @param alpha transparency (0 to 1); default = 0.5
#'
#' @return `DT::datatable()`
#' @importFrom scales col_numeric alpha
#' @importFrom DT formatStyle styleInterval
#' @concept report
#' @export
dt_color_fld <- function(dt, fld_data, fld_style = glue::glue("{fld_data}_clr"), alpha = 1){

  v <- dt$x$data[[fld_data]]
  if (length(unique(na.omit(v))) == 1){
    cuts <- 1
    clrs <- scales::col_numeric(
      palette = "Spectral",
      domain  = c(0,2),
      reverse = T)(c(1,2))
  } else {
    q <- seq(min(v), max(v), length.out = 20)
    cuts <- q[-1] - diff(q)[1]/2
    clrs <- scales::col_numeric(
      palette = "Spectral",
      domain  = range(q),
      reverse = T)(q)
  }
  clrs <- scales::alpha(clrs, alpha)

  # fld_style <- attr(dt$x,"colnames")[which(names(d$x$data) == fld_data)]

  dt |>
    DT::formatStyle(
      fld_style,
      valueColumns = fld_data,
      backgroundColor = DT::styleInterval(cuts, clrs))
}

#' Tabulate Blocks with Score as deviation from average in zone
#'
#' @param m htmlwidget map as `leaflet::leaflet()` with attributes "zone_name",
#' "zone_key" and "block_data" as returned from `oh_map_zone_score_dev()`
#'
#' @return either a table of type `DT::datatable()` for interactive html output
#' (if `knitr::is_html_output() == TRUE`), or a static table of type `gt:gt()`
#' for Word docx output
#' @export
#' @concept report
#' @importFrom knitr is_html_output
#' @import DT
#' @import gt
#' @import dplyr
#' @import htmltools
#' @importFrom glue glue
oh_tbl_zone_score_dev <- function(m){

  zone_key <- attr(m, "zone_key")
  zone_name <- attr(m, 'zone_name')
  d <- attr(m, "block_data")

  cap <- glue("Table [tbl-blocks-{zone_key}]. Block scores for the {zone_name} Zone. Score colors range from highest to lowest: red, orange, yellow, green, blue, violet.")
  cap_html <- glue("{cap} Columns are sortable and searchable. ")
  cap_docx <- glue("{cap} Of the {nrow(d)} Blocks for this Zone, only the bottom and top 5 scoring are displayed. The full table, which is sortable and searchable, is available [online](https://offshorewindhabitat.info/report/results.html#tbl-blocks-{zone_key}).")

  hdr_html = withTags(table(
    class = "display",
    thead(
      tr(
        th("Plan"),
        th("Block"),
        th("Score"),
        th(""),
        th("Rank"),
        th("SD"))) ))

  tbl_html <- d |>
    datatable(
      container = hdr_html,
      rownames = F,
      escape = F,
      caption = cap_html,
      options = list(
        columnDefs = list(
          list(
            width = '0px', targets = "score_clr"),
          list(
            orderable = F, targets = "score_clr"))) ) |>
    formatRound("score", digits=3) |>
    dt_color_fld("score", "score_clr") |>
    formatPercentage("score_pct_rank", digits=3) |>
    formatRound("score_zone_sd", digits=4)

  tbl_docx <- bind_rows(
    slice_head(d, n=5),
    tibble(
      plan = glue("...({nrow(d) - 10} rows)...")),
    slice_tail(d, n=5)) |>
    gt() |>
    sub_missing(missing_text = "") |>
    fmt_number(
      score, decimals = 3) |>
    data_color(
      columns = score, target_columns = score_clr,
      method  = "numeric", palette = "Spectral", reverse = T) |>
    fmt_percent(
      score_pct_rank, decimals = 3) |>
    fmt_number(
      score_zone_sd, decimals = 4) |>
    cols_label(
      plan           = "Plan",
      block          = "Block",
      score          = "Score",
      score_clr      = "",
      score_pct_rank = "Rank",
      score_zone_sd  = "SD") |>
    tab_caption(cap_docx)
  tbl_docx

  switch(
    knitr::is_html_output() |> as.character(),
    `TRUE`  = tbl_html,
    `FALSE` = tbl_docx)
}


#' Get Available Gene Panels For a Database
#'
#' @return a dataframe of metadata regarding each available panel
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @export
#'
#'
available_gene_panels <- function(base_url = NULL) {

    # query ---------------------------------------------------------------------
  url_path <- "gene-panels??"

  res <- cbp_api(url_path, base_url = base_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}



#' Retrieve Genes Included For a Specified Panel ID
#'
#' @param panel_id name of panel. See `available_gene_panels()` to get panel ID
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of genes in a specified panel
#' @export
#'
#'
get_gene_panel <- function(panel_id = NULL, base_url = NULL) {

  if (is.null(panel_id)) {
    stop("You must provide at least one `panel_id`")
  }

  panel_data <- purrr::map_dfr(panel_id, function(x) {
    res <- paste0("gene-panels/", x) %>%
      cbp_api(url_path = ., base_url = base_url)

    tib <- tibble::as_tibble(res$content)

    tib %>%
      mutate(data = purrr::map(.data$genes, ~as_tibble(.x))) %>%
      select(.data$genePanelId, .data$data, .data$description) %>%
      tidyr::unnest(cols = .data$data)

  })


  return(panel_data)
}

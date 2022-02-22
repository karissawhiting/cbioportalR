
#' Get all available gene panels in a database
#'
#' @return a dataframe of metadata regarding each available panel
#' @export
#'
#'
all_available_panels <- function(base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

    # query ---------------------------------------------------------------------
  url_path <- "gene-panels??"

  res <- cbp_api(url_path, base_url = final_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}



#' Retrieve data for a specific panel
#'
#' @param panel_id name of panel
#'
#' @return A dataframe of genes in a specified panel
#' @export
#'
#'
get_panel <- function(panel_id, base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  url_path <- paste0("gene-panels/", panel_id)

  res <- cbp_api(url_path, base_url = final_url)
  df <- tibble::as_tibble(res$content) %>%
    mutate(data = purrr::map(.data$genes, ~as_tibble(.x))) %>%
    select(.data$data) %>%
    tidyr::unnest(cols = .data$data)

  return(df)
}

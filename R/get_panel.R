
#' Get all available gene panels in a database
#'
#' @return a dataframe of metadata regarding each available panel
#' @export
#'
#' @examples
#' get_cbioportal_db("public")
#' all_available_panels()
#'
all_available_panels <- function() {

    # query ---------------------------------------------------------------------
  url_path <- "gene-panels??"

  res <- cbp_api(url_path)
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
#' @examples
get_panel <- function(panel_id) {

  url_path <- paste0("gene-panels/", panel_id)

  res <- cbp_api(url_path)
  df <- tibble::as_tibble(res$content) %>%
    mutate(data = purrr::map(.data$genes, ~as_tibble(.x))) %>%
    select(.data$data) %>%
    tidyr::unnest(cols = .data$data)

  return(df)
}


#' See what genetic data is available for a given study
#'
#' @param study_id A character vector of lenth 1 indicating study_id. See `get_studies()` to see available studies.
#'
#' @return A dataframe of available genetic profiles and their names
#' @export
#'
#'

available_profiles <- function(study_id = NULL,
                               base_url =  NULL) {


  final_url <- base_url %||% get_cbioportal_url()

  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }

    # query ---------------------------------------------------------------------
  url_path <- paste0(
    "studies/", study_id,
    "/molecular-profiles?"
  )

  res <- cbp_api(url_path, base_url = final_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}



all_available_profiles <- function(base_url =  NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  # query ---------------------------------------------------------------------
  url_path <- "molecular-profiles?"

  res <- cbp_api(url_path, base_url = final_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}

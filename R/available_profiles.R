
#' See what genetic data is available for a given study
#'
#' @param study_id A character vector of lenth 1 indicating study_id. See `get_studies()` to see available studies.
#'
#' @return A dataframe of available genetic profiles and their names
#' @export
#'
#' @examples
#' get_cbioportal_db('public')
#' available_profiles("acc_tcga")
#'
available_profiles <- function(study_id = NULL) {
  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }

    # query ---------------------------------------------------------------------
  url_path <- paste0(
    "studies/", study_id,
    "/molecular-profiles?"
  )

  res <- cbp_api(url_path)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}



all_available_profiles <- function() {

    # query ---------------------------------------------------------------------
  url_path <- "molecular-profiles?"

  res <- cbp_api(url_path)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}

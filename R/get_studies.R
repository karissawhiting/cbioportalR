#' Get information about available studies in a cBioPortal instance
#'
#' @param study_id Character vector of one single study ID (e.g. `"mskimpact"`) or NULL.
#' Default is NULL which returns all available studies. Currently multiple study ids are not supported.
#' @return A tibble of study meta data
#' @export
#'
#' @examples
#' get_cbioportal_db("public")
#' get_studies()
#' get_studies(study_id = "msk_impact_2017")
#'
get_studies <- function(study_id = NULL) {

  url_path <- paste0("studies/", study_id)
  res <- cbp_api(url_path)

  if(is.null(study_id)) {
    x <- purrr::map_df(res$content, ~tibble::as_tibble(.x))
  } else (
    x <- tibble::enframe(res$content) %>% tidyr::pivot_wider()
  )

  return(x)

}

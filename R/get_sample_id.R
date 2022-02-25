#' Get sample IDs for a given set of patient IDs
#'
#' @param patient_ids A character string of sample IDs to query
#' @param base_url The database URL to query
#'
#' @return A dataframe of patient IDs and corresponding sample IDs. If patient
#' has multiple samples, there will be multiple rows per patient.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' get_sample_id(patient_id = c("P-0000034", "P-0000036"))
#' }
#'
get_sample_id <- function(patient_id = NULL,
                           study_id = NULL,
                           base_url = NULL) {

  .check_for_patient_id(patient_id)

  # Need this for default study
  resolved_url <- base_url %>%
    .resolve_url() %||%
    .get_cbioportal_url()  %||%
    abort(message = "must supply a url. Try `set_cbioportal_db()`")

  # if no study ID, guess defaults based on resolved URL
  study_id <- .guess_study_id(study_id, resolved_url)

  # query --------------------------------------------------------------------
  list_of_urls <- purrr::map(patient_ids,
                             ~paste0("studies/", study_id,
                                     "/patients/",
                                     .x, "/samples?"))


  api_results <- purrr::map_dfr(list_of_urls, function(x) {
    res <- cbp_api(url_path = x, base_url = base_url)
    res$content
    df <- as.data.frame(res$content) %>%
      select(patientId, sampleId, sampleType, studyId)
    df
    })


  df <- api_results %>%
    dplyr::distinct()

  return(df)

}


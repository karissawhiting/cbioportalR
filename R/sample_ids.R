#' Get sample IDs for a given set of patient IDs
#'
#' @param patient_id A character string of sample IDs to query
#' @param study_id A character string indicating which study ID should be searched.
#' Only 1 study allowed. If NULL, we will guess a default study ID based on your database URL.
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of patient IDs and corresponding sample IDs. If patient
#' has multiple samples, there will be multiple rows per patient.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' get_sample_id_by_patient(patient_id = c("P-0000034", "P-0000036"))
#' }
#'
get_sample_by_patient <- function(patient_id = NULL,
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
  list_of_urls <- purrr::map(patient_id,
                             ~paste0("studies/", study_id,
                                     "/patients/",
                                     .x, "/samples?"))


  api_results <- purrr::map_dfr(list_of_urls, function(x) {
    res <- cbp_api(url_path = x, base_url = base_url)
    res$content
    df <- as.data.frame(res$content) %>%
      select(.data$patientId, .data$sampleId,
             .data$sampleType, .data$studyId)
    df
    })


  df <- api_results %>%
    dplyr::distinct()

  return(df)

}

#' Get All Sample IDs in a Study
#'
#' @param study_id A character string indicating which study ID should be searched.
#' Only 1 study allowed. If NULL, we will guess a default study ID based on your database URL.
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of sample_ids in a given study
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' get_samples_by_study(study_id = "acc_tcga")
#' }
#'
get_samples_by_study<- function(study_id = NULL,
                               base_url = NULL) {

  .check_for_study_id(study_id)

  # query --------------------------------------------------------------------
  list_of_urls <- purrr::map(study_id,
                             ~paste0("studies/", .x,
                                     "/samples?"))


  api_results <- purrr::map_dfr(list_of_urls, function(x) {
    res <- cbp_api(url_path = x, base_url = base_url)
    res$content
    df <- bind_rows(res$content) %>%
      select(.data$patientId, .data$sampleId,
             .data$sampleType, .data$studyId)
    df
  })


  df <- api_results %>%
    dplyr::distinct()

  return(df)

}



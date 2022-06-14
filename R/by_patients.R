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
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' get_samples_by_patient(patient_id = c("P-0000034", "P-0000036"))
#'
#'
get_samples_by_patient <- function(patient_id = NULL,
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




#' Get clinical data by attribute, study ID and patient ID
#'
#' @inheritParams get_clinical_by_sample
#' @param patient_id a cBioPortal patient_id
#' @param patient_study_pairs A dataframe with columns: `patient_id`, `study_id`.
#' This can be used in place of `patient_id`, `study_id`, arguments above if you
#' need to pull samples from several different studies at once. If passed, this will take overwrite `patient_id` and `study_id` if they are also passed.
#' @return a dataframe of a specific clinical attribute
#' @export
#'
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#'
#' ex <- tibble::tribble(
#' ~patient_id, ~study_id,
#' "P-0001453", "blca_nmibc_2017",
#' "P-0002166", "blca_nmibc_2017",
#' "P-0003238", "blca_nmibc_2017",
#' "P-0000004", "msk_impact_2017",
#' "P-0000023", "msk_impact_2017")
#'
#' x <- get_clinical_by_patient(patient_study_pairs = ex,
#'  clinical_attribute = NULL, base_url = 'www.cbioportal.org/api')
#'

get_clinical_by_patient <- function(study_id = NULL,
                                   patient_id = NULL,
                                   patient_study_pairs = NULL,
                                   clinical_attribute = NULL,
                                   base_url = NULL) {


  # Check Arguments ---------------------------------------------------------

  if(is.null(patient_id) & is.null(patient_study_pairs))  {
    cli::cli_abort("You must pass either {.code patient_id} or {.code patient_study_pairs}")
  }

  # * if no patient_study_pairs ----

  # `patient_study_pairs` gets priority. If that is NULL then consider other args
  if(is.null(patient_study_pairs)) {

    # Need final URL to guess default study
    resolved_url <- base_url %>%
      .resolve_url() %||%
      .get_cbioportal_url()  %||%
      abort(message = "must supply a url. Try `set_cbioportal_db()`")


    # get study ID
    resolved_study_id <- study_id %>%
      purrr::when(!is.null(.) ~ .,
                  ~ suppressMessages(.guess_study_id(study_id, resolved_url)))



    # create lookup dataframe -
    patient_study_pairs <- data.frame("patient_id" = patient_id,
                                     "study_id" = resolved_study_id)

  }

  # * check sample_study_pairs-------
  if(
    !("data.frame" %in% class(patient_study_pairs)) |
    !("patient_id" %in% colnames(patient_study_pairs)) |
    !("study_id" %in% colnames(patient_study_pairs))
  ) {

    rlang::abort("`patient_study_pairs` must be a `data.frame` with the following columns: `patient_id` and `study_id`")
  }

  # Prep data frame for Query ------------------------------------------------
  patient_study_pairs_nest <- patient_study_pairs %>%
    group_by(.data$study_id) %>%
    tidyr::nest(sample_id_nest = .data$patient_id)

  # Query --------------------------------------------------------------------
  df <- purrr::map2_dfr(patient_study_pairs_nest$study_id, patient_study_pairs_nest$sample_id_nest,
                        ~.get_clinical_pat_by_list_item (study_id = .x,
                                                     patient_id = .y$patient_id,
                                                     clinical_attribute = clinical_attribute,
                                                     base_url = base_url))
  return(df)
}

#' Get clinical data by attribute, study ID and patient ID
#'
#' @inheritParams .get_clinical_by_list_item
#' @param patient_id a cBioPortal patient_id
#' @return a dataframe of a specific clinical attribute
#' @keywords internal
#' @export
#'
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' .get_clinical_pat_by_list_item(study_id = "msk_impact_2017",
#'  patient_id = "P-0001453",
#'   base_url = 'www.cbioportal.org/api')
#'
.get_clinical_pat_by_list_item <- function(study_id = NULL,
                                       patient_id = NULL,
                                       clinical_attribute = NULL,
                                       base_url = NULL) {

  # check arguments  -----------------------------------------------------------
  .check_for_patient_id(patient_id)
  .check_for_study_id(study_id)

  resolved_clinical_attributes <- clinical_attribute %||%
    (available_clinical_attributes(study_id, base_url = base_url) %>%
       pull(.data$clinicalAttributeId) %>%
       unique())

  if(is.null(clinical_attribute)) {
    cli_alert_warning("No {.var clinical_attribute} passed. Defaulting to returning
                      all clinical attributes in {.val {study_id}} study")
  }

  # query ---------------------------------------------------------------------
  url_path <- paste0("studies/",
                     study_id,
                     "/clinical-data/fetch?clinicalDataType=PATIENT")


  body <- list(
    attributeIds = resolved_clinical_attributes,
    ids = patient_id
  )

  res <- cbp_api(url_path,
                 method = "post",
                 body = body,
                 base_url = base_url)


  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  return(df)
}



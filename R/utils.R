#' Check for study ID
#'
#' @param study_id
#'
#' @return stop if no arg
#' @keywords internal
#' @noRd
#' @export
#'
.check_for_study_id <- function(study_id) {
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on your database")
  }
}

#' Check for Sample ID
#'
#' @param sample_id
#'
#' @return stop if no sample_id arg
#' @keywords internal
#' @noRd
#' @export
#'
.check_for_sample_id <- function(sample_id) {
  if (is.null(sample_id)) {
    stop("You must provide at least one sample id")
  }
}

#' Check for Patient ID
#'
#' @param patient_id
#'
#' @return stop if no sample_id arg
#' @keywords internal
#' @noRd
#' @export
#'
.check_for_patient_id <- function(patient_id) {
  if (is.null(patient_id)) {
    stop("You must provide at least one patient id")
  }
}


#' Check for Patient ID
#'
#' @param study_id
#' @param resolved_url
#'
#' @return a guess at which study_id a user may want to use
#' @keywords internal
#' @noRd
#' @export
#'
.guess_study_id <- function(study_id, resolved_url) {

  study_id_guess <- study_id %||%
    switch(stringr::str_detect(resolved_url, "mskcc.org"), "mskimpact") %||%
    switch(resolved_url == "www.cbioportal.org/api",
           "msk_impact_2017")

  study_id_guess %||% rlang::abort("Unable to guess a study_id for your database.
                                   You must provide a study_id")

  cli::cli_alert_info("No {.code study_id} provided. Using {.val {study_id_guess}} as default study")

  return(study_id_guess)
}



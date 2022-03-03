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

#' Check for Molecular Profile ID
#'
#' @param molecular_profile_id
#'
#' @return stop if no arg
#' @keywords internal
#' @noRd
#' @export
#'
.check_for_molecular_profile_id <- function(molecular_profile_id) {

  if (is.null(molecular_profile_id)) {
    stop("You must provide a molecular_profile_id. See `available_profiles(<study_id>)` to view available profiles for a study")
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


#' Guess Study ID based on URL
#'
#' @param study_id a study id passed by a user
#' @param resolved_url the database URL
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


#' Get Molecular Profile Name for Data Type
#'
#'@description See: https://docs.cbioportal.org/5.1-data-loading/data-loading/file-formats#discrete-copy-number-data
# for definition of molecular profiles. CNA can have _cna, _gistic, _rae molecular profile names

#' @param data_type specify what type of data to return. Options are`mutation`, `cna`, `fusion`.
#' @param study_id study id for which to lookup profiles
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return find molecular profile name for a specified data type
#' @keywords internal
#' @noRd
#' @export
#'
.lookup_profile_name <- function(data_type, study_id, base_url) {

  profs <- available_profiles(study_id = study_id, base_url = base_url)

  resolved_profile <- switch(data_type,
                             mutation = filter(profs, .data$molecularAlterationType == "MUTATION_EXTENDED") %>%
                               pull(.data$molecularProfileId),
                             fusion = filter(profs, .data$molecularAlterationType == "STRUCTURAL_VARIANT") %>%
                               pull(.data$molecularProfileId),
                             cna = filter(profs, .data$molecularAlterationType == "COPY_NUMBER_ALTERATION" &
                                            .data$datatype == "DISCRETE") %>%
                               pull(.data$molecularProfileId))


  if(length(resolved_profile) == 0) {
    cli::cli_abort("No molecular profile for {.code data_type = {data_type}} found in {.val {study_id}}.  See {.code available_profiles('{study_id}')}")
  }

  resolved_profile
}

#' Get Study ID From Passed Mutation Profile ID
#'

#' @param molecular_profile_id Molecular profile name
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return find study ID for a specified molecular profile
#' @keywords internal
#' @noRd
#' @export
#'
.lookup_study_name <- function(molecular_profile_id, base_url) {

  # study_id = NULL- will return all studies
  quiet_available_profiles <- purrr::quietly(available_profiles)
  profs <- quiet_available_profiles(study_id = NULL, base_url = base_url)

  resolved_study_id <- profs$result %>%
    filter(.data$molecularProfileId == molecular_profile_id) %>%
    pull(.data$studyId)

  if(length(resolved_study_id) == 0) {
    cli::cli_abort("No molecular profile {.val {molecular_profile_id}} found. See {.code available_profiles() }")
  }

  resolved_study_id

}





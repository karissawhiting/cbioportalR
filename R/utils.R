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

  study_id %>% purrr::when(
    is.null(.) ~ cli::cli_abort(c("You must provide a {.code study_id}. See {.code get_studies()} to view available studies on your database")),
    length(.) > 1 ~ cli::cli_abort(c("{.code length(study_id)} must be 1. You can only pass one {.code study_id} at a time")),
    ~ NULL
  )

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

  study_id_guess %||% cli::cli_abort("Unable to guess a {.code study_id} for your database.
                                   Please provide a {.code study_id}.")

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
.lookup_study_name <- function(molecular_profile_id, study_id, base_url) {

  # study_id = NULL- will return all studies quietly
  # If study ID is supplied but wrong (doesn't exist in database) this will fail
  quiet_available_profiles <- purrr::quietly(available_profiles)
  profs <- tryCatch(

    # ** Maybe there can be a better API fail message that propogates thorughout because base_url should  always be checked/throw error before
    #  any parameter issues.
    quiet_available_profiles(study_id = study_id, base_url = base_url),
                     error = function(e) cli::cli_abort("API Failed, check your database connection ({.code test_cbioportal_db()}) and make sure {.val study_id:}{.code {study_id}} exists ({.code available_studies()})"))


  resolved_study_id <- profs$result %>%
    filter(.data$molecularProfileId == molecular_profile_id) %>%
    pull(.data$studyId)

  if(length(resolved_study_id) == 0) {
    cli::cli_abort("Molecular profile {.val {molecular_profile_id}} doesn't exist, or molecular profile doesn't match the {.val study_id} you passed. See {.code available_profiles()} or {.code available_studies()}")
  }

  resolved_study_id

}


#' Get Hugo Symbols From Genomics Data Pulls
#'

#' @param df data frame resulting from genomic data pulls
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return a dataframe that matches input data frame but with hugoGeneSymbol column
#' @keywords internal
#' @noRd
#' @export
#'
.lookup_hugo <- function(df, base_url) {

  hugo <- get_hugo_symbol(unique(df$entrezGeneId)) %>%
    select(-.data$type)

  df_with_hugo <- left_join(df, hugo, by ="entrezGeneId" ) %>%
    select(.data$hugoGeneSymbol, .data$entrezGeneId, everything())

  # If there happens to be more than 1 hugo per entrez
  if(!(nrow(df_with_hugo) == nrow(df))) {
    cli::cli_abort("Could not automatically add Hugo Symbol.
                 To proceed, please set {.code add_hugo = FALSE}.
                 You can investigate issue by calling {.code get_hugo(<your df>$entrezGeneId)} on results")
  }

  # When hugo symbol unknown, return entrez_gene with signifier
  df_with_hugo <-df_with_hugo %>%
    mutate(hugoGeneSymbol = case_when(is.na(hugoGeneSymbol) ~ paste0("unk_gene_", entrezGeneId),
                                      TRUE ~ hugoGeneSymbol))

  return(df_with_hugo)


}




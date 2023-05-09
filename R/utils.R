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


#' Check Sample ID-Study ID and Patient ID-Study ID pairs input data frames
#'
#' @param input_df input data frame to check
#'
#' @return A valid `sample_study_pairs` or `patient_study_pairs` data frame. If `input_df` is NULL, it will return NULL.
#' @keywords internal
#' @noRd
#' @export
#'
.check_input_pair_df <- function(input_df) {

  input_df %||% return(NULL)

  # may change this to: exists(arg, envir = rlang::caller_env())
  arg_name <- deparse(substitute(input_df))

  # must be a data.frame
  switch(!inherits(input_df, "data.frame"),
    rlang::abort("{arg_name} must be a `data.frame`")
  )

  # must have sample_id and study_id columns
  names(input_df) <- names(input_df) %>%
    stringr::str_remove_all(., stringr::fixed(" ")) %>%
    stringr::str_remove_all(., stringr::fixed("_")) %>%
    stringr::str_remove_all(., stringr::fixed(".")) %>%
    stringr::str_to_lower()

  switch(arg_name,
    "sample_study_pairs" = {
      final_names <- c("sample_id", "study_id")
    },
    "patient_study_pairs" = {
      final_names <- c("patient_id", "study_id")
      accepted_names <- c(final_names, stringr::str_remove_all(final_names, "_"))
    }
  )

  accepted_names <- c(final_names, stringr::str_remove_all(final_names, "_"))

  output_df <- input_df %>%
    purrr::when(
      !(any(stringr::str_detect(names(.), paste0(accepted_names[c(1, 3)], collapse = "|"))) &
        any(stringr::str_detect(names(.), paste0(accepted_names[c(2, 4)], collapse = "|")))) ~
        cli::cli_abort("{arg_name} must have the following columns: {final_names}"),
      TRUE ~ select(
        ., (contains("sample") | contains("patient")),
        contains("study")
      ) %>%
        purrr::set_names(final_names)
    )

  # if molecular_profile_id passed, keep it
  optional_molec <- c("molecular_profile_id",
                      stringr::str_remove_all("molecular_profile_id", "_"))

  molec_col <- names(input_df)[stringr::str_detect(names(input_df),
                                          paste0(optional_molec, collapse = "|"))]

  if(length(molec_col > 0)) {
    output_df <- input_df %>%
      transmute("molecular_profile_id" = .data[[molec_col]]) %>%
      bind_cols(output_df, .)
  }

  output_df
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
#' @description See: https://docs.cbioportal.org/5.1-data-loading/data-loading/file-formats#discrete-copy-number-data
#  for definition of molecular profiles. CNA can have _cna, _gistic, _rae molecular profile names

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
                               pull(.data$molecularProfileId),
                             segment = "Not Applicable")


  if(length(resolved_profile) == 0) {
    cli::cli_abort("No molecular profile for `data_type = {data_type}` found in {.val {study_id}}.  See `available_profiles('{study_id}')`")
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

    # ** Maybe there can be a better API fail message that propagates throughout because base_url should  always be checked/throw error before
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

  if(any(stringr::str_detect(names(df), "hugoGeneSymbol"))) {
    cli::cli_warn("{.field 'hugoGeneSymbol'} column already exists. Overwriting this column. Please set {.code add_hugo = FALSE} to supress this}")
    df <- df %>%
      select(-all_of('hugoGeneSymbol'))
  }

  hugo <- get_hugo_symbol(unique(df$entrezGeneId)) %>%
    select(-.data$type)

  df_with_hugo <- left_join(df, hugo, by = "entrezGeneId" ) %>%
    select(.data$hugoGeneSymbol, .data$entrezGeneId, everything())

  # If there happens to be more than 1 hugo per entrez
  if(!(nrow(df_with_hugo) == nrow(df))) {
    cli::cli_abort("Could not automatically add Hugo Symbol.
                 To proceed, please set {.code add_hugo = FALSE}.
                 You can investigate issue by calling {.code get_hugo(<your df>$entrezGeneId)} on results")
  }

  # When hugo symbol unknown, return entrez_gene with signifier
  df_with_hugo <- df_with_hugo %>%
    mutate(hugoGeneSymbol = case_when(is.na(hugoGeneSymbol) ~ paste0("unk_gene_", entrezGeneId),
                                      TRUE ~ hugoGeneSymbol))

  return(df_with_hugo)


}


#' Get Entrez Gene IDs for a given panel ID
#'

#' @param panel_id Panel ID for which to retrieve gene IDs (see `available_gene_panels()` for options).
#' If NULL, it will return NULL
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return a vector of Entrez Gene IDs
#' @keywords internal
#' @noRd
#' @export
#'
.get_panel_entrez <- function(panel_id, base_url) {

  # if NULL, return NULL
  panel_id %||% return(panel_id)

  res <- tryCatch(get_gene_panel(panel_id = panel_id, base_url = base_url),
    error = function(e) {
      cli::cli_abort("There was an error pullling genes for panel ID: {.code {panel_id}}. See {.code available_gene_panels()} for supported panels.")
    })

  entrez_ids <- res %>%
    dplyr::pull(.data$entrezGeneId) %>%
    unique()

  return(entrez_ids)

    }


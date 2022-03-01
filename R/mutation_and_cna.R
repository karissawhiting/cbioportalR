
#' Get Mutation or CNA Data By Study ID
#'
#' @description Endpoints for retrieving mutation and cna data are structurally similar.
#' This internal function allows you to pull data from either endpoint. It has
#' logic for sensible default guesses at `study_id` and `molecular_profile_id` when those are `NULL`
#' @param study_id A study ID to query mutations. If NULL, guesses study ID based
#' on molecular_profile_id.
#' @param molecular_profile_id a molecular profile to query mutations.
#' If NULL, guesses molecular_profile_id based on study ID.
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return
#' @export
#'
#' @examples
#' set_cbioportal_db("public")
#' get_data_by_study(study_id = "prad_msk_2019", data_type = "cna")
#' get_data_by_study(study_id = "prad_msk_2019", data_type = "mutations")
#' get_data_by_study(molecular_profile_id = "prad_msk_2019_cna", data_type = "cna")
#'
get_data_by_study <- function(study_id = NULL,
                              molecular_profile_id = NULL,
                              data_type = c("mutations", "cna"),
                              base_url = NULL) {

  data_type <- match.arg(data_type)
  und_data_type <- paste0("_", data_type)
  url_data_type <- ifelse(data_type == "mutations",
                          "mutations",
                          "discrete-copy-number")

  # check ----------------------------------------------------------------------
  if(is.null(study_id) & is.null(molecular_profile_id)) {
    rlang::abort("You must provide a `study_id` or a `molecular_profile_id`. See `available_profiles(<study_id>)` to view available profiles for a study")
  }


  molecular_profile_id <- molecular_profile_id %||%
    paste0(study_id, und_data_type)

  study_id <- study_id %||%
    stringr::str_remove(molecular_profile_id, und_data_type)

  # API requires that you pass a sample list ID. All studies should have an "all" list which is the default for this function
  sample_list_id <- paste0(study_id, "_all")

  # query ----------------------------------------------------------------------

  url_list <- paste0(
    "molecular-profiles/", molecular_profile_id,
    "/",
    url_data_type,
    "?sampleListId=",
    sample_list_id
  )

  df <- purrr::map_df(url_list, function(x) {
    res <- cbp_api(x, base_url = base_url)
    df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  })

  cli::cli_alert_info("Returning all data for the {.val {molecular_profile_id}} molecular profile in the {.val {study_id}} study")


  return(df)


}


#' Get Mutations By Study ID
#'
#' @param study_id A study ID to query mutations. If NULL, guesses study ID based
#' on molecular_profile_id.
#' @param molecular_profile_id a molecular profile to query mutations.
#' If NULL, guesses molecular_profile_id based on study ID.
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return
#' @export
#'
#' @examples
#' get_mutation_by_study(study_id = "prad_msk_2019")
#' get_mutation_by_study(molecular_profile_id = "prad_msk_2019_mutations")
#'
#'
get_mutation_by_study <- function(study_id = NULL,
                                  molecular_profile_id = NULL,
                                  base_url = NULL) {

  get_data_by_study(study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     data_type = c("mutations"),
                     base_url = base_url)
}

#' Get CNA By Study ID
#'
#' @inheritParams get_data_by_study
#'
#' @return a data frame of CNA data
#' @export
#'
#' @examples
#' get_cna_by_study(study_id = "prad_msk_2019")
#' get_cna_by_study(molecular_profile_id = "prad_msk_2019_cna")
#'

get_cna_by_study <- function(study_id = NULL,
                                  molecular_profile_id = NULL,
                                  base_url = NULL) {

  get_data_by_study(study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     data_type = c("cna"),
                     base_url = base_url)
}


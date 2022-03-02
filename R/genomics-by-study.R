
# Generalized Data Pull Function ------------------------------------------------------------

#' Internal Function to Get Mutations/CNA/Fusion By Study ID
#'
#' @description Endpoints for retrieving mutation and cna data are structurally similar.
#' This internal function allows you to pull data from either endpoint. It has
#' logic for sensible default guesses at `study_id` and `molecular_profile_id` when those are `NULL`
#'
#' @param study_id A study ID to query mutations. If NULL, guesses study ID based
#' on molecular_profile_id.
#' @param molecular_profile_id a molecular profile to query mutations.
#' If NULL, guesses molecular_profile_id based on study ID.
#' @param data_type specify what type of data to return. Options are`mutation`, `cna`, `fusion`.
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return a dataframe of mutations or CNAs
#' @export
#' @keywords internal
#'
#' @examples
#' set_cbioportal_db("public")
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "cna")
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "mutation")
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "fusion")
#'
#' .get_data_by_study(molecular_profile_id = "prad_msk_2019_cna", data_type = "cna")
#' .get_data_by_study(molecular_profile_id = "prad_msk_2019_mutations", data_type = "mutation")
#' .get_data_by_study(molecular_profile_id = "prad_msk_2019_fusion", data_type = "fusion")
#'
.get_data_by_study <- function(study_id = NULL,
                              molecular_profile_id = NULL,
                              data_type = c("mutation", "cna", "fusion"),
                              base_url = NULL) {


  data_type <- match.arg(data_type)

  url_data_type <- case_when(
    data_type == "mutation" ~ "mutations",
    data_type == "fusion" ~ "fusion",
    data_type == "cna" ~ "discrete-copy-number")

  und_data_type <- case_when(
    url_data_type %in% c("mutations", "fusion") ~ url_data_type,
    url_data_type == "discrete-copy-number" ~ "cna") %>%
    paste0("_", .)

  # Check Arguments  -----------------------------------------------------------
  if(is.null(study_id) & is.null(molecular_profile_id)) {
    rlang::abort("You must provide a `study_id` or a `molecular_profile_id`. See `available_profiles(<study_id>)` to view available profiles for a study")
  }


  molecular_profile_id <- molecular_profile_id %||%
    paste0(study_id, und_data_type)

  study_id <- study_id %||%
    stringr::str_remove(molecular_profile_id, und_data_type)

  # Some API endpoints require that you pass a sample list ID. All studies should have an "all" list which is the default for this function
  sample_list_id <- paste0(study_id, "_all")


  # MUTATION/CNA query ----------------------------------------------------------------------

  if(data_type %in% c("mutation", "cna")) {

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

  }

  # FUSIONS query ----------------------------------------------------------------------

  # Fusions endpoint works a little differently than Mut and CNA
  # Instead of passing a sample list, you pass individual sample IDs (retrieved using list)
  # Main Goal in this function is to return all results without specifying specific genes to query (as is needed in other endpoints).

  if(data_type == "fusion") {

    # Need to get all sample IDs in study for fusion retrieval
    sample_list_url <- paste0("sample-lists/", sample_list_id)

    all_samples_in_study <- cbp_api(
      url_path = sample_list_url,
      base_url = base_url
    )$content$sampleIds %>%
      unlist() %>% unique()


    fus_imp <- purrr::map(all_samples_in_study, function(x) {

      body <- list(
   #     entrezGeneIds = c(2078, 7113),
        sampleMolecularIdentifiers = as.data.frame(list(
          molecularProfileId = jsonlite::unbox(molecular_profile_id),
          sampleId = x
        ))
      )


      fus <- cbp_api(
        url_path = "structural-variant/fetch?",
        method = "post",
        body = body,
        base_url = base_url
      )

      fus$content
   #
    })

    df <- bind_rows(fus_imp)

  }

  cli::cli_alert_info("Returning all data for the {.val {molecular_profile_id}} molecular profile in the {.val {study_id}} study")
  return(df)


}


# Wrapper Functions ------------------------------------------------------------

#' Get Mutations By Study ID
#'
#' @inheritParams .get_data_by_sample
#'
#' @return A dataframe of mutations (maf file format)
#' @export
#' @examples
#' get_mutation_by_study(study_id = "prad_msk_2019")
#' get_mutation_by_study(molecular_profile_id = "prad_msk_2019_mutations")
#'
#'
get_mutation_by_study <- function(study_id = NULL,
                                  molecular_profile_id = NULL,
                                  base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    data_type = c("mutation"),
                    base_url = base_url)
}

#' Get CNA By Study
#'
#' @inheritParams .get_data_by_study
#' @return A dataframe of CNAs
#' @export
#'
#'
#' @examples
#' get_cna_by_study(study_id = "prad_msk_2019")
#' get_cna_by_study(molecular_profile_id = "prad_msk_2019_cna")
#'

get_cna_by_study <- function(study_id = NULL,
                             molecular_profile_id = NULL,
                             base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    data_type = c("cna"),
                    base_url = base_url)
}


#' Get Fusions By Study
#'
#' @inheritParams .get_data_by_study
#' @return A dataframe of fusions
#' @export
#'
#'
#' @examples
#' get_fusion_by_study(study_id = "prad_msk_2019")
#' get_fusion_by_study(molecular_profile_id = "prad_msk_2019_fusion")
#'

get_fusion_by_study <- function(study_id = NULL,
                             molecular_profile_id = NULL,
                             base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    data_type = c("fusion"),
                    base_url = base_url)
}




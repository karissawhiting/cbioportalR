
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
#' @param data_type specify what type of data to return. Options are`mutation`, `cna`, `fusion`, or`structural_variant` (same as `fusion`),
#' and `segment` (copy number segmentation data)..
#' @param add_hugo Logical indicating whether `HugoGeneSymbol` should be added to your resulting data frame, if not already present in raw API results.
#' Argument is `TRUE` by default. If `FALSE`, results will be returned as is (i.e. any existing Hugo Symbol columns in raw results will not be removed).
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return a dataframe of mutations, CNAs or structural variants
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "cna")
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "mutation")
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "fusion")
#'
#' .get_data_by_study(molecular_profile_id = "prad_msk_2019_cna", data_type = "cna")
#' .get_data_by_study(molecular_profile_id = "prad_msk_2019_mutations", data_type = "mutation")
#' .get_data_by_study(molecular_profile_id = "prad_msk_2019_structural_variants", data_type = "fusion")
#' }
#'
.get_data_by_study <- function(study_id = NULL,
                              molecular_profile_id = NULL,
                              data_type = c("mutation", "cna", "fusion", "structural_variant", "segment"),
                              base_url = NULL,
                              add_hugo = TRUE) {



  # Check Arguments  -----------------------------------------------------------

  # For queries, you need both study_id and molecular_profile_id

  # If both study_id & molecular_profile_id are NULL- stop
  if(is.null(study_id) & is.null(molecular_profile_id)) {
    cli::cli_abort("You must provide a {.code study_id} or a {.code molecular_profile_id}. See {.code available_profiles(<study_id>)} to view available profiles for a study")
  }

  # fusions and structural_variants are the same. fusion is older nomenclature.
  data_type <- match.arg(data_type)

  data_type <- dplyr::case_when(
    data_type == "structural_variant" ~ "fusion",
    TRUE ~ data_type)

  if(data_type != "segment") {
    # study ID provided and profile is NULL
    # If study ID is not correct, informative error thrown
    molecular_profile_id <- molecular_profile_id %||%
      .lookup_profile_name(data_type, study_id, base_url = base_url)


    # if study_id is NULL or not NULL molecular profile ID can't be NULL
    study_id <- .lookup_study_name(molecular_profile_id = molecular_profile_id,
                         study_id = study_id,
                         base_url = base_url)
  }


  # this text goes in query URL path
  url_data_type <- switch(
    data_type,
    "mutation" = "mutations",
    "fusion" = "structural-variant",
    "cna" = "discrete-copy-number",
    "segment" = "copy-number-segments")

  # Some API endpoints require that you pass a sample list ID. All studies should have an "all" list which is the default for this function
  sample_list_id <- paste0(study_id, "_all")


  # MUTATION/CNA query ----------------------------------------------------------------------

  # GET: /molecular-profiles/{molecularProfileId}/molecular-data?{sample_list_id}

  if(data_type %in% c("mutation", "cna")) {

    url_list <- paste0(
      "molecular-profiles/", molecular_profile_id,
      "/",
      url_data_type,
      "?sampleListId=",
      sample_list_id
    )

    df <- purrr::map_dfr(url_list, function(x) {
      res <- cbp_api(x, base_url = base_url)

      if (length(res$content) > 0) {
          result <- purrr::map_dfr(res$content, ~ purrr::list_flatten(.x))
        } else {
          result <- NULL
        }
        result
      })
    }

  # FUSIONS/SEGMENTS query ----------------------------------------------------------------------

  # POST: /structural-variant/fetch ---
  # BODY: sampleMolecularIdentifiers: molecularProfileId, sampleId

  # Fusions endpoint works a little differently than Mutation and CNA
  # Instead of passing a sample list, you pass individual sample IDs (retrieved using list)
  # Main Goal in this function is to return all results without specifying specific genes to query (as is needed in other endpoints).

  if(data_type %in% c("fusion", "segment")) {

    # Need to get all sample IDs in study for fusion retrieval
    sample_list_url <- paste0("sample-lists/", sample_list_id)

    all_samples_in_study <- cbp_api(
      url_path = sample_list_url,
      base_url = base_url
    )$content$sampleIds %>%
      unlist() %>% unique()


    fus_imp <- purrr::map_dfr(all_samples_in_study, function(x) {

      body <- switch(data_type,
             fusion = {
               list(
                 sampleMolecularIdentifiers = as.data.frame(list(
                   molecularProfileId = jsonlite::unbox(molecular_profile_id),
                   sampleId = x)))
               },
             segment = {
               data.frame(
                 "sampleId" = x,
                 "studyId" = study_id)
               })

      fus <- cbp_api(
        url_path = paste0(url_data_type, "/fetch?"),
        method = "post",
        body = body,
        base_url = base_url
      )

      if(length(fus$content) > 0) {
        result <- purrr::map_dfr(fus$content, ~purrr::list_flatten(.x))
      } else {
        result <- NULL
      }

    })

    df <- fus_imp

  }


  # * Add Hugo Symbol & Return -----

  if(add_hugo) {

    # Fusions already has hugo by default from API, segment doesn't need
    df <- switch(data_type,
                 "fusion" = df,
                 "segment" = df,
                 "mutation" = if(nrow(df) > 0) .lookup_hugo(df, base_url = base_url),
                 "cna" = if(nrow(df) > 0) .lookup_hugo(df, base_url = base_url))

  }

  alert_string = ifelse(data_type != "segment",
                        "Returning all data for the {.val {molecular_profile_id}} molecular profile in the {.val {study_id}} study",
                        "Returning all {.val copy number segmentation} data for the {.val {study_id}} study")

  if(data_type == "segment" & nrow(df) == 0) {
    cli::cli_abort("No {.val copy number segmentation} data found for {.val {study_id}} study")
  }

  cli::cli_alert_info(alert_string)
  return(df)


}


# Wrapper Functions ------------------------------------------------------------

#' Get Mutations By Study ID
#'
#' @inheritParams .get_data_by_study
#'
#' @return A dataframe of mutations (maf file format)
#' @export
#' @examples
#' \dontrun{
#' get_mutations_by_study(study_id = "prad_msk_2019")
#' get_mutations_by_study(molecular_profile_id = "prad_msk_2019_mutations")
#' }
#'
get_mutations_by_study <- function(study_id = NULL,
                                  molecular_profile_id = NULL,
                                  add_hugo = TRUE,
                                  base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    data_type = c("mutation"),
                    add_hugo = add_hugo,
                    base_url = base_url)
}

#' Get CNA By Study
#'
#' @inheritParams .get_data_by_study
#' @return A dataframe of CNAs
#' @export
#' @examples
#' \dontrun{
#' get_cna_by_study(study_id = "prad_msk_2019")
#' get_cna_by_study(molecular_profile_id = "prad_msk_2019_cna")
#' }

get_cna_by_study <- function(study_id = NULL,
                             molecular_profile_id = NULL,
                             add_hugo = TRUE,
                             base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    data_type = c("cna"),
                    add_hugo = add_hugo,
                    base_url = base_url)
}


#' Get Fusions By Study
#'
#' @inheritParams .get_data_by_study
#' @return A dataframe of fusions
#' @export
#' @aliases get_structural_variants_by_study
#' @examples
#' \dontrun{
#' # These return the same results
#' get_fusions_by_study(molecular_profile_id = "prad_msk_2019_structural_variants")
#'
#' get_structural_variants_by_study(molecular_profile_id =
#'        "prad_msk_2019_structural_variants")
#'        }

get_fusions_by_study <- function(study_id = NULL,
                             molecular_profile_id = NULL,
                             base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    data_type = c("fusion"),
                    # this doesn't matter. `add_hugo` doesn't do anything for fusion data
                    add_hugo = TRUE,
                    base_url = base_url)
}

#' @rdname get_fusions_by_study
#' @export
get_structural_variants_by_study <- get_fusions_by_study


#' Get Copy Number Segmentation Data By Study
#'
#' @inheritParams .get_data_by_study
#' @return A dataframe of CNA segments
#' @export
#' @examples
#' \dontrun{
#' get_segments_by_study(study_id = "prad_msk_2019")
#' get_segments_by_study(molecular_profile_id = "prad_msk_2019_cna")
#' }

get_segments_by_study <- function(study_id = NULL,
                             add_hugo = TRUE,
                             base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                     molecular_profile_id = NULL,
                     data_type = c("segment"),
                     add_hugo = add_hugo,
                     base_url = base_url)
}



#' Get All Genomic Information By Study
#'
#' @inheritParams .get_data_by_study
#' @return_segments Default is `FALSE` where copy number segmentation data won't be returned in addition to the mutation, cna and structural variant data.
#' `TRUE` will return any available segmentation data with results.
#' @return A list of mutations, cna and structural variants (including fusions), if available. Will also return copy number segmentation data if `return_segments = TRUE`.
#' @export
#' @examples
#' \dontrun{
#' get_genetics_by_study(study_id = "prad_msk_2019")
#' }
#
get_genetics_by_study <- function(study_id = NULL,
                                  add_hugo = TRUE,
                                  base_url = NULL,
                                  return_segments = FALSE) {

  # ** Not using `.check_for_study_id()` here because we allow no study ID to be passed,
  # but still don't allow study_id > 1. Maybe generalized that check function to
  # Make each check optional?
  if(length(study_id) > 1) {
    cli::cli_abort(c("{.code length(study_id)} must be 1. You can only pass one {.code study_id} at a time"))}

  data_types <- c("mutation", "cna", "structural_variant")

  if(return_segments) {
    data_types <- c(data_types, "segment")
  }

  safe_get_data <- purrr::safely(.get_data_by_study, quiet = TRUE)

 res <-  data_types %>%
   purrr::set_names() %>%
   purrr::map(., function(x) {
                       safe_get_data(study_id = study_id,
                                          molecular_profile_id = NULL,
                                          data_type = x,
                                     add_hugo = add_hugo,
                                     base_url = base_url,
                                     return_segments = FALSE)
                       })

 genetics <- purrr::compact(purrr::map(res, "result"))
 errors <- purrr::compact(purrr::map(res, "error"))

 switch(!purrr::is_empty(errors),
        purrr::imap(errors, ~cli_alert_warning(c("No {.val { .y}} data returned. Error:  ",
                                          # why no red :(
                                        cli::col_red('{ .x$message}'))))
        )

 genetics
}

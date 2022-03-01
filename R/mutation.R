# OLD VERSION ---------------
#' Get Data By Study ID
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
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "cna")
#' .get_data_by_study(study_id = "prad_msk_2019", data_type = "mutations")
#' .get_data_by_study(molecular_profile_id = "prad_msk_2019_cna", data_type = "cna")
#'
.get_data_by_study <- function(study_id = NULL,
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

  .get_data_by_study(study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     data_type = c("mutations"),
                     base_url = base_url)
}

#' Get CNA By Study ID
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
#' get_cna_by_study(study_id = "prad_msk_2019")
#' get_cna_by_study(molecular_profile_id = "prad_msk_2019_cna")
#'
#'
get_cna_by_study <- function(study_id = NULL,
                                  molecular_profile_id = NULL,
                                  base_url = NULL) {

  .get_data_by_study(study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     data_type = c("cna"),
                     base_url = base_url)
}



#------------------------------------------------------------------------

#' Get Mutations By Sample ID
#'
#' @param sample_id a vector of sample IDs (character)
#' @param study_id A string indicating the study ID from which to pull data. If no study ID, will
#' guess the study ID based on your URL and inform. Only 1 study ID can be passed. If mutations from
#' more than 1 study needed, see `sample_study_pairs`
#' @param molecular_profile_id  A string indicating the molecular profile ID from which to pull data. If ID supplied, will
#' guess the molecular profile ID based on the study ID. Only 1 molecular profile ID can be passed. If mutations from
#' more than 1 study needed, see `sample_study_pairs`
#' @param sample_study_pairs A dataframe with columns: `sample_id`, `study_id` and `molecular_profile_id` (optional).
#' This can be used in place of `sample_id`, `study_id`, `molecular_profile_id` arguments above if you
#' need to pull samples from several different studies at once.
#' @param genes A vector of entrez ids. If NULL, will return results for all
#' IMPACT genes (see `gnomeR::impact_gene_info`)
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return
#' @export
#'
#' @examples
#' .get_data_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01" ),
#'  study_id = "acc_tcga")
#'
#' df_pairs <- data.frame(
#' "sample_id" = c("s_C_36924L_P001_d",
#' "s_C_03LNU8_P001_d"),
#' "study_id" = c("prad_msk_2019"))
#'
#' .get_data_by_sample(sample_study_pairs = df_pairs)
#' .get_data_by_sample(sample_study_pairs = df_pairs, genes = 7157)
#' .get_data_by_sample(sample_study_pairs = df_pairs, data_type = "cna")
#'
.get_data_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   molecular_profile_id = NULL,
                                   sample_study_pairs = NULL,
                                   data_type = c("mutations", "cna"),

                                   genes = NULL,
                                   base_url = NULL) {

  # Check Arguments ------------------------------------------------------------


  data_type <- match.arg(data_type)
  und_data_type <- paste0("_", data_type)
  url_data_type <- ifelse(data_type == "mutations",
                          "mutations",
                          "discrete-copy-number")

  # `sample_study_pairs` gets priority. If that is NULL then consider other args
  if(is.null(sample_study_pairs)) {


    # Need this to guess default study
    resolved_url <- base_url %>%
      .resolve_url() %||%
      .get_cbioportal_url()  %||%
      abort(message = "must supply a url. Try `set_cbioportal_db()`")


    # Get study ID ---------
    resolved_study_id <-

      # make sure length = 1
      switch(
        length(study_id) > 1,
        rlang::abort("More than 1 `study_id` passed. Please
                   use `sample_study_pairs` argument instead")) %||%

      # guess based on molec profile
      switch((is.null(study_id) & !is.null(molecular_profile_id)),
             stringr::str_remove(molecular_profile_id, und_data_type)) %||%

      # guess based on URL
      suppressMessages(.guess_study_id(study_id, resolved_url))

    # Get molecular profile ID ---------
    resolved_molecular_profile_id <-

      # make sure length = 1
      switch(
        length(molecular_profile_id) > 1,
        rlang::abort("More than 1 `molecular_profile_id` passed. Please
                   use `sample_study_pairs` argument instead")) %||%

      molecular_profile_id %||%
      paste0(resolved_study_id, und_data_type)


    # create lookup dataframe ------
    sample_study_pairs <- data.frame("sample_id" = sample_id,
                                     "study_id" = resolved_study_id,
                                     "molecular_profile_id" = resolved_molecular_profile_id)

  }


  # default to IMPACT genes if `genes` are NULL
  resolved_genes <- genes %||%
    gnomeR::impact_gene_info$entrez_id %>% unlist()


  # Prep Data for Query -------------------------------------------------------

  if(!("data.frame" %in% class(sample_study_pairs))) {
    rlang::abort("`sample_study_pairs` must be a `data.frame` with the following columns: `sample_id`, `study_id`")
  }

  if(!("molecular_profile_id" %in% colnames(sample_study_pairs))) {
    sample_study_pairs <- sample_study_pairs %>%
      mutate(molecular_profile_id = paste0(.data$study_id, und_data_type))
  }


  sample_study_pairs_nest_raw <- sample_study_pairs %>%
    group_by(.data$study_id, .data$molecular_profile_id) %>%
    tidyr::nest(sample_id_nest = .data$sample_id) %>%
    mutate(url_path = paste0("molecular-profiles/",
                             .data$molecular_profile_id,
                             "/", url_data_type, "/fetch?"))

  sample_study_pairs_nest <- sample_study_pairs_nest_raw %>%
    ungroup() %>%
    select(.data$url_path, .data$sample_id_nest)

  # query ---------------------------------------------------------------------
  quer_res <- purrr::map2(
    sample_study_pairs_nest$url_path,
    sample_study_pairs_nest$sample_id_nest,

      function(x, y) {

        body_n = list(
          entrezGeneIds = resolved_genes,
          sampleIds = y$sample_id)

        res <- cbp_api(url_path = x,
               method = "post",
               body = body_n)


        bind_rows(res$content)

        })


  genes_msg <- genes %||%
    "all IMPACT genes (see `gnomeR::impact_gene_info`)"

  cli::cli_text("The following parameters were used in query:")
  cli::cli_dl(c("{.field Study ID}" = "{.val {unique(sample_study_pairs_nest_raw$study_id)}}",
           "{.field Molecular Profile ID}" = "{.val {unique(sample_study_pairs_nest_raw$molecular_profile_id)}}",
           "{.field Genes}" = "{.val {genes_msg}}"
           ))


  df <- quer_res %>% bind_rows()

  return(df)

  }



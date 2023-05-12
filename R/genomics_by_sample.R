
# Generalized Data Pull Function ------------------------------------------------------------

#' Internal Function to Get Mutations/CNA/Fusion By Sample ID
#'
#' @param sample_id a vector of sample IDs (character)
#' @param study_id A string indicating the study ID from which to pull data. If no study ID, will
#' guess the study ID based on your URL and inform. Only 1 study ID can be passed. If mutations/cna from
#' more than 1 study needed, see `sample_study_pairs`
#' @param molecular_profile_id  A string indicating the molecular profile ID from which to pull data. If ID supplied, will
#' guess the molecular profile ID based on the study ID. Only 1 molecular profile ID can be passed. If mutations from
#' more than 1 study needed, see `sample_study_pairs`
#' @param sample_study_pairs A dataframe with columns: `sample_id`, `study_id` and `molecular_profile_id` (optional). Variations in capitalization of column names are accepted.
#' This can be used in place of `sample_id`, `study_id`, `molecular_profile_id` arguments above if you
#' need to pull samples from several different studies at once. If passed this will take overwrite `sample_id`, `study_id`, `molecular_profile_id` if also passed.
#' @param data_type specify what type of data to return. Options are`mutations`, `cna`, `fusion` or `structural_variant` (same as `fusion`).
#' @param genes A vector of Entrez ids or Hugo symbols. If Hugo symbols are supplied, they will be converted to entrez ids using the `get_entrez_id()` function.
#' If `panel` and `genes` are both supplied, genes from both arguments will be returned. If both are NULL (default), it will return gene results for all available genomic data for that sample.
#' @param panel One or more panel IDs to query (e.g. 'IMPACT468').
#' If `panel` and `genes` are both supplied, genes from both arguments will be returned. If both are NULL (default), it will return gene results for all available genomic data for that sample.
#' @param add_hugo Logical indicating whether `HugoGeneSymbol` should be added to your resulting data frame, if not already present in raw API results.
#' Argument is `TRUE` by default. If `FALSE`, results will be returned as is (i.e. any existing Hugo Symbol columns in raw results will not be removed).
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return A dataframe of mutations, CNAs or fusions.
#' @export
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' .get_data_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
#'  study_id = "acc_tcga", data_type = "mutation")
#'
#' .get_data_by_sample(sample_id = c("DS-sig-010-P2"),
#'  molecular_profile_id = "blca_plasmacytoid_mskcc_2016_cna", data_type = "cna")
#'
#'
#' .get_data_by_sample(sample_id = c("P-0002146-T01-IM3"),
#'  study_id = "blca_plasmacytoid_mskcc_2016", data_type = "fusion")
#'
#' df_pairs <- data.frame(
#' "sample_id" = c("s_C_36924L_P001_d",
#' "s_C_03LNU8_P001_d"),
#' "study_id" = c("prad_msk_2019"))
#'
#' .get_data_by_sample(sample_study_pairs = df_pairs, data_type = "mutation")
#' .get_data_by_sample(sample_study_pairs = df_pairs, genes = 7157, data_type = "mutation")
#' .get_data_by_sample(sample_study_pairs = df_pairs, data_type = "cna")
#' .get_data_by_sample(sample_study_pairs = df_pairs, data_type = "fusion")
#'
#' df_pairs2 <- data.frame(
#' "sample_id" = c("P-0002146-T01-IM3", "s_C_CAUWT7_P001_d"),
#'  "study_id" = c("blca_plasmacytoid_mskcc_2016", "prad_msk_2019"))
#'
#' .get_data_by_sample(sample_study_pairs = df_pairs2, data_type = "mutation")
#' .get_data_by_sample(sample_study_pairs = df_pairs2, genes = 7157)
#' .get_data_by_sample(sample_study_pairs = df_pairs2, data_type = "cna")
#' .get_data_by_sample(sample_study_pairs = df_pairs2, data_type = "fusion")
#' }
#'
.get_data_by_sample <- function(sample_id = NULL,
                                study_id = NULL,
                                molecular_profile_id = NULL,
                                sample_study_pairs = NULL,
                                data_type = c("mutation", "cna", "fusion", "structural_variant", "segment"),

                                genes = NULL,
                                panel = NULL,
                                add_hugo = TRUE,
                                base_url = NULL) {

  # Check Arguments -----------------------------------------------------------

  if(is.null(sample_id) & is.null(sample_study_pairs))  {
    cli::cli_abort("You must pass either {.code sample_id} or {.code sample_study_pairs}")
  }

  if(!is.null(study_id) & !is.null(molecular_profile_id))  {
    cli::cli_alert_warning("You have passed both a {.code study_id} AND {.code molecular_profile_id}. Ignoring {.code study_id}")

    study_id <- NULL
  }

  # make sure length = 1
  if(length(study_id) > 1 | length(molecular_profile_id) > 1) {
     cli::cli_abort("More than 1 {.code study_id} or {.code molecular_profile_id} was passed. Please use the {.code sample_study_pairs} argument instead")
  }

  data_type <- match.arg(data_type)

  data_type <- dplyr::case_when(
    data_type == "structural_variant" ~ "fusion",
    TRUE ~ data_type)

  # this has to go in query URL
  url_data_type <- switch(
    data_type,
    "mutation" = "mutations",
    "fusion" = "structural-variant",
    "cna" = "discrete-copy-number",
    "segment" = "copy-number-segments")

  # Make Informed guesses on parameters ---------------------------------------

  # `sample_study_pairs` gets priority. If that is NULL then consider other args
  if(is.null(sample_study_pairs)) {

    # Need final URL to guess default study
    resolved_url <- base_url %>%
      .resolve_url() %||%
      .get_cbioportal_url()  %||%
      abort(message = "must supply a url. Try `set_cbioportal_db()`")


    # Get study ID ---------
    resolved_study_id <- study_id %>%
      purrr::when(!is.null(.) ~ .,
                  !is.null(molecular_profile_id) ~ .lookup_study_name(molecular_profile_id = molecular_profile_id,
                                                                      study_id = .,
                                                                      base_url = base_url),
                  # if both NULL
                  ~ suppressMessages(.guess_study_id(study_id, resolved_url)))


    # Get molecular profile ID ---------
    resolved_molecular_profile_id <- molecular_profile_id %||%
      .lookup_profile_name(data_type, resolved_study_id, base_url = base_url)



    # create lookup dataframe
    sample_study_pairs <- data.frame("sample_id" = sample_id,
                                     "study_id" = resolved_study_id,
                                     "molecular_profile_id" = resolved_molecular_profile_id)

  }


  # Check sample_study_pairs-------
  sample_study_pairs <- .check_input_pair_df(input_df = sample_study_pairs)

  # Get genes to query --------

  # get entrez ids for a panel
  panel_genes <- .get_panel_entrez(panel_id = panel, base_url = base_url)

  # if genes arg passed, get gene IDs if panel, or entrez IDs if hugo
  resolved_genes <- genes %>%
    purrr::when(
      is.character(.) ~ {
#        cli::cli_inform("Hugo symbols were converted to entrez IDs in order to query the cBioPortal API (see {.code ?get_entrez_id} for more info)")
        get_entrez_id(., base_url = base_url)$entrezGeneId
      },
      TRUE ~ .)

  resolved_genes <- c(panel_genes, resolved_genes) %>%
    unique()


  # * Prep data frame for Query -------------------------------------------------

  # If user passes study_id and data_type we can pull the correct molecular ID
  if(!("molecular_profile_id" %in% colnames(sample_study_pairs))) {

    unique_study_id <- distinct(select(sample_study_pairs, .data$study_id)) %>%
      mutate(molecular_profile_id =
               purrr::map(.data$study_id,
                           ~.lookup_profile_name(.x,
                                                 data_type = data_type,
                                                 base_url = base_url)))

    sample_study_pairs <- sample_study_pairs %>%
      left_join(unique_study_id)

  }


  # * MUTATION/CNA query ----------------------------------------------------------------------

  if(data_type %in% c("mutation", "cna")) {


    sample_study_pairs_nest <- sample_study_pairs %>%
      group_by(.data$study_id, .data$molecular_profile_id) %>%
      tidyr::nest(sample_id_nest = .data$sample_id) %>%
      mutate(url_path = paste0("molecular-profiles/",
                               .data$molecular_profile_id,
                               "/", url_data_type, "/fetch?")) %>%
      ungroup() %>%
      select(.data$url_path, .data$sample_id_nest)

    quer_res <- purrr::map2_dfr(
      sample_study_pairs_nest$url_path,
      sample_study_pairs_nest$sample_id_nest,

      function(x, y) {

        body_n = list(
          entrezGeneIds = resolved_genes,
          sampleIds = y$sample_id) %>%
          purrr::discard(is.null)

        res <- cbp_api(url_path = x,
                       method = "post",
                       body = body_n, base_url = base_url)


        # if embedded list in API response (e.g. namespace columns)
        if(length(res$content) > 0) {
          result <- purrr::map_dfr(res$content, ~purrr::list_flatten(.x))
        } else {
          result <- NULL
        }

      })

    df <- quer_res

  }

  # * FUSIONS/SEGMENTS query ----------------------------------------------------------------------

  # POST: /structural-variant/fetch ---
  # BODY: sampleMolecularIdentifiers: molecularProfileId, sampleId

  # Fusions endpoint works a little differently than Mut and CNA
  # Instead of passing a sample list, you pass individual sample IDs (retrieved using list)
  # Main Goal in this function is to return all results without specifying specific genes to query (as is needed in other endpoints).

  if(data_type %in% c("fusion", "segment")) {

    quer_res <- purrr::pmap_dfr(
      sample_study_pairs,

      function(sample_id, molecular_profile_id, study_id) {

        body_n <- switch(data_type,
                       fusion = {
                         list(
                           sampleMolecularIdentifiers = as.data.frame(list(
                             molecularProfileId = jsonlite::unbox(molecular_profile_id),
                             sampleId = sample_id
                           ))
                         )
                       },
                       segment = {
                         data.frame(
                           "sampleId" = sample_id,
                           "studyId" = study_id)
                       })

        res <- cbp_api(url_path = paste0(url_data_type, "/fetch?"),
                       method = "post",
                       body = body_n,
                       base_url = base_url)

        # if embedded list in API response (e.g. namespace columns)
        if(length(res$content) > 0) {
          result <- purrr::map_dfr(res$content, ~purrr::list_flatten(.x))
        } else {
          result <- NULL
        }

      })

    df_fus <- quer_res

    # Since you don't query by genes, filter genes at end so behaviour is consistent
    # with mutation/cna endpoints where you have to specify genes to query
    df <- switch(data_type,
      fusion = {
        if (nrow(.) > 0 & !is.null(resolved_genes)) {
          filter(df_fus, (.data$site1EntrezGeneId %in% resolved_genes) | (.data$site2EntrezGeneId %in% resolved_genes))
        } else {
          df_fus
        }
      },
      segment = df_fus
    )

  }


  # * Add Hugo Symbol & Return -----

  if(add_hugo) {

    # Fusions already has hugo by default from API
    df <- switch(data_type,
                 "fusion" = df,
                 "segment" = df,
                 "mutation" = if(nrow(df) > 0) .lookup_hugo(df, base_url = base_url),
                 "cna" = if(nrow(df) > 0) .lookup_hugo(df, base_url = base_url))

  }
      genes_msg <- c(panel, genes) %||%
        "All available genes"

      cli::cli_text("The following parameters were used in query:")
      cli::cli_dl(c("{.field Study ID}" = "{.val {unique(sample_study_pairs$study_id)}}",
                    "{.field Molecular Profile ID}" = "{.val {unique(sample_study_pairs$molecular_profile_id)}}",
                    "{.field Genes}" = "{.val {genes_msg}}"
      ))

      return(df)
  }




# Wrapper Functions ------------------------------------------------------------

#' Get Mutations By Sample ID
#'
#' @inheritParams .get_data_by_sample
#' @return A data frame of mutations (maf file format)
#' @export
#'
#'
#' @examples
#' \dontrun{
#' get_mutations_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
#' study_id = "acc_tcga",
#' base_url = "public")
#'}
get_mutations_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   molecular_profile_id = NULL,
                                   sample_study_pairs = NULL,
                                   genes = NULL,
                                   panel = NULL,
                                   add_hugo = TRUE,
                                   base_url = NULL) {


  .get_data_by_sample(sample_id = sample_id,
                    study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    sample_study_pairs = sample_study_pairs,
                    data_type = c("mutation"),
                    genes = genes,
                    panel = panel,
                    add_hugo = add_hugo,
                    base_url = base_url)


}




#' Get CNA By Sample ID
#'
#' @inheritParams .get_data_by_sample
#' @return A data frame of CNAs
#' @export
#'
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' get_cna_by_sample(sample_id = c("s_C_36924L_P001_d"),
#'                  study_id = "prad_msk_2019")
#' }
get_cna_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   molecular_profile_id = NULL,
                                   sample_study_pairs = NULL,
                                   genes = NULL,
                                   panel = NULL,
                                   add_hugo = TRUE,
                                   base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                     study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     sample_study_pairs = sample_study_pairs,
                     data_type = c("cna"),
                     genes = genes,
                     panel = panel,
                     add_hugo = add_hugo,
                     base_url = base_url)


}


#' Get Fusions By Sample ID
#'
#' @inheritParams .get_data_by_sample
#' @return A data frame of Fusions
#' @export
#' @aliases get_structural_variants_by_sample
#'
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#'
#' #' # These return the same results
#' get_fusions_by_sample(sample_id = c("s_C_CAUWT7_P001_d"),
#'                  study_id = "prad_msk_2019")
#' get_structural_variants_by_sample(sample_id = c("s_C_CAUWT7_P001_d"),
#'                  study_id = "prad_msk_2019")
#'                  }

get_fusions_by_sample <- function(sample_id = NULL,
                              study_id = NULL,
                              molecular_profile_id = NULL,
                              sample_study_pairs = NULL,
                              genes = NULL,
                              panel = NULL,
                              base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                     study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     sample_study_pairs = sample_study_pairs,
                     data_type = c("fusion"),
                     genes = genes,
                     panel = panel,
                     add_hugo = TRUE,
                     base_url = base_url)


}

#' @rdname get_fusions_by_sample
#' @export
get_structural_variants_by_sample <- get_fusions_by_sample



#' Get Copy Number Segmentation Data By Sample ID
#'
#' @inheritParams .get_data_by_sample
#' @return A dataframe of CNA segments
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#'
#' get_segments_by_sample(sample_id = c("s_C_CAUWT7_P001_d"),
#'                  study_id = "prad_msk_2019")
#'                  }


get_segments_by_sample <- function(sample_id = NULL,
                                  study_id = NULL,
                                  sample_study_pairs = NULL,
                                  base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                      study_id = study_id,
                      molecular_profile_id = NULL,
                      sample_study_pairs = sample_study_pairs,
                      data_type = c("segment"),
                      genes = NULL,
                      panel = NULL,
                      # this shouldn't matter for seg data
                      add_hugo = TRUE,
                      base_url = base_url)


}

#' Get All Genomic Information By Sample IDs
#'
#' @inheritParams .get_data_by_sample
#' @return_segments Default is `FALSE` where copy number segmentation data won't be returned in addition to the mutation, cna and structural variant data.
#' `TRUE` will return any available segmentation data with results.
#' @return A list of mutations, cna and structural variants (including fusions), if available. Will also return copy number segmentation data if `return_segments = TRUE`.
#' @export
#'
#'
#' @examples
#' \dontrun{
#' get_genetics_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
#'  study_id = "acc_tcga",
#'  return_segments = TRUE)
#' }
#
get_genetics_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   sample_study_pairs = NULL,
                                   genes = NULL,
                                   panel = NULL,
                                   add_hugo = TRUE,
                                   base_url = NULL,
                                   return_segments = FALSE) {

  data_types <- c("mutation", "cna", "structural_variant")

  if(return_segments) {
    data_types <- c(data_types, "segment")
  }

  safe_get_data <- purrr::safely(.get_data_by_sample, quiet = TRUE)

  res <- data_types %>%
    purrr::set_names() %>%
    purrr::map(., function(x) {
      safe_get_data(sample_id = sample_id,
                    study_id = study_id,
                    molecular_profile_id = NULL,
                    sample_study_pairs = sample_study_pairs,
                    genes = genes,
                    panel = panel,
                    add_hugo = add_hugo,
                    base_url = base_url,
                    data_type = x)
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



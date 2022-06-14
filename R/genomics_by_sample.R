
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
#' @param sample_study_pairs A dataframe with columns: `sample_id`, `study_id` and `molecular_profile_id` (optional).
#' This can be used in place of `sample_id`, `study_id`, `molecular_profile_id` arguments above if you
#' need to pull samples from several different studies at once. If passed this will take overwrite `sample_id`, `study_id`, `molecular_profile_id` if also passed.
#' @param data_type specify what type of data to return. Options are`mutations`, `cna`, `fusion`
#' @param genes A vector of Entrez ids or Hugo symbols. If Hugo symbols are supplied, they will be converted to entrez ids using the `get_entrez_id()` function.  If `panel` and `genes` are both NULL (default), it will return gene results for all available genomic data for that sample.
#' @param panel One or more panel IDs to query (e.g. 'IMPACT468'). If `panel`  and `genes` are both NULL (default), it will return gene results for all available genomic data for that sample.
#' @param add_hugo Logical indicating whether `HugoSymbol` should be added to your results. cBioPortal API does not return this by default (only EntrezId) but this function's default is `TRUE` and adds this by default.
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return A dataframe of mutations or CNAs
#' @export
#'
#' @keywords internal
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
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
#' \donttest{
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
                                data_type = c("mutation", "cna", "fusion"),

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

  # this has to go in query URL
  url_data_type <- switch(
    data_type,
    "mutation" = "mutations",
    "fusion" = "fusion",
    "cna" = "discrete-copy-number")

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
    resolved_molecular_profile_id <- molecular_profile_id %>%
      purrr::when(
        !is.null(.) ~ .,
        ~ .lookup_profile_name(data_type, study_id = resolved_study_id, base_url))



    # create lookup dataframe
    sample_study_pairs <- data.frame("sample_id" = sample_id,
                                     "study_id" = resolved_study_id,
                                     "molecular_profile_id" = resolved_molecular_profile_id)

  }


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

  if(
     !("data.frame" %in% class(sample_study_pairs)) |
     !("sample_id" %in% colnames(sample_study_pairs)) |
     !("study_id" %in% colnames(sample_study_pairs))
       #| "molecular_profile_id" %in% colnames(sample_study_pairs))
     ) {

    rlang::abort("`sample_study_pairs` must be a `data.frame` with the following columns: `sample_id` and `study_id`")
  }


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

  # ** I don't think we should allow molecular profile ID quiery without study IDs in sample_study_pairs because this could conflict with data_type- think about later
  # if(!("study_id" %in% colnames(sample_study_pairs))) {
  #
  #   unique_molec <- distinct(select(sample_study_pairs, .data$molecular_profile_id)) %>%
  #     mutate(study_id =
  #              purrr::map(.data$molecular_profile_id,
  #                         ~.lookup_study_name(.x, study_id = NULL,
  #                                             base_url = base_url)))
  #
  #   sample_study_pairs <- sample_study_pairs %>%
  #     left_join(unique_molec)
  #
  # }



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

    quer_res <- purrr::map2(
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


        bind_rows(res$content)

      })

    df <- quer_res %>% bind_rows()

  }

  # * FUSIONS query ----------------------------------------------------------------------

  # Fusions endpoint works a little differently than Mut and CNA
  # Instead of passing a sample list, you pass individual sample IDs (retrieved using list)
  # Main Goal in this function is to return all results without specifying specific genes to query (as is needed in other endpoints).

  if(data_type == "fusion") {

    quer_res <- purrr::map2(
      sample_study_pairs$sample_id,
      sample_study_pairs$molecular_profile_id,

      function(x, y) {


        body_n <- list(
          sampleMolecularIdentifiers = as.data.frame(list(
            molecularProfileId = jsonlite::unbox(y),
            sampleId = x
          ))
        )


        res <- cbp_api(url_path = "structural-variant/fetch?",
                       method = "post",
                       body = body_n,
                       base_url = base_url)

        res$content

      })

    df_fus <- quer_res %>% bind_rows()

    df <- df_fus %>%
      purrr::when(
        (nrow(.) > 0 & !is.null(resolved_genes)) ~ filter(., .data$site1EntrezGeneId %in% resolved_genes),
        TRUE ~ .)

    # Since you don't query by genes, filter genes at end so behaviour is consistent
    # with mutation/cna endpoints where you have to specify genes to query


  }


  # * Add Hugo Symbol & Return -----

  if(add_hugo) {

    # Fusions already has hugo by default from API
    df <- switch(data_type,
                 "fusion" = df,
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
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' get_mutations_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
#' study_id = "acc_tcga",
#' base_url = "public")
#'
get_mutations_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   molecular_profile_id = NULL,
                                   sample_study_pairs = NULL,
                                   genes = NULL,
                                   panel = NULL,
                                   add_hugo = NULL,
                                   base_url = NULL) {


  .get_data_by_sample(sample_id = sample_id,
                    study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    sample_study_pairs = sample_study_pairs,
                    data_type = c("mutation"),
                    genes = genes,
                    panel = panel,
                    base_url = base_url)


}




#' Get CNA By Sample ID
#'
#' @inheritParams .get_data_by_sample
#' @return A data frame of CNAs
#' @export
#'
#'
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' set_cbioportal_db("public")
#' get_cna_by_sample(sample_id = c("s_C_36924L_P001_d"),
#'                  study_id = "prad_msk_2019")
#'
get_cna_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   molecular_profile_id = NULL,
                                   sample_study_pairs = NULL,
                                   genes = NULL,
                                   panel = NULL,
                                   add_hugo = NULL,
                                   base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                     study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     sample_study_pairs = sample_study_pairs,
                     data_type = c("cna"),
                     genes = genes,
                     panel = panel,
                     base_url = base_url)


}


#' Get Fusions By Sample ID
#'
#' @inheritParams .get_data_by_sample
#' @return A data frame of Fusions
#' @export
#'
#'
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' set_cbioportal_db("public")
#' get_fusions_by_sample(sample_id = c("s_C_CAUWT7_P001_d"),
#'                  study_id = "prad_msk_2019")
#'
get_fusions_by_sample <- function(sample_id = NULL,
                              study_id = NULL,
                              molecular_profile_id = NULL,
                              sample_study_pairs = NULL,
                              genes = NULL,
                              panel = NULL,
                              add_hugo = NULL,
                              base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                     study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     sample_study_pairs = sample_study_pairs,
                     data_type = c("fusion"),
                     genes = genes,
                     panel = panel,
                     base_url = base_url)


}


#' Get All Genomic Information By Sample IDs
#'
#' @inheritParams .get_data_by_sample
#' @return A list of mutations, cna and fusions (if available)
#' @export
#'
#'
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' get_genetics_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
#'  study_id = "acc_tcga")
#'
#
get_genetics_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   sample_study_pairs = NULL,
                                   genes = NULL,
                                   panel = NULL,
                                   add_hugo = NULL,
                                   base_url = NULL) {

  safe_get_data <- purrr::safely(.get_data_by_sample, quiet = TRUE)

  res <-  c("mutation", "cna", "fusion") %>%
    purrr::set_names() %>%
    purrr::map(., function(x) {
      safe_get_data(sample_id = sample_id,
                    study_id = study_id,

                    molecular_profile_id = NULL,
                    sample_study_pairs = sample_study_pairs,
                    genes = genes,
                    panel = panel,
                    base_url = base_url,
                    data_type = x)
    })

  genetics <- purrr::compact(purrr::map(res, "result"))
  errors <- purrr::compact(purrr::map(res, "error"))

  switch(!purrr::is_empty(errors),
         purrr::imap(errors, ~cli_alert_warning(c("No {.val {.y}} data returned. Error:  ",
                                                  # why no red :(
                                                  cli::col_red('{.x$message}'))))
  )

  genetics
}



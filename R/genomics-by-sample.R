
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
#' @param genes A vector of entrez ids. If NULL, will return results for all
#' IMPACT genes (see `cbioportalR::impact_gene_info`)
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#'
#' @return A dataframe of mutations or CNAs
#' @export
#'
#' @keywords internal
#' @examples
#' set_cbioportal_db("public")
#' .get_data_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
#'  study_id = "acc_tcga", data_type = "mutation")
#'
#'
#' .get_data_by_sample(sample_id = c("DS-sig-010-P2"),
#'  molecular_profile_id = "blca_plasmacytoid_mskcc_2016_cna", data_type = "cna")
#'
#' .get_data_by_sample(sample_id = c("P-0002146-T01-IM3"),
#'  molecular_profile_id = "blca_plasmacytoid_mskcc_2016_mutations", data_type = "mutation")
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
#'
.get_data_by_sample <- function(sample_id = NULL,
                                study_id = NULL,
                                molecular_profile_id = NULL,
                                sample_study_pairs = NULL,
                               data_type = c("mutation", "cna", "fusion"),

                                genes = NULL,
                                base_url = NULL) {

  # Check Arguments ------------------------------------------------------------


  data_type <- match.arg(data_type)

  url_data_type <- case_when(
    data_type == "mutation" ~ "mutations",
    data_type == "fusion" ~ "fusion",
    data_type == "cna" ~ "discrete-copy-number")

  und_data_type <- case_when(
    url_data_type %in% c("mutations", "fusion") ~ url_data_type,
    url_data_type == "discrete-copy-number" ~ "cna") %>%
    paste0("_", .)

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
    cbioportalR::impact_gene_info$entrez_id %>% unlist()


  # Prep Data for Query -------------------------------------------------------

  if(!("data.frame" %in% class(sample_study_pairs))) {
    rlang::abort("`sample_study_pairs` must be a `data.frame` with the following columns: `sample_id`, `study_id`")
  }

  if(!("molecular_profile_id" %in% colnames(sample_study_pairs))) {
    sample_study_pairs <- sample_study_pairs %>%
      mutate(molecular_profile_id = paste0(.data$study_id, und_data_type))
  }


  # MUTATION/CNA query ----------------------------------------------------------------------

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
          sampleIds = y$sample_id)

        res <- cbp_api(url_path = x,
                       method = "post",
                       body = body_n, base_url = base_url)


        bind_rows(res$content)

      })

    df <- quer_res %>% bind_rows()

  }

  # FUSIONS query ----------------------------------------------------------------------

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
        nrow(.) > 0 ~ filter(., .data$site1EntrezGeneId %in% resolved_genes),
        TRUE ~ .)

    # Since you don't query by genes, filter genes at end so behaviour is consistent
    # with mutation/cna endpoints where you have to specify genes to query


  }

  genes_msg <- genes %||%
    "all IMPACT genes (see `gnomeR::impact_gene_info`)"

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
#' get_mutation_by_sample(sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
#' study_id = "acc_tcga",
#' base_url = "public")
#'
get_mutation_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   molecular_profile_id = NULL,
                                   sample_study_pairs = NULL,
                                   base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                    study_id = study_id,
                    molecular_profile_id = molecular_profile_id,
                    sample_study_pairs = sample_study_pairs,
                    data_type = c("mutation"),
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
#' set_cbioportal_db("public")
#' get_cna_by_sample(sample_id = c("s_C_36924L_P001_d"),
#'                  study_id = "prad_msk_2019")
#'
get_cna_by_sample <- function(sample_id = NULL,
                                   study_id = NULL,
                                   molecular_profile_id = NULL,
                                   sample_study_pairs = NULL,
                                   base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                     study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     sample_study_pairs = sample_study_pairs,
                     data_type = c("cna"),
                     base_url = base_url)


}


#' Get Fusions By Sample ID
#'
#' @inheritParams .get_data_by_sample
#' @return A data frame of Fusions
#' @export
#'
#'
#' @examples
#' set_cbioportal_db("public")
#' get_fusion_by_sample(sample_id = c("s_C_CAUWT7_P001_d"),
#'                  study_id = "prad_msk_2019")
#'
get_fusion_by_sample <- function(sample_id = NULL,
                              study_id = NULL,
                              molecular_profile_id = NULL,
                              sample_study_pairs = NULL,
                              base_url = NULL) {

  .get_data_by_sample(sample_id = sample_id,
                     study_id = study_id,
                     molecular_profile_id = molecular_profile_id,
                     sample_study_pairs = sample_study_pairs,
                     data_type = c("fusion"),
                     base_url = base_url)


}


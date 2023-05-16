

#' Get clinical data by attribute, study ID and sample ID
#'
#' @inheritParams .get_data_by_sample
#' @param clinical_attribute one or more clinical attributes for your study.
#' If none provided, will return all attributes available for studies
#' @return a dataframe of a specific clinical attribute
#' @export
#'
#' @examples
#' \dontrun{
#' get_clinical_by_sample(study_id = "acc_tcga", sample_id = "TCGA-OR-A5J2-01",
#'  clinical_attribute = "CANCER_TYPE", base_url = 'www.cbioportal.org/api')
#'
#' ex <- tibble::tribble(
#' ~sample_id, ~study_id,
#' "P-0001453-T01-IM3", "blca_nmibc_2017",
#' "P-0002166-T01-IM3", "blca_nmibc_2017",
#' "P-0003238-T01-IM5", "blca_nmibc_2017",
#' "P-0000004-T01-IM3", "msk_impact_2017",
#' "P-0000023-T01-IM3", "msk_impact_2017")
#'
#' x <- get_clinical_by_sample(sample_study_pairs = ex,
#'  clinical_attribute = NULL, base_url = 'www.cbioportal.org/api')
#'  }

get_clinical_by_sample <- function(study_id = NULL,
                                   sample_id = NULL,
                                   sample_study_pairs = NULL,
                                   clinical_attribute = NULL,
                                   base_url = NULL) {


  # Check Arguments ---------------------------------------------------------

  if(is.null(sample_id) & is.null(sample_study_pairs))  {
    cli::cli_abort("You must pass either {.code sample_id} or {.code sample_study_pairs}")
  }

  # * if no sample_study_pairs ----

  # `sample_study_pairs` gets priority. If that is NULL then consider other args
  if(is.null(sample_study_pairs)) {

    # Need final URL to guess default study
    resolved_url <- base_url %>%
      .resolve_url() %||%
      .get_cbioportal_url()  %||%
      abort(message = "must supply a url. Try `set_cbioportal_db()`")


    # get study ID
    resolved_study_id <- study_id %||%
      suppressMessages(.guess_study_id(study_id, resolved_url))



    # create lookup dataframe -
    sample_study_pairs <- data.frame("sample_id" = sample_id,
                                     "study_id" = resolved_study_id)

  }

  # * check sample_study_pairs-------
  sample_study_pairs <- .check_input_pair_df(input_df = sample_study_pairs)

  # Prep data frame for Query ------------------------------------------------
  sample_study_pairs_nest <- sample_study_pairs %>%
    group_by(.data$study_id) %>%
    tidyr::nest(sample_id_nest = "sample_id")

  # Query --------------------------------------------------------------------
  df <- purrr::map2_dfr(sample_study_pairs_nest$study_id, sample_study_pairs_nest$sample_id_nest,
                       ~.get_clinical_by_list_item (study_id = .x,
                                               sample_id = .y$sample_id,
                                               clinical_attribute = clinical_attribute,
                                               base_url = base_url))
  return(df)
}

#' Get clinical data by attribute, study ID and sample ID
#'
#' @inheritParams get_clinical_by_sample
#' @return a dataframe of a specific clinical attribute
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#' .get_clinical_by_list_item(study_id = "acc_tcga", sample_id = "TCGA-OR-A5J2-01",
#'  clinical_attribute = "CANCER_TYPE", base_url = 'www.cbioportal.org/api')
#'  }
#'
.get_clinical_by_list_item <- function(study_id = NULL,
                              sample_id = NULL,
                              clinical_attribute = NULL,
                              base_url = NULL) {

  # check arguments  -----------------------------------------------------------
  .check_for_study_id(study_id)

  resolved_clinical_attributes <- clinical_attribute %||%
    (available_clinical_attributes(study_id, base_url = base_url) %>%
    dplyr::pull(.data$clinicalAttributeId) %>%
    unique())

  if(is.null(clinical_attribute)) {
    cli_alert_warning("No {.var clinical_attribute} passed. Defaulting to returning
                      all clinical attributes in {.val {study_id}} study")
  }

  # query ---------------------------------------------------------------------
  url_path <- paste0("studies/",
                     study_id,
                     "/clinical-data/fetch?")


  body <- list(
    attributeIds = resolved_clinical_attributes,
    ids = sample_id
  )

  res <- cbp_api(url_path,
                 method = "post",
                 body = body,
                 base_url = base_url)


  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  return(df)
}



#' Get Gene Panel by study ID and sample ID
#'
#' @inheritParams get_clinical_by_sample
#' @return a dataframe of a specific clinical attribute
#' @export
#'
#' @examples
#' \dontrun{
#' get_panel_by_sample(study_id = "blca_plasmacytoid_mskcc_2016",
#'  sample_id = "DS-sig-010-P2",
#'  base_url = 'www.cbioportal.org/api')
#'  }
#'
get_panel_by_sample <- function(study_id = NULL,
                                   sample_id = NULL,
                                    sample_study_pairs = NULL,
                                   base_url = NULL) {

  res <- get_clinical_by_sample(study_id = study_id,
                         sample_id = sample_id,
                         sample_study_pairs = sample_study_pairs,
                         clinical_attribute = "GENE_PANEL",
                         base_url = base_url)


  if(nrow(res) < 1) {
    cli::cli_abort(c("No gene panel data found. Did you specify the correct {.code study_id} for your {.code sample_id}? ,",
                   "Is {.val GENE_PANEL} an available clinical attribute in your queried studies?"))
  }

  res <- transmute(res,  .data$sampleId, .data$studyId, genePanel = .data$value)
  return(res)

}


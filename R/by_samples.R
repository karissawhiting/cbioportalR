

#' Get clinical data by attribute, study ID and sample ID
#'
#' @param study_id study ID
#' @param sample_id a vector of sample IDs
#' @param clinical_attribute one or more clinical attributes for your study.
#' If none provided, will return all attributes available for
#' that study (`available_clinical_attributes(<study_id>)`)
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return a dataframe of a specific clinical attribute
#' @export
#'
#' @examples
#' get_clinical_by_sample(study_id = "acc_tcga", sample_id = "TCGA-OR-A5J2-01",
#'  clinical_attribute = "CANCER_TYPE", base_url = 'www.cbioportal.org/api')
#'
get_clinical_by_sample <- function(study_id = NULL,
                              sample_id = NULL,
                              clinical_attribute = NULL,
                              base_url = NULL) {

  # check arguments  -----------------------------------------------------------
  .check_for_sample_id(sample_id)
  .check_for_study_id(study_id)

  resolved_clinical_attributes <- clinical_attribute %||%
    (available_clinical_attributes(study_id, base_url = base_url) %>%
    pull(.data$clinicalAttributeId) %>%
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
  df
  return(df)
}



#' Get Gene Panel by study ID and sample ID
#'
#' @inheritParams get_clinical_by_sample
#' @return a dataframe of a specific clinical attribute
#' @export
#'
#' @examples
#' get_panel_by_sample(study_id = "blca_plasmacytoid_mskcc_2016",
#'  sample_id = "DS-sig-010-P2",
#'  base_url = 'www.cbioportal.org/api')
#'
get_panel_by_sample <- function(study_id = NULL,
                                   sample_id = NULL,
                                   base_url = NULL) {

  res <- get_clinical_by_sample(study_id = study_id,
                         sample_id = sample_id,
                         clinical_attribute = "GENE_PANEL",
                         base_url = base_url)

  res %>%
    purrr::when(nrow(.) < 1 ~ cli::cli_abort("No gene panel data found. Did you specify the correct {.code study_id} for your {.code sample_id}?
                                        Is {.val GENE_PANEL} an available clinical attribute in {.var {study_id}} "),

              TRUE ~ transmute(.,  .data$sampleId, .data$studyId, genePanel = .data$value))



}


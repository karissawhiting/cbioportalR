
#' Get all available clinical attribute IDs for a study
#'
#' @param study_id cbioportal study ID
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return a data frame of available clinical attributes for that study
#' @export
#'
#' @examples
#' available_clinical_attributes("acc_tcga", base_url = 'www.cbioportal.org/api')
#'
available_clinical_attributes <- function(study_id = NULL, base_url = NULL) {

  # checks ---------------------------------------------------------------------
   .check_for_study_id(study_id)

  # query ---------------------------------------------------------------------
  url_path <- paste0(
    "studies/", study_id,
    "/clinical-attributes?"
  )

  res <- cbp_api(url_path, base_url = base_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}


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



#' Get all available clinical data for a specified study
#'
#' @param study_id study ID
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return a dataframe of all available clinical attributes and their values
#' @export
#'
#' @examples
#' get_clinical_by_study(study_id = "acc_tcga", base_url = 'www.cbioportal.org/api')
#'
get_clinical_by_study <- function(study_id = NULL, base_url = NULL) {

  # checks ---------------------------------------------------------------------
    .check_for_study_id(study_id)

    # query ---------------------------------------------------------------------
  url_path <- paste0(
    "studies/", study_id,
    "/clinical-data?"
  )

  res <- cbp_api(url_path, base_url = base_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}


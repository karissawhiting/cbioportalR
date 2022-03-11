#' Get Metadata on All Available Studies in Database or a Specified Study
#'
#' @param study_id one or more study IDs (seee `available_studies()` to lookup IDs)
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of study metadata
#' @export
#'
#' @examples
#' set_cbioportal_db("public")
#' get_study_info("acc_tcga")

get_study_info <- function(study_id = NULL, base_url = NULL) {

  # checks ---------------------------------------------------------------------
  .check_for_study_id(study_id)

  url_path <- purrr::map(study_id, ~ paste0("studies/", .x))

  # ** This technically works already with multiple studies but limiting to one for consistency
  # If multiple studies passed (which they can't be as of now):
  # have to do as.data.frame() with map_df() to return one row
  # per study (map_df/bind_rows returns multiple rows per study
  # as.data.frame deals with nested lists better it seems. Is there a better way?
  res <- purrr::map_df(url_path, ~ cbp_api(.x, base_url = base_url)$content %>%
    as.data.frame(.))

  res
}


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

#' Get all available clinical data for a specified study
#'
#' @param study_id study ID
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @param clinical_attribute one or more clinical attributes for your study.
#' If none provided, will return all attributes available for
#' that study (`available_clinical_attributes(<study_id>)`)
#' @return a dataframe of all available clinical attributes and their values
#' @export
#'
#' @examples
#' get_clinical_by_study(study_id = "acc_tcga", base_url = 'www.cbioportal.org/api')
#' get_clinical_by_study(study_id = "acc_tcga",
#'  clinical_attribute = "CANCER_TYPE", base_url = 'www.cbioportal.org/api')
get_clinical_by_study <- function(study_id = NULL,
                                  clinical_attribute = NULL,
                                  base_url = NULL) {

  # checks ---------------------------------------------------------------------
  .check_for_study_id(study_id)

  url_path <- paste0(
    "studies/",
    study_id,
    "/clinical-data?")

  res <- cbp_api(url_path, base_url = base_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  # Filter selected clinical attributes if not NULL
  df_return <- df %>%
    purrr::when(
      !is.null(clinical_attribute) ~ filter(., clinicalAttributeId %in% clinical_attribute),
      ~{cli_alert_warning("No {.var clinical_attribute} passed. Defaulting to returning all clinical attributes in {.val {study_id}} study")
      .})

  return(df_return)
}

#' Get All Sample IDs in a Study
#'
#' @param study_id A character string indicating which study ID should be searched.
#' Only 1 study allowed. If NULL, we will guess a default study ID based on your database URL.
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of sample_ids in a given study
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' available_samples(study_id = "acc_tcga")
#' }
#'
available_samples<- function(study_id = NULL,
                                base_url = NULL) {

  .check_for_study_id(study_id)

  # query --------------------------------------------------------------------
  list_of_urls <- purrr::map(study_id,
                             ~paste0("studies/", .x,
                                     "/samples?"))


  api_results <- purrr::map_dfr(list_of_urls, function(x) {
    res <- cbp_api(url_path = x, base_url = base_url)
    res$content
    df <- bind_rows(res$content) %>%
      select(.data$patientId, .data$sampleId,
             .data$sampleType, .data$studyId)
    df
  })


  df <- api_results %>%
    dplyr::distinct()

  return(df)

}



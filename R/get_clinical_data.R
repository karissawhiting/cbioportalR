
#' Get all clinical attributes available for a study
#'
#' @param study_id cbioportal study ID
#' @param base_url The database URL to query
#'
#' @return a data frame of available clinical attributes for that study
#' @export
#'
#' @examples
#' get_clinical_attributes("acc_tcga", base_url = 'www.cbioportal.org/api')
#'
get_clinical_attributes <- function(study_id = NULL, base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }

    # query ---------------------------------------------------------------------
  url_path <- paste0(
    "studies/", study_id,
    "/clinical-attributes?"
  )

  print(url_path)

  res <- cbp_api(url_path, base_url = final_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}


#' Get clinical data by attribute study ID and sample ID
#'
#' @param study_id study ID
#' @param sample_id a single tumor sample ID
#' @param clinical_attribute a specific clinical attribute
#' @param base_url The database URL to query
#'
#' @return a dataframe of a specific clinical attribute
#' @export
#'
#' @examples
#' get_clinical_by_patient(study_id = "acc_tcga", sample_id = "TCGA-OR-A5J2-01",
#'  clinical_attribute = "CANCER_TYPE", base_url = 'www.cbioportal.org/api')
get_clinical_by_patient <- function(study_id = NULL,
                              sample_id = NULL,
                              clinical_attribute = "CANCER_TYPE",
                              base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }

    # query ---------------------------------------------------------------------
  url_path <- paste0(
    "studies/", study_id,
    "/samples/",
    sample_id,
    "/clinical-data?attributeId=",
    clinical_attribute
  )

  print(url_path)
  res <- cbp_api(url_path, base_url = final_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}



#' Get all available clinical data for a specified study
#'
#' @param study_id study ID
#' @param base_url The database URL to query
#'
#' @return a dataframe of all available clinical attributes and their values
#' @export
#'
#' @examples
#' get_clinical_by_study(study_id = "acc_tcga", base_url = 'www.cbioportal.org/api')
#'
get_clinical_by_study <- function(study_id = NULL, base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }

    # query ---------------------------------------------------------------------
  url_path <- paste0(
    "studies/", study_id,
    "/clinical-data?"
  )

  print(url_path)
  res <- cbp_api(url_path, base_url = final_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}


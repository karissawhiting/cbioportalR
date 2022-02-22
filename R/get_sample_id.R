#' Get sample IDs for a given set of patient IDs
#'
#' @param patient_ids A character string of sample IDs to query
#'
#' @return A dataframe of patient IDs and corresponding sample IDs. If patient
#' has multiple samples, there will be multiple rows per patient.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_cbioportal_db("public")
#' get_sample_ids(patient_ids = c("P-0000034", "P-0000036"))
#' }
#'
get_sample_ids <- function(patient_ids = NULL, study_id = NULL,
                           base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  if(is.null(patient_ids)) stop("Must specify at least one `patient_id`")

  # if no study ID and MSK
  if (is.null(study_id) & stringr::str_detect(final_url, "mskcc.org")) {
    study_id = "mskimpact"
  }

  if (is.null(study_id) & final_url == "www.cbioportal.org/api") {
    study_id = "msk_impact_2017"
    warning("If you are an MSK researcher, for most up to date IMPACT data you should connect to MSK cbioportal. This function is using limited public IMPACT data (study_id = 'msk_impact_2017')")
    } else {

      stop("you need to specify a study ID to look for samples within")
    }


  # url_path <- paste0("studies/", study_id, "/patients/",
  #   patient_ids, "/samples?")

  list_of_urls <- purrr::map(patient_ids, ~paste0("studies/",
                                                  study_id, "/patients/",
    .x, "/samples?"))


  api_results <- purrr::map_df(list_of_urls,
    ~cbp_api(url_path = .x)$content) %>%
    select(
    patient_id = .data$patientId,
    sample_id = .data$sampleId)

  df <- api_results %>%
    dplyr::distinct()

  return(df)

}


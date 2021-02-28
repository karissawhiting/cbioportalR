#' Get segmentation data
#'
#' @param sample_ids A character vector of sample ids.
#' @param study_id A character vector of lenght 1 indicating study_id.
#' Default is "all_tcga_studies" which queries all TCGA cancer studies, unless you
#' are connected to MSKCC host in which case default is "mskimpact".
#'
#' @return Returns a dataframe of segmentation data
#' @export
#'
#' @examples
#' get_cbioportal_db('public')
#' get_segments(sample_ids = "TCGA-OR-A5J2-01", study_id = "acc_tcga")
#'
get_segments <- function(sample_ids = NULL,
  study_id = "all_tcga_studies") {


  # study ID dictates url and body of call -----------------------------------
  study <- case_when(
    (study_id == "mskimpact" | stringr::str_detect(base_url, "mskcc.org")) ~ "mskimpact",
    (study_id == "all_tcga_studies" | stringr::str_detect(base_url, "www.cbioportal.org/api")) ~ "all_tcga_studies",
    TRUE ~ "other_study")


  if(study == "all_tcga_studies") {

    # gets necessary sites for your sample IDs
     samples_and_sites <- cbioportalR::tcga_samples %>%
      dplyr::filter(.data$patient_id %in% sample_ids) %>%
      dplyr::transmute(
        sample_ids = .data$patient_id,
        cancer_code = tolower(.data$Cancer_Code)) %>%
      distinct()

    list_of_bodies <- purrr::map2(
      samples_and_sites$sample_ids,
      paste0(samples_and_sites$cancer_code, "_tcga"),
      ~list(sampleId = .x,
      studyId = .y))

  } else {

    list_of_bodies <- purrr::map(sample_ids,
    ~list(sampleId = .x,
      studyId = study))
  }

  url_path <- "copy-number-segments/fetch?"

  api_results <- purrr::map_df(list_of_bodies,
    ~cbp_api(url_path = url_path,
      body = .x,
      method = "post",
      extra_box = TRUE)$content)

  return(api_results)


}



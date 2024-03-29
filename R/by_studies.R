#' Get Metadata on All Available Studies in Database or a Specified Study
#'
#' @param study_id one or more study IDs (see `available_studies()` to lookup IDs)
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of study metadata
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' get_study_info("acc_tcga")
#' }

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
#' \dontrun{
#' available_clinical_attributes("acc_tcga", base_url = 'www.cbioportal.org/api')
#' }
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
#' Returns all sample-level and patient-level clinical data for a given study
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
#' \dontrun{
#' get_clinical_by_study(study_id = "acc_tcga",
#'  clinical_attribute = "CANCER_TYPE", base_url = 'www.cbioportal.org/api')
#'
#' get_clinical_by_study(study_id = "acc_tcga", base_url = 'www.cbioportal.org/api')
#' }
#'
get_clinical_by_study <- function(study_id = NULL,
                                  clinical_attribute = NULL,
                                  base_url = NULL) {

  # checks ---------------------------------------------------------------------
  .check_for_study_id(study_id)

  # get sample level --------------------------------------------------------
  url_path <- paste0(
    "studies/",
    study_id,
    "/clinical-data?")

  res <- cbp_api(url_path, base_url = base_url)
  df_samp <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  # filter selected clinical attributes if not NULL
  df_samp <-
    if(nrow(df_samp) > 0 & is_not_null(clinical_attribute)) {
    filter(df_samp, .data$clinicalAttributeId %in% clinical_attribute)

  } else {
    cli_alert_warning("Sample Level Clinical Data: No {.var clinical_attribute} passed. Defaulting to returning all clinical attributes in {.val {study_id}} study")
    df_samp

  }

  df_samp <- df_samp %>%
    mutate(dataLevel = "SAMPLE")


  # get patient level ---------------------------------------------------------

  url_path <- paste0(
    "studies/",
    study_id,
    "/clinical-data?clinicalDataType=PATIENT")

  res <- cbp_api(url_path, base_url = base_url)
  df_pat <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  # filter selected clinical attributes if not NULL
  df_pat <-
    if(nrow(df_pat) > 0 & !is.null(clinical_attribute)) {
      filter(df_pat, .data$clinicalAttributeId %in% clinical_attribute)
    } else {
      cli_alert_warning("Patient Level Clinical Data: No {.var clinical_attribute} passed. Defaulting to returning all clinical attributes in {.val {study_id}} study")
      df_pat
    }

  df_pat <- df_pat %>%
    mutate(dataLevel = "PATIENT",
           sampleId = NA_character_)


  # put together  ---------------------------------------------------------

  if(nrow(df_pat) > 0 & nrow(df_samp > 0)) {
    common_names <- intersect(names(df_pat), names(df_samp))

    df_all <- df_pat %>% select(all_of(common_names)) %>%
      bind_rows(select(df_samp, all_of(common_names)))

    return(df_all)
  }

  # return whichever is not 0 rows
  ind_results <- list(df_pat, df_samp)
  index <- which(purrr::map(ind_results, ~nrow(.x)) > 0)

  return(ind_results[[index]])


}

#' Get All Sample IDs in a Study
#'
#' Pulls all available sample IDs for a given study ID or sample list ID.
#' Either a study ID or sample list ID must be passed. If both `sample_list` and `study_id` are not `NULL`,
#' `sample_list` ID will be searched and `study_id` will be ignored.
#'
#' @param study_id A character string indicating which study ID should be searched.
#' Only 1 study ID allowed.
#' @param sample_list_id A character string indicating which sample list ID should be searched.
#' Only 1 sample list ID allowed.
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
#' available_samples(sample_list_id = "acc_tcga_cna")
#' }
#'
available_samples <- function(study_id = NULL, sample_list_id = NULL,
                              base_url = NULL) {

  sample_list <- sample_list_id %||%
    .check_for_study_id(study_id)

  # query by sample list -------------------------------------------------------
  if (!is.null(sample_list)) {
    list_of_urls <- purrr::map(
      sample_list,
      ~ paste0(
        "sample-lists/", .x,
        "/sample-ids"
      )
    )

    api_results <- purrr::map_df(list_of_urls, function(x) {
      res <- cbp_api(url_path = x, base_url = base_url)
      res$content
      df <- unlist(res$content) %>%
        tibble::enframe(name = NULL, value = "sampleId")

      df
    })

    api_results <- api_results %>%
      mutate(sampleListId = sample_list)
  } else {

    # query by study ID ----------------------------------------------------------
    list_of_urls <- purrr::map(
      study_id,
      ~ paste0(
        "studies/", .x,
        "/samples?"
      )
    )


    api_results <- purrr::map_dfr(list_of_urls, function(x) {
      res <- cbp_api(url_path = x, base_url = base_url)
      res$content
      df <- bind_rows(res$content) %>%
        select(
          "patientId", "sampleId",
          "sampleType", "studyId"
        )
      df
    })
  }

  df <- api_results %>%
    dplyr::distinct()

  return(df)
}

#' Get All Patient IDs in a Study
#'
#' @inheritParams available_samples
#' @return A dataframe of patient_ids in a given study
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' available_samples(study_id = "acc_tcga")
#' }
#'
available_patients <- function(study_id = NULL,
                              base_url = NULL) {

  .check_for_study_id(study_id)

  # query --------------------------------------------------------------------
  list_of_urls <- purrr::map(study_id,
                             ~paste0("studies/", .x,
                                     "/patients?"))


  api_results <- purrr::map_dfr(list_of_urls, function(x) {
    res <- cbp_api(url_path = x, base_url = base_url)
    res$content
    df <- bind_rows(res$content) %>%
      select("patientId", "studyId")
    df
  })


  df <- api_results %>%
    dplyr::distinct()

  return(df)

}

#' Get All Sample Lists Available For a Study
#'
#' @inheritParams available_samples
#' @return A dataframe of patient_ids in a given study
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' available_sample_lists(study_id = "acc_tcga")
#' }
#'
available_sample_lists <- function(study_id = NULL,
                               base_url = NULL) {

  .check_for_study_id(study_id)

  # query --------------------------------------------------------------------
  list_of_urls <- purrr::map(study_id,
                             ~paste0("studies/", .x,
                                     "/sample-lists?"))


  api_results <- purrr::map_dfr(list_of_urls, function(x) {

    res <- cbp_api(url_path = x, base_url = base_url)
    res$content
    df <- bind_rows(res$content)

    df
  })


  df <- api_results %>%
    dplyr::distinct()

  return(df)

}



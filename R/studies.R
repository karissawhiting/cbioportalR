#' Get Metadata on All Available Studies in a Database
#'
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of available studies and their metadata
#' @export
#'
#' @example
#' set_cbioportal_db("public")
#' available_studies()

available_studies <- function(base_url =  NULL) {

  # query ---------------------------------------------------------------------

  url_path <- paste0("studies/")
  res <- cbp_api(url_path, base_url = base_url)$content %>%
    dplyr::bind_rows(.) %>%
    select(.data$studyId, everything())

  return(res)
}

#' Get Metadata on All Available Studies in Database or a Specified Study
#'
#' @param study_id one or more study IDs (seee `available_studies()` to lookup IDs)
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of study metadata
#' @export
#'
#' @example
#' set_cbioportal_db("public")
#' get_study_info("acc_tcga")

get_study_info <- function(study_id = NULL, base_url = NULL) {

  # checks ---------------------------------------------------------------------
  .check_for_study_id(study_id)

  url_path <- purrr::map(study_id, ~ paste0("studies/", .x))

  # ** Maybe improve?
  # have to do as.data.frame() with map_df() to return one row
  # per study (map_df/bind_rows returns multiple rows per study
  # as.data.frame deals with nested lists better it seems. Is there a better way?
  res <- purrr::map_df(url_path, ~ cbp_api(.x, base_url = base_url)$content %>%
    as.data.frame(.))

  res
}



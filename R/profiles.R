
#' Get Available Genomic Profiles For a Study or Database
#'
#' @param study_id A character vector of length 1 indicating study_id.
#' See `get_studies()` to see all available studies for your URL. If `NULL`, it will
#' return all profiles for your current database url
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of available genetic profiles and their profile ids
#' @export
#'
#' @example
#' set_cbioportal_db("public")
#' available_profiles()

available_profiles <- function(study_id = NULL,
                               base_url =  NULL) {

  # checks ---------------------------------------------------------------------
  if(is.null(study_id)) {
    url_path <- "molecular-profiles?"

    # Need this for message to  user- usually this is handled in cbi_api()
    resolved_url <- base_url %>%
      .resolve_url() %||%
      .get_cbioportal_url()  %||%
      abort(message = "must supply a url. Try `set_cbioportal_db()`")


    cli::cli_alert_info("No {.code study_id} provided. Returning all available genomic profiles for {.url {resolved_url}}")

  } else {

  url_path <- paste0(
    "studies/", study_id,
    "/molecular-profiles?"
  )
  }

  # query ---------------------------------------------------------------------


  res <- cbp_api(url_path, base_url = base_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}



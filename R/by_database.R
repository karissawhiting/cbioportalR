#' Get Metadata on All Available Studies in a Database
#'
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of available studies and their metadata
#' @export
#'
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
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


#' Get Available Gene Panels For a Database
#'
#' @return a dataframe of metadata regarding each available panel
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @export
#'
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' set_cbioportal_db("public")
#' available_gene_panels()
#'
available_gene_panels <- function(base_url = NULL) {

  # query ---------------------------------------------------------------------
  url_path <- "gene-panels??"

  res <- cbp_api(url_path, base_url = base_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  return(df)
}


#' Get A List of Genes for a Specified Database
#'
#' @return A dataframe of gene ids, hugo symbols, and gene types
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @export
#' @examples
#' \donttest{
#' get_genes(base_url = 'www.cbioportal.org/api')
#' }
get_genes <- function(base_url  =  NULL) {

  url_path <- paste0("genes")

  res <- cbp_api(url_path = url_path, base_url = base_url)
  purrr::map_df(res$content, ~tibble::as_tibble(.x))

}


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
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' set_cbioportal_db("public")
#' available_profiles()
#' available_profiles(study_id = "acc_tcga")

available_profiles <- function(study_id = NULL,
                               base_url =  NULL) {

  # checks ---------------------------------------------------------------------

  # ** Not using `.check_for_study_id()` here because we allow no study ID to be passed,
  # but still don't allow study_id > 1. Maybe generalized that check function to
  # Make each check optional?
  if(length(study_id) > 1) {
    cli::cli_abort(c("{.code length(study_id)} must be 1. You can only pass one {.code study_id} at a time"))}

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


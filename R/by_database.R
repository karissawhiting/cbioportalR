.flatten_list_field <- function(entries, field_name) {
  if (length(entries) == 0) return(stats::setNames(list(NA_character_), field_name))
  sub_keys  <- names(entries[[1]])
  col_names <- stats::setNames(sub_keys, paste0(field_name, ".", sub_keys))
  purrr::map(col_names, function(key) {
    values <- vapply(entries, function(e) {
      val <- e[[key]]
      if (is.list(val)) jsonlite::toJSON(val, auto_unbox = TRUE) else as.character(val)
    }, character(1))
    dplyr::na_if(paste(values, collapse = ","), "")
  })
}

#' Get Metadata on All Available Studies in a Database
#'
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of available studies and their metadata
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' available_studies()
#' }

available_studies <- function(base_url =  NULL) {

  # query ---------------------------------------------------------------------

  url_path <- "studies?projection=SUMMARY"
  raw_studies <- cbp_api(url_path, base_url = base_url)$content

  res <- purrr::map(raw_studies, function(study) {
      is_list_field <- purrr::map_lgl(study, is.list)
      list_fields   <- study[is_list_field]
      study[is_list_field] <- NULL

      flat_cols <- purrr::imap(list_fields, .flatten_list_field)

      tibble::as_tibble(c(study, do.call(c, flat_cols)))
  })
  
  res <- res %>%
    dplyr::bind_rows() %>%
    dplyr::select("studyId", everything())

  # remove extra resource counts fields to create cleaner output
  rc_cols <- names(res)[stringr::str_detect(names(res), "resourceCounts")]
  rc_cols_rm <- setdiff(rc_cols,
    c("resourceCounts.resourceCounts.resourceId", 
      "resourceCounts.resourceCounts.displayName", 
      "resourceCounts.resourceCounts.description"))
  
  res <- res %>%
    select(-all_of(rc_cols_rm))

  return(res)
}


#' Get Available Gene Panels For a Database
#'
#' @return a dataframe of metadata regarding each available panel
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @export
#'
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' available_gene_panels()
#' }
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
#' \dontrun{
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
#' @examples
#' \dontrun{
#' set_cbioportal_db("public")
#' available_profiles()
#' available_profiles(study_id = "acc_tcga")
#' }

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


#' Lookup all available information on what studies a sample ID or patient ID belongs to
#'
#' This is a general look up function that can take a study ID or patient ID and return what samples
#' exist across entire cBioPortal website (depends on your base URL) that match that ID.
#' It will return which studies include that sample or patient.
#'
#' This can also be useful to see all samples a particular patient has available across all studies on
#' cBioPortal (see also `get_samples_by_patient()`).
#'
#'
#' @param lookup_id a sample ID or patient ID
#' @param base_url The database URL to query
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of general info for sample of patient IDs given
#' @export
#'
#' @examples
#' \dontrun{
#' lookup_id <- c("P-0001453-T01-IM3", "P-0000004-T01-IM3", "TCGA-OR-A5JA")
#' x <- lookup_id(lookup_id = lookup_id,  base_url = 'www.cbioportal.org/api')
#' x
#'  }

lookup_id <- function(lookup_id = NULL,
                      base_url = NULL) {

  # * checks ---------------------------------------------------------

  lookup_id %||% cli::cli_abort("You must pass at least one {.code lookup_id}")


  # get sample level --------------------------------------------------------
  url_path <- paste0(
    "samples?keyword=",
    lookup_id)

  res <- purrr::map_df(url_path, ~cbp_api(.x, base_url = base_url)$content)

  all_returned_ids <- unique(c(res$patientId, res$sampleId))
  not_in_results <- setdiff(lookup_id, all_returned_ids)

  if(length(not_in_results) > 0) {
    cli::cli_warn("The following patient or sample IDs were not found: {.var {not_in_results}}.
                  Please make sure you are connected to the correct cBioPortal database and that samples exist.")
  }
  return(res)
}



#' General on what studies a sample ID or patient ID belongs to
#'
#' This is a general look up function that can take a study ID or patient ID and return what samples
#' exist across entire cBioPortal website (depends on your base URL) that match that ID.
#' It will return which studies include that sample or patient.
#'
#' This can also be useful to see all samples a particular patient has available across all studies on
#' cBioPortal.
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

  return(res)
}

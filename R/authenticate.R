# ------------------------------------------------------------------------------
# set environment in which to store URL variable that persists
cbioportal_env <- rlang::new_environment()

# ------------------------------------------------------------------------------
#' Connect to cBioPortal DB
#'
#' This function sets a base cBioPortal url
#' @param db The database URL to use as base URL for calls, or "public" for https://www.cbioportal.org/
#' @export
#' @examples
#' \dontrun{
#' set_cbioportal_db(db = "public")
#' }
#'
set_cbioportal_db <- function(db = NULL) {

  # if no db passed, check environment db
  db_set <- .resolve_url(db)
  db_set <- db_set %||% Sys.getenv("CBIOPORTAL_URL")

  if (identical(db_set, "")) {
    rlang::abort("No `db` specified and no CBIOPORTAL_URL in `.Renviron`.
      Try specifying `db` or use `usethis::edit_r_environ()` to add a database URL.
      You can also specify `db = 'public'` to connect to https://www.cbioportal.org/")

  }


  assign("portal_url",
         value = db_set,
         envir = cbioportal_env)

  cli_alert_success(" {.field {'portal_url'}} for this R session is {.val {.get_cbioportal_url()}} ")
}

# ------------------------------------------------------------------------------
#' Get cBioPortal Access Token
#'
#' Convenience function that retrieves cBioPortal token System Environment variable "CBIOPORTAL_TOKEN"
#' @export
#' @examples
#' \dontrun{
#' get_cbioportal_token()
#' }
#'
#'
get_cbioportal_token <- function() {
  x <- Sys.getenv("CBIOPORTAL_TOKEN")
  if (identical(x, "")) {
    rlang::warn("No CBIOPORTAL_TOKEN in `.Renviron`. Try `usethis::edit_r_environ()` to add a token")
  }
  x
}


# ------------------------------------------------------------------------------
#' Get Database URL to Use in Functions
#'
#' Pulls the set URL from the internal package environment
#'
#' @return saved url in the `cbioportal_env` environment
#' @export
#' @keywords internal
#' @noRd
#' @examples
#' .get_cbioportal_url()
#'
.get_cbioportal_url <- function() {
  get0("portal_url",
       envir = cbioportal_env,
      ifnotfound = "No Database URL Found. Have you set your database URL? Try `set_cbioportal_db(<your_url>)`")
}


# ------------------------------------------------------------------------------
#' Process and make a best guess of URL string passed to authentication functions
#'
#' @param raw_url The URL passed to a function by a user
#'
#' @return A string with a final URL to be used
#' @export
#' @keywords internal
#' @noRd
#'
#'
.resolve_url <- function(raw_url = NULL) {

  if(!is.null(raw_url)) {

    # could httr::parse_url() here maybe instead
    url <- stringr::str_remove(raw_url, "https://")

    url_resolved <- dplyr::case_when(
      tolower(url) %in% c("msk", "mskcc") ~ "cbioportal.mskcc.org",
      url == "public" ~ "www.cbioportal.org",
      TRUE ~ url
    )

    if (!stringr::str_detect(url_resolved, c("api"))) {
      url_resolved <- paste0(url_resolved, "/api")
    }

    return(url_resolved)
  }


  return(NULL)
}



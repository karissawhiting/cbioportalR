
# set environment in which to store URL variable that persists
cbioportal_env <- rlang::new_environment()


#' Get cBioPortal Access Token
#'
#' This function retrieves cBioPortal token System Environment variable "CBIOPORTAL_TOKEN"
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

# Default Base URL  -----------------------------------------------------

#' Figure out which base URL to use
#'
#' @param raw_url The URL passed to a funtion by a user
#'
#' @return A string with a final URL chosen
#' @keywords internal
#' @noRd
#'
#'
.resolve_url <- function(raw_url = NULL) {

  if(!is.null(raw_url)) {

    # could httr::parse_url() here maybe instead
    url <- stringr::str_remove(raw_url, "https://")

    url_resolved <- dplyr::case_when(
      url %in% c("MSK", "msk") ~ "cbioportal.mskcc.org",
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

  ui_done("{ui_field('base_url')} for this R session is {ui_value(get_cbioportal_url())}")

}


#' Check for cBioPortal DB
#'
#' This function  checks for and retrieves cBioPortal URL System Environment variable "CBIOPORTAL_URL"
#' @return a saved data base URLir NULL if none exists
#' @export
#' @examples
#' check_for_saved_db()
#'
check_for_saved_db <- function() {
  x <- Sys.getenv("CBIOPORTAL_URL")
  if (identical(x, "")) {
    rlang::warn("No CBIOPORTAL_URL in `.Renviron`. Try `usethis::edit_r_environ()` to add a database URL")
    return(NULL)
  } else {
    return(x)
  }
}



#' Get Database URL to Use in Functions
#'
#' Pulls the set URL from the internal package environement
#'
#' @return saved url in the `cbioportal_env` environment
#' @export
#' @examples
#' get_cbioportal_url
#'
get_cbioportal_url <- function() {
  get0("portal_url",
       envir = cbioportal_env,
      ifnotfound = "No Database URL Found. Have you set your database URL? Try `set_cbioportal_db(<your_url>)`")
  }



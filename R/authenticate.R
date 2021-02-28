#' Get cBioPortal Access Token
#'
#' This function retrieves cBioPortal token System Environment variable "CBIOPORTAL_TOKEN"
#' @export
#' @examples
#' \dontrun{
#' get_cbioportal_token()
#' }
#'
get_cbioportal_token <- function() {
  x <- Sys.getenv("CBIOPORTAL_TOKEN")
  if (identical(x, "")) {
    rlang::warn("No CBIOPORTAL_TOKEN in `.Renviron`. Try `usethis::edit_r_environ()` to add a token")
  }
  x
}

#' Connect to cBioPortal DB
#'
#' This function sets a base cBioPortal url
#' @param db The database URL to use as base URL for calls, or "public" for https://www.cbioportal.org/
#' @export
#' @examples
#' \dontrun{
#' get_cbioportal_db(db = "public")
#' }
#'
get_cbioportal_db <- function(db = NULL) {
  if (is.null(db)) {
    x <- Sys.getenv("CBIOPORTAL_URL")
    if (identical(x, "")) {
      rlang::abort("No `db` specified and no CBIOPORTAL_URL in `.Renviron`.
      Try specifying `db` or use `usethis::edit_r_environ()` to add a database URL.
      You can also specify `db = 'public'` to connect to https://www.cbioportal.org/")
    }
  } else {
    db <- db

    db_url <- dplyr::case_when(
      db == "public" ~ "www.cbioportal.org/api",
      TRUE ~ db
    )
  }

  assign("base_url", db_url, envir = .GlobalEnv)

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



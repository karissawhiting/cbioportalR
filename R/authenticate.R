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

  # if no db passed, check environment db
  db_set <- db %||% Sys.getenv("CBIOPORTAL_URL")

  if (identical(db_set, "")) {
    rlang::abort("No `db` specified and no CBIOPORTAL_URL in `.Renviron`.
      Try specifying `db` or use `usethis::edit_r_environ()` to add a database URL.
      You can also specify `db = 'public'` to connect to https://www.cbioportal.org/")
  }


  if(!is.null(db)) {

    # could httr::parse_url() here maybe instead
    db <- stringr::str_remove(db, "https://")

    db_set <- dplyr::case_when(
      db %in% c("MSK", "msk") ~ "cbioportal.mskcc.org",
      db == "public" ~ "www.cbioportal.org",
      TRUE ~ db
    )

    if (!stringr::str_detect(db_set, c("api"))) {
      db_set <- paste0(db_set, "/api")
    }

  }

#  assign("base_url", db_url, envir = .GlobalEnv)
  options(session_base_url = db_set)

  ui_done("{ui_field('base_url')} for this R session is {ui_value(getOption('session_base_url'))}")

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



# ------------------------------------------------------------------------------
# set environment in which to store URL variable that persists
cbioportal_env <- rlang::new_environment()

# ------------------------------------------------------------------------------
#' Connect to cBioPortal DB
#'
#' This function sets a base cBioPortal url
#' @param db The database URL to use as base URL for calls, or "public" for https://www.cbioportal.org/
#' @export
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' set_cbioportal_db(db = "public")
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

  test_cbioportal_db()

  cli_alert_success("{.field {'base_url'}} for this R session is now set to {.val {.get_cbioportal_url()}} ")
}

# ------------------------------------------------------------------------------
#' Get cBioPortal Access Token
#'
#' Convenience function that retrieves cBioPortal token System Environment variable "CBIOPORTAL_TOKEN"
#' @export
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' get_cbioportal_token()
#'
get_cbioportal_token <- function() {
  x <- Sys.getenv("CBIOPORTAL_TOKEN")

  if (identical(x, "")) {
    rlang::warn("No CBIOPORTAL_TOKEN in `.Renviron`. Try `usethis::edit_r_environ()` to add a token")
  }
  x
}

# ------------------------------------------------------------------------------
#' Test the Database Connection Anytime During your R Session
#'
#' Helps troubleshoot API issues during an R session
#' @export
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' set_cbioportal_db("public")
#' test_cbioportal_db()
#'
#'
test_cbioportal_db <- function() {

  db_to_test <- .get_cbioportal_url()

  y <- tryCatch({
    cbp_api(url_path = "/cancer-types?", base_url = db_to_test) },
    error = function(e) {
      cli::cli_abort("Not able to connect to {.val {db_to_test}}")})


  if (y$response$status_code == 200) {
    db_return <- stringr::str_remove(db_to_test, "/api")

  cli_alert_success("You are successfully connected!")
  }
}


# ------------------------------------------------------------------------------
#' Get Database URL to Use in Functions
#'
#' Pulls the set URL from the internal package environment
#'
#' @return saved url in the `cbioportal_env` environment
#' @export
#' @author Karissa Whiting, Daniel D. Sjoberg
#' @keywords internal
#' @noRd
#' @examplesIf !httr::http_error("www.cbioportal.org/api")
#' .get_cbioportal_url()
#'
.get_cbioportal_url <- function() {

  url <- tryCatch({
    get("portal_url",
        envir = cbioportal_env)},
    error = function(e) {
      cli::cli_abort("No Database URL Found. Have you set your database? Try {.code set_cbioportal_db(<your_url>)} ?")})

  url
}


# ------------------------------------------------------------------------------
#' Process and make a best guess of URL string passed to authentication functions
#'
#' @param raw_url The URL passed to a function by a user
#'
#' @return A string with a final URL to be used
#' @export
#' @author Karissa Whiting, Daniel D. Sjoberg
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



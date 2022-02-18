
# Default Base URL  -----------------------------------------------------

#' Figure out which base URL to use
#'
#' @param base_url_passed The URL passed to a funtion by a user
#'
#' @return A string with a final URL chosen
#' @keywords internal
#' @noRd
#'
.determine_base_url <- function(base_url_passed = NULL, ...) {



  # get correct base_url -------------
  global_base_url <- switch(
    exists("base_url", envir = .GlobalEnv) +1,
    NULL,
    base_url)

  saved_db <-  suppressWarnings(check_for_saved_db())
  final_base_url <- base_url_passed %||% global_base_url %||% saved_db

  if(is.null(final_base_url)) rlang::abort("Please specify a database e.g. `get_cbioportal_db('public')`")

  final_base_url

}




#' Core function to retrieve data from cBioPortal API.
#'
#'
#' @param url_path The url path for API call
#' @param method Which API method to use. Default is GET.
#' @param token Authentication token, if needed. Default is `get_cbioportal_token()`
#' @param body Arguments passed to API call (e.g. sample ID or gene IDs)
#' @param extra_box Some functions require an additional list() wrapping around body idk why
#'
#' @return A parsed API response
#' @export
#' @import httr
#'
#' @examples
#' get_cbioportal_db("public")
#' cbp_api(url_path = "genes/TP53")
#'
cbp_api <- function(url_path,
                    method = c("get", "post"),
                    token = get_cbioportal_token(),
                    body = NULL,
  extra_box = FALSE) {

  method = match.arg(method)

  # get base URL for DB
  if(exists("base_url", envir = .GlobalEnv)) {
    hostname = base_url
  } else {
    saved_db <-  suppressWarnings(check_for_saved_db())
    if(is.null(saved_db)) rlang::abort("Please specify a database e.g. `get_cbioportal_db('public')`")
    base_url <- Sys.getenv("CBIOPORTAL_URL")
  }

  url <- httr::modify_url(url = "",
                    scheme = "https",
                    hostname = base_url,
    path = url_path
    )

#  print(url)

  if(method == "get") {

    resp <- httr::GET(url,
      httr::add_headers(Authorization = paste("Bearer ",
        token,
        sep = "")))

  } else {

    if(extra_box == TRUE) {

      body_format = jsonlite::toJSON(list(body), pretty = T, auto_unbox = T)

    } else {
      body_format = jsonlite::toJSON(body, pretty = T, auto_unbox = F)
    }
 #   print(body_format)

    resp <- httr::POST(url = url,
                      body =  body_format,
                      encode = "json",
                      httr::add_headers(`accept` = 'application/json'),
                      httr::content_type('application/json'),
      httr::add_headers(Authorization = paste("Bearer ",
        token,
        sep = "")))
  }


  statuscode <- httr::status_code(resp)

  if (statuscode >= 400) {
    stop(
      glue::glue(
        'GitHub API request failed: ',
        httr::status_code(resp)
      ),
      call. = FALSE
    )
  }


  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  parse_url <- httr::parse_url(url)
  path <- parse_url$path

  x <- structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "cbp_api"
  )

  x
}


print.cbp_api <- function(x, ...) {
  cat("<GitHub ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}


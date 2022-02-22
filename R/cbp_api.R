#' Core function to retrieve data from cBioPortal API.
#'
#'
#' @param url_path The url path for API call
#' @param method Which API method to use. Must be "get" or "post". Default is "get".
#' @param token Authentication token, if needed. Default is `get_cbioportal_token()`
#' @param body Arguments passed to API call (e.g. sample ID or gene IDs)
#' @param extra_box Some functions require an additional list() wrapping around body idk why
#' @param quiet Returns queried URL. Default is TRUE
#' @param base_url The database URL to query
#'
#' @return A parsed API response
#' @export
#' @import httr
#' @examples
#'
#' cbp_api(url_path = "genes/TP53", base_url = "www.cbioportal.org/api")
#'
cbp_api <- function(url_path,
                    method = NULL,
                    token = get_cbioportal_token(),
                    body = NULL,
                    extra_box = FALSE,
                    quiet = TRUE,
                    base_url = NULL) {

  method = match.arg(method, choices = c("get", "post"))

  final_base_url <- base_url %||%
    get_cbioportal_url() %||%
    abort(message = "must supply a url. Try `set_cbioportal_db()`")


  url <- httr::modify_url(url = "",
                    scheme = "https",
                    hostname = final_base_url,
    path = url_path
    )

  if(quiet == FALSE) {
    print(url)
  }

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


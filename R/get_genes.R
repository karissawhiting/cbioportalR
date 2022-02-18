
#' Get a list of all available genes
#'
#' @return A dataframe of gene ids, hugo dymbols, and gene types
#' @export
#' @examples
#' \dontrun{
#' get_cbioportal_db('public')
#' get_genes()
#' }
get_genes <- function() {

  url_path <- paste0("genes")


  res <- cbp_api(url_path = url_path)

  purrr::map_df(res$content, ~tibble::as_tibble(.x))
}




#' Get Entrez Gene ID for a given set of Hugo Symbols
#'
#' This only works 1 gene at a time right now
#'
#' @param hugo_symbol a hugo symbol for which to return an Entrez Gene ID
#' @return A dataframe with Entrez Gene IDs and Hugo Symbols
#' @export
#' @examples
#' get_cbioportal_db('public')
#' get_gene_id("FGFR3")
#'
get_gene_id <- function(hugo_symbol = NULL) {

  if(is.null(hugo_symbol)) rlang::abort("Must specify a `hugo_symbol`")
  url_path = paste0("genes/", hugo_symbol)
  res <- cbp_api(url_path)
  tibble::as_tibble(res$content)
}


#
# available_genes <- function(hugo_symbol = NULL) {
#
#   if(is.null(hugo_symbol)) rlang::abort("Must specify a `hugo_symbol`")
#   url_path = paste0("genes/", hugo_symbol)
#   res <- cbp_api(url_path)
#   tibble::as_tibble(res$content)
# }


#' Get Gene Name Alias for a Given Hugo Symbol
#'
#' This only works 1 gene at a time right now
#'
#' @param hugo_symbol a hugo symbol for which to return aliases
#' @param ... Not used
#' @return A character string with all aliases
#' @export
#' @examples
#' get_cbioportal_db('public')
#' get_alias("FGFR3")
#'
get_alias <- function(hugo_symbol, ...) {
  url_path = paste0("genes/", hugo_symbol, "/aliases")
  res <- cbp_api(url_path, ...)

  res <- res$content
  unlist(res)
}


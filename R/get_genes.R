
#' Get a list of all available genes
#'
#' @return A dataframe of gene ids, hugo dymbols, and gene types
#' @param base_url The database URL to query
#' @export
#' @examples
#' \dontrun{
#' get_genes(base_url = 'www.cbioportal.org/api')
#' }
get_genes <- function(base_url  =  NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  url_path <- paste0("genes")


  res <- cbp_api(url_path = url_path, base_url = final_url)

  purrr::map_df(res$content, ~tibble::as_tibble(.x))
}




#' Get Entrez Gene ID for a given set of Hugo Symbols
#'
#' This only works 1 gene at a time right now
#'
#' @param hugo_symbol a hugo symbol for which to return an Entrez Gene ID
#' @param base_url The database URL to query
#' @return A dataframe with Entrez Gene IDs and Hugo Symbols
#' @export
#' @examples
#'
#' get_gene_id("FGFR3", base_url = 'www.cbioportal.org/api')
#'
get_gene_id <- function(hugo_symbol = NULL, base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  if(is.null(hugo_symbol)) rlang::abort("Must specify a `hugo_symbol`")
  url_path = paste0("genes/", hugo_symbol)
  res <- cbp_api(url_path, base_url = final_url)
  tibble::as_tibble(res$content)
}




#' Get Gene Name Alias for a Given Hugo Symbol
#'
#' This only works 1 gene at a time right now
#'
#' @param hugo_symbol a hugo symbol for which to return aliases
#' @param base_url The database URL to query
#' @return A character string with all aliases
#' @export
#' @examples
#'
#' get_alias(hugo_symbol = "FGFR3", base_url = 'www.cbioportal.org/api')
#'
get_alias <- function(hugo_symbol, base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  url_path = paste0("genes/", hugo_symbol, "/aliases")
  res <- cbp_api(url_path, base_url=  final_url)

  res <- res$content
  unlist(res)
}


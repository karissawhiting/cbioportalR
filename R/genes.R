#' Get Hugo Symbol for a given set of Entrez IDs
#'
#' @param entrez_id a character or numeric vector of Entrez gene IDs
#' @param base_url The database URL to query
#' @return A dataframe with Entrez Gene IDs and Hugo Symbols
#' @export
#' @examples
#'
#' get_hugo_symbol(entrez_id = 2261, base_url = 'www.cbioportal.org/api')
#' get_hugo_symbol(entrez_id = c(2261, 7157) , base_url = 'www.cbioportal.org/api')


get_hugo_symbol <- function(entrez_id = NULL,
                            base_url = NULL) {

  # checks -------------------------------------------------------------------
  entrez_id %||% rlang::abort("Must specify at least one `entrez_id`")

  # query ----------------------------------------------------------------------
  url_path = paste0("genes/fetch?geneIdType=ENTREZ_GENE_ID")
  body <- entrez_id

  res <- cbp_api(url_path,
                 method = "post",
                 body = body,
                 base_url = base_url)

  # here bind_rows works because no list columns returned from API
  df <- bind_rows(res$content)

  return(df)
}


#' Get Entrez Gene ID for a given set of Hugo Symbols
#'
#' @param hugo_symbol a character vector of Hugo Symbols
#' @param base_url The database URL to query
#' @return A dataframe with Entrez Gene IDs and Hugo Symbols
#' @export
#' @examples
#'
#' get_entrez_id(hugo_symbol = "TAP1", base_url = 'www.cbioportal.org/api')
#' get_entrez_id(hugo_symbol = c("FGFR1", "TP53") , base_url = 'www.cbioportal.org/api')


get_entrez_id <- function(hugo_symbol = NULL,
                            base_url = NULL) {

  # checks -------------------------------------------------------------------
  hugo_symbol %||% rlang::abort("Must specify at least one `hugo_symbol`")

  # query ----------------------------------------------------------------------
  url_path = paste0("genes/fetch?geneIdType=HUGO_GENE_SYMBOL")
  body <- hugo_symbol

  res <- cbp_api(url_path,
                 method = "post",
                 body = body,
                 base_url = base_url)

  # here bind_rows works because no list columns returned from API
  df <- bind_rows(res$content)

  return(df)
}



#' Get Gene Name Alias for a Given Hugo Symbol
#'
#' @description This function grabs known gene aliases for a given Hugo Symbol.
#' You may notice that genes -alias pairs are not always consistent. For example
#' get_alias("KMT2D") will return "MLL2" but get_alias("MLL2") will not return "KMT2D"
#' This function relies on the existing cBioPortal API which controls this database
#' of aliases. Therefore, this is a convenience function but
#' you may want to consider a more carefully curated alias list like `gnomeR::impact_gene_info`
#'
#' @param hugo_symbol a hugo symbol for which to return aliases
#' @param base_url The database URL to query
#' @return A character string with all aliases
#' @export
#' @examples
#'
#' get_alias(hugo_symbol = "FGFR3", base_url = 'www.cbioportal.org/api')
#' get_alias(hugo_symbol = c("FGFR3", "TP53"), base_url = 'www.cbioportal.org/api')
get_alias <- function(hugo_symbol = NULL,
                      base_url = NULL) {

  lookup_hugo <- hugo_symbol %||% rlang::abort("Must specify at least one `hugo_symbol`")

  # ** This is a hot mess of nested things
  res <- lookup_hugo %>%
    tibble::enframe(name = NULL, value = "hugo_symbol") %>%
    mutate(url_path = paste0("genes/", .data$hugo_symbol, "/aliases")) %>%
    mutate(alias = purrr::map(.data$url_path,
                   ~cbp_api(.x, base_url = base_url)$content)) %>%
    mutate(alias = purrr::simplify_all(.data$alias)) %>%
    tidyr::unnest(.data$alias) %>%
    select(-.data$url_path)

  return(res)
}

#' Retrieve Genes Included For a Specified Panel ID
#'
#' @param panel_id name of panel. See `available_gene_panels()` to get panel ID
#' @param base_url The database URL to query.
#' If `NULL` will default to URL set with `set_cbioportal_db(<your_db>)`
#' @return A dataframe of genes in a specified panel
#' @export
#'
#'
get_gene_panel <- function(panel_id = NULL, base_url = NULL) {

  if (is.null(panel_id)) {
    stop("You must provide at least one `panel_id`")
  }

  panel_data <- purrr::map_dfr(panel_id, function(x) {
    res <- paste0("gene-panels/", x) %>%
      cbp_api(url_path = ., base_url = base_url)

    tib <- tibble::as_tibble(res$content)

    tib %>%
      mutate(data = purrr::map(.data$genes, ~as_tibble(.x))) %>%
      select(.data$genePanelId, .data$data, .data$description) %>%
      tidyr::unnest(cols = .data$data)

  })


  return(panel_data)
}



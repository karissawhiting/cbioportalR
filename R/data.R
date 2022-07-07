

#' IMPACT Gene Meta Data
#'
#' Dataframe labeling all genes included in IMPACT panels along with their corresponding
#' platform ID and Entrez ID.
#'
#' @format A data frame with 470 genes
#' \describe{
#'   \item{hugo_symbol}{Factor w/ 574 levels,
#'    Column containing all HUGO symbols genes included in IMPACT}
#'   \item{entrez_id}{Integer, contains all Entrez IDs for genes included in IMPACT}
#'   \item{platform_341}{Character, indicates whether each gene was included in
#'   IMPACT platform 341. Options are `included` and `not included`}
#'   \item{platform_410}{Character, indicates whether each gene was included in
#'   IMPACT platform 410. Options are `included` and `not included`}
#'   \item{platform_468}{Character, indicates whether each gene was included in
#'   IMPACT platform 468. Options are `included` and `not included`}
#'   \item{alias}{A nested dataframe of aliases for each gene and corresponding entrez gene ids for aliases if they exist}
#' }
#'
#' @source \url{http://www.cbioportal.org/}
#'
"impact_gene_info"


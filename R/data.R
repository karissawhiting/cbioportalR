

#'A vector of 19441 hugo symbols in TCGA
#'
#' @format A data frame with 19441 genes
#' \describe{
#'     }
"tcga_genes"


#' Data frame of all TCGA sample ids and their corresponding cancer type
#'
#' Data frame of all TCGA sample ids and their corresponding cancer type
#'
#' @format A data frame with 10275 samples
#' \describe{
#'     \item{patient_id}{Sample ID}
#'     \item{Cancer_Code}{Cancer site following TCGA abbreviations}
#'     }
"tcga_samples"

#' IMPACT Gene Meta Data
#'
#' Dataframe labeling all genes found in IMPACT along with their corresponding
#' platform and Entrez ID.
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
#' @source \url{https://cbioportal.org/}
"impact_gene_info"


#' A table to lookup Hugo Symbol by Entrez ID
#'
#' @format A data frame
#' \describe{
#'     \item{entrezGeneId}{entrez Gene Id}
#'     \item{hugoGeneSymbol}{Gene Hugo Symbol}
#'     \item{type}{Gene type}
#'     }
"all_genes_lookup"


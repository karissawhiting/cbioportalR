#' Get all genetic data for a study or list of IDs
#'
#' @param sample_ids A character vector of sample ids
#' @param study_id A character vector of length 1 specifying study to query. See `get_studies()` to see list of available studies.
#' @param genes A list of genes to query. default is all impact genes.
#' @param panel OPTIONAL argument. A character vector of length 1 indicating a specific panel to be used. If not NULL,
#' the panel will be looked up with `get_panel()` and only genes in that panel will be returned.
#' @param mutations Boolean specifying if mutation data should be fetched. Default is TRUE.
#' @param fusions Boolean specifying if fusion data should be fetched. Default is TRUE.
#' @param cna Boolean specifying if cna data should be fetched. Default is TRUE.
#' @param seg Boolean specifying if segmentation data should be fectched. Default is FALSE.
#'
#' @return A list of dataframes containing genetic data.
#' @export
#'
#' @examples
#' \dontrun{
#' # IMPACT #
#' get_genetics(sample_ids = c("P-0000004-T01-IM3", "P-0000012-T02-IM3"),
#' genes = 207)
#'
#' # TCGA #
#' get_genetics(sample_ids =  c("TCGA-17-Z023-01","TCGA-02-0003-01","TCGA-02-0055-01"),
#'  database = "tcga")
#' }

get_genetics <- function(
  sample_ids = NULL,
  study_id = NULL,
  genes = NULL,
  panel = NULL,
  mutations = TRUE,
  fusions = TRUE,
  cna = TRUE,
  seg = FALSE) {


  # checks ---------------------------------------------------------------------

  if(all(c(!mutations, !fusions, !cna)))
    stop("At least one of the following arguments must be TRUE: mutations, fusions, cna.")
  mut.dat <- NULL
  cna.dat <- NULL
  seg.dat <- NULL

  # Query -----------------------------------------------------------------------

  if(mutations || fusions){
      mut.dat <- get_mutations(sample_ids = sample_ids,
                              study_id = study_id,
                              panel = panel,
                               genes = genes)

      if(!is.null(mut.dat)) {
        mut.dat <- mut.dat %>%
          dplyr::rename(Tumor_Sample_Barcode = "sampleId", Hugo_Symbol = NULL,
                      Variant_Classification = "mutationType", Mutation_Status = "mutationStatus",
                      Variant_Type = "variantType")
      }

      if(!fusions) {
        mut.dat <- mut.dat %>%
          filter(.data$Variant_Classification != "Fusion")
      }

      class(mut.dat) <- c("api", "tbl_df", "tbl", "data.frame")
    }

    if(cna){
      cna.dat <- get_cna(sample_ids = sample_ids,
                         study_id = study_id,
                         panel = panel,
                         genes= genes)
      if(!is.null(cna.dat)) {
        class(cna.dat) <- c("api", "tbl_df", "tbl", "data.frame")
      }
    }
  if(seg){
    seg.dat <- get_segments(sample_ids = sample_ids)
    return(list("mut"= mut.dat,
                "cna" = cna.dat,
                "seg" = seg.dat))
  }

    return(list("mut"= mut.dat,
                "cna" = cna.dat))
}


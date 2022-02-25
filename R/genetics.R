#' Get all genetic data for a study or list of IDs
#'
#' @param sample_ids A character vector of sample ids. Can be NULL if a `study_id` is passed. If sample IDs are
#' passed with no `study_id`, IMPACT studies will be queried by default.
#' If non IMPACT ID samples passed, a `study_id` is required as well to retrieve data.
#' @param study_id A character vector study IDs. See `get_studies()` to see available studies. If passed with no specified `sample_id`
#' all samples for specified studies will be returned.
#' @param genes A vector of Entrez IDs of genes to query. default is all impact genes.
#' @param panel OPTIONAL argument. A character vector of length 1 indicating a specific panel to be used. If not NULL,
#' the panel will be looked up with `get_panel()` and only genes in that panel will be returned.
#' @param mutations Boolean specifying if mutation data should be fetched. Default is TRUE.
#' @param fusions Boolean specifying if fusion data should be fetched. Default is TRUE.
#' @param cna Boolean specifying if cna data should be fetched. Default is TRUE.
#' @param base_url The database URL to query
#' @return A list of dataframes containing genetic data.
#' @export
#'
#' @examples
#' set_cbioportal_db("public")
#' get_genetics(sample_id = c("P-0005217-T03-IM5", "P-0038798-T01-IM6"))

get_genetics <- function(
  sample_ids = NULL,
  study_id = NULL,
  genes = NULL,
  panel = NULL,
  mutations = TRUE,
  fusions = TRUE,
  cna = TRUE,
  base_url = NULL) {


    final_url <- base_url %>% .resolve_url() %||% .get_cbioportal_url()

  # checks ---------------------------------------------------------------------

  if(all(c(!mutations, !fusions, !cna)))
    stop("At least one of the following arguments must be TRUE: mutations, fusions, cna.")
  mut.dat <- NULL
  cna.dat <- NULL
  seg.dat <- NULL

  # Query -----------------------------------------------------------------------

    if(cna){
      cna.dat <- get_cna(sample_id = sample_ids,
                         study_id = study_id,
                         panel = panel,
                         genes = genes,
                         base_url = final_url)
      if(!is.null(cna.dat)) {
        class(cna.dat) <- c("api", "tbl_df", "tbl", "data.frame")
      }
    }


  if(mutations || fusions){
      mut.dat <- get_mutations(sample_id = sample_ids,
                              study_id = study_id,
                              panel = panel,
                               genes = genes,
                              base_url = final_url)

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


  # if(seg){
  #   seg.dat <- get_segments(sample_ids = sample_ids)
  #   return(list("mut"= mut.dat,
  #               "cna" = cna.dat,
  #               "seg" = seg.dat))
  # }

    return(list("mut"= mut.dat,
                "cna" = cna.dat))
}


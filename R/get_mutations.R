
# Mutations by sample  -----------------------------------------------------

#' Function pulls mutation data by sample ID
#'
#' @param sample_id A vector of sample ids
#' @param study_id A study id indicating where samples are housed
#' @param genes A list of genes to query
#' @param panel A specified gene panel
#' @param ... Not used
#'
#' @return A dataframe of mutations
#' @keywords internal
#' @noRd
.get_mutations_by_sample_id <- function(sample_id = NULL,
                                       study_id = NULL,
                                       genes,
                                       panel, ...) {

  # args <- list(...)
  #
  #
  #
  # if (length(args) > 0) {
  #   for(i in 1:length(args)) {
  #     assign(x = names(args)[i], value = args[[i]])
  #   }
  # }

  final_base_url <- .determine_base_url(...)

  input_study_id <- study_id

  # separate impact samples
  impact_ids <- sample_id[stringr::str_detect(sample_id, "P-0")]
  non_impact_ids <-sample_id[!stringr::str_detect(sample_id, "P-0")]

  if(length(impact_ids) == 0 & is.null(study_id)) {
    stop("All non-IMPACT samples passed with no default `study_id`. Need to specify `study_id` to query non IMPACT")
  }


  # if no study ID and MSK db, default to IMPACT study ID
  if (is.null(study_id) & stringr::str_detect(final_base_url, "mskcc")) {
    study_id = "mskimpact"
    warning(paste0("no `study_id` provided, defaulting to searching within `mskimpact` study. The following non IMPACT IDs will be ignored:\n ",
                   paste0(non_impact_ids, collapse = ", ")))
  }

  if (is.null(study_id) & final_base_url == "www.cbioportal.org/api") {

    study_id = "msk_impact_2017"
    warning("If you are an MSK researcher, for most up to date IMPACT data you should connect to MSK cbioportal. \nThis function is using limited public IMPACT data (study_id = 'msk_impact_2017')")

    } else {

      study_id <- study_id
      if(is.null(study_id)) stop("you need to specify a `study_id` to look for samples.")

    }


  if(length(study_id) > 3) stop("Must specify 3 or less study_ids in one call. Try separating into different calls. ")

  # Function to search sample IDs within multiple studies
  search_mult_studies <- function(study_id, sample_id) {

    url_path <- paste0("molecular-profiles/", study_id, "_mutations/mutations/fetch?")

    body <- list(
      entrezGeneIds = genes,
      sampleIds = sample_id
    )

    res <- cbp_api(url_path,
      method = "post",
      body = body,
      base_url = final_base_url,
    )

   purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  }

  all_study_ids <- c(study_id, ifelse(stringr::str_detect(final_base_url, "mskcc"),
                                      "mskimpact", "msk_impact_2017")) %>%
    unique()

  all <- purrr::map_df(all_study_ids,
                       ~search_mult_studies(sample_id = sample_id,
                                                        study_id = .x))

  # if(any(str_detect(all$studyId, c("mskimpact|msk_impact_2017"))) &
  #   (!("mskimpact" %in% input_study_id) | !("msk_impact_2017" %in% input_study_id))) {
  #   warning("Some sample_id not in your specified studies (,", study_id, ") had data available in MSK IMPACT which is returned. If you would like to only keep results from your study, filter results by `study_id`, e.g. `filter(df, studyId != 'msk_impact')`")
  # }

  # Check if certain samples are in multiple studies

  if(nrow(all) > 0) {
    in_multiple_studies <- all  %>%
      select(.data$studyId, .data$sampleId) %>%
      distinct() %>%
      group_by(.data$sampleId) %>%
      mutate(n = n()) %>%
      filter(n >= 2)

  if(nrow(in_multiple_studies) > 0) {

    warning(paste0("WARNING! The following samples are in multiple studies, therefore are duplicated in results: ",
                                                     paste0(in_multiple_studies$sampleId, collapse = ", ")))
  }
  }

    return(all)
  }



# Mutations by sample ID  -----------------------------------------------------

#' Function pulls mutation data by cBioPortal study ID
#'
#' @param study_id A study id to query
#' @param ... Other arguments passed on to `cbp_api()`
#'
#' @return A dataframe of all mutations from specified study. all available data will be returned for all genes in study
#' @keywords internal
#' @noRd

.get_mutations_by_study_id <- function(study_id = NULL, ...) {

  # arguments ------------------------------------------------------------------
  args <- list(...)

  if (length(args) > 0) {
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  }

  final_base_url <- .determine_base_url()

  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }


  # query ---------------------------------------------------------------------

  df <- purrr::map_df(study_id, function(x) {
    url_path <- paste0(
      "molecular-profiles/", x,
      "_mutations/mutations?sampleListId=", x, "_all"
    )
    #  body <- list(entrezGeneIds = genes)
    res <- cbp_api(url_path, base_url = final_base_url)
    df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  })

  message(paste0("Returning mutations from the following studies: ", paste0(study_id, collapse = ", ")))

  return(df)

}


#' Get gene mutations by sample ID or Study
#'
#' @param sample_id A character vector of sample ids. Can be NULL if a `study_id` is passed. If sample IDs are
#' passed with no `study_id`, IMPACT studies will be queried by default.
#' If non IMPACT ID samples passed, a `study_id` is required as well to retrieve data.
#' @param study_id A character vector study IDs. See `get_studies()` to see available studies. If passed with no specified `sample_id`
#' all samples for specified studies will be returned.
#' @param panel OPTIONAL argument. A character vector of length 1 indicating a specific panel to be used. If not NULL,
#' the panel will be looked up with `get_panel()` and only genes in that panel will be returned.
#' @param genes A list of genes to query. default is all impact genes.
#' @param ... Not used
#' @return A dataframe of mutations for each sample ID (in maf file format)
#' @export
#'
#' @examples
#' get_cbioportal_db("public")
#' get_mutations(sample_id = c("P-0005217-T03-IM5", "P-0038798-T01-IM6"))
#'
get_mutations <- function(sample_id = NULL,
                          study_id = NULL,
                          panel = NULL,
                          genes = NULL, ...) {

  # checks ---------------------------------------------------------------------

  if (all(is.null(sample_id), is.null(study_id))) {
    stop("Must specify either a list of sample ids, and/or
       a study id. See `get_studies()` to view available studies. ")
  }


  # get entrez IDs and genes ------------------------------------------------

  # If user specified a panel---
    if (!is.null(panel)) {
      all_ids_df <- get_panel(panel)

      genes <- all_ids_df %>%
        pull(.data$entrezGeneId)
  }

  # else default to IMPACT Panel
  ent_id <- cbioportalR::impact_gene_info %>%
    dplyr::select(.data$hugo_symbol, .data$entrez_id, .data$alias) %>%
    tidyr::unnest(c(.data$entrez_id, .data$alias))

  ent_id1 <- ent_id %>%
    transmute(hugo_symbol = .data$hugo_symbol, entrez_id = .data$alias_entrez_id) %>%
    distinct()

  ent_id2 <- ent_id %>%
    select(.data$hugo_symbol, .data$entrez_id)

  all_ids_df <- bind_rows(ent_id1, ent_id2) %>%
    distinct() %>%
    filter(!is.na(.data$entrez_id))

  all_impact_ids <- all_ids_df %>%
    pull(.data$entrez_id)

  # use all impact genes by default
  if (is.null(panel) & !is.null(genes)) {
    if (genes[1] == "all_impact") {
      genes <- all_impact_ids
    }
  }

  # queries -------------------------------------------------------------------

  if (!is.null(sample_id)) {

    if (!is.null(sample_id) & is.null(genes)) {
      message("No genes specified by user, so querying curated list of impact genes.
              To view queried gene list run `cbioportalR::impact_gene_info`")

  }

    if(is.null(genes))  {genes <- all_impact_ids}

    df <- .get_mutations_by_sample_id(sample_id = sample_id,
                                     study_id = study_id,
                                     genes = genes,
                                     panel = panel)
  }

  if (!is.null(study_id) & is.null(sample_id)) {

    # by default it returns all genes for that study and filters later
    df <- .get_mutations_by_study_id(study_id = study_id)

    if (!is.null(genes)) {

      df <- df %>% filter(.data$entrezGeneId %in% genes)

    }

  }

  if (nrow(df) > 0) {
    df <- df %>%
      left_join(y=all_ids_df, by = c("entrezGeneId" = "entrez_id")) %>%
      rename("Hugo_Symbol" = .data$hugo_symbol) %>%
      mutate(
        HGVSp_Short = paste0("p.", .data$proteinChange),
        Protein_position = ifelse(.data$proteinPosStart < 0, "",
          .data$proteinPosStart
        )
      )

    df_no_na <- df %>%
      filter(!is.na(.data$Hugo_Symbol))

    df_na <- df %>%
      filter(is.na(.data$Hugo_Symbol)) %>%
      select(-.data$Hugo_Symbol) %>%
      left_join(transmute(cbioportalR::all_genes_lookup,
                          .data$entrezGeneId,
                          Hugo_Symbol = .data$hugoGeneSymbol),
                by = "entrezGeneId")

    df <- bind_rows(df_na, df_no_na)

    # when hugo missing, rename with entrez id

    df <- df %>%
      mutate(Hugo_Symbol =
               case_when(
                 is.na(.data$Hugo_Symbol) ~ paste0("entrez_id_", .data$entrezGeneId),
                 TRUE ~ .data$Hugo_Symbol))

  }

  return(df)
}


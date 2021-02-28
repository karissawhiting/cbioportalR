

get_mutations_by_sample_id <- function(sample_ids = NULL,
                                       genes,
                                       panel) {


  # separate impact samples from tcga
  impact_ids <- sample_ids[stringr::str_detect(sample_ids, "P-0")]
  tcga_ids <- sample_ids[stringr::str_detect(sample_ids, "TCGA")]


  if (is.null(sample_ids)) {
    stop("You must provide at least one sample id (e.g. 'TCGA-19-5959-01')")
  }

  # if IMPACT ------------------------------------------------------------------

  if (length(impact_ids) >= 1) {
    url_path <- paste0("molecular-profiles/mskimpact_mutations/mutations/fetch?")

    body <- list(
      entrezGeneIds = genes,
      sampleIds = sample_ids
    )

    res <- cbp_api(url_path,
      method = "post",
      body = body
    )

    df_impact <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  }

  # if TCGA ------------------------------------------------------------------

  if (length(tcga_ids) >= 1) {

    # gets necessary sites for your sample IDs
    samples_and_sites <- cbioportalR::tcga_samples %>%
      dplyr::filter(.data$patient_id %in% tcga_ids) %>%
      dplyr::transmute(
        sample_ids = .data$patient_id,
        cancer_code = tolower(.data$Cancer_Code)
      ) %>%
      distinct()

    url_path <- paste0(
      "molecular-profiles/",
      samples_and_sites$cancer_code,
      "_tcga_pan_can_atlas_2018_mutations/mutations/fetch?"
    )

    body <- purrr::map(
      samples_and_sites$sample_ids,
      ~ list(
        entrezGeneIds = genes,
        sampleIds = .x
      )
    )

    df_tcga <- purrr::map2_df(
      url_path, body,
      ~ cbp_api(.x,
        method = "post",
        body = .y
      )$content
    ) %>%

      # add mutation status to TCGA results
      mutate(mutationStatus = "SOMATIC")
  }

  # May Need to Combine results if TCGA/IMPACT Mixed List----------------------

  # Combine TCGA and IMPACT if needed
  if (exists("df_tcga") & exists("df_impact")) {
    df <- bind_rows(df_impact, df_tcga)
    return(df)
  } else if (exists("df_tcga")) {
    df <- df_tcga
  } else if (exists("df_impact")) {
    df <- df_impact
  }

  return(df)
}


get_mutations_by_study_id <- function(study_id = NULL, ...) {

  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }

  # query ---------------------------------------------------------------------
  url_path <- paste0(
    "molecular-profiles/", study_id,
    "_mutations/mutations?sampleListId=", study_id, "_all"
  )

#  body <- list(entrezGeneIds = genes)
  res <- cbp_api(url_path)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  return(df)

}


#' Get gene mutations by sample ID or Study
#'
#' @param sample_ids A character vector of sample ids
#' @param study_id A character vector of length 1 indicating study_id. See `get_studies()` to see available studies.
#' @param panel OPTIONAL argument. A character vector of length 1 indicating a specific panel to be used. If not NULL,
#' the panel will be looked up with `get_panel()` and only genes in that panel will be returned.
#' @param genes A list of genes to query. default is all impact genes.
#'
#' @return A dataframe of mutations for each sample ID (in maf file format)
#' @export
#'
#' @examples
#' get_cbioportal_db("public")
#' get_mutations(sample_ids = c("TCGA-19-5959-01", "TCGA-18-4083-01"))

#'
get_mutations <- function(sample_ids = NULL,
                          study_id = NULL,
                          panel = NULL,
                          genes = NULL) {

  # checks ---------------------------------------------------------------------

  if (all(is.null(sample_ids), is.null(study_id))) {
    stop("Must specify either a list of sample ids (TCGA or MSK), or
       a study id. See `get_studies()` to view available studies. ")
  }

    if (!is.null(sample_ids) & !is.null(study_id)) {
    stop("Must specify either a list of sample ids (TCGA or MSK), or
       a study id, not both.")
  }


  # get entrez IDs and genes ------------------------------------------------

  # If user specified a panel---
    if (!is.null(panel)) {
    # if (panel %in% c(
    #   "IMPACT341", "IMPACT410", "IMPACT468",
    #   "MSK-IMPACT341", "MSK-IMPACT410", "MSK-IMPACT468"
    # )) {
    #   genes <- "all_impact"
    #   panel <- NULL
    #   message("Querying curated list of impact genes. To view queried gene list run `cbioportalr::impact_gene_info`")
    # } else {
      all_ids_df <- get_panel(panel)

      genes <- all_ids_df %>%
        pull(.data$entrezGeneId)
   # }
  }


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

  if (!is.null(sample_ids)) {

    if (!is.null(sample_ids) & is.null(genes)) {
      message("No genes specified by user, so querying curated list of impact genes.
              To view queried gene list run `cbioportalR::impact_gene_info`")

  }

    if(is.null(genes))  {genes <- all_impact_ids}

    df <- get_mutations_by_sample_id(sample_ids = sample_ids,
                                     genes = genes,
                                     panel = panel)
  }

  if (!is.null(study_id)) {

    # by default it returns all genes for that study and filters later
    df <- get_mutations_by_study_id(study_id = study_id)

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


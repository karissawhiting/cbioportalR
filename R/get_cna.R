get_cna_by_sample_id  <- function(sample_id = NULL,
                                       study_id = NULL,
                                       genes,
                                       panel,
                                  base_url =NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  input_study_id <- study_id

  # separate impact samples
  impact_ids <- sample_id[stringr::str_detect(sample_id, "P-0")]
  non_impact_ids <-sample_id[!stringr::str_detect(sample_id, "P-0")]

  if(length(impact_ids) == 0 & is.null(study_id)) {
    stop("All non-IMPACT samples passed with no default `study_id`. Need to specify `study_id` to query non IMPACT")
  }

  # if no study ID and MSK db, default to IMPACT study ID
  if (is.null(study_id) & stringr::str_detect(final_url, "mskcc")) {
    study_id = "mskimpact"
    warning(paste0("no `study_id` provided, defaulting to searching within `mskimpact` study. The following non IMPACT IDs will be ignored:\n ",
                   paste0(non_impact_ids, collapse = ", ")))
  }

  if (is.null(study_id) & final_url == "www.cbioportal.org/api") {

    study_id = "msk_impact_2017"
    warning("If you are an MSK researcher, for most up to date IMPACT data you should connect to MSK cbioportal. \nThis function is using limited public IMPACT data (study_id = 'msk_impact_2017')")

    } else {

      study_id <- study_id
      if(is.null(study_id)) stop("you need to specify a `study_id` to look for samples.")

    }

  if(length(study_id) > 3) stop("Must specify 3 or less study_ids in one call. Try separating into different calls. ")

  search_mult_studies <- function(study_id, sample_id) {

   url_path <- paste0("molecular-profiles/", study_id, "_cna/discrete-copy-number/fetch?")

    body <- list(
      entrezGeneIds = genes,
      sampleIds = sample_id
    )

    res <- cbp_api(url_path,
      method = "post",
      body = body, base_url = final_url
    )

    purrr::map_df(res$content, ~ tibble::as_tibble(.x))
  }

    all_study_ids <- c(study_id, ifelse(stringr::str_detect(final_url, "mskcc"),
                                      "mskimpact", "msk_impact_2017")) %>%
    unique()

  all <- purrr::map_df(all_study_ids,
                       ~search_mult_studies(sample_id = sample_id,
                                                        study_id = .x))
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


get_cna_by_study_id <- function(study_id = NULL, base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

  # checks ---------------------------------------------------------------------
  if (is.null(study_id)) {
    stop("You must provide a study id. See `get_studies()` to view available studies on database")
  }


  # get CNA prof name  ---------------------------------------------------------------------
  cna_prof <- available_profiles(study_id) %>%
    dplyr::filter(.data$molecularAlterationType == "COPY_NUMBER_ALTERATION") %>%
    dplyr::filter(.data$datatype == "DISCRETE")

  cna_prof <- cna_prof %>%
    pull(.data$molecularProfileId)

  cna_prof <- cna_prof[1]

  if(is.na(cna_prof)) {
    stop(paste0("There does not appear to be a CNA profile available for this study. Check available data using `available_profiles(<your_study_id>)`"))

  }


  message(paste0("Resulting CNA data is from the ", cna_prof, " CNA profile. Data coding may differ between studies. Please check `available_profiles(<your_study_id>)` to see details on this data and how it is coded."))


  # query ---------------------------------------------------------------------
  url_path <- paste0(
    "molecular-profiles/",  cna_prof,
    "/discrete-copy-number?sampleListId=", study_id, "_all"
  )


#  body <- list(entrezGeneIds = genes)
  res <- cbp_api(url_path, base_url = final_url)
  df <- purrr::map_df(res$content, ~ tibble::as_tibble(.x))

  return(df)
}


#' Get copy number alterations by sample ID or study ID
#'
#' @param sample_id A character vector of sample ids. Can be NULL if a `study_id` is passed. If sample IDs are
#' passed with no `study_id`, IMPACT studies will be queried by default.
#' If non IMPACT ID samples passed, a `study_id` is required as well to retrieve data.
#' @param study_id A character vector study IDs. See `get_studies()` to see available studies. If passed with no specified `sample_id`
#' all samples for specified studies will be returned.
#' @param panel OPTIONAL argument. A character vector of length 1 indicating a specific panel to be used. If not NULL,
#' the panel will be looked up with `get_panel()` and only genes in that panel will be returned.
#' @param genes A list of genes to query. default is all impact genes.
#' @param base_url The database URL to query
#' @return A dataframe of CNA for each sample ID (in maf file format)
#' @export
#'
#' @examples
#'
#' get_cna(sample_id = c("P-0000004-T01-IM3", "P-0000015-T01-IM3"),
#'  base_url = 'www.cbioportal.org/api')
#'

get_cna <- function(sample_id = NULL,
                          study_id = NULL,
                          panel = NULL,
                          genes = NULL,
                    base_url = NULL) {

  final_url <- base_url %||% get_cbioportal_url()

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

    df <- get_cna_by_sample_id(sample_id = sample_id,
                                     study_id = study_id,
                                     genes = genes,
                                     panel = panel,
                               base_url = final_url)
  }

  if (!is.null(study_id) & is.null(sample_id)) {

    # by default it returns all genes for that study and filters later
    df <- get_cna_by_study_id(study_id = study_id)

    if (!is.null(genes)) {

      df <- df %>% filter(.data$entrezGeneId %in% genes)

    }
  }

  if (nrow(df) > 0) {
    df <- df %>%
      left_join(y=all_ids_df, by = c("entrezGeneId" = "entrez_id")) %>%
      rename("Hugo_Symbol" = .data$hugo_symbol)


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

    # when hugo missing, rename with entrez i

    df <- df %>%
      mutate(Hugo_Symbol =
               case_when(
                 is.na(.data$Hugo_Symbol) ~ paste0("entrez_id_", .data$entrezGeneId),
                 TRUE ~ .data$Hugo_Symbol))

  }

  return(df)
}



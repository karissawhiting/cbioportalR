
# Tests all sample ID pull functions (and some non genomic sample tests, e.g. clinical pulls)------

# Sample ID AND Study ID Endpoints ----------------------------------------------

test_that("test endpoints - with sample ID", {
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "prad_msk_2019"
  sample_id = c("s_C_36924L_P001_d")

  endpoint_funs <- c(
    get_mutations_by_sample = get_mutations_by_sample,
    get_cna_by_sample = get_cna_by_sample,
    get_clinical_by_sample = get_clinical_by_sample,
    get_panel_by_sample = get_panel_by_sample)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, study_id = study_id,
                                             sample_id = sample_id))



  expect_equal(names(res), names(endpoint_funs))


})

test_that("test endpoints - with sample  ID, no study ID", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = NULL
  sample_id = c("P-0000004-T01-IM3")

  endpoint_funs <- c(
    get_mutations_by_sample = get_mutations_by_sample,
    get_cna_by_sample = get_cna_by_sample,
    get_clinical_by_sample = get_clinical_by_sample)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, study_id = study_id,
                                             sample_id = sample_id))



  expect_equal(names(res), names(endpoint_funs))

  expect_error(get_panel_by_sample(study_id = study_id,
                                   sample_id = sample_id), "No*")


})


test_that("test endpoints - no sample ID or sample_study_pair", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = NULL
  sample_id = NULL

  expect_error(get_mutations_by_sample(study_id = study_id,
                                       sample_id = sample_id), "You*")
  expect_error(get_cna_by_sample(study_id = study_id,
                                       sample_id = sample_id), "You*")
  expect_error(get_clinical_by_sample(study_id = study_id,
                                       sample_id = sample_id), "You*")
  expect_error(get_panel_by_sample(study_id = study_id,
                                       sample_id = sample_id), "You*")

})

test_that("test endpoints - sample_study_pair", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  ex <- tibble::tribble(
    ~sampleId, ~studyId,
    "P-0001453-T01-IM3", "blca_nmibc_2017",
    "P-0002166-T01-IM3", "blca_nmibc_2017",
    "P-0003238-T01-IM5", "blca_nmibc_2017",
    "P-0000004-T01-IM3", "msk_impact_2017",
    "P-0000023-T01-IM3", "msk_impact_2017"
  )

  endpoint_funs <- c(
    get_mutations_by_sample = get_mutations_by_sample,
    get_cna_by_sample = get_cna_by_sample,
    get_clinical_by_sample = get_clinical_by_sample)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, sample_study_pairs = ex))


  expect_equal(names(res), names(endpoint_funs))


  # test out of order
  ex <- tibble::tribble(
    ~`StUdY ID`, ~sampleID, ~free, ~other,
    "blca_nmibc_2017", "P-0001453-T01-IM3", "p", "r",
    "blca_nmibc_2017", "P-0002166-T01-IM3", "p", "r",
    "blca_nmibc_2017", "P-0003238-T01-IM5", "p", "r",
    "msk_impact_2017", "P-0000004-T01-IM3", "p", "r",
    "msk_impact_2017", "P-0000023-T01-IM3", "p",  "r"
  )

  endpoint_funs <- c(
    get_mutations_by_sample = get_mutations_by_sample,
    get_cna_by_sample = get_cna_by_sample,
    get_clinical_by_sample = get_clinical_by_sample)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, sample_study_pairs = ex))


  expect_equal(names(res), names(endpoint_funs))

})

test_that("test endpoints - sample_study_pair wrong format", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  ex <- c("P-0000023-T01-IM3", "P-0000023-T01-IM3")


  expect_error(get_mutations_by_sample(sample_study_pairs = ex), "*")
  expect_error(get_cna_by_sample(sample_study_pairs = ex), "*")
  expect_error(get_clinical_by_sample(sample_study_pairs = ex), "*")
  expect_error(get_panel_by_sample(sample_study_pairs = ex), "*")

})

# Test Clinical Functions --------------------------------------------------
test_that("test clinical functions", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  study_id = "acc_tcga"
  t_sample_ids <- c("TCGA-OR-A5J2-01", "TCGA-OR-A5J4-01")

  clinical_attribute <- c("FRACTION_GENOME_ALTERED", "CANCER_TYPE")

  # test passing one single sample id and clinical attribute
  res <- get_clinical_by_sample(study_id = study_id,
                                sample_id = t_sample_ids[1],
                                clinical_attribute = clinical_attribute[1],
                                base_url = 'www.cbioportal.org/api')

  expect_equal(clinical_attribute[1], res$clinicalAttributeId[1])
  expect_equal(t_sample_ids[1], res$sampleId[1])

})

#  Test Getting Panels -------------------------------------------------
test_that("test get panel by sample", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"

  # no panel attribute in this study
  expect_error(get_panel_by_sample(study_id = "acc_tcga",
                                sample_id = c("TCGA-OR-A5J2-01", "TCGA-OR-A5J4-01"),
                                base_url = 'www.cbioportal.org/api'), "No*")

  expect_error(get_panel_by_sample(sample_id = c("TCGA-OR-A5J2-01", "TCGA-OR-A5J4-01"),
                                   base_url = 'www.cbioportal.org/api'), "*")

})


test_that("test get panels", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  set_cbioportal_db("public")

  expect_error(get_gene_panel(), "You must provide*")

  # test with one panel
  pan468 <- get_gene_panel(panel_id = "IMPACT468")
  expect_equal(unique(pan468$genePanelId), "IMPACT468")

  # test with more than one panel
  pan468_505 <- get_gene_panel(panel_id = c("IMPACT468", "IMPACT505"))
  expect_equal(unique(pan468_505$genePanelId), c("IMPACT468",  "IMPACT505"))

})


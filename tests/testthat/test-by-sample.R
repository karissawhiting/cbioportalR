
# Sample ID AND Study ID Endpoints -----------------------------------------------------------

test_that("test endpoints - with sample ID", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "prad_msk_2019"
  sample_id = c("s_C_36924L_P001_d")

  endpoint_funs <- c(
    get_mutation_by_sample = get_mutation_by_sample,
    get_cna_by_sample = get_cna_by_sample,
    get_clinical_by_sample = get_clinical_by_sample,
    get_panel_by_sample = get_panel_by_sample)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, study_id = study_id,
                                             sample_id = sample_id))



  expect_equal(names(res), names(endpoint_funs))

  # These should be the same (mutation is default data_type)
  # expect_equal(res$.get_data_by_sample, res$get_mutation_by_sample)
  #
  #   cna_res <- .get_data_by_sample(study_id = study_id,
  #                                sample_id = sample_id,
  #                                data_type = "cna")
  #
  #   expect_equal(cna_res, res$get_cna_by_sample)


})

# Test Clinical Functions --------------------------------------------------
test_that("test clinical functions", {

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

  # # test passing multiple sample ids and clinical attributes
  # res <- get_clinical_by_sample(study_id = "acc_tcga",
  #                               sample_id = sample_ids,
  #                               clinical_attribute = clinical_attribute,
  #                               base_url = 'www.cbioportal.org/api')
  #
  # expect_equal(sort(clinical_attribute), sort(unique(res$clinicalAttributeId)))
  # expect_equal(sort(sample_ids), sort(unique(res$sampleId)))
})

#  Test Getting Panels -------------------------------------------------
test_that("test get panel by sample", {

  db_test <- "public"

  # no panel attribute in this study
  expect_error(get_panel_by_sample(study_id = "acc_tcga",
                                sample_id = c("TCGA-OR-A5J2-01", "TCGA-OR-A5J4-01"),
                                base_url = 'www.cbioportal.org/api'), "No*")

  expect_error(get_panel_by_sample(sample_id = c("TCGA-OR-A5J2-01", "TCGA-OR-A5J4-01")),"You*")

})


test_that("test get panels", {

  set_cbioportal_db("public")

  expect_error(get_gene_panel(), "You must provide*")

  # test with one panel
  pan468 <- get_gene_panel(panel_id = "IMPACT468")
  expect_equal(unique(pan468$genePanelId), "IMPACT468")

  # test with more than one panel
  pan468_505 <- get_gene_panel(panel_id = c("IMPACT468", "IMPACT505"))
  expect_equal(unique(pan468_505$genePanelId), c("IMPACT468",  "IMPACT505"))

})


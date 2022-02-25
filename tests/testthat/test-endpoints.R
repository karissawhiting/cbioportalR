
# No Parameter Endpoints -------------------------------------------------------
test_that("test endpoints", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  endpoint_funs <- c(available_profiles = available_profiles,
                     available_gene_panels = available_gene_panels,
                     get_genes = get_genes)

  res <- expect_error(purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn)), NA)

  expect_equal(names(res), names(endpoint_funs))

})


# Study ID Endpoints -----------------------------------------------------------

test_that("test endpoints - with study_id", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "acc_tcga"

  endpoint_funs <- c(available_profiles = available_profiles,
                     available_clinical_attributes = available_clinical_attributes,
                     get_clinical_by_study = get_clinical_by_study)

  res <- expect_error(purrr::map(endpoint_funs,
       function(fn) rlang::exec(fn, study_id = study_id)), NA)

  expect_equal(names(res), names(endpoint_funs))

})

test_that("test endpoints - missing study_id arg", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  study_id = NULL
  endpoint_funs <- c(available_profiles = available_profiles,
                     available_clinical_attributes = available_clinical_attributes)

  expect_error(purrr::map(endpoint_funs,
                    function(fn) exec(fn, study_id = study_id)), "*")


})

test_that("test endpoints - with study_id & base_url", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  res <- expect_error(available_profiles(study_id = "brca_jup_msk_2020",
                            base_url = "public"), NA)

})


# Sample ID AND Study ID Endpoints -----------------------------------------------------------

test_that("test endpoints - with study_id", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  study_id = "acc_tcga"
  endpoint_funs <- c(available_profiles = available_profiles,
                     available_clinical_attributes = available_clinical_attributes)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, study_id = "acc_tcga"))

  expect_equal(names(res), names(endpoint_funs))

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



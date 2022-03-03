
# No Parameter Endpoints -------------------------------------------------------
test_that("test endpoints- no parameters", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  endpoint_funs <- c(available_profiles = available_profiles,
                     available_gene_panels = available_gene_panels,
                     available_studies = available_studies,
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
                     get_clinical_by_study = get_clinical_by_study,
                     get_study_info = get_study_info,
                     get_samples_by_study = get_samples_by_study,
                     get_mutation_by_study = get_mutation_by_study
                     )

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


# Sample ID AND Study ID Endpoints -----------------------------------------------------------

test_that("test endpoints - with sample ID", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "prad_msk_2019"
  sample_id = c("s_C_36924L_P001_d")

  endpoint_funs <- c(
                     get_mutation_by_sample = get_mutation_by_sample,
                     get_cna_by_sample = get_cna_by_sample,
                     get_clinical_by_sample = get_clinical_by_sample)

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


#  Test Getting Genes -------------------------------------------------

test_that("test get genes", {

  set_cbioportal_db("public")

  expect_error(
    get_entrez_id(),
    "Must specify at least one*")

  expect_error(
    get_hugo_symbol(),
    "Must specify at least one*")

  expect_error(
    get_hugo_symbol(),
    "Must specify at least one*")

  # capital and lowercase - becomes capital (this actually happens in the API itself)
  expect_equal(
    get_entrez_id(c("tp53", "FGFR3"))$hugoGeneSymbol,
    get_hugo_symbol(get_entrez_id(c("tp53", "FGFR3"))$entrezGeneId)$hugoGeneSymbol)

  # single or multiple args work
  expect_equal(
    get_entrez_id(c("APC"))$hugoGeneSymbol,
    get_hugo_symbol(get_entrez_id(c("APC"))$entrezGeneId)$hugoGeneSymbol)

  # passing character as entrez(also happens in API)
  x <- get_hugo_symbol(324)
  y <- get_hugo_symbol("324")
  expect_equal(x, y)

  # get alias
  expect_true("MLL2" %in% get_alias("KMT2D")$alias)



})

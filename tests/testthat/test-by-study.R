

# Study ID Endpoints -----------------------------------------------------------

# * General Tests -----------------
test_that("With study_id-  works fine", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "mpnst_mskcc"

  endpoint_funs <- c(available_profiles = available_profiles,
                     available_clinical_attributes = available_clinical_attributes,
                     get_clinical_by_study = get_clinical_by_study,
                     get_study_info = get_study_info,
                     available_samples = available_samples,

                     get_mutation_by_study = get_mutation_by_study,
                     get_cna_by_study = get_cna_by_study,
                     get_fusion_by_study = get_fusion_by_study,
                     get_genetics_by_study = get_genetics_by_study
                     )

  res <- expect_error(
    purrr::map(endpoint_funs,
               function(fn) rlang::exec(fn, study_id = study_id)), NA)

  expect_equal(names(res), names(endpoint_funs))

})

test_that("Missing study_id - arg throws an error", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  study_id = NULL
  endpoint_funs <- c(available_clinical_attributes = available_clinical_attributes,
                     get_clinical_by_study = get_clinical_by_study,
                     get_study_info = get_study_info,
                     available_samples = available_samples,

                     get_mutation_by_study = get_mutation_by_study,
                     get_cna_by_study = get_cna_by_study,
                     get_fusion_by_study = get_fusion_by_study
                     # throws a message instead
#                     get_genetics_by_study = get_genetics_by_study
                     )

  # **is there a better way??!?!

  purrr::map(endpoint_funs, function(fn) {
    expect_error(rlang::exec(fn), "*")
  })


})


test_that("Incorrect study_id - API error", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "blerg"

  endpoint_funs <- c(available_clinical_attributes = available_clinical_attributes,
                     get_clinical_by_study = get_clinical_by_study,
                     get_study_info = get_study_info,
                     available_samples = available_samples,

                     get_mutation_by_study = get_mutation_by_study,
                     get_cna_by_study = get_cna_by_study,
                     get_fusion_by_study = get_fusion_by_study
 #                    get_genetics_by_study = get_genetics_by_study - handled differently
                     )

  purrr::map(endpoint_funs, function(fn) {
    expect_error(rlang::exec(fn, study_id = study_id), "API request failed*")
  })


})



test_that("Missing study_id - arg defaults to sensible database value, no error", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  study_id = NULL
  endpoint_funs <- c(available_profiles = available_profiles)


  purrr::map(endpoint_funs, function(fn) {
    expect_error(rlang::exec(fn), NA)
  })


})


test_that("More than 1 study_id - throws an error", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = c("mpnst_mskcc", "prad_msk_2019")

  endpoint_funs <- c(available_profiles = available_profiles,
                     available_clinical_attributes = available_clinical_attributes,
                     get_clinical_by_study = get_clinical_by_study,
                     get_study_info = get_study_info,
                     available_samples = available_samples,

                     get_mutation_by_study = get_mutation_by_study,
                     get_cna_by_study = get_cna_by_study,
                     get_fusion_by_study = get_fusion_by_study,
                     get_genetics_by_study = get_genetics_by_study
                     )

  purrr::map(endpoint_funs, function(fn) {
    expect_error(rlang::exec(fn, study_id = study_id), "*")
  })


})


# * Clinical Data  -----------------

test_that("Clinical data by study- no attribute, defaults to all", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "acc_tcga"

  expect_message(get_clinical_by_study(study_id = study_id), "*all clinical attributes")

})

test_that("Clinical data by study- 1 attribute ", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "acc_tcga"
  clinical_attribute = c("CANCER_TYPE")

  expect_error(
    get_clinical_by_study(study_id = study_id,
                          clinical_attribute), NA)

  res <- get_clinical_by_study(study_id = study_id,
                        clinical_attribute)

  expect_equal(unique(res$clinicalAttributeId), clinical_attribute)
})

test_that("Clinical data by study- 2 attributes ", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "acc_tcga"
  clinical_attribute = c("CANCER_TYPE", "SAMPLE_TYPE")

  expect_error(
    get_clinical_by_study(study_id = study_id,
                          clinical_attribute), NA)

  res <- get_clinical_by_study(study_id = study_id,
                               clinical_attribute)

  expect_equal(sort(unique(res$clinicalAttributeId)), sort(clinical_attribute))
})


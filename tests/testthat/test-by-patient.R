

# Patient ID Endpoints -----------------------------------------------------------

test_that("test endpoints - with patient ID", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "msk_impact_2017"
  patient_id = c("P-0000034", "P-0000036")

  endpoint_funs <- c(
    get_samples_by_patient = get_samples_by_patient,
    get_clinical_by_patient = get_clinical_by_patient)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, study_id = study_id,
                                             patient_id = patient_id))



  expect_equal(names(res), names(endpoint_funs))


})

test_that("test endpoints - with patient  ID, no study ID", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = NULL
  patient_id = c("P-0000034", "P-0000036")

  endpoint_funs <- c(
    get_samples_by_patient = get_samples_by_patient,
    get_clinical_by_patient = get_clinical_by_patient)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, study_id = study_id,
                                             patient_id = patient_id))

  expect_equal(names(res), names(endpoint_funs))


})


test_that("test endpoints - no patient/ sample_study_pair", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = NULL
  patient_id = NULL

  expect_error(get_samples_by_patient(study_id = study_id,
                                       patient_id = patient_id), "You*")
  expect_error(get_clinical_by_patient(study_id = study_id,
                                       patient_id = patient_id), "You*")


})

test_that("test endpoints - sample_study_pair", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  ex <- tibble::tribble(
    ~`Patient ID`, ~STUDYID, ~other,
    "P-0001453", "blca_nmibc_2017", "r",
    "P-0002166", "blca_nmibc_2017", "r",
    "P-0003238", "blca_nmibc_2017", "r",
    "P-0000004", "msk_impact_2017", "r",
    "P-0000023", "msk_impact_2017", "r"
  )

  endpoint_funs <- c(
    get_clinical_by_patient = get_clinical_by_patient)

  res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, patient_study_pairs = ex))

  expect_message(res <- purrr::map(endpoint_funs,
                                   function(fn) rlang::exec(fn, patient_study_pairs = ex)), "*")


  expect_equal(names(res), names(endpoint_funs))

  expect_message(res <- purrr::map(endpoint_funs,
                    function(fn) rlang::exec(fn, patient_study_pairs = ex,
                                             clinical_attribute = "SEX")), NA)


  expect_equal(names(res), names(endpoint_funs))

})

test_that("test endpoints - sample_study_pair wrong format", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  ex <- c("P-0000023-T01-IM3", "P-0000023-T01-IM3")


  expect_error(get_clinical_by_patient(sample_study_pairs = ex), "*")


})


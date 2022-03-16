
# Sample ID AND Study ID Endpoints -----------------------------------------------------------

test_that("test endpoints - with patient ID", {


  set_cbioportal_db("public")

  expect_error(
    get_samples_by_patient(patient_id = c("P-0000034", "P-0000036")),
    NA)

  expect_message(
    get_samples_by_patient(patient_id = c("P-0000034", "P-0000036")),
    "No*")

  res <- get_samples_by_patient(patient_id = c("P-0000034", "P-0000036"))
  unique(res$studyId) == "msk_impact_2017"
})



test_that("test endpoints - error with n patient ID", {

  set_cbioportal_db("public")

  expect_error(
    get_samples_by_patient(),
    "You*"
    )

})

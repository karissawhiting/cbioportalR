

# Mutations By Study ID/Molecular Profile --------------------------------------
test_that("get mutations by study id - no error", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "mpnst_mskcc"

  expect_error(get_mutation_by_study(study_id = study_id), NA)
  expect_message(get_mutation_by_study(study_id = study_id), "Returning*")

})

test_that("get mutations by molecular profile - no error", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  molecular_profile_id = "mpnst_mskcc_mutations"

  expect_error(get_mutation_by_study(molecular_profile_id = molecular_profile_id), NA)
  expect_message(get_mutation_by_study(molecular_profile_id = molecular_profile_id), "Returning*")

})

test_that("get mutations by molecular profile/ study id/ get_genetics all the same", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  molecular_profile_id = "mpnst_mskcc_mutations"
  study_id = "mpnst_mskcc"

  by_study <- get_mutation_by_study(study = study_id)
  by_prof <- get_mutation_by_study(molecular_profile_id = molecular_profile_id)
  get_gen <- get_genetics_by_study(study = study_id)$mut
  expect_identical(by_study, by_prof, get_gen)

})

test_that("incorrect study id or profile", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  data_type = "mutation"

  # no study_id, incorrect profile- gives an informative error
  expect_error(.get_data_by_study(
    study_id = NULL,
    molecular_profile_id = "nope",
    data_type = data_type), "No*")

  # no profile, incorrect study_id - **API fail error (could be more informative)
  expect_error(.get_data_by_study(
    study_id = "not_here",
    molecular_profile_id = NULL,
    data_type = data_type), "API*")

  # both wrong - API fail
  expect_error(.get_data_by_study(
    study_id = "nope",
    molecular_profile_id = "also wrong",
    data_type = data_type), "API*")

  # correct study ID, wrong  profile - Custom
  expect_error(.get_data_by_study(
    study_id = "acc_tcga",
    molecular_profile_id = "wrong",
    data_type = data_type), "No molec*")

  # correct profile, wrong study ID - Custom API fail wooot
  expect_error(.get_data_by_study(
    study_id = "wrong",
    molecular_profile_id = "acc_tcga_mutations",
    data_type = data_type), "API*")

  # both correct, but bad base URL
  expect_error(.get_data_by_study(study_id = "acc_tcga",
                                  molecular_profile_id = "acc_tcga_mutations",
                                  data_type = data_type, base_url = "plunk"), "API*")

  # all correct- no errors
  expect_message(.get_data_by_study(study_id = "acc_tcga",
                                  molecular_profile_id = "acc_tcga_mutations",
                                  data_type = data_type), "Returning*")

  # fusions don't exist
  expect_error(.get_data_by_study(
    study_id = "acc_tcga",
    molecular_profile_id = "acc_tcga_fusions", data_type = "fusion"), "No molecular profile")
})


test_that("mismatching study id and molecular profile", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  molecular_profile_id = "mpnst_mskcc_mutations"
  study_id = "mpnst_mskcc"

  by_study <- get_mutation_by_study(study = study_id)
  by_prof <- get_mutation_by_study(molecular_profile_id = molecular_profile_id)
  get_gen <- get_genetics_by_study(study = study_id)$mut
  expect_identical(by_study, by_prof, get_gen)

})





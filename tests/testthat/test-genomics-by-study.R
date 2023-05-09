

# Mutations By Study ID/Molecular Profile --------------------------------------
test_that("get mutations by study id - no error", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "mpnst_mskcc"

  expect_error(get_mutations_by_study(study_id = study_id), NA)
  expect_message(get_mutations_by_study(study_id = study_id), "Returning*")

})

test_that("get mutations by molecular profile - no error", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  molecular_profile_id = "mpnst_mskcc_mutations"

  expect_error(get_mutations_by_study(molecular_profile_id = molecular_profile_id), NA)
  expect_message(get_mutations_by_study(molecular_profile_id = molecular_profile_id), "Returning*")

})

test_that("get mutations by molecular profile/ study id/ get_genetics all the same", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  molecular_profile_id = "mpnst_mskcc_mutations"
  study_id = "mpnst_mskcc"

  by_study <- get_mutations_by_study(study = study_id)
  by_prof <- get_mutations_by_study(molecular_profile_id = molecular_profile_id)
  get_gen <- get_genetics_by_study(study = study_id)$mutation
  expect_identical(by_study, by_prof, get_gen)

})


test_that("Test study_id and Profile Param", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  # > expand.grid(study_id = c("correct", "incorrect", "NULL"),
  # profile = c("correct", "incorrect", "NULL"))
  #
  # study_id   profile
  # 1   correct   correct
  # 2 incorrect   correct
  # 3      NULL   correct
  # 4   correct incorrect
  # 5 incorrect incorrect
  # 6      NULL incorrect
  # 7   correct      NULL
  # 8 incorrect      NULL
  # 9      NULL      NULL

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  data_type = "mutation"

  # Parameter Tests ------------

  # study_id = correct, profile = correct ~ WORKS
  expect_message(.get_data_by_study(
    study_id = "acc_tcga",
    molecular_profile_id = "acc_tcga_mutations",
    data_type = data_type), "Returning*")

  # study_id = incorrect, profile = correct ~ FAIL Informative Error
  expect_error(.get_data_by_study(
    study_id = "not_here",
    molecular_profile_id = "acc_tcga_mutations",
    data_type = data_type), "API*")


  # study_id = NULL, profile = correct - WORKS - guesses study ID
  expect_error(.get_data_by_study(
    study_id = NULL,
    molecular_profile_id = "acc_tcga_mutations",
    data_type = data_type), NA)

  # study_id = correct, profile = incorrect -FAIL Informative Error based on API data
  expect_error(.get_data_by_study(
    study_id = "acc_tcga",
    molecular_profile_id = "nope",
    data_type = data_type), "Molecular*")

  #study_id = incorrect, profiles = incorrect - gives informative error
  expect_error(.get_data_by_study(
    study_id = "blah",
    molecular_profile_id = "wrong",
    data_type = data_type), "API*")

  # study ID = correct, profile = NULL - gives informative error
  expect_error(.get_data_by_study(
    study_id = NULL,
    molecular_profile_id = "blah",
    data_type = data_type), "Molecular*")

  # study ID = correct, profile = NULL -WORKS- looks up profile ID
  expect_error(.get_data_by_study(
    study_id = "acc_tcga",
    molecular_profile_id = NULL,
    data_type = data_type), NA)

  # study_id = incorrect, profile = NULL **API fail error (could be more informative)
  expect_error(.get_data_by_study(
    study_id = "not_here",
    molecular_profile_id = NULL,
    data_type = data_type), "API*")

  # study_id = NULL, profile = NULL ~ gives informative error
  expect_error(.get_data_by_study(
    study_id = NULL,
    molecular_profile_id = NULL,
    data_type = data_type), "You must*")


  # Other-------------

  # both correct, but bad base URL
  expect_error(.get_data_by_study(study_id = "acc_tcga",
                                  molecular_profile_id = "acc_tcga_mutations",
                                  data_type = data_type, base_url = "plunk"), "API*")

  # both exist but mismatched
  expect_error(.get_data_by_study(study_id = "acc_tcga",
                                  molecular_profile_id = "mpnst_mskcc_mutations",
                                  data_type = data_type), "Molecular profile*")


  # fusions don't exist
  expect_error(.get_data_by_study(
    study_id = "acc_tcga",
    molecular_profile_id = "acc_tcga_fusions", data_type = "fusion"), "Molecular profile*")


})

test_that("Hugo Symbol is added by default", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  set_cbioportal_db("public")
  df <- get_genetics_by_study(study_id = "acc_tcga")

  expect_true(length(df$mutation$hugoGeneSymbol) > 1)
  expect_true(length(df$cna$hugoGeneSymbol) > 1)


})

test_that("`add_hugo` = FALSE doesn't add column if it's not there", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  set_cbioportal_db("public")
  df_hugo_false <- get_genetics_by_study(study_id = "prad_msk_2019", add_hugo = FALSE)

  df_hugo_true <- get_genetics_by_study(study_id = "prad_msk_2019", add_hugo = TRUE)

  expect_true(setdiff(names(df_hugo_true$mutation), names(df_hugo_false$mutation)) == "hugoGeneSymbol")
  expect_true(setdiff(names(df_hugo_true$cna), names(df_hugo_false$cna)) == "hugoGeneSymbol")
  expect_true(length(setdiff(names(df_hugo_true$structural_variant), names(df_hugo_false$structural_variant))) == 0)

})

test_that("data is same regardless of function", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  molecular_profile_id = "mpnst_mskcc_mutations"
  study_id = "mpnst_mskcc"
  get_gen <- get_genetics_by_study(study = study_id)

  # Mutation
  molecular_profile_id = "mpnst_mskcc_mutations"

  by_study <- get_mutations_by_study(study = study_id)
  by_prof <- get_mutations_by_study(molecular_profile_id = molecular_profile_id)
  expect_identical(by_study, by_prof, get_gen$mutation)

  # CNA ----
  molecular_profile_id = "mpnst_mskcc_cna"
  by_study <- get_cna_by_study(study = study_id)
  by_prof <- get_cna_by_study(molecular_profile_id = molecular_profile_id)
  expect_identical(by_study, by_prof, get_gen$cna)

  # Fusions ----
  molecular_profile_id = "mpnst_mskcc_structural_variants"
  by_study <- get_fusions_by_study(study = study_id)
  by_study2 <- get_structural_variants_by_study(study = study_id)
  by_prof <- get_fusions_by_study(molecular_profile_id = molecular_profile_id)
  by_prof2<- get_structural_variants_by_study(molecular_profile_id = molecular_profile_id)
  expect_identical(by_study, by_study2, by_prof, by_prof2, get_gen$structural_variant)

  # Segmentation ----
  study_id = "acc_tcga"
  get_gen2 <- get_genetics_by_study(study_id)
  by_study <- get_seg_by_study(study = study_id)
  expect_identical(by_study,  get_gen2$segment)

})

test_that("get_genetics- one data type non existant", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  study_id = "acc_tcga"

  # throws message and returns others
  expect_message(
    get_genetics_by_study(study = study_id), "*")

})




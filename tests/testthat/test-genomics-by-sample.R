
test_that("Test study_id and Profile Param", {

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
  expect_message(.get_data_by_sample(
    study_id = "acc_tcga",
    molecular_profile_id = "acc_tcga_mutations",
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), "*")

  # HERE
  # study_id = incorrect, profile = correct ~ Throw message ignoring study_id
  expect_message(.get_data_by_sample(
    study_id = "not_here",
    molecular_profile_id = "acc_tcga_mutations",
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), "You have passed*")


  # study_id = NULL, profile = correct - WORKS - guesses study ID
  expect_error(.get_data_by_sample(
    study_id = NULL,
    molecular_profile_id = "acc_tcga_mutations",
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), NA)

  # study_id = correct, profile = incorrect -FAIL Informative Error
  expect_error(.get_data_by_sample(
    study_id = "acc_tcga",
    molecular_profile_id = "nope",
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), "Molecular*")

  #study_id = incorrect, profiles = incorrect - gives informative error
  expect_error(.get_data_by_sample(
    study_id = "blah",
    molecular_profile_id = "wrong",
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), "Molecular profile*")

  # study ID = correct, profile = NULL - gives informative error
  expect_error(.get_data_by_sample(
    study_id = NULL,
    molecular_profile_id = "blah",
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), "Molecular*")

  # study ID = correct, profile = NULL -WORKS- looks up profile ID
  expect_error(.get_data_by_sample(
    study_id = "acc_tcga",
    molecular_profile_id = NULL,
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), NA)

  # study_id = incorrect, profile = NULL **API fail error (could be more informative)
  expect_error(.get_data_by_sample(
    study_id = "not_here",
    molecular_profile_id = NULL,
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), "API*")

  # study_id = NULL, profile = NULL ~ uses default study
  expect_error(.get_data_by_sample(
    study_id = NULL,
    molecular_profile_id = NULL,
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    data_type = data_type), NA)


  # Other-------------

  # no sample IDs passed
  expect_error(.get_data_by_sample(
    study_id = "acc_tcga",
    molecular_profile_id = NULL,
    data_type = data_type), "You must pass*")


  # study_id > 1  passed
  expect_error(.get_data_by_sample(
    study_id = c("acc_tcga", "tt"),
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    molecular_profile_id = NULL,
    data_type = data_type), "More*")


  # both correct, but bad base URL
  expect_error(.get_data_by_sample(study_id = "acc_tcga",
                                  molecular_profile_id = "acc_tcga_mutations",
                                  sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
                                  data_type = data_type, base_url = "plunk"), "API*")

  # both exist but mismatched- ignores study ID but works
  expect_error(.get_data_by_sample(study_id = "acc_tcga",
                                  molecular_profile_id = "mpnst_mskcc_mutations",
                                  sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
                                  data_type = data_type), NA)


  # fusions don't exist
  expect_error(.get_data_by_sample(
    study_id = "acc_tcga",
    sample_id = c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
    molecular_profile_id = "acc_tcga_fusions", data_type = "fusion"), "Molecular profile*")
})



test_that("Test sample-study pairs df", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  data_type = "mutation"

  # works ---
  df_pairs <- data.frame(
  "sample_id" = c("P-0002146-T01-IM3", "s_C_CAUWT7_P001_d"),
   "study_id" = c("blca_plasmacytoid_mskcc_2016", "prad_msk_2019"))

  expect_error(.get_data_by_sample(sample_study_pairs = df_pairs,
                      data_type = data_type), NA)

  # doesn't work (for now) ---
  df_pairs <- data.frame(
    "sample_id" = c("P-0002146-T01-IM3", "s_C_CAUWT7_P001_d"),
    "molecular_profile_id" = c("blca_plasmacytoid_mskcc_2016_mutations", "prad_msk_2019_mutations"))

  expect_error(.get_data_by_sample(sample_study_pairs = df_pairs,
                                   data_type = "fusion"), "*")


  # need colnames ---
  df_pairs <- data.frame(
    "wrong" = c("P-0002146-T01-IM3", "s_C_CAUWT7_P001_d"),
    "study_id" = c("blca_plasmacytoid_mskcc_2016", "prad_msk_2019"))


  expect_error(.get_data_by_sample(sample_study_pairs = df_pairs,
                                   data_type = data_type), "*")

  # need colnames ---
  df_pairs <- data.frame(
    "sample_id" = c("P-0002146-T01-IM3", "s_C_CAUWT7_P001_d"),
    "wrong" = c("blca_plasmacytoid_mskcc_2016", "prad_msk_2019"))

  expect_error(.get_data_by_sample(sample_study_pairs = df_pairs,
                                   data_type = data_type), "*")


})

test_that("data is same regardless of function", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  sample_id = c("s_C_03LNU8_P001_d", "s_C_36924L_P001_d", "s_C_CAUWT7_P001_d")

#  molecular_profile_id = "mpnst_mskcc_mutations"
  study_id = "prad_msk_2019"
  get_gen <- get_genetics_by_sample(sample_id = sample_id,
                                    study_id = study_id)

  # Mutation
  molecular_profile_id = "prad_msk_2019_mutations"

  by_study <- get_mutations_by_sample(sample_id = sample_id, study_id = study_id)
  by_prof <- get_mutations_by_sample(sample_id = sample_id, molecular_profile_id = molecular_profile_id)
  expect_identical(by_study, by_prof, get_gen$mut)

  # CNA ---
  molecular_profile_id = "prad_msk_2019_cna"
  by_study <- get_cna_by_sample(sample_id = sample_id, study_id = study_id)
  by_prof <- get_cna_by_sample(sample_id = sample_id, molecular_profile_id = molecular_profile_id)
  expect_identical(by_study, by_prof, get_gen$cna)

  # Fusions ---
  molecular_profile_id = "prad_msk_2019_fusion"
  by_study <- get_fusions_by_sample(sample_id = sample_id, study_id = study_id)
  by_prof <- get_fusions_by_sample(sample_id = sample_id, molecular_profile_id = molecular_profile_id)
  expect_identical(by_study, by_prof, get_gen$fusion)

})


test_that("Unknown Hugo Symbol returns Unk ", {

  set_cbioportal_db("public")
  df <- get_cna_by_sample(sample_id =c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
                          study_id = "acc_tcga")
  df[16, ] <- df[15,]
  df[16, 'entrezGeneId'] <- 1000000

  df <- df %>% select(-.data$hugoGeneSymbol)
  df2 <- .lookup_hugo(df)

  expect_true(any(stringr::str_detect(df2$hugoGeneSymbol, "unk")))


})

test_that("Hugo Symbol is added by default ", {

  set_cbioportal_db("public")
  df <- get_genetics_by_sample(sample_id =c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
                          study_id = "acc_tcga")

  expect_true(length(df$mutation$hugoGeneSymbol) > 1)
  expect_true(length(df$cna$hugoGeneSymbol) > 1)


})

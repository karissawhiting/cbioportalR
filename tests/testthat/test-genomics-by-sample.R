# Tests core `.get_data_by_sample` (and genomics-specific tests) -----------------

test_that("Test study_id and Profile Param", {

skip_on_cran()
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
    molecular_profile_id = "acc_tcga_structural_variants", data_type = "fusion"), "Molecular profile*")
})



test_that("Test sample-study pairs df", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

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


  # approximate colnames work ---
  sample_study_pairs <- data.frame(
    "SAMPLE ID" = c("P-0002146-T01-IM3", "s_C_CAUWT7_P001_d"),
    "studyID" = c("blca_plasmacytoid_mskcc_2016", "prad_msk_2019"),
    "moLEcularProfile ID" = c("blca_plasmacytoid_mskcc_2016_mutations", "prad_msk_2019_mutations"))

  expect_error(.get_data_by_sample(sample_study_pairs = sample_study_pairs,
                                   data_type = data_type), NA)

  #additional columns ignored  ---
  sample_study_pairs <- data.frame(
    "SAMPLE ID" = c("P-0002146-T01-IM3", "s_C_CAUWT7_P001_d"),
    "studyID" = c("blca_plasmacytoid_mskcc_2016", "prad_msk_2019"),
    "moLEcularProfile ID" = c("blca_plasmacytoid_mskcc_2016_mutations", "prad_msk_2019_mutations"),
    "rando" = c("h", "i"))

  expect_error(.get_data_by_sample(sample_study_pairs = sample_study_pairs,
                                   data_type = data_type), NA)

})

test_that("data is same regardless of function", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

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
  molecular_profile_id = "prad_msk_2019_structural_variants"
  by_study <- get_fusions_by_sample(sample_id = sample_id, study_id = study_id)
  by_study2 <- get_structural_variants_by_sample(sample_id = sample_id, study_id = study_id)
  by_prof <- get_fusions_by_sample(sample_id = sample_id, molecular_profile_id = molecular_profile_id)
  by_prof2 <- get_structural_variants_by_sample(sample_id = sample_id, molecular_profile_id = molecular_profile_id)
  expect_identical(by_study, by_study2, by_prof, by_prof2, get_gen$fusion)

})


test_that("Unknown Hugo Symbol returns Unk ", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

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

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  set_cbioportal_db("public")
  df <- get_genetics_by_sample(sample_id =c("TCGA-OR-A5J2-01","TCGA-OR-A5J6-01"),
                          study_id = "acc_tcga")

  expect_true(length(df$mutation$hugoGeneSymbol) > 1)
  expect_true(length(df$cna$hugoGeneSymbol) > 1)


})

test_that("Returns same results as pulling by study ID ", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  set_cbioportal_db("public")
  all <- available_samples("blca_plasmacytoid_mskcc_2016")
  resolved_genes <- cbioportalR::impact_gene_info$entrez_id %>% unlist()
  x <- .get_data_by_sample(sample_id = all$sampleId,
                           study_id = "blca_plasmacytoid_mskcc_2016", data_type = "cna")

  y <-.get_data_by_sample(sample_id = all$sampleId,
                          study_id = "blca_plasmacytoid_mskcc_2016", data_type = "cna")

  resolved_genes <- cbioportalR::impact_gene_info$entrez_id %>% unlist()

  z <-.get_data_by_sample(sample_id = all$sampleId,
                          study_id = "blca_plasmacytoid_mskcc_2016", data_type = "cna",
                          genes = resolved_genes)
  expect_true(length(setdiff(x$hugoGeneSymbol, z$hugoGeneSymbol)) != 0)


})


test_that("test entrez ID to hugo symbol in get_xx_by_sample functions", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))
  set_cbioportal_db("public")

  # get all genes returned for this study
  all_genes <- get_genetics_by_study(study_id = "blca_plasmacytoid_mskcc_2016")

  # try to pull genetics by sample using entrez IDs
  # get study ID-sample ID pairs
  s1 <- available_samples("blca_plasmacytoid_mskcc_2016") %>%
    transmute(sample_id = sampleId, study_id = studyId)

  all_genomic_entrez <- get_genetics_by_sample(sample_study_pairs = s1,
                                               genes = all_genes$mutation$entrezGeneId)

  # convert relevant entrez IDs to hugo symbols
  entrez_to_hugo <- get_hugo_symbol(all_genes$mutation$entrezGeneId)

  # try to pull genetics by sample using the converted Hugo symbols
  all_genomic_hugo <- get_genetics_by_sample(sample_study_pairs = s1,
                                             genes = entrez_to_hugo$hugoGeneSymbol)

  expect_equal(all_genomic_entrez$mutation, all_genomic_hugo$mutation)
  expect_equal(all_genomic_entrez$cna, all_genomic_hugo$cna)
  expect_equal(all_genomic_entrez$fusion, all_genomic_hugo$fusion)
})


test_that("pulling with gene ID (entrez or hugo) works with no error", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))
  set_cbioportal_db("public")

  genes = get_entrez_id(c("ERBB2", "PIK3C2G",
                           "CDKN1A", "EPHA2", "NOTCH2"))

  s1 <- available_samples("blca_plasmacytoid_mskcc_2016") %>%
    transmute(sample_id = sampleId, study_id = studyId)

  gen_by_entrez <- get_genetics_by_sample(sample_study_pairs = s1,
                                               genes = genes$entrezGeneId)

  gen_by_hugo <- get_genetics_by_sample(sample_study_pairs = s1,
                                               genes =  genes$hugoGeneSymbol)


  expect_true(identical(gen_by_entrez$mutation, gen_by_hugo$mutation))
  expect_true(identical(gen_by_entrez$cna, gen_by_hugo$cna))
  expect_true(identical(gen_by_entrez$fusion, gen_by_hugo$fusion))

})

test_that("pulling with panel ID works with no error and matches pull by gene", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))
  set_cbioportal_db("public")

  s1 <- available_samples("blca_plasmacytoid_mskcc_2016") %>%
    transmute(sample_id = sampleId, study_id = studyId)

  expect_error(gen_by_panel <- get_genetics_by_sample(sample_study_pairs = s1,
                                          panel = "IMPACT468"), NA)

  genes <- get_gene_panel("IMPACT468")

  gen_by_entrez <- get_genetics_by_sample(sample_study_pairs = s1,
                                          genes = genes$entrezGeneId)


  expect_true(identical(gen_by_panel$mutation, gen_by_entrez$mutation))
  expect_true(identical(gen_by_panel$cna, gen_by_entrez$cna))
  expect_true(identical(gen_by_panel$fusion, gen_by_entrez$fusion))

})

test_that("pulling with panel ID works with no error and matches pull by gene", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))
  set_cbioportal_db("public")

  s1 <- available_samples("blca_plasmacytoid_mskcc_2016") %>%
    dplyr::transmute(sample_id = sampleId, study_id = studyId)

  # error if misspecified panel
  expect_error(get_mutations_by_sample(sample_study_pairs = s1,
                                      panel = "ttt"), "*")

  # no error if correct panel
  expect_error(gen_by_panel <- get_genetics_by_sample(sample_study_pairs = s1,
                                                      panel = "IMPACT468"), NA)

  genes <- get_gene_panel("IMPACT468")

  gen_by_entrez <- get_genetics_by_sample(sample_study_pairs = s1,
                                          genes = genes$entrezGeneId)


  expect_true(identical(gen_by_panel$mutation, gen_by_entrez$mutation))
  expect_true(identical(gen_by_panel$cna, gen_by_entrez$cna))
  expect_true(identical(gen_by_panel$fusion, gen_by_entrez$fusion))

})

test_that("pull by panel ID + gene IDs", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))
  set_cbioportal_db("public")

  s1 <- available_samples("blca_plasmacytoid_mskcc_2016") %>%
    dplyr::transmute(sample_id = sampleId, study_id = studyId)

  plus_gene <- "NOTCH4"
  plus_entrez = get_entrez_id(plus_gene)$entrezGeneId

  expect_error(gen_by_panel <- get_genetics_by_sample(sample_study_pairs = s1,
                                                      panel = "sarc_mskcc_panel", genes = plus_gene), NA)

  expect_error(gen_by_panel2 <- get_genetics_by_sample(sample_study_pairs = s1,
                                                      panel = "sarc_mskcc_panel", genes = plus_entrez), NA)

  expect_error(gen_by_panel3 <- get_genetics_by_sample(sample_study_pairs = s1,
                                                       panel = "sarc_mskcc_panel"), NA)

  expect_true(identical(gen_by_panel$mutation, gen_by_panel2$mutation))
  expect_true(identical(gen_by_panel$cna, gen_by_panel2$cna))
  expect_true(identical(gen_by_panel$fusion, gen_by_panel2$fusion))

  expect_equal(setdiff(gen_by_panel$mutation$hugoGeneSymbol,
          gen_by_panel3$mutation$hugoGeneSymbol), plus_gene)


})

test_that("pull by two panel IDs", {

skip_on_cran()
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))
  set_cbioportal_db("public")

  s1 <- c("DS-sig-010-P2", "DS-sig-010-P1", "DS-sig-018-P")
  panels <- c("IMPACT468", "sarc_mskcc_panel")

  ids <- get_gene_panel(panels) %>%
    dplyr::pull(entrezGeneId) %>%
    unique()

  expect_error(gen_by_panel <- get_mutations_by_sample(sample_id = s1,
                                                      study_id = "blca_plasmacytoid_mskcc_2016",
                                                      panel =  c("IMPACT468", "sarc_mskcc_panel"),
                                                      genes = "NOTCH2"), NA)

  expect_error(gen_by_panel2 <- get_mutations_by_sample(sample_id = s1,
                                                      study_id = "blca_plasmacytoid_mskcc_2016",
                                                      panel =  c("sarc_mskcc_panel"),
                                                      genes = "NOTCH2"), NA)


  expect_true(length(gen_by_panel$hugoGeneSymbol) > length(gen_by_panel2$hugoGeneSymbol))


})



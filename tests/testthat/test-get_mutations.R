
genes <- impact_gene_info$entrez_id %>% unlist()
test_base_url <- "www.cbioportal.org/api"

# Test determine base URL ------------------------

test_that("determine base url", {
  expect_equal(.determine_base_url("www.cbioportal.org/api"),
               "www.cbioportal.org/api")
  })

# Test pull by sample ID -------------------------------------------------------
test_that("pull mutation by sample- works with default study_id", {

  genes <- impact_gene_info$entrez_id %>% unlist()
  test_base_url <- "www.cbioportal.org/api"

  # one sample
  expect_warning(

    one_samp <- .get_mutations_by_sample_id(
      sample_id =
        c("P-0000065-T01-IM3"),
      genes = genes,
      base_url = "www.cbioportal.org/api"
    )
  )

  # two samples
  expect_warning(
    two_samp <- .get_mutations_by_sample_id(
      sample_id =
        c("P-0000065-T01-IM3", "P-0000113-T01-IM3"),
      genes = genes,
      base_url = test_base_url
    )
  )

  # three samples
  expect_warning(
    three_samp <- .get_mutations_by_sample_id(
      sample_id =
        c("P-0000065-T01-IM3", "P-0000113-T01-IM3", "P-0000145-T01-IM3"),
      genes = genes,
      base_url = test_base_url
    )
  )


  expect_true(
    nrow(one_samp) < nrow(two_samp),
    nrow(two_samp) < nrow(three_samp)
  )
})


# test_that("pull mutation by sample- with study blca_tcga", {
#
#   ids_to_test <-  c("TCGA-BL-A13I-01", "TCGA-GV-A3JZ-01")
#
#   bladder_samp <- .get_mutations_by_sample_id(
#     sample_id = ids_to_test,
#     study_id = "blca_tcga",
#     genes = genes,
#     base_url = test_base_url
#   )
#
#   expect_true(nrow(bladder_samp) > 1)
#
#   expect_equal(
#     unique(bladder_samp$studyId), "blca_tcga"
#   )
#
#
#   expect_equal(
#     unique(bladder_samp$sampleId), ids_to_test
#   )
#
#
# })
#
#
# test_that("pull mutation by sample- check errors", {
#
#   ids_to_test <-  c("TCGA-BL-A13I-01", "TCGA-GV-A3JZ-01")
#
#   expect_error(bladder_samp <- .get_mutations_by_sample_id(
#     sample_id = NULL,
#     study_id = "blca_tcga",
#     genes = genes,
#     base_url = test_base_url
#   ))
#
#   # non impact study IDs but no study specified
#   expect_error(
#     bladder_samp <- .get_mutations_by_sample_id(
#       sample_id = ids_to_test,
#       study_id = NULL,
#       genes = genes,
#       base_url = test_base_url
#   ),
#   "All non-IMPACT samples passed with no default `study_id`")
#
#
#
# })
#
# # Test pull by study ID -------------------------------------------------------
#
# test_that("pull mutation by study- error when no study ID", {
#   base_url <- test_base_url
#
#   expect_error(
#
#     one_samp <- .get_mutations_by_study_id(
#       base_url = test_base_url
#     ),
#     "You must provide a study id"
#   )
#
#   # check return by study
#
#   study_test <- "nhl_bcgsc_2011"
#
#   study_mut <- .get_mutations_by_study_id(
#     study_id = study_test
#   )
#
#   expect_equal(
#     unique(study_mut$studyId), study_test
#   )
#
#
# })
#

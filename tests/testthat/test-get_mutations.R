

# Mutations By Study ID -------------------------------------------------------
test_that("test get mutations by study id", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  x <- get_mutation_by_study(study_id = "acc_tcga")

  # res <- expect_error(purrr::map(endpoint_funs,
  #                                function(fn) rlang::exec(fn)), NA)
  #
  # expect_equal(names(res), names(endpoint_funs))

})

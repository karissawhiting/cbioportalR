
# No Parameter Endpoints -------------------------------------------------------
test_that("test endpoints- no parameters works without error", {

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

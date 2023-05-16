
# No Parameter Endpoints -------------------------------------------------------
test_that("test endpoints- no parameters works without error", {
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

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


# Test lookup_id() -------------------------------------------------------
test_that("test lookup_id- works without error", {
  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  lookup_id <- c("P-0001453-T01-IM3", "P-0000004-T01-IM3", "TCGA-OR-A5JA")

  expect_no_error( x <- lookup_id(lookup_id = lookup_id,  base_url = 'www.cbioportal.org/api'))
  expect_no_error( y <- lookup_id(lookup_id = c(lookup_id, "wrong"),
                                  base_url = 'www.cbioportal.org/api'))

  expect_equal(x, y)

})

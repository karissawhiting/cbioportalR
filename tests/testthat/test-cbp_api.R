

# No Parameter Endpoints -------------------------------------------------------
test_that("test main API function", {

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  res <- cbp_api(url_path = "genes/TP53", base_url = "public")

  expect_equal(res$response$status_code, 200)
  clean_res <- as.data.frame(res$content)

  expect_equal(names(clean_res), c("entrezGeneId", "hugoGeneSymbol", "type"))
})



# No Parameter Endpoints -------------------------------------------------------
test_that("test main API function", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  res <- cbp_api(url_path = "genes/TP53", base_url = "public")

  expect_equal(res$response$status_code, 200)
  clean_res <- as.data.frame(res$content)

  expect_equal(names(clean_res), c("entrezGeneId", "hugoGeneSymbol", "type"))
})


test_that("test quiet arg", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)
  expect_output(
    cbp_api(url_path = "genes/TP53",
            base_url = "public",
                 quiet = FALSE),
    "https://www.cbioportal.org/api/genes/TP53")

})

test_that("test extra_box arg", {

  skip_on_cran()
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  url_path = paste0("genes/fetch?geneIdType=HUGO_GENE_SYMBOL")
  body <- c("FGFR1", "TP53")

  expect_error(cbp_api(url_path,
                 method = "post",
                 body = body,
                 extra_box = TRUE,
                 base_url = NULL), "*")

  expect_error(cbp_api(url_path,
                       method = "post",
                       body = body,
                       extra_box = FALSE,
                       base_url = NULL), NA)

  x <- cbp_api(url_path,
                method = "post",
                body = body,
                extra_box = FALSE,
                base_url = NULL)

  expect_error(print.cbp_api(x), NA)


})


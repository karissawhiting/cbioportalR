


test_that("set url- public", {
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <- "public"
  set_cbioportal_db(db = db_test)

  x <- get("portal_url", envir = cbioportal_env)
  expect_equal(x, "www.cbioportal.org/api")

})

test_that("set url-  add api", {
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <-"www.cbioportal.org"
  set_cbioportal_db(db = db_test)

  x <- get("portal_url", envir = cbioportal_env)
  expect_equal(x, paste0(stringr::str_remove(db_test, "https://"), "/api"))

})


test_that("set url-nothing passed", {
  skip_if(httr::http_error("www.cbioportal.org/api"))

  db_test <-NULL
  expect_error(set_cbioportal_db(db = db_test), "*")

})


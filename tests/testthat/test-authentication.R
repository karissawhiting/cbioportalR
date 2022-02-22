
test_that("set url", {

  db_test <-"test"
  set_cbioportal_db(db = db_test)

  x <- get("portal_url", envir = cbioportal_env)
  expect_equal(x, paste0(db_test, "/api"))

})

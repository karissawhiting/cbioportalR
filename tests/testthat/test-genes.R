
#  Test Getting Genes -------------------------------------------------

test_that("test get genes", {

  skip_if(httr::http_error("www.cbioportal.org/api"))

  set_cbioportal_db("public")

  expect_error(
    get_entrez_id(),
    "Must specify at least one*")

  expect_error(
    get_hugo_symbol(),
    "Must specify at least one*")

  expect_error(
    get_hugo_symbol(),
    "Must specify at least one*")

  # capital and lowercase - becomes capital (this actually happens in the API itself)
  expect_equal(
    get_entrez_id(c("tp53", "FGFR3"))$hugoGeneSymbol,
    get_hugo_symbol(get_entrez_id(c("tp53", "FGFR3"))$entrezGeneId)$hugoGeneSymbol)

  # single or multiple args work
  expect_equal(
    get_entrez_id(c("APC"))$hugoGeneSymbol,
    get_hugo_symbol(get_entrez_id(c("APC"))$entrezGeneId)$hugoGeneSymbol)

  # passing character as entrez(also happens in API)
  x <- get_hugo_symbol(324)
  y <- get_hugo_symbol("324")
  expect_equal(x, y)

  # get alias
  expect_true("MLL2" %in% get_alias("KMT2D")$alias)



})

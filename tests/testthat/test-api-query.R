test_that("URL created correctly with a search parameter", {
  search_url <- paste0(BASE_URL, sprintf("/%s/search", "domestic"))
  output <- create_search_url(search_url,
                              parameters = c(`energy-band` = "b"))
  expect_identical(output,
                   "https://epc.opendatacommunities.org/api/v1/domestic/search?energy-band=b")
})

test_that("URL created correctly with multiple search parameters", {
  search_url <- paste0(BASE_URL, sprintf("/%s/search", "domestic"))
  output <- create_search_url(search_url,
                              parameters = c(`energy-band` = "b",
                                             address = "London",
                                             postcode = "SW1A"))
  expect_identical(output,
                   "https://epc.opendatacommunities.org/api/v1/domestic/search?energy-band=b&address=London&postcode=SW1A")
})

test_that("search URL returned where no query parameters are set", {
  search_url <- paste0(BASE_URL, sprintf("/%s/search", "domestic"))
  output <- create_search_url(search_url)
  expect_identical(output, search_url)
})

test_that("EPC data query returns a response", {

  skip_on_ci()
  skip_if_offline()
  skip_if(Sys.getenv("EPC_API_KEY") == "")

  output <- query_epc_data("https://epc.opendatacommunities.org/api/v1/domestic/certificate/219873319402019053122194154717408")

  expect_type(output, "list")
  expect_named(output, c("response", "content"))
  expect_s3_class(output[["response"]], "response")
  expect_s3_class(output[["content"]][["rows"]], "data.frame")

})

test_that("only valid filters are accepted", {

  parameters <- list(address = "10 The High Street",
                     `local-authority` = "E000000000")

  expect_null(validate_query(parameters))

})

test_that("invalid filters raise an error", {

  parameters <- list(address = "10 The High Street",
                     ward = "Riverside")

  expect_error(validate_query(parameters),
               "Query parameter not recognised.\nSee https://epc.opendatacommunities.org/docs/api for valid filters.")

})

test_that("data types are set correctly", {

  input <- data.frame(lmk_key = "12345",
                      multi_glaze_proportion = "23",
                      lodgement_datetime = "2021-03-03 15:41:33",
                      yr1_renewables_co2 = "9.3",
                      or_assessment_end_date = "2021-02-01")
  output <- set_data_types(input)

  expect_type(output[["lmk_key"]], "character")
  expect_type(output[["multi_glaze_proportion"]], "integer")
  expect_type(output[["yr1_renewables_co2"]], "double")
  expect_s3_class(output[["lodgement_datetime"]], "POSIXct")
  expect_s3_class(output[["or_assessment_end_date"]], "Date")

})

test_that("a warning is thrown for unrecognised column headers", {

  input <- data.frame(lmk_key = "12345",
                      multi_glaze_proportion = "23",
                      lodgement_datetime = "2021-03-03 15:41:33",
                      yr1_renewables_co2 = "9.3",
                      or_assessment_end_date = "2021-02-01",
                      unknown_field = "")

  expect_warning(output <- set_data_types(input))
  expect_type(output[["unknown_field"]], "character")

})

test_that("response object is processed and cleaned correctly", {

  input <- list(response = structure(
    list(url = "https://epc.opendatacommunities.org/api/v1"),
    class = "response"
  ),
  content = list(
    rows = data.frame(`lmk-key` = "12345",
                      `multi-glaze-proportion` = "23",
                      `lodgement-datetime` = "2021-03-03 15:41:33",
                      `yr1-renewables-co2` = "9.3",
                      `or-assessment-end-date` = "2021-02-01",
                      check.names = FALSE),
    `column-names` = c("lmk-key", "multi-glaze-proportion", "lodgement-datetime",
                       "yr1-renewables-co2", "or-assessment-end-date")
  ),
  url = url)

  output <- process_response(input)

  expect_s3_class(output, "epc_api")
  expect_named(output, c("content", "response"))
  expect_equal(output[["response"]][["url"]],
               "https://epc.opendatacommunities.org/api/v1")
  expect_named(output[["content"]],
               c("lmk_key", "multi_glaze_proportion", "lodgement_datetime",
                 "yr1_renewables_co2", "or_assessment_end_date"))

})


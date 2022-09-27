test_that("get_lmk_key runs query with correct URL and returns expected output", {

  local({

    mockr::local_mock(
      query_epc_data = function(url) {
        list(response = structure(list(url = url), class = "response"),
             content = list(
               rows = data.frame(`lmk-key` = "12345",
                                 `multi-glaze-proportion` = "23",
                                 `lodgement-datetime` = "2021-03-03 15:41:33",
                                 `yr1-renewables-co2` = "9.3",
                                 `or-assessment-end-date` = "2021-02-01",
                                 check.names = FALSE),
               `column-names` = c("lmk-key", "multi-glaze-proportion", "lodgement-datetime",
                                  "yr1-renewables-co2", "or-assessment-end-date")
             )
        )
      }
    )

    output <- get_data_for_lmk_key("display", "thing", "12345")

    expect_s3_class(output, "epc_api")
    expect_named(output, c("content", "response"))
    expect_equal(output[["response"]][["url"]],
                 "https://epc.opendatacommunities.org/api/v1/display/thing/12345")

  })

})

test_that("get_epc_certifcate correctly constructs URL", {

  local({

    mockr::local_mock(
      query_epc_data = function(url) {
        list(response = structure(list(url = url), class = "response"),
             content = list(
               rows = data.frame(`lmk-key` = "12345",
                                 `multi-glaze-proportion` = "23",
                                 `lodgement-datetime` = "2021-03-03 15:41:33",
                                 `yr1-renewables-co2` = "9.3",
                                 `or-assessment-end-date` = "2021-02-01",
                                 check.names = FALSE),
               `column-names` = c("lmk-key", "multi-glaze-proportion", "lodgement-datetime",
                                  "yr1-renewables-co2", "or-assessment-end-date")
             )
        )
      }
    )

    output <- get_epc_certificate("domestic", "12345")

    expect_s3_class(output, "epc_api")
    expect_named(output, c("content", "response"))
    expect_equal(output[["response"]][["url"]],
                 "https://epc.opendatacommunities.org/api/v1/domestic/certificate/12345")

  })

})

test_that("get_epc_recommendations correctly constructs URL", {

  local({

    mockr::local_mock(
      query_epc_data = function(url) {
        list(response = structure(list(url = url), class = "response"),
             content = list(
               rows = data.frame(`lmk-key` = "12345",
                                 `multi-glaze-proportion` = "23",
                                 `lodgement-datetime` = "2021-03-03 15:41:33",
                                 `yr1-renewables-co2` = "9.3",
                                 `or-assessment-end-date` = "2021-02-01",
                                 check.names = FALSE),
               `column-names` = c("lmk-key", "multi-glaze-proportion", "lodgement-datetime",
                                  "yr1-renewables-co2", "or-assessment-end-date")
             )
        )
      }
    )

    output <- get_epc_recommendations("domestic", "12345")

    expect_s3_class(output, "epc_api")
    expect_named(output, c("content", "response"))
    expect_equal(output[["response"]][["url"]],
                 "https://epc.opendatacommunities.org/api/v1/domestic/recommendations/12345")

  })

})

test_that("search_epc_data returns expected epc_api object", {

  local({

    mockr::local_mock(
      query_epc_data = function(url) {
        list(response = structure(list(url = url), class = "response"),
             content = list(
               rows = data.frame(`lmk-key` = "12345",
                                 `multi-glaze-proportion` = "23",
                                 `lodgement-datetime` = "2021-03-03 15:41:33",
                                 `yr1-renewables-co2` = "9.3",
                                 `or-assessment-end-date` = "2021-02-01",
                                 check.names = FALSE),
               `column-names` = c("lmk-key", "multi-glaze-proportion", "lodgement-datetime",
                                  "yr1-renewables-co2", "or-assessment-end-date")
             )
        )
      }
    )

    output <- search_epc_data("non-domestic")

    expect_s3_class(output, "epc_api")
    expect_named(output, c("content", "response"))
    expect_equal(output[["response"]][["url"]],
                 "https://epc.opendatacommunities.org/api/v1/non-domestic/search")

  })

})

test_that("search_epc_data returns response object with paginated query", {

  local({

    mockr::local_mock(

      query_epc_data = function(url) {

        size <- 50

        if (!grepl("from", url, fixed = TRUE)) {
          idx <- 1
        } else if (grepl("from=50", url, fixed = TRUE)) {
          idx <- 51
        } else {
          return(
            list(response = structure(list(url = url),
                                      class = "response"),
                 content = list(
                   rows = data.frame(),
                   `column-names` = c("lmk-key",
                                      "multi-glaze-proportion",
                                      "lodgement-datetime",
                                      "yr1-renewables-co2",
                                      "or-assessment-end-date",
                                      "page")
                 )
            )
          )
        }

        idx_range <- idx:(idx + size - 1)

        list(response = structure(list(url = url), class = "response"),
             content = list(
               rows = data.frame(`lmk-key` = rep("12345", 100)[idx_range],
                                 `multi-glaze-proportion` = rep("23", 100)[idx_range],
                                 `lodgement-datetime` = rep("2021-03-03 15:41:33", 100)[idx_range],
                                 `yr1-renewables-co2` = rep("9.3", 100)[idx_range],
                                 `or-assessment-end-date` = rep("2021-02-01", 100)[idx_range],
                                 check.names = FALSE),
               `column-names` = c("lmk-key", "multi-glaze-proportion", "lodgement-datetime",
                                  "yr1-renewables-co2", "or-assessment-end-date")
             ),
             url = url)

      }

    )

    output <- search_epc_data("non-domestic", size = 50, paginated = TRUE)

    expect_s3_class(output, "epc_api")
    expect_named(output, c("content", "response"))
    expect_length(output[["response"]], 2)
    expect_equal(output[["response"]][["page_1"]][["url"]],
                 "https://epc.opendatacommunities.org/api/v1/non-domestic/search?size=50")
    expect_equal(output[["response"]][["page_2"]][["url"]],
                 "https://epc.opendatacommunities.org/api/v1/non-domestic/search?size=50&from=50")
    expect_s3_class(output[["content"]], "data.frame")
    expect_named(output[["content"]],
                 c("lmk_key", "multi_glaze_proportion", "lodgement_datetime",
                   "yr1_renewables_co2", "or_assessment_end_date", "page"))

    expect_identical(output[["content"]],
                     dplyr::tibble(lmk_key = rep("12345", 100),
                                   multi_glaze_proportion = rep(23L, 100),
                                   lodgement_datetime = rep(as.POSIXct("2021-03-03 15:41:33"), 100),
                                   yr1_renewables_co2 = rep(9.3, 100),
                                   or_assessment_end_date = rep(as.Date("2021-02-01"), 100),
                                   page = c(rep(1, 50), rep(2, 50))))

  })

})

test_that("error thrown if page size of more than 5000 set", {

  expect_error(
    search_epc_data("domestic", size = 5001),
    "Specified page size is greater than maximum of 5,000"
  )

})

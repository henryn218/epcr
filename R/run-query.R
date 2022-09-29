#' Search EPC data
#'
#' Queries the EPC API to retrieve EPC data filtered for user-specified
#' parameters.
#'
#' Note that this function will prompt you for a valid API key. You will need a registered account before you can
#' access the data. See the \href{https://epc.opendatacommunities.org/docs/api}{documentation} for further guidance.
#'
#' @param record_type One of "domestic", "non-domestic" or "display"
#' @param ... Named query parameters. These are outlined in detail in the \href{https://epc.opendatacommunities.org/docs/api}{EPC API documentation}.
#'
#' Possible query parameters are:
#' \itemize{
#'  \item{\code{address}: An arbitrary search string such as 'liverpool road'}
#'  \item{\code{postcode}: An arbitrary postcode or prefix string of e.g. 'P', 'PR', 'PR8' or 'PR82EG'}
#'  \item{\code{`local-authority`}: A local authority code of the form E00000000 for England and W00000000 for Wales}
#'  \item{\code{`property-type`}: Any of 'bungalow', 'flat', 'house', 'maisonette' or 'park home'}
#'  \item{\code{`floor-area`}: Any of 'unknown', 's', 'm', 'l', 'xl', 'xxl' or 'xxxl', corresponding to size categories enumerated in the documentation}
#'  \item{\code{`energy-band`}: Any of 'a', 'b', 'c', 'd', 'e', 'f', 'g'}
#'  }
#'  If multiple values for a particular filter are required, simply specify that argument as many times as required.
#'  For example, if multiple energy bands are required: \code{search_epc_data("domestic", `energy-band` = "a", `energy-band` = "b")}.
#'
#'  The following filters are used to specify lodgement date:
#'  \itemize{
#'  \item{\code{from-month}: A numeric month identifier 1-12, to establish the start of a date range, where 1 is January and 12 is December. If no from-month parameter is supplied 1 (January) is assumed.}
#'  \item{\code{from-year}: A numeric year identifier to estalish the start of a date range between 2008 and 2022 e.g. 2015. If no year parameter is supplied 2008 is assumed.}
#'  \item{\code{to-month}: A numeric month identifier 1-12, to establish the end of a date range, where 1 is January and 12 is December. If no to-month parameter is supplied then 12 is assumed.}
#'  \item{\code{to-year}: A numeric year identifier between 2008 and 2022 e.g. 2015. If no to-year parameter is supplied then 2022 is assumed.}
#'  }
#' @param size A query parameter indicating the size of the desired result set. If \code{paginated = TRUE},
#' this indicates the desired number of results per page. The API defaults to a page size of 25 if
#' not specified.
#' @param paginated A Boolean value indicating whether a paginated result is required.
#' Note that the API can handle result sets of up to 10,000 with a maximum page size of
#' 5,000. If fewer than 5,000 results are expected, it may be easier to leave this set to
#' FALSE (the default) and specify a sufficiently large \code{size} parameter to return a non-paginated
#' result set. Otherwise, set this to TRUE and set the \code{size} argument to indicate the
#' number of results per page (up to a maximum of 5,000, which would return two pages for a
#' query returning more than 5,000 results).
#'
#' @return A list of class 'epc_api' holding a data frame containing the data and
#' a list containing API response objects for each page.
#'
#' @examples
#' \dontrun{
#' search_epc_data("domestic", postcode = "SW1A")
#' search_epc_data("domestic", postcode = "TW1 4PG", size = 2, paginated = TRUE)
#' }
#'
#' @importFrom purrr map_dfr map
#' @export
#'
search_epc_data <- function(record_type, ..., size = NULL, paginated = FALSE) {

  check_api_key()

  search_url <- paste0(BASE_URL, sprintf("/%s/search", record_type))

  dots <- list(...)

  if (!is.null(size)) {
    dots[["size"]] <- size
  } else if (paginated) {
    size <- DEFAULT_PAGE_SIZE
  }

  stopifnot(
    `Specified page size is greater than maximum of 5,000` = size <= MAX_PAGE_SIZE
  )

  resp_list <- list()
  i <- 0

  while (TRUE) {

    url <- create_search_url(search_url, dots)
    resp <- run_epc_query(url)

    if (!paginated || nrow(resp[["content"]]) == 0 && i == 0) {
      return(resp)
    }

    if (nrow(resp[["content"]]) == 0) {
      break
    }

    i <- i + 1
    resp[["content"]]["page"] <- i
    resp_list[[paste0("page_", i)]] <- resp

    from <- dots[["from"]]
    dots[["from"]] <- if (is.null(from)) size else from + size

    if (nrow(resp[["content"]]) < size || dots[["from"]] >= MAX_RESULT_SET) {
      break
    }

    if (dots[["from"]] + size > MAX_RESULT_SET) {
      dots[["size"]] <- MAX_RESULT_SET - dots[["from"]]
    }

  }

  content_df <- purrr::map_dfr(resp_list, "content")
  response_list <- purrr::map(resp_list, "response")

  structure(
    list(
      content = content_df,
      response = response_list
    ),
    class = "epc_api"
  )

}


#' Retrieves data for specific LMK key
#'
#' Queries EPC API to retrieve either recommendations or a certificate for a specific
#' LMK key. \code{get_epc_certificate} and \code{get_epc_recommendations} are wrapper
#' functions for \code{get_data_for_lmk_key} with \code{api_type} set to "certificate"
#' and "recommendations" respectively.
#'
#' Note that this function will prompt you for a valid API key. You will need a registered account before you can
#' access the data. See the \href{https://epc.opendatacommunities.org/docs/api}{documentation} for further guidance.
#'
#' @param record_type One of "domestic", "non-domestic" or "display"
#' @param api_type One of "certificate" or "recommendations"
#' @param lmk_key An LMK key corresponding to a specific EPC certifcate
#'
#' @return A list object of class 'epc_api' holding a data frame containing the data and
#' a list object containing the API response data.
#'
#' @examples
#' \dontrun{
#' get_data_for_lmk_key("domestic", "certificate", "887164559962017031316220145408873")
#' }
#'
#' @export
#'
get_data_for_lmk_key <- function(record_type, api_type, lmk_key) {

  check_api_key()

  url <- paste0(BASE_URL, sprintf("/%s/%s/%s",
                                  record_type,
                                  api_type,
                                  lmk_key))

  run_epc_query(url)

}


#' Retrieve an EPC certificate
#'
#' @examples
#' \dontrun{
#' get_epc_certificate("domestic", "887164559962017031316220145408873")
#' }
#'
#' @rdname get_data_for_lmk_key
#' @export
#'
get_epc_certificate <- function(record_type, lmk_key) {

  get_data_for_lmk_key(record_type, "certificate", lmk_key)

}


#' Retrieve EPC recommendations
#'
#' @examples
#' \dontrun{
#' get_epc_recommendations("domestic", "887164559962017031316220145408873")
#' }
#'
#' @rdname get_data_for_lmk_key
#' @export
#'
get_epc_recommendations <- function(record_type, lmk_key) {

  get_data_for_lmk_key(record_type, "recommendations", lmk_key)

}


#' Run EPC query
#'
#' @param url URL for the EPC API
#'
#' @return A list of class 'epc_api' containing a data frame holding the results of the
#' query ("contents") and a response object ("response")
#'
#' @keywords internal
#' @noRd
#'
run_epc_query <- function(url) {

  resp <- query_epc_data(url)
  process_response(resp)

}

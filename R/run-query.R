#' Search EPC data
#'
#' Queries the EPC API to retrieve EPC data filtered for user-specified
#' parameters.
#'
#' @param record_type One of "domestic", "non-domestic" or "display"
#' @param ... Named query parameters
#' @param size A query parameter indicating the size of the desired result set. If \code{paginated = TRUE},
#' this indicates the desired number of results per page. The API defaults to a page size of 25 if
#' not specified.
#' @param paginated A Boolean value indicating whether a paginated result is required.
#' Note that the API can handle result sets of up to 10,000 with a maximum page size of
#' 5,000. If fewer than 5,000 results are expected, it may be easier to leave this set to
#' FALSE (the default) and specify a sufficiently large \code{size} parameter to return a non-paginated
#' result set. Otherwise, set this to TRUE and set the \code{size} argument to to indicate the
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

    if (!paginated) {
      resp_list[[1]] <- resp
      break
    }

    if (nrow(resp[["content"]]) == 0 && i > 0) {
      break
    }

    i <- i + 1
    resp[["content"]]["page"] <- i
    resp_list[[paste0("page_", i)]] <- resp

    from <- dots[["from"]]
    dots[["from"]] <- ifelse(is.null(from), size, from + size)

    if (nrow(resp[["content"]]) < size || dots[["from"]] >= MAX_RESULT_SET) {
      break
    }

    if (dots[["from"]] + size > MAX_RESULT_SET) {
      dots[["size"]] <- MAX_RESULT_SET - dots[["from"]]
    }

  }

  content_df <- purrr::map_dfr(resp_list, "content")
  response_list <- if (paginated) {
    purrr::map(resp_list, "response")
  } else {
    resp_list[[1]][["response"]]
  }

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
#'
run_epc_query <- function(url) {

  resp <- query_epc_data(url)
  process_response(resp)

}

BASE_URL <- "https://epc.opendatacommunities.org/api/v1"
MAX_PAGE_SIZE <- 5000
DEFAULT_PAGE_SIZE <- 25
MAX_RESULT_SET <- 10000

#' Query EPC web API
#'
#' Makes an HTTP get request to retrieve EPC data from a specified URL.
#'
#' @param url URL for the EPC API including the required query string
#'
#' @return A list containing the data retrieved and API response
#'
#' @examples
#' \dontrun{
#' # Default domestic query
#' query_epc_data("https://epc.opendatacommunities.org/api/v1/domestic/search")
#' }
#'
#' @importFrom httr GET add_headers user_agent http_error status_code http_type content
#' @importFrom jsonlite fromJSON
#'
#' @export
query_epc_data <- function(url) {

  check_api_key()
  resp <- httr::GET(url,
                    httr::add_headers(Authorization = paste("Basic",
                                                            Sys.getenv("EPC_API_KEY")),
                                      Accept = "application/json"),
                    httr::user_agent("https://github.com/hamstr147/epcr"))

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "EPC API request failed [%s]:\n%s",
        httr::status_code(resp),
        httr::content(resp, as = "text")
      ),
      call. = FALSE
    )
  }

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  output_json <- httr::content(resp, "text", encoding = "UTF-8")
  output <- if (output_json %in% c("", "Resource not found.")) "Resource not found." else jsonlite::fromJSON(output_json)

  list(
    response = resp,
    content = output
  )

}

#' Create URL
#'
#' Appends a query string to a URL
#'
#' @param search_url A URL used to construct the API query
#' @param parameters A named list specifying API query parameters
#'
#' @return A URL containing a query string
#'
#' @keywords internal
#'
#' @importFrom purrr imap
#' @importFrom httr modify_url
#' @importFrom utils URLencode
#'
create_search_url <- function(search_url, parameters = NULL) {

  query <- utils::URLencode(
    paste(
      purrr::imap(parameters, ~ sprintf("%s=%s", .y, .x)),
      collapse = "&"
    )
  )

  httr::modify_url(search_url,
                   query = query)

}

#' Validate query parameters
#'
#' Checks that valid filters are applied.
#'
#' @param parameters A named list of query parameters corresponding to filters described
#' in the EPC API documentation.
#'
#' @keywords internal
#'
validate_query <- function(parameters) {

  check <- all(
    names(parameters) %in% c("address",
                             "postcode",
                             "energy-band",
                             "local-authority",
                             "constituency",
                             "property-type",
                             "floor-area",
                             "from-year",
                             "from-month",
                             "to-year",
                             "to-month",
                             "size",
                             "from")
  )

  if (!check) {
    stop("Query parameter not recognised.\nSee https://epc.opendatacommunities.org/docs/api for valid filters.",
         call. = FALSE)
  }

}

#' Function to check API key
#'
#' Checks for an environment variable called 'EPC_API_KEY'. If this is set, it returns to
#' the parent frame. If it is not set, it prompts the user to set this before continuing.
#' The function can be called interactively to reset the key using `Sys.setenv`.
#'
#' @param reset A Boolean value indicating whether or not you want to reset the
#' API key environment variable.
#'
#' @export
check_api_key <- function(reset = FALSE) {

  if (Sys.getenv("EPC_API_KEY") != "" && !reset) {
    return(invisible())
  }

  if (interactive()) {
    key <- readline("Please enter your API key and press enter: ")
  } else {
    cat("Please enter your API key and press enter: ")
    key <- readLines(con = "stdin", n = 1)
  }

  if (identical(key, "")) {
    stop("EPC API key entry failed", call. = FALSE)
  }

  Sys.setenv(EPC_API_KEY = key)

  invisible()

}

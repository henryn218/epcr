#' Function to check API key
#'
#' Checks for an environment variable called 'EPC_API_KEY'. If this is set, it returns to
#' the parent frame. If it is not set, it prompts the user to set this before continuing.
#' The function can be called interactively to reset the key.
#'
#' Note that you should use the Base64-encoded account identifier composed of your email address and api-key as
#' provided in the \href{https://epc.opendatacommunities.org/docs/api/domestic#using_this_api}{documentation}.
#'
#' @param reset A Boolean value indicating whether or not you want to reset the
#' API key environment variable.
#'
#' @export
check_api_key <- function(reset = FALSE) {

  if (require(keyring)) {
    key_found <- tryCatch(
      Sys.setenv("EPC_API_KEY" = keyring::key_get("EPC_API_KEY")),
      error = function(e) {
        if (grepl("The specified item could not be found in the keychain", e, fixed = TRUE)) FALSE
      }
    )
    if (key_found) {
      return(invisible())
    }
  }

  message("Consider storing API credentials using the 'keyring' package.")

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

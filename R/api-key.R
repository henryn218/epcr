#' Function to check

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

# Update query_epc_data function to use this

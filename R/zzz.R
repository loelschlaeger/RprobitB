#' @noRd
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  ### startup message
  msg <- paste0(
    "Thanks for using RprobitB ", utils::packageVersion("RprobitB"),
    ", happy choice modeling!\n",
    "See https://loelschlaeger.de/RprobitB for help.\n",
    "Type 'citation(\"RprobitB\")' for citing this R package."
  )
  packageStartupMessage(msg)
  invisible()
}

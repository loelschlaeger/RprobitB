.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "Thanks for using RprobitB ", packageVersion("RprobitB")), ", happy choice modeling!\n",
    "See https://loelschlaeger.github.io/RprobitB for help.\n",
    "Type 'citation(\"RprobitB\")' for citing this R package.")
  packageStartupMessage(msg)
  invisible()
}

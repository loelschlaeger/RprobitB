.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "RprobitB version ", packageVersion("RprobitB")),
    "\nType 'citation(\"RprobitB\")' for citing this R package in publications.",
    "\nSee https://github.com/loelschlaeger/RprobitB for references.")
  packageStartupMessage(msg)
  invisible()
}

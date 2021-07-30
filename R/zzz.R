.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "Thanks for installing 'RprobitB' version ", packageVersion("RprobitB")),
    "\nType 'citation(\"RprobitB\")' for citing the R package in publications.",
    "\n Happy choice modelling!")
  packageStartupMessage(msg)
  invisible()
}

#' Print method for \code{RprobitB_model}.
#' @description
#' This function is the print method for an object of class \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param ...
#' Ignored.
#' @noRd

print.RprobitB_model <- function(x, ...) {
  cat("Probit model '", deparse1(x$data$form), "'.\n", sep = "")
  return(invisible(x))
}

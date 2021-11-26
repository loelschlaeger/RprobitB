#' Print method for the summary of \code{RprobitB_model}.
#' @description
#' This function is the print method for an object of class
#' \code{summary.RprobitB_model}.
#' @param x
#' An object of class \code{summary.RprobitB_model}.
#' @param digits
#' The number of printed decimal places.
#' @param ...
#' Ignored.
#' @export

print.summary.RprobitB_model <- function(x, digits = 2, ...) {
  cat("Probit model '", deparse1(x$form), "'.\n\n", sep = "")

  ### summary of model
  cat("MCMC settings:\n")
  cat("- R:", x$R, "\n")
  cat("- B:", x$B, "\n")
  cat("- Q:", x$Q, "\n")
  cat("\n")

  ### summary of normalization
  print(x$normalization)
  cat("\n")

  ### legend of alternatives
  cat("Legend of alternatives:\n")
  print(data.frame("name" = x$alternatives))
  cat("\n")

  ### legend of linear coefficients
  cat("Legend of linear coefficients:\n")
  print(x$linear_coeffs)
  cat("\n")

  ### legend of latent classes
  if (x$P_r > 0) {
    print(x$latent_classes)
    cat("\n")
  }

  ### overview of estimates
  print(x = x$statistics, true = x$true_parameter, digits = digits)

  ### return 'x' invisibly
  return(invisible(x))
}

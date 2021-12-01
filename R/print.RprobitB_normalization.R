#' Print method for \code{RprobitB_normalization}.
#' @description
#' This function is the print method for an object of class
#' \code{RprobitB_normalization}.
#' @param x
#' An object of class \code{RprobitB_normalization}.
#' @param ...
#' Ignored.
#' @noRd

print.RprobitB_normalization <- function(x, ...) {
  cat("Normalization:\n")
  cat(paste0(
    "- Level: Utility differences with respect to alternative ",
    x$level, ".\n"
  ))
  if (x$scale$parameter == "a") {
    norm_scale <- paste0(x$cov_fix[x$scale$index], " (alpha_", x$scale$index, ")")
  }
  if (x$scale$parameter == "s") {
    norm_scale <- paste0("the ", x$scale$index, ". error term variance in Sigma")
  }
  cat(paste0(
    "- Scale: Coefficient of ", norm_scale, " fixed to ", x$scale$value,
    ".\n"
  ))
  return(invisible(x))
}

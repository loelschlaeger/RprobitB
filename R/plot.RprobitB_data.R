#' Plot method for \code{RprobitB_data}.
#' @description
#' This function is the plot method for an object of class
#' \code{RprobitB_data}.
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param ...
#' Ignored.
#' @return
#' No return value. Draws a plot to the current device.

plot.RprobitB_data <- function(x, ...) {
  plot(x$choice_data)
}

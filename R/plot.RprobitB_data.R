#' Plot method for \code{RprobitB_data}.
#'
#' @description
#' This function is the plot method for an object of class
#' \code{RprobitB_data}.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' TODO: Remove ASC, choice, id and idc from plot. Change labels in legend.

plot.RprobitB_data <- function(x, ...) {

  x = x$choice_data
  xx = x %>% gather()
  cc = as.factor(rep(x$choice, 9))
  ggplot(xx, aes(x = value, colour = cc)) +
    geom_histogram(alpha = 0.2, position="dodge") +
    facet_wrap(~ key, scales = "free")

}

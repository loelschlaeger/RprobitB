#' Plot method for \code{RprobitB_data}.
#'
#' @description
#' This function is the plot method for an object of class
#' \code{RprobitB_data}.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param alpha
#' Passed to \link[ggplot2]{geom_histogram}.
#' @param bins
#' Passed to \link[ggplot2]{geom_histogram}.
#' @param position
#' Passed to \link[ggplot2]{geom_histogram}.
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes scale_color_hue geom_histogram facet_wrap labs

plot.RprobitB_data <- function(x, alpha = 0.8, bins = 30, position = "dodge", ...) {
  vis_data <- x$choice_data
  vis_data[unlist(x$res_var_names)] <- NULL
  vis_data <- tidyr::gather(vis_data)
  choices_extended <- rep(
    x$choice_data[[x$res_var_names$choice]],
    (dim(vis_data) / dim(x$choice_data))[1]
  )
  alternatives <- x$alternatives
  ggplot2::ggplot(vis_data, ggplot2::aes(x = .data$value, fill = as.factor(choices_extended))) +
    ggplot2::scale_fill_hue(labels = alternatives) +
    ggplot2::geom_histogram(alpha = alpha, position = position, bins = bins) +
    ggplot2::facet_wrap(~key, scales = "free") +
    ggplot2::labs(fill = "alternatives") +
    ggplot2::theme_bw()
}

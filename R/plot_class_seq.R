#' Visualizing the number of classes during Gibbs sampling
#'
#' @description
#' This function plots the number of latent Glasses during Gibbs sampling
#' to visualize the class updating.
#'
#' @inheritParams RprobitB_fit
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal

plot_class_seq <- function(class_sequence, B) {
  data <- data.frame(i = 1:length(class_sequence), c = class_sequence)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$i, y = .data$c)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Number of classes during Gibbs sampling",
      subtitle = "The grey area shows the updating phase",
      x = "Iteration",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous() +
    ggplot2::scale_y_continuous(
      breaks = 1:max(class_sequence),
      labels = as.character(1:max(class_sequence)),
      minor_breaks = NULL
    ) +
    ggplot2::expand_limits(y = 1) +
    ggplot2::annotate(
      geom = "rect",
      xmin = 0, xmax = B, ymin = -Inf, ymax = Inf,
      fill = "grey", alpha = 0.2
    )
  print(plot)
}

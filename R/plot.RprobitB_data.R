#' Visualize choice data
#'
#' @description
#' This function is the plot method for an object of class \code{RprobitB_data}.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param by_choice
#' Set to \code{TRUE} to group the covariates by the chosen alternatives.
#' @param alpha,position
#' Passed to \code{\link[ggplot2]{ggplot}}.
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ cost | 0,
#'   N = 100,
#'   T = 10,
#'   J = 2,
#'   alternatives = c("bus", "car"),
#'   true_parameter = list("alpha" = -1)
#' )
#' plot(data, by_choice = TRUE)

plot.RprobitB_data <- function(x, by_choice = FALSE, alpha = 1,
                               position = "dodge", ...) {
  ### extract the data to be plotted
  data_red <- x$choice_data[names(x$choice_data) %in%
                              unlist(x$res_var_names[c("choice", "cov")])]

  ### transform covariates with less than 10 values to factors
  for (i in 1:ncol(data_red)) {
    if (length(unique(data_red[, i])) < 10) {
      data_red[, i] <- as.factor(data_red[, i])
    }
  }

  ### keep order of alternatives in case of the ordered probit model
  if (x$ordered) {
    data_red[[x$res_var_names$choice]] <- factor(
      data_red[[x$res_var_names$choice]],
      levels = x$alternatives
    )
  }

  ### create basis of plot
  base_plot <- ggplot2::ggplot(data = data_red) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(y = "")

  plots <- list()

  plots[[1]] <- base_plot + ggplot2::geom_bar(
    mapping = ggplot2::aes(
      x = .data[[x$res_var_names$choice]],
      fill = if (by_choice) .data[[x$res_var_names$choice]] else NULL
    ),
    position = position, alpha = alpha
  )

  for (cov in setdiff(names(data_red), x$res_var_names$choice)) {
    if (is.factor(data_red[[cov]])) {
      p <- ggplot2::geom_bar(
        mapping = ggplot2::aes(
          x = .data[[cov]],
          fill = if (by_choice) .data[[x$res_var_names$choice]] else NULL
        ),
        position = position, alpha = alpha
      )
    } else {
      p <- ggplot2::geom_freqpoly(
        mapping = ggplot2::aes(
          x = .data[[cov]],
          color = if (by_choice) .data[[x$res_var_names$choice]] else NULL
        ),
        alpha = alpha
      )
    }

    plots[[length(plots) + 1]] <- base_plot + p
  }

  suppressMessages(gridExtra::grid.arrange(grobs = plots))
}

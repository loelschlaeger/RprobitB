#' Extract model effects
#'
#' @description
#' This function extracts the estimated model effects.
#'
#' @return
#' An object of class \code{RprobitB_coef}.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param x
#' An object of class \code{RprobitB_coef}.
#'
#' @param sd \[`integer(1)`\]\cr
#' The number of standard deviations to display.
#'
#' @param het \[`logical(1)`\]\cr
#' Show the standard deviation of the mixing distribution? If `FALSE`,
#' standard deviation of the estimate are shown.
#'
#' @param ...
#' Currently not used.
#'
#' @export

coef.RprobitB_fit <- function(object, ...) {

  oeli::input_check_response(
    check = checkmate::check_class(object, "RprobitB_fit"),
    var_name = "object"
  )

  ### compute Gibbs samples statistics
  C <- object$latent_classes$C
  statistics <- RprobitB_gibbs_samples_statistics(
    gibbs_samples = filter_gibbs_samples(
      x = object$gibbs_samples,
      P_f = object$data$P_f,
      P_r = object$data$P_r,
      J = object$data$J,
      C = C,
      cov_sym = FALSE,
    ),
    FUN = c("mean" = mean, "sd" = stats::sd)
  )

  ### allocate space for output
  coef <- matrix(NA_real_, nrow = 0, ncol = 4)
  coef_name <- c()
  coef_class <- c()

  ### create entries for fixed-effect coefficients
  fixed_coefs <- object$data$effects[object$data$effects$random == FALSE, ]
  for (row in seq_len(nrow(fixed_coefs))) {
    coef <- rbind(coef, c(statistics$alpha[row, 1:2], NA_real_, NA_real_))
    coef_name <- c(coef_name, fixed_coefs[row, "effect"])
    coef_class <- c(coef_class, NA_real_)
  }

  ### create entries for random-effect coefficients
  random_coefs <- object$data$effects[object$data$effects$random == TRUE, ]
  for (row in seq_len(nrow(random_coefs))) {
    mean <- statistics$b[paste0(1:C, ".", row), 1]
    mean_sd <- statistics$b[paste0(1:C, ".", row), 2]
    var <- statistics$Omega[paste0(1:C, ".", row, ",", row), 1]
    var_sd <- statistics$Omega[paste0(1:C, ".", row, ",", row), 2]
    coef <- rbind(coef, cbind(mean, mean_sd, var, var_sd))
    coef_name <- c(coef_name, rep(random_coefs[row, "effect"], C))
    coef_class <- c(coef_class, 1:C)
  }

  ### create output
  rownames(coef) <- coef_name
  colnames(coef) <- c("mean", "mean_sd", "var", "var_sd")
  attr(coef, "coef_class") <- coef_class
  attr(coef, "s") <- statistics[["s"]]
  attr(coef, "C") <- C
  class(coef) <- "RprobitB_coef"
  return(coef)
}

#' @rdname coef.RprobitB_fit
#' @export

print.RprobitB_coef <- function(x, ...) {
  oeli::input_check_response(
    check = checkmate::check_class(x, "RprobitB_coef"),
    var_name = "x"
  )
  classes <- sapply(
    attr(x, "coef_class"),
    function(cl) {
      if (is.na(cl) || attr(x, "C") == 1) {
        ""
      } else {
        paste0("[", cl, "]")
      }
    }
  )
  out <- data.frame(
    sprintf("%s %s", rownames(x), classes),
    sprintf("%.2f", x[, "mean"]),
    sprintf("(%.2f)", x[, "mean_sd"]),
    sprintf("%.2f", x[, "var"]),
    sprintf("(%.2f)", x[, "var_sd"])
  )
  colnames(out) <- c(" ", "Estimate", "(sd)", "Variance", "(sd)")
  if (all(is.na(x[, c("var", "var_sd")]))) out <- out[, 1:3]
  print(out)
  invisible(x)
}

#' @rdname coef.RprobitB_fit
#' @exportS3Method

plot.RprobitB_coef <- function(x, sd = 1, het = FALSE, ...) {
  oeli::input_check_response(
    check = checkmate::check_class(x, "RprobitB_coef"),
    var_name = "x"
  )
  oeli::input_check_response(
    check = checkmate::check_count(sd, positive = FALSE),
    var_name = "sd"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(het),
    var_name = "het"
  )
  s <- attr(x, "s")
  x <- data.frame(
    "name" = rownames(x),
    "cl" = attr(x, "coef_class"),
    unclass(x)
  )
  mapping <- if (all(is.na(x$cl))) {
    ggplot2::aes(x = .data$mean, y = .data$name)
  } else {
    ggplot2::aes(x = .data$mean, y = .data$name, color = factor(.data$cl))
  }
  p <- ggplot2::ggplot(data = x, mapping = mapping) +
    ggplot2::geom_vline(aes(xintercept = 0), linetype = 2) +
    ggplot2::geom_point(
      size = 2,
      position = ggplot2::position_dodge(width = -0.3)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data$mean - sd * .data[[if (het) "var" else "mean_sd"]],
        xmax = .data$mean + sd * .data[[if (het) "var" else "mean_sd"]],
        width = 0
      ),
      position = ggplot2::position_dodge(width = -0.3)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Average effects",
      subtitle = if (sd > 0) {
        paste(
          "The horizontal lines show \u00B1", sd,
          "standard deviation of the",
          ifelse(het, "mixing distribution", "estimate")
        )
      } else NULL,
      color = "Class"
    )

  ### add class proportions
  if (!all(is.na(x$cl))) {
    p <- p + ggplot2::scale_color_discrete(
      labels = sprintf("%s (%.2f%%)", 1:nrow(s), s[, "mean"] * 100)
    )
  }

  suppressWarnings(print(p))
  invisible(p)
}

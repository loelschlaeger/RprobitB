#' Linear coefficients
#'
#' @description
#' This function returns the estimated linear coefficients.
#'
#' @return
#' An object of class \code{RprobitB_coef}.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#' @param ...
#' Ignored.
#'
#' @export

coef <- function(object, ...) {
  UseMethod("coef", object)
}

#' @noRd
#' @export
#' @importFrom stats sd

coef.RprobitB_fit <- function(object, ...) {

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
  coef <- matrix(NA, nrow = 0, ncol = 4)
  coef_name <- c()
  coef_class <- c()

  ### create entries for fixed-effect coefficients
  fixed_coefs <- object$data$linear_coefs[object$data$linear_coefs$re == FALSE, ]
  for (row in seq_len(nrow(fixed_coefs))) {
    coef <- rbind(coef, c(statistics$alpha[row, 1:2], NA, NA))
    coef_name <- c(coef_name, fixed_coefs[row, "name"])
    coef_class <- c(coef_class, NA)
  }

  ### create entries for random-effect coefficients
  random_coefs <- object$data$linear_coefs[object$data$linear_coefs$re == TRUE, ]
  for (row in seq_len(nrow(random_coefs))) {
    mean_mean <- statistics$b[paste0(1:C, ".", row), 1]
    mean_sd <- statistics$b[paste0(1:C, ".", row), 2]
    sd_mean <- statistics$Omega[paste0(1:C, ".", row, ",", row), 1]
    sd_sd <- statistics$Omega[paste0(1:C, ".", row, ",", row), 2]
    coef <- rbind(coef, cbind(mean_mean, mean_sd, sd_mean, sd_sd))
    coef_name <- c(coef_name, rep(random_coefs[row, "name"], C))
    coef_class <- c(coef_class, 1:C)
  }

  ### create output
  rownames(coef) <- coef_name
  colnames(coef) <- c("mean_mean", "mean_sd", "sd_mean", "sd_sd")
  attr(coef, "coef_class") <- coef_class
  class(coef) <- "RprobitB_coef"
  return(coef)
}

#' @noRd
#' @export

print.RprobitB_coef <- function(x, ...) {
  classes <- sapply(
    attr(x, "coef_class"),
    function(cl) ifelse(is.na(cl), "", paste0("[", cl, "]"))
  )
  out <- data.frame(
    sprintf("%s %s", rownames(x), classes),
    sprintf("%.2f (%.2f)", x[, "mean_mean"], x[, "mean_sd"]),
    sprintf("%.2f (%.2f)", x[, "sd_mean"], x[, "sd_sd"])
  )
  colnames(out) <- c(" ", "Average Effect", "Variability")
  print(out)
}

#' @noRd
#' @export
#' @importFrom ggplot2 aes ggplot geom_vline geom_point geom_errorbar position_dodge theme_minimal labs
#' @importFrom rlang .data

plot.RprobitB_coef <- function(x, ...) {
  x <- data.frame(
    "name" = rownames(x),
    "cl" = attr(x, "coef_class"),
    unclass(x)
  )
  mapping <- if (all(is.na(x$cl))) {
    ggplot2::aes(x = .data$mean_mean, y = .data$name)
  } else {
    ggplot2::aes(x = .data$mean_mean, y = .data$name, color = factor(.data$cl))
  }
  p <- ggplot2::ggplot(data = x, mapping = mapping) +
    ggplot2::geom_vline(aes(xintercept = 0), linetype = 2) +
    ggplot2::geom_point(
      size = 2,
      position = ggplot2::position_dodge(width = 0.3)
    ) +
    ggplot2::geom_errorbar(ggplot2::aes(
      xmin = .data$mean_mean - .data$mean_sd,
      xmax = .data$mean_mean + .data$mean_sd,
      width = 0.2
    ),
    position = ggplot2::position_dodge(width = 0.3)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Average effects",
      color = "Class"
    )
  print(p)
}

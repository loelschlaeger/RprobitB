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
    mean <- statistics$b[paste0(1:C, ".", row), 1]
    mean_sd <- statistics$b[paste0(1:C, ".", row), 2]
    var <- statistics$Omega[paste0(1:C, ".", row, ",", row), 1]
    var_sd <- statistics$Omega[paste0(1:C, ".", row, ",", row), 2]
    coef <- rbind(coef, cbind(mean, mean_sd, var, var_sd))
    coef_name <- c(coef_name, rep(random_coefs[row, "name"], C))
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

#' @noRd
#' @export

print.RprobitB_coef <- function(x, ...) {
  classes <- sapply(
    attr(x, "coef_class"),
    function(cl) {
      if(is.na(cl) || attr(x, "C") == 1) {
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
  if (all(is.na(x[, c("var", "var_sd")]))) {
    out <- out[, 1:3]
  }
  print(out)
}

#' @param sd
#' The number of standard deviations to display.
#' @param het
#' Set to \code{FALSE} to show the standard deviation of the estimate.
#' Set to \code{TRUE} to show the standard deviation of the mixing distribution.
#' @noRd
#' @export
#' @importFrom ggplot2 aes ggplot geom_vline geom_point geom_errorbar
#' position_dodge theme_minimal labs
#' @importFrom rlang .data

plot.RprobitB_coef <- function(x, sd = 1, het = FALSE, ...) {
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
    ggplot2::geom_errorbar(ggplot2::aes(
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
      subtitle = paste(
        "The horizontal lines show \u00B1", sd,
        "standard deviation of the",
        ifelse(het, "mixing distribution", "estimate")
      ),
      color = "Class"
    )

  ### add class proportions
  if (!all(is.na(x$cl))) {
    p <- p + ggplot2::scale_color_discrete(
      labels = sprintf("%s (%.2f%%)", 1:nrow(s), s[, "mean"])
    )
  }

  suppressWarnings(print(p))
}

#' Estimated covariance matrix of the mixing distribution
#'
#' @description
#' This convenience function returns the estimated covariance matrix of the
#' mixing distribution.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @param cor
#' If \code{TRUE}, returns the correlation matrix instead.
#'
#' @return
#' The estimated covariance matrix of the mixing distribution. In case of
#' multiple classes, a list of matrices for each class.

cov_mix <- function(x, cor = FALSE) {
  if(x$data$P_r == 0){
    stop("No random effects.")
  }
  est_Omega <- point_estimates(x)$Omega
  cov_names <- subset(x$data$linear_coefs, re == TRUE)$name
  out <- list()
  for(c in 1:x$latent_classes$C){
    out[[c]] <- matrix(est_Omega[,c], nrow = x$data$P_r)
    colnames(out[[c]]) <- rownames(out[[c]]) <- cov_names
  }
  if(cor){
    out <- lapply(out, cov2cor)
  }
  if(x$latent_classes$C == 1){
    return(out[[1]])
  } else {
    return(out)
  }
}

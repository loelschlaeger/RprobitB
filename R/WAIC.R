#' Compute WAIC value
#'
#' @description
#' This function computes the WAIC value of an \code{RprobitB_fit} object.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @details
#' WAIC is short for Widely Applicable (or Watanabe-Akaike) Information
#' Criterion. As for AIC and BIC, the smaller the WAIC value the better the
#' model. Its definition is
#' \deqn{WAIC = -2 \cdot lppd + 2 \cdot  p_{WAIC},}
#' where \eqn{lppd} stands for log pointwise predictive density and
#' \eqn{p_{WAIC}} is a penalty term proportional to the variance in the
#' posterior distribution that is sometimes called effective number of
#' parameters.
#' The \eqn{lppd} is approximated as follows. Let
#' \deqn{p_{is} = \Pr(y_i\mid \theta_s)} be the probability of observation
#' \eqn{y_i} given the \eqn{s}th set \eqn{\theta_s} of parameter samples from
#' the posterior. Then
#' \deqn{lppd = \sum_i \log S^{-1} \sum_s p_{si}.}
#' The penalty term is computed as the sum over the variances in log-probability
#' for each observation:
#' \deqn{p_{WAIC} = \sum_i V_{\theta} \left[ \log p_{si} \right].}
#'
#' @return
#' A numeric, the WAIC value, with the following attributes:
#' \itemize{
#'   \item \code{se_waic}, the standard error of the WAIC value,
#'   \item \code{lppd}, the log pointwise predictive density,
#'   \item \code{p_waic}, the effective number of parameters,
#'   \item \code{p_waic_vec}, the vector of summands of \code{p_waic},
#'   \item \code{p_si}, the output of \code{\link{compute_p_si}}.
#' }
#'
#' @keywords internal
#'
#' @export

WAIC <- function(x) {
  ### check input
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be an object of class 'RprobitB_fit'.",
         call. = FALSE
    )
  }

  ### check if 'x' contains 'p_si'
  if (is.null(x[["p_si"]])) {
    stop("Cannot compute WAIC.\n",
         "Please compute the probability for each observed choice at posterior samples first.\n",
         "For that, use the function 'compute_p_si()'.",
         call. = FALSE
    )
  }

  ### calculate p_si and log(p_si)
  p_si <- x[["p_si"]]
  log_p_si <- log(p_si)

  ### calculate WAIC
  lppd <- sum(log(rowSums(p_si)) - log(ncol(p_si)))
  p_waic_vec <- apply(log_p_si, 1, var)
  p_waic <- sum(p_waic_vec)
  waic <- -2 * (lppd - p_waic)
  se_waic <- sqrt(nrow(p_si) * var(p_waic_vec))

  ### prepare and return output
  out <- waic
  attr(out, "se_waic") <- se_waic
  attr(out, "lppd") <- lppd
  attr(out, "p_waic") <- p_waic
  attr(out, "p_waic_vec") <- p_waic_vec
  attr(out, "p_si") <- p_si
  class(out) <- c("RprobitB_waic", "numeric")
  return(out)
}

#' @noRd
#' @export

print.RprobitB_waic <- function(x, digits = 2, ...) {
  cat(sprintf(
    paste0("%.", digits, "f", " (%.", digits, "f)"), x,
    attr(x, "se_waic")
  ))
}

#' @noRd
#' @exportS3Method

plot.RprobitB_waic <- function(x, ...) {
  ### extract 'p_si' from 'x'
  p_si <- attr(x, "p_si")
  S <- ncol(p_si)
  log_p_si <- log(p_si)

  ### compute sequence of waic value for progressive sets of posterior samples
  pb <- RprobitB_pb(
    title = "Preparing WAIC convergence plot",
    total = S,
    tail = "Gibbs samples"
  )
  waic_seq <- numeric(S)
  se_waic_seq <- numeric(S)
  RprobitB_pb_tick(pb)
  for (s in 2:S) {
    RprobitB_pb_tick(pb)
    lppd_temp <- sum(log(rowSums(p_si[, 1:s, drop = FALSE])) - log(s))
    p_waic_i_temp <- apply(log_p_si[, 1:s, drop = FALSE], 1, var)
    p_waic_temp <- sum(p_waic_i_temp)
    waic_seq[s] <- -2 * (lppd_temp - p_waic_temp)
    se_waic_seq[s] <- sqrt(nrow(p_si) * var(p_waic_i_temp))
  }
  seq <- data.frame(waic_seq = waic_seq[-1], se_waic_seq = se_waic_seq[-1])

  ### plot sequence
  p <- ggplot2::ggplot(data = seq, ggplot2::aes(x = 1:nrow(seq), y = waic_seq)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = waic_seq - se_waic_seq,
        ymax = waic_seq + se_waic_seq
      ),
      alpha = 0.2
    ) +
    ggplot2::labs(
      x = "Number of posterior samples",
      y = "WAIC",
      title = "The WAIC value for different sizes of posterior samples"
    ) +
    ggplot2::theme_minimal()
  print(p)
}

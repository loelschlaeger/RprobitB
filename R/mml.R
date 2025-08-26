#' Approximate marginal model likelihood
#'
#' @description
#' This function approximates the model's marginal likelihood.
#'
#' @details
#' The model's marginal likelihood \eqn{p(y\mid M)} for a model \eqn{M} and data
#' \eqn{y} is required for the computation of Bayes factors. In general, the
#' term has no closed form and must be approximated numerically.
#'
#' This function uses the posterior Gibbs samples to approximate the model's
#' marginal likelihood via the posterior harmonic mean estimator.
#' To check the convergence, call `plot(x$mml)`, where `x` is the output
#' of this function. If the estimation does not seem to have
#' converged, you can improve the approximation by combining the value
#' with the prior arithmetic mean estimator. The final approximation of the
#' model's marginal likelihood than is a weighted sum of the posterior harmonic
#' mean estimate and the prior arithmetic mean estimate,
#' where the weights are determined by the sample sizes.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @param S \[`integer(1)`\]\cr
#' The number of prior samples for the prior arithmetic mean estimate. Per
#' default, \code{S = 0}. In this case, only the posterior samples are used
#' for the approximation via the posterior harmonic mean estimator, see the
#' details section.
#'
#' @param ncores \[`integer(1)`\]\cr
#' The number of cores for parallel computation.
#'
#' If set to 1, no parallel backend is used.
#'
#' @param recompute \[`logical(1)`\]\cr
#' Recompute the probabilities?
#'
#' @param log \[`logical(1)`\]\cr
#' Return the logarithm of the marginal model likelihood?
#'
#' @param ...
#' Currently not used.
#'
#' @return
#' The object \code{x}, including the object \code{mml}, which is the model's
#' approximated marginal likelihood value.
#'
#' @export

mml <- function(
    x, S = 0, ncores = parallel::detectCores() - 1, recompute = FALSE
  ) {

  ### input checks
  oeli::input_check_response(
    check = checkmate::check_class(x, "RprobitB_fit"),
    var_name = "x"
  )
  if (is.null(x[["p_si"]])) {
    stop(
      "Please compute the choice probabilities at posterior samples first.\n",
      "For that, use the function 'compute_p_si()'.",
      call. = FALSE
    )
  }
  oeli::input_check_response(
    check = checkmate::check_int(S, lower = 0),
    var_name = "S"
  )
  oeli::input_check_response(
    check = checkmate::check_count(ncores, positive = TRUE),
    var_name = "ncores"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(recompute),
    var_name = "recompute"
  )

  ### check if 'mml' in 'x' already exists if 'recompute = FALSE'
  if (!recompute && !is.null(x[["mml"]])) return(x)

  ### helper variables
  add_args <- list(
    P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
    N = x$data$N, C = x$latent_classes$C, sample = FALSE
  )

  ### compute posterior harmonic mean estimate
  p_si <- x[["p_si"]]
  N <- nrow(p_si)
  S_post <- ncol(p_si)
  cont_post <- numeric(S_post)
  const <- round(0.5 * N)
  for (s in 1:S_post) {
    cont_post[s] <- 1 / exp(sum(log(p_si[, s]) + const / N))
  }
  mml_value <- S_post / sum(cont_post)
  approx_seq <- seq_along(cont_post) / cumsum(cont_post)

  if (S > 0) {

    ### set up parallel only if ncores > 1
    if (ncores > 1) {
      cluster <- parallel::makeCluster(ncores)
      doSNOW::registerDoSNOW(cluster)
    }

    ### register progress bar
    if (getOption("RprobitB_progress")) {
      pb <- RprobitB_pb(
        title = "Computing prior arithmetic mean estimate",
        total = S,
        tail = "parameter sets"
      )
      opts <- list(progress = function(n) pb$tick())
    } else {
      opts <- list()
    }

    ### compute prior arithmetic mean estimate
    s <- NULL
    `%op%` <- if (ncores > 1) foreach::`%dopar%` else foreach::`%do%`
    cont_prior <- foreach::foreach(
      s = 1:S, .packages = "RprobitB", .combine = "cbind", .options.snow = opts
    ) %op% {
      prior_sample <- draw_from_prior(x$prior, C = x$latent_classes$C)
      par <- do.call(what = RprobitB_parameter, args = c(prior_sample, add_args))
      probs <- choice_probabilities(x = x, par_set = par)
      choices <- as.character(unlist(sapply(x$data$data, `[[`, "y")))
      ll <- 0
      for (row in 1:nrow(probs)) {
        ll <- ll + log(probs[row, choices[row]]) + const / N
      }
      exp(ll)
    }

    ### stop parallel backend if used
    if (ncores > 1) parallel::stopCluster(cluster)

    ### merge posterior harmonic mean estimate with prior arithmetic mean estimate
    cont_prior <- cont_prior[cont_prior != 0]
    S_new <- length(cont_prior)
    if (S_new == 0) {
      warning("Could not use any prior sample.", call. = FALSE)
    } else {
      if (S_new < S) {
        warning("Could only use ", S_new, " of ", S,
                " prior samples that led to a positive probability.",
                call. = FALSE
        )
      }
      mml_value_prior <- sum(cont_prior) / S_new
      S_total <- S_post + S_new

      mml_value <- mml_value * S_post / S_total + mml_value_prior * S_new / S_total
      approx_seq <- c(approx_seq, mml_value * S_post / (S_post + seq_along(cont_prior)) + mml_value_prior * seq_along(cont_prior) / (S_post + seq_along(cont_prior)))
    }
  }

  ### save 'mml_value' in 'x'
  out <- mml_value
  attr(out, "mmll") <- log(mml_value) - const
  attr(out, "mml_vec") <- approx_seq
  attr(out, "factor") <- const
  class(out) <- c("RprobitB_mml", "numeric")
  x[["mml"]] <- out

  ### return 'x'
  return(x)
}

#' @rdname mml
#' @export

print.RprobitB_mml <- function(x, log = FALSE, ...) {
  if (!log) {
    cat(sprintf(paste0("%.2e * exp(-%.f)"), x, attr(x, "factor")))
  } else {
    cat(attr(x, "mmll"))
  }
}

#' @rdname mml
#' @export

plot.RprobitB_mml <- function(x, log = FALSE, ...) {
  if (log) {
    mml_vec <- log(attr(x, "mml_vec")) - attr(x, "factor")
  } else {
    mml_vec <- attr(x, "mml_vec")
  }
  p <- ggplot2::ggplot(
    data = data.frame(
      "S" = seq_along(mml_vec),
      "mml_vec" = mml_vec
    ),
    ggplot2::aes(x = .data$S, y = .data$mml_vec)
  ) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
  if (log) {
    p <- p + ggplot2::labs(
      x = "Number of samples",
      y = "Marginal log-likelihood",
      title = "The marginal log-likelihood value for different sample sizes"
    )
  } else {
    p <- p + ggplot2::labs(
      x = "Number of samples",
      y = paste("Marginal likelihood *", sprintf("exp(-%.f)", attr(x, "factor"))),
      title = "The marginal likelihood value for different sample sizes"
    ) +
      ggplot2::scale_y_log10()
  }
  print(p)
}

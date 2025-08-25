#' Compute choice probabilities at posterior samples
#'
#' @description
#' This function computes the probability for each observed choice at the
#' (normalized, burned and thinned) samples from the posterior.
#'
#' These probabilities are required to compute the \code{\link{WAIC}} and the
#' marginal model likelihood \code{\link{mml}}.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @param ncores \[`integer(1)`\]\cr
#' The number of cores for parallel computation.
#'
#' If set to 1, no parallel backend is used.
#'
#' @param recompute \[`logical(1)`\]\cr
#' Recompute the probabilities?
#'
#' @return
#' The object \code{x}, including the object \code{p_si}, which is a matrix of
#' probabilities, observations in rows and posterior samples in columns.
#'
#' @export

compute_p_si <- function(
    x, ncores = parallel::detectCores() - 1, recompute = FALSE
  ) {

  ### check input
  oeli::input_check_response(
    check = checkmate::check_class(x, "RprobitB_fit"),
    var_name = "x"
  )
  oeli::input_check_response(
    check = checkmate::check_count(ncores, positive = TRUE),
    var_name = "ncores"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(recompute),
    var_name = "recompute"
  )

  ### check if 'p_si' in 'x' already exists if 'recompute = FALSE'
  if (isFALSE(recompute) && !is.null(x$p_si)) return(x)

  ### extract pars from Gibbs samples
  pars <- posterior_pars(x)

  ### set up parallel only if ncores > 1
  if (ncores > 1) {
    cluster <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cluster)
  }

  ### register progress bar
  if (getOption("RprobitB_progress")) {
    pb <- RprobitB_pb(
      title = "Computing p_si",
      total = length(pars),
      tail = "parameter sets"
    )
    opts <- list(progress = function(n) pb$tick())
  } else {
    opts <- list()
  }

  ### compute probability for each observation i (rows) and sample s (columns)
  s <- NULL
  `%op%` <- if (ncores > 1) foreach::`%dopar%` else foreach::`%do%`
  p_si <- foreach::foreach(
    s = 1:length(pars), .packages = "RprobitB",
    .combine = "cbind", .options.snow = opts
  ) %op% {
    out <- c()
    for (n in 1:x$data$N) {
      X_n <- x$data$data[[n]]$X
      y_n <- x$data$data[[n]]$y
      for (t in 1:x$data$T[n]) {
        X_nt <- X_n[[t]]
        y_nt <- y_n[t]
        alt_index <- which(x$data$alternatives == y_nt)
        out <- c(out, compute_choice_probabilities(
          X = X_nt, alternatives = alt_index, parameter = pars[[s]]
        )[alt_index])
      }
    }
    out
  }

  ### stop parallel backend if used
  if (ncores > 1) parallel::stopCluster(cluster)

  ### save 'p_si' in 'x'
  x[["p_si"]] <- p_si

  ### return 'x'
  return(x)
}

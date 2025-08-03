#' Fit probit model to choice data
#'
#' @description
#' This function performs Markov chain Monte Carlo simulation for fitting
#' different types of probit models (binary, multivariate, mixed, latent class,
#' ordered, ranked) to discrete choice data.
#'
#' @details
#' See [the vignette on model fitting](https://loelschlaeger.de/RprobitB/articles/v03_model_fitting.html)
#' for more details.
#'
#' @param data
#' An object of class \code{RprobitB_data}.
#' @inheritParams RprobitB_normalization
#' @param R
#' The number of iterations of the Gibbs sampler.
#' @param B
#' The length of the burn-in period, i.e. a non-negative number of samples to
#' be discarded.
#' @param Q
#' The thinning factor for the Gibbs samples, i.e. only every \code{Q}th
#' sample is kept.
#' @param print_progress
#' A boolean, determining whether to print the Gibbs sampler progress and the
#' estimated remaining computation time.
#' @param prior
#' A named list of parameters for the prior distributions. See the documentation
#' of \code{\link{check_prior}} for details about which parameters can be
#' specified.
#' @inheritParams RprobitB_latent_classes
#' @param seed
#' Set a seed for the Gibbs sampling.
#' @param fixed_parameter
#' Optionally specify a named list with fixed parameter values for \code{alpha},
#' \code{C}, \code{s}, \code{b}, \code{Omega}, \code{Sigma}, \code{Sigma_full},
#' \code{beta}, \code{z}, or \code{d} for the simulation.
#' See [the vignette on model definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
#' for definitions of these variables.
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ var | 0, N = 100, T = 10, J = 3, seed = 1
#' )
#' model <- fit_model(data = data, R = 1000, seed = 1)
#' summary(model)
#'
#' @export
#'
#' @seealso
#' \itemize{
#'   \item [prepare_data()] and [simulate_choices()] for building an
#'         \code{RprobitB_data} object
#'   \item [update()] for estimating nested models
#'   \item [transform()] for transforming a fitted model
#' }

fit_model <- function(
    data, scale = "Sigma_1,1 := 1", R = 1000, B = R / 2, Q = 1,
    print_progress = getOption("RprobitB_progress"), prior = NULL,
    latent_classes = NULL, seed = NULL, fixed_parameter = list()) {
  ### check inputs
  if (!inherits(data, "RprobitB_data")) {
    stop(
      "'data' must an object of class 'RprobitB_data', i.e. the output of",
      " 'RprobitB::prepare()' or 'RprobitB::simulate()'.",
      call. = FALSE
    )
  }
  if (!data[["choice_available"]]) {
    stop(
      "Cannot use 'data' for model fitting because information on choices",
      " is not available.",
      call. = FALSE
    )
  }
  if (!is.numeric(R) || !R %% 1 == 0 || !R > 0) {
    stop("'R' must be a positive integer.",
         call. = FALSE
    )
  }
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.",
         call. = FALSE
    )
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.",
         call. = FALSE
    )
  }
  if (!isTRUE(print_progress) && !isFALSE(print_progress)) {
    stop("'print_progress' must be a boolean.",
         call. = FALSE
    )
  }

  ### set normalization
  normalization <- RprobitB_normalization(
    level = NULL, scale = scale, form = data$form, re = data$re,
    alternatives = data$alternatives, base = data$base, ordered = data$ordered
  )

  ### set latent classes
  latent_classes <- RprobitB_latent_classes(latent_classes = latent_classes)
  if (latent_classes$dp_update && is.null(prior[["delta"]])) {
    prior[["delta"]] <- 0.1
  }

  ### set fixed parameter
  fixed_parameter <- unclass(do.call(
    what = RprobitB_parameter,
    args = c(
      list(
        "P_f" = data$P_f, "P_r" = data$P_r, "J" = data$J, "N" = data$N,
        "C" = latent_classes$C, "ordered" = data$ordered, sample = FALSE
      ),
      fixed_parameter
    )
  ))[names(fixed_parameter)]
  if (latent_classes[["class_update"]]) {
    no_fix <- c("s", "z", "b", "Omega")
    if (any(names(fixed_parameter) %in% no_fix)) {
      stop("You cannot fix parameter ",
           paste(intersect(no_fix, names(fixed_parameter)), collapse = ", "),
           " when updating C.",
           call. = FALSE
      )
    }
  }

  ### set prior parameters
  prior <- do.call(
    what = check_prior,
    args = c(list(
      "P_f" = data$P_f, "P_r" = data$P_r, "J" = data$J,
      "ordered" = data$ordered
    ), prior)
  )

  ### compute sufficient statistics
  suff_stat <- sufficient_statistics(data = data, normalization = normalization)

  ### set initial values for the Gibbs sampler
  init <- set_initial_gibbs_values(
    N = data[["N"]], T = data[["T"]], J = data[["J"]], P_f = data[["P_f"]],
    P_r = data[["P_r"]], C = latent_classes[["C"]], ordered = data[["ordered"]],
    suff_stat = suff_stat
  )

  ### Gibbs sampling
  if (!is.null(seed)) {
    set.seed(seed)
  }
  timer_start <- Sys.time()
  gibbs_samples <- gibbs_sampler(
    sufficient_statistics = suff_stat, prior = prior,
    latent_classes = unclass(latent_classes), fixed_parameter = fixed_parameter,
    init = init, R = R, B = B, print_progress = print_progress,
    ordered = data[["ordered"]], ranked = data[["ranked"]]
  )
  timer_end <- Sys.time()
  if (data$ordered) {
    gibbs_samples$alpha <- gibbs_samples$alpha * 1.4
    gibbs_samples$b <- gibbs_samples$b * 1.4
  }

  ### filter Gibbs samples
  if (data$P_f == 0) {
    gibbs_samples["alpha"] <- NULL
  }
  if (data$P_r == 0) {
    gibbs_samples[c("s", "z", "b", "Omega", "class_sequence")] <- NULL
  }
  if (!data$ordered) {
    gibbs_samples["d"] <- NULL
  }

  if (latent_classes[["class_update"]]) {
    ### update number of latent classes
    latent_classes[["C"]] <- sum(utils::tail(gibbs_samples[["s"]], 1) != 0)

    ### remove zeros for unoccupied classes
    gibbs_samples[["s"]] <- gibbs_samples[["s"]][,
                                                 1:latent_classes[["C"]],
                                                 drop = FALSE
    ]
    gibbs_samples[["b"]] <- gibbs_samples[["b"]][,
                                                 1:(data[["P_r"]] * latent_classes[["C"]]),
                                                 drop = FALSE
    ]
    gibbs_samples[["Omega"]] <- gibbs_samples[["Omega"]][,
                                                         1:(data[["P_r"]]^2 * latent_classes[["C"]]),
                                                         drop = FALSE
    ]
  }

  ### save class sequence
  if (!is.null(gibbs_samples[["class_sequence"]])) {
    class_sequence <- as.vector(gibbs_samples[["class_sequence"]])
    gibbs_samples <- within(gibbs_samples, rm(class_sequence))
  } else {
    class_sequence <- NULL
  }

  ### label Gibbs samples
  labels <- parameter_labels(
    P_f = data$P_f, P_r = data$P_r, J = data$J, C = latent_classes[["C"]],
    ordered = data$ordered, cov_sym = TRUE, drop_par = NULL
  )
  for (par in names(labels)) {
    colnames(gibbs_samples[[par]]) <- labels[[par]]
  }

  ### normalize, burn and thin 'gibbs_samples'
  gibbs_samples <- transform_gibbs_samples(
    gibbs_samples = gibbs_samples, R = R, B = B, Q = Q,
    normalization = normalization
  )

  ### normalize true model parameters based on 'normalization'
  if (data$simulated) {
    data$true_parameter <- transform_parameter(
      parameter = data$true_parameter, normalization = normalization,
      ordered = data$ordered
    )
  }

  ### build 'RprobitB_fit' object
  out <- RprobitB_fit(
    data = data,
    scale = scale,
    level = NULL,
    normalization = normalization,
    R = R,
    B = B,
    Q = Q,
    latent_classes = latent_classes,
    prior = prior,
    gibbs_samples = gibbs_samples,
    class_sequence = class_sequence,
    comp_time = difftime(timer_end, timer_start)
  )

  ### calculate log-likelihood
  RprobitB_pp("Computing log-likelihood")
  if (!data$ordered) out[["ll"]] <- suppressMessages(logLik.RprobitB_fit(out))

  ### return 'RprobitB_fit' object
  return(out)
}

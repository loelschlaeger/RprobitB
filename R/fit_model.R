#' Fit probit model to choice data
#'
#' @description
#' This function performs MCMC simulation for fitting different types of probit
#' models (binary, multivariate, mixed, latent class, ordered, ranked) to
#' discrete choice data.
#'
#' @param data
#' An object of class \code{RprobitB_data}.
#'
#' @param R \[`integer(1)`\]\cr
#' The number of iterations of the Gibbs sampler.
#'
#' @param B \[`integer(1)`\]\cr
#' The length of the burn-in period.
#'
#' @param Q \[`integer(1)`\]\cr
#' The thinning factor for the Gibbs samples.
#'
#' @param print_progress \[`logical(1)`\]\cr
#' Print the Gibbs sampler progress?
#'
#' @param prior \[`list`\]\cr
#' A named list of parameters for the prior distributions. See the documentation
#' of \code{\link{check_prior}} for details about which parameters can be
#' specified.
#'
#' @param fixed_parameter
#' Optionally specify a named list with fixed parameter values for \code{alpha},
#' \code{C}, \code{s}, \code{b}, \code{Omega}, \code{Sigma}, \code{Sigma_full},
#' \code{beta}, \code{z}, or \code{d} for the simulation.
#' See [the vignette on model definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
#' for definitions of these variables.
#'
#' @param save_beta_draws \[`logical(1)`\]\cr
#' Save draws for decider-specific coefficient vectors? Usually not recommended,
#' as it requires a lot of storage space.
#'
#' @inheritParams RprobitB_normalization
#' @inheritParams RprobitB_latent_classes
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @examples
#' set.seed(1)
#' form <- choice ~ var | 0
#' data <- simulate_choices(form = form, N = 100, T = 10, J = 3, re = "var")
#' model <- fit_model(data = data, R = 1000)
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
    print_progress = getOption("RprobitB_progress", default = TRUE),
    prior = NULL, latent_classes = NULL, fixed_parameter = list(),
    save_beta_draws = FALSE
  ) {

  ### check inputs
  if (!inherits(data, "RprobitB_data")) {
    stop(
      "'data' must an object of class 'RprobitB_data'.",
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

  ### Gibbs sampling
  timer_start <- Sys.time()
  gibbs_samples <- gibbs_sampler(
    sufficient_statistics = sufficient_statistics(
      data = data, normalization = normalization
    ), prior = prior, latent_classes = unclass(latent_classes),
    fixed_parameter = fixed_parameter, R = R, B = B,
    print_progress = print_progress,
    ordered = data[["ordered"]], ranked = data[["ranked"]],
    save_beta_draws = save_beta_draws
  )
  timer_end <- Sys.time()

  ### filter Gibbs samples
  if (data$P_f == 0) {
    gibbs_samples["alpha"] <- NULL
  }
  if (data$P_r == 0) {
    gibbs_samples[c("s", "z", "b", "Omega", "beta", "class_sequence")] <- NULL
  }
  if (!data$ordered) {
    gibbs_samples["d"] <- NULL
  }

  if (latent_classes[["class_update"]]) {
    ### update number of latent classes
    latent_classes[["C"]] <- sum(utils::tail(gibbs_samples[["s"]], 1) != 0)

    ### remove zeros for unoccupied classes
    gibbs_samples[["s"]] <- gibbs_samples[["s"]][,1:latent_classes[["C"]],drop = FALSE]
    gibbs_samples[["b"]] <- gibbs_samples[["b"]][,1:(data[["P_r"]] * latent_classes[["C"]]),drop = FALSE]
    gibbs_samples[["Omega"]] <- gibbs_samples[["Omega"]][,1:(data[["P_r"]]^2 * latent_classes[["C"]]),drop = FALSE]
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
  RprobitB_fit(
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
}

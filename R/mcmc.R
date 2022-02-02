#' Markov chain Monte Carlo simulation for probit model fitting.
#'
#' @description
#' This function performs Markov chain Monte Carlo simulation for fitting a
#' (latent class) (mixed) (multinomial) probit model to discrete choice data.
#'
#' @details
#' See the vignette "Model fitting" for more details:
#' \code{vignette("model_fitting", package = "RprobitB")}.
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
#' @inheritParams RprobitB_latent_classes
#' @inheritParams check_prior
#' @param seed
#' Set a seed for the Gibbs sampling.
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @examples
#' \dontrun{
#' ### probit model
#' p <- simulate_choices(form = choice ~ var | 0, N = 100, T = 10, J = 2, seed = 1)
#' m1 <- mcmc(data = p, seed = 1)
#'
#' ### multinomial probit model
#' mnp <- simulate_choices(form = choice ~ var | 0, N = 100, T = 10, J = 3, seed = 1)
#' m2 <- mcmc(data = mnp, seed = 1)
#'
#' ### mixed multinomial probit model
#' mmnp <- simulate_choices(
#'   form = choice ~ 0 | var, N = 100, T = 10, J = 3, re = "var",
#'   seed = 1
#' )
#' m3 <- mcmc(data = mmnp, seed = 1)
#'
#' ### mixed multinomial probit model with 2 latent classes
#' lcmmnp <- simulate_choices(
#'   form = choice ~ 0 | var, N = 100, T = 10, J = 3,
#'   re = "var", seed = 1, C = 2
#' )
#' m4 <- mcmc(data = lcmmnp, latent_classes = list("C" = 2), seed = 1)
#'
#' ### update of latent classes
#' m5 <- simulate_choices(data = lcmmnp, latent_classes = list("update" = TRUE), seed = 1)
#' }
#'
#' @export
#'
#' @importFrom utils tail
#' @import Rcpp

mcmc <- function(data, scale = list("parameter" = "s", "index" = 1, "value" = 1),
                 R = 1e4, B = R / 2, Q = 1, print_progress = TRUE, prior = NULL,
                 latent_classes = NULL, seed = NULL) {

  ### check inputs
  if (!inherits(data, "RprobitB_data")) {
    stop(
      "'data' must an object of class 'RprobitB_data', i.e. the output of",
      " 'RprobitB::prepare()' or 'RprobitB::simulate()'."
    )
  }
  if (!data[["choice_available"]]) {
    stop(
      "Cannot use 'data' for model fitting because information on choices",
      " is not available."
    )
  }
  if (!is.numeric(R) || !R %% 1 == 0 || !R > 0) {
    stop("'R' must be a positive integer.")
  }
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.")
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.")
  }
  if (!is.logical(print_progress)) {
    stop("'progress' must be a boolean.")
  }
  normalization <- RprobitB_normalization(
    J = data$J, P_f = data$P_f,
    scale = scale
  )
  latent_classes <- RprobitB_latent_classes(latent_classes = latent_classes)
  prior <- check_prior(prior = prior, P_f = data$P_f, P_r = data$P_r, J = data$J)

  ### compute sufficient statistics
  ss <- sufficient_statistics(data = data, normalization = normalization)

  ### set initial values for the Gibbs sampler
  init <- set_initial_gibbs_values(
    N = data$N, T = data$T, J = data$J, P_f = data$P_f, P_r = data$P_r,
    C = latent_classes$C
  )

  ### perform Gibbs sampling
  if (!is.null(seed)) {
    set.seed(seed)
  }
  gibbs_samples <- gibbs_sampling(
    R = R, B = B, print_progress = print_progress, N = data$N, J = data$J,
    P_f = data$P_f, P_r = data$P_r, latent_classes = unclass(latent_classes),
    sufficient_statistics = ss, prior = prior, init = init
  )

  if (latent_classes$update) {
    ### update number of latent classes
    latent_classes$C <- sum(utils::tail(gibbs_samples$s, 1) != 0)

    ### remove zeros
    gibbs_samples$s <- gibbs_samples$s[, 1:latent_classes$C]
    gibbs_samples$b <- gibbs_samples$b[, 1:(data$P_r * latent_classes$C)]
    gibbs_samples$Omega <- gibbs_samples$Omega[, 1:(data$P_r^2 * latent_classes$C)]
  }

  ### save classification
  if (!is.null(gibbs_samples$classification)) {
    classification <- gibbs_samples$classification + 1
    gibbs_samples <- within(gibbs_samples, rm(classification))
  } else {
    classification <- NULL
  }

  ### label Gibbs samples
  labels <- parameter_labels(
    P_f = data$P_f, P_r = data$P_r, J = data$J,
    C = length(tail(gibbs_samples$s, 1)), cov_sym = TRUE,
    drop_par = NULL
  )
  for (par in names(gibbs_samples)) {
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
      parameter = data$true_parameter, normalization = normalization
    )
  }

  ### build and return an 'RprobitB_fit'
  out <- RprobitB_fit(
    data = data,
    normalization = normalization,
    R = R,
    B = B,
    Q = Q,
    latent_classes = latent_classes,
    prior = prior,
    gibbs_samples = gibbs_samples,
    classification = classification
  )
  return(out)
}

#' Check \code{prior}.
#'
#' @description
#' This function checks the input \code{prior} and sets missing values to
#' default values.
#'
#' @param prior
#' A named list of parameters for the prior distributions of the normalized
#' parameters:
#' \itemize{
#'   \item \code{eta}:
#'   The mean vector of length \code{P_f} of the normal prior for
#'   \code{alpha}.
#'   \item \code{Psi}:
#'   The covariance matrix of dimension \code{P_f} x \code{P_f} of the
#'   normal prior for \code{alpha}.
#'   \item \code{delta}:
#'   The concentration parameter of length 1 of the Dirichlet prior for \code{s}.
#'   \item \code{xi}:
#'   The mean vector of length \code{P_r} of the normal prior for each
#'   \code{b_c}.
#'   \item \code{D}:
#'   The covariance matrix of dimension \code{P_r} x \code{P_r} of the
#'   normal prior for each \code{b_c}.
#'   \item \code{nu}:
#'   The degrees of freedom (a natural number greater than \code{P_r}) of
#'   the Inverse Wishart prior for each \code{Omega_c}.
#'   \item \code{Theta}:
#'   The scale matrix of dimension \code{P_r} x \code{P_r} of the
#'   Inverse Wishart prior for each \code{Omega_c}.
#'   \item \code{kappa}:
#'   The degrees of freedom (a natural number greater than \code{J-1}) of
#'   the Inverse Wishart prior for \code{Sigma}.
#'   \item \code{E}:
#'   The scale matrix of dimension \code{J-1} x \code{J-1} of the
#'   Inverse Wishart prior for \code{Sigma}.
#' }
#' @inheritParams RprobitB_data
#'
#' @return
#' The checked input \code{prior}
#'
#' @keywords
#' internal

check_prior <- function(prior, P_f, P_r, J) {

  ### check if prior is a list
  if (!is.null(prior)) {
    if (!is.list(prior)) {
      stop("'prior' must be either 'NULL' or a list.")
    }
  } else {
    prior <- list()
  }

  ### check supplied and set missing prior parameters
  if (P_f > 0) {

    ### alpha ~ MVN(eta,Psi)
    if (is.null(prior$eta)) {
      prior$eta <- numeric(P_f)
    }
    if (!is.numeric(prior$eta) || length(prior$eta) != P_f) {
      stop("'prior$eta' must be a numeric vector of length 'P_f'.")
    }
    if (is.null(prior$Psi)) {
      prior$Psi <- diag(P_f)
    }
    if (!is.numeric(prior$Psi) || !is.matrix(prior$Psi) ||
      any(dim(prior$Psi) != c(P_f, P_f))) {
      stop("'prior$Psi' must be a numeric matrix of dimension 'P_f' x 'P_f'.")
    }
  } else {
    prior$eta <- NA
    prior$Psi <- NA
  }
  if (P_r > 0) {

    ### s ~ D(delta)
    if (is.null(prior$delta)) {
      prior$delta <- 1
    }
    if (!is.numeric(prior$delta) || length(prior$delta) != 1) {
      stop("'prior$delta' must be a single numeric value.")
    }

    ### b_c ~ MVN(xi,D)
    if (is.null(prior$xi)) {
      prior$xi <- numeric(P_r)
    }
    if (!is.numeric(prior$xi) || length(prior$xi) != P_r) {
      stop("'prior$xi' must be a numeric vector of length 'P_r'.")
    }
    if (is.null(prior$D)) {
      prior$D <- diag(P_r)
    }
    if (!is.numeric(prior$D) || !is.matrix(prior$D) ||
      any(dim(prior$D) != c(P_r, P_r))) {
      stop("'prior$D' must be a numeric matrix of dimension 'P_r' x 'P_r'.")
    }

    ### Omega_c ~ IW(nu,Theta)
    if (is.null(prior$nu)) {
      ### nu must exceed P_r; more diffuse with lower nu;
      ### if nu = P_r+2, Theta represents the mean
      prior$nu <- P_r + 2
    }
    if (!is.numeric(prior$nu) || length(prior$nu) != 1 || prior$nu <= P_r) {
      stop("'prior$nu' must be a single numeric value greater 'P_r'.")
    }
    if (is.null(prior$Theta)) {
      prior$Theta <- diag(P_r)
    }
    if (!is.numeric(prior$Theta) || !is.matrix(prior$Theta) ||
      any(dim(prior$Theta) != c(P_r, P_r))) {
      stop("'prior$Theta' must be a numeric matrix of dimension 'P_r' x 'P_r'.")
    }
  } else {
    prior$delta <- NA
    prior$xi <- NA
    prior$D <- NA
    prior$nu <- NA
    prior$Theta <- NA
  }

  ### Sigma ~ IW(kappa,E)
  if (is.null(prior$kappa)) {
    ### kappa must exceed J-1; more diffuse with lower kappa;
    ### if kappa = J-1+2, E represents the mean
    prior$kappa <- J - 1 + 2
  }
  if (!is.numeric(prior$kappa) || length(prior$kappa) != 1 || prior$kappa <= J - 1) {
    stop("'prior$kappa' must be a single numeric value greater 'J-1'.")
  }
  if (is.null(prior$E)) {
    prior$E <- diag(J - 1)
  }
  if (!is.numeric(prior$E) || !is.matrix(prior$E) ||
    any(dim(prior$E) != c(J - 1, J - 1))) {
    stop("'prior$E' must be a numeric matrix of dimension 'J-1' x 'J-1'.")
  }

  ### add class to 'prior'
  class(prior) <- "RprobitB_prior"

  ### return prior
  return(prior)
}

#' Set initial values for the Gibbs sampler.
#'
#' @description
#' This function sets initial values for the Gibbs sampler.
#'
#' @inheritParams RprobitB_data
#'
#' @param C
#' The number (greater or equal 1) of latent classes.
#'
#' @return
#' A list of initial values for the Gibbs sampler.
#'
#' @keywords
#' internal

set_initial_gibbs_values <- function(N, T, J, P_f, P_r, C) {

  ### check inputs
  stopifnot(is.numeric(N), N %% 1 == 0, N > 0)
  stopifnot(is.numeric(T), T %% 1 == 0, T > 0)
  stopifnot(is.numeric(P_f), P_f %% 1 == 0, P_f >= 0)
  stopifnot(is.numeric(P_r), P_r %% 1 == 0, P_r >= 0)
  stopifnot(is.numeric(C), C %% 1 == 0, C > 0)

  ### define initial values
  alpha0 <- if (P_f > 0) numeric(P_f) else NA
  m0 <- if (P_r > 0) round(rep(N, C) * 2^(C:1 - 1) / sum(2^(C:1 - 1))) else NA
  b0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = C) else NA
  Omega0 <-
    if (P_r > 0) matrix(rep(as.vector(diag(P_r)), C), nrow = P_r * P_r, ncol = C) else NA
  beta0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = N) else NA
  U0 <- matrix(0, nrow = J - 1, ncol = N * max(T))
  Sigma0 <- diag(J - 1)

  ### define 'init'
  init <- list(
    "alpha0" = alpha0,
    "m0" = m0,
    "b0" = b0,
    "Omega0" = Omega0,
    "beta0" = beta0,
    "U0" = U0,
    "Sigma0" = Sigma0
  )

  ### return 'init'
  return(init)
}

#' Compute sufficient statistics.
#'
#' @description
#' This function computes sufficient statistics from \code{data} for estimation.
#'
#' @inheritParams mcmc
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @return
#' A list of sufficient statistics.
#'
#' @keywords
#' internal

sufficient_statistics <- function(data, normalization) {

  ### check input
  if (!inherits(data, "RprobitB_data")) {
    stop("'data' must be of class 'RprobitB_data'.")
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.")
  }

  ### extract parameters
  N <- data$N
  T <- if (length(data$T) == 1) rep(data$T, N) else data$T
  J <- data$J
  P_r <- data$P_r
  P_f <- data$P_f

  ### compute utility differences with respect to 'normalization$level'
  for (n in seq_len(N)) {
    for (t in seq_len(T[n])) {
      data$data[[n]]$X[[t]] <- delta(J, normalization$level) %*% data$data[[n]]$X[[t]]
    }
  }

  ### compute sufficient statistics
  y <- matrix(0, nrow = N, ncol = max(T))
  for (n in 1:N) {
    ### decode choice to numeric wrt appearance in data$alternatives
    y_n <- match(data$data[[n]][[2]], data$alternatives)
    y[n, ] <- c(y_n, rep(NA, max(T) - length(y_n)))
  }
  W <- list()
  X <- list()
  if (P_f > 0 & P_r > 0) {
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        W[[sum(T[seq_len(n - 1)]) + t]] <-
          data$data[[n]][[1]][[t]][, seq_len(P_f), drop = FALSE]
        X[[sum(T[seq_len(n - 1)]) + t]] <-
          data$data[[n]][[1]][[t]][, -seq_len(P_f), drop = FALSE]
      }
    }
  }
  if (P_f > 0 & P_r == 0) {
    X <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        W[[sum(T[seq_len(n - 1)]) + t]] <- data$data[[n]][[1]][[t]]
      }
    }
  }
  if (P_f == 0 & P_r > 0) {
    W <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        X[[sum(T[seq_len(n - 1)]) + t]] <- data$data[[n]][[1]][[t]]
      }
    }
  }
  XkX <- NA
  if (P_r > 0) {
    XkX <- list()
    for (n in seq_len(N)) {
      XnkXn <- 0
      for (t in seq_len(T[n])) {
        XnkXn <- XnkXn +
          kronecker(
            t(X[[sum(T[seq_len(n - 1)]) + t]]),
            t(X[[sum(T[seq_len(n - 1)]) + t]])
          )
      }
      XkX[[n]] <- XnkXn
    }
  }
  WkW <- NA
  if (P_f > 0) {
    WkW <- matrix(0, nrow = P_f^2, ncol = (J - 1)^2)
    for (n in seq_len(N)) {
      for (t in seq_len(T[n])) {
        WkW <- WkW +
          kronecker(
            t(W[[sum(T[seq_len(n - 1)]) + t]]),
            t(W[[sum(T[seq_len(n - 1)]) + t]])
          )
      }
    }
  }

  ### build and return 'suff_statistics'
  suff_statistics <- list(
    "Tvec" = T,
    "csTvec" = cumsum(T) - T,
    "W" = W,
    "X" = X,
    "y" = y,
    "XkX" = XkX,
    "WkW" = WkW
  )
  return(suff_statistics)
}

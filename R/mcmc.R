#' Probit model fitting via Markov chain Monte Carlo simulation.
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
#' @param prior
#' A named list of parameters for the prior distributions. See the documentation
#' of \code{\link{check_prior}} for details about which parameters can be specified.
#' @inheritParams RprobitB_latent_classes
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
#' m5 <- simulate_choices(data = lcmmnp, latent_classes = list("weight_update" = TRUE), seed = 1)
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
  normalization <- RprobitB_normalization(J = data$J, P_f = data$P_f, scale = scale)
  latent_classes <- RprobitB_latent_classes(latent_classes = latent_classes)
  prior <- do.call(what = check_prior, args = c(list("P_f" = data$P_f, "P_r" = data$P_r, "J" = data$J), prior))

  ### compute sufficient statistics
  ss <- sufficient_statistics(data = data, normalization = normalization)

  ### set initial values for the Gibbs sampler
  init <- set_initial_gibbs_values(
    N = data$N, T = data$T, J = data$J, P_f = data$P_f, P_r = data$P_r,
    C = latent_classes[["C"]]
  )

  ### perform Gibbs sampling
  if (!is.null(seed)) set.seed(seed)
  gibbs_samples <- gibbs_sampling(
    sufficient_statistics = ss, prior = prior, latent_classes = unclass(latent_classes),
    init = init, R = R, B = B, print_progress = print_progress
  )

  if (latent_classes[["weight_update"]] || latent_classes[["dp_update"]]) {
    ### update number of latent classes
    latent_classes[["C"]] <- sum(utils::tail(gibbs_samples[["s"]], 1) != 0)

    ### remove zeros
    gibbs_samples[["s"]] <- gibbs_samples[["s"]][, 1:latent_classes[["C"]]]
    gibbs_samples[["b"]] <- gibbs_samples[["b"]][, 1:(data[["P_r"]] * latent_classes[["C"]])]
    gibbs_samples[["Omega"]] <- gibbs_samples[["Omega"]][, 1:(data[["P_r"]]^2 * latent_classes[["C"]])]
  }

  ### save classification
  if (!is.null(gibbs_samples[["classification"]])) {
    classification <- gibbs_samples[["classification"]]
    gibbs_samples <- within(gibbs_samples, rm(classification))
  } else {
    classification <- NULL
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
    classification = classification,
    class_sequence = class_sequence
  )
  return(out)
}

#' Check prior parameters
#'
#' @description
#' This function checks compatibility of submitted parameters for the prior distributions
#' and sets missing values to default values.
#'
#' @inheritParams RprobitB_data
#' @param eta
#' The mean vector of length \code{P_f} of the normal prior for \code{alpha}.
#' @param Psi
#' The covariance matrix of dimension \code{P_f} x \code{P_f} of the
#' normal prior for \code{alpha}.
#' @param delta
#' A numeric for the concentration parameter vector \code{rep(delta,C)}
#' of the Dirichlet prior for \code{s}.
#' @param xi
#' The mean vector of length \code{P_r} of the normal prior for each \code{b_c}.
#' @param D
#' The covariance matrix of dimension \code{P_r} x \code{P_r} of the normal
#' prior for each \code{b_c}.
#' @param nu
#' The degrees of freedom (a natural number greater than \code{P_r}) of
#' the Inverse Wishart prior for each \code{Omega_c}.
#' @param Theta
#' The scale matrix of dimension \code{P_r} x \code{P_r} of the
#' Inverse Wishart prior for each \code{Omega_c}.
#' @param kappa
#' The degrees of freedom (a natural number greater than \code{J-1}) of
#' the Inverse Wishart prior for \code{Sigma}.
#' @param E
#' The scale matrix of dimension \code{J-1} x \code{J-1} of the Inverse Wishart
#' prior for \code{Sigma}.
#'
#' @details
#' A priori, we assume that the model parameters follow these distributions:
#' \itemize{
#'   \item \eqn{\alpha \sim N(\eta, \Psi)}
#'   \item \eqn{s \sim Dir(\delta)}
#'   \item \eqn{b_c \sim N(\xi, D)} for all classes \eqn{c}
#'   \item \eqn{\Omega_c \sim IW(\nu,\Theta)} for all classes \eqn{c}
#'   \item \eqn{\Sigma \sim IW(\kappa,E)}
#' }
#' where \eqn{N} denotes the normal, \eqn{Dir} the Dirichlet, and \eqn{IW}
#' the Inverted Wishart distribution.
#'
#' @return
#' A list containing all prior parameters. Parameters that are not relevant
#' for the model configuration are set to \code{NA}.
#'
#' @export
#'
#' @examples
#' check_prior(P_f = 1, P_r = 2, J = 3)

check_prior <- function(P_f, P_r, J, eta = numeric(P_f), Psi = diag(P_f),
                        delta = 1, xi = numeric(P_r), D = diag(P_r),
                        nu = P_r + 2, Theta = diag(P_r), kappa = J + 1, E = diag(J - 1)) {

  ### initialize prior list
  prior <- list()

  ### check supplied values and set missing prior parameters to default values
  if (P_f > 0) {

    ### alpha ~ MVN(eta,Psi)
    if (!is.numeric(eta) || length(eta) != P_f) {
      stop("'eta' must be a numeric vector of length 'P_f'.")
    }
    if (!is.numeric(Psi) || !is.matrix(Psi) || any(dim(Psi) != c(P_f, P_f))) {
      stop("'Psi' must be a numeric matrix of dimension 'P_f' x 'P_f'.")
    }
  } else {
    eta <- NA
    Psi <- NA
  }
  if (P_r > 0) {

    ### s ~ D(delta)
    if (!is.numeric(delta) || length(delta) != 1) {
      stop("'delta' must be a single numeric value.")
    }

    ### b_c ~ MVN(xi,D)
    if (!is.numeric(xi) || length(xi) != P_r) {
      stop("'xi' must be a numeric vector of length 'P_r'.")
    }
    if (!is.numeric(D) || !is.matrix(D) ||
      any(dim(D) != c(P_r, P_r))) {
      stop("'D' must be a numeric matrix of dimension 'P_r' x 'P_r'.")
    }

    ### Omega_c ~ IW(nu,Theta)
    if (!is.numeric(nu) || length(nu) != 1 || nu <= P_r) {
      stop("'nu' must be a single numeric value greater 'P_r'.")
    }
    if (!is.numeric(Theta) || !is.matrix(Theta) || any(dim(Theta) != c(P_r, P_r))) {
      stop("'Theta' must be a numeric matrix of dimension 'P_r' x 'P_r'.")
    }
  } else {
    delta <- NA
    xi <- NA
    D <- NA
    nu <- NA
    Theta <- NA
  }

  ### Sigma ~ IW(kappa,E)
  if (!is.numeric(kappa) || length(kappa) != 1 || kappa <= J - 1) {
    stop("'kappa' must be a single numeric value greater 'J-1'.")
  }
  if (!is.numeric(E) || !is.matrix(E) ||
    any(dim(E) != c(J - 1, J - 1))) {
    stop("'E' must be a numeric matrix of dimension 'J-1' x 'J-1'.")
  }

  ### build and return prior parameters
  prior <- list("eta" = eta,
                "Psi" = Psi,
                "delta" = delta,
                "xi" = xi,
                "D" = D,
                "nu" = nu,
                "Theta" = Theta,
                "kappa" = kappa,
                "E" = E)
  return(prior)
}

#' Set initial values for the Gibbs sampler
#'
#' @description
#' This function sets initial values for the Gibbs sampler.
#'
#' @inheritParams RprobitB_data
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
  z0 <- if (P_r > 0) rep(1, N) else NA
  m0 <- if (P_r > 0) round(rep(N, C) * 2^(C:1 - 1) / sum(2^(C:1 - 1))) else NA
  b0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = C) else NA
  Omega0 <- if (P_r > 0) matrix(rep(as.vector(diag(P_r)), C), nrow = P_r * P_r, ncol = C) else NA
  beta0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = N) else NA
  U0 <- matrix(0, nrow = J - 1, ncol = N * max(T))
  Sigma0 <- diag(J - 1)

  ### define 'init'
  init <- list(
    "alpha0" = alpha0,
    "z0" = z0,
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

#' Compute sufficient statistics
#'
#' @description
#' This function computes sufficient statistics from \code{data} for the Gibbs sampler.
#'
#' @details
#' The function provides a set of sufficient statistics from the \code{data} object
#' for the estimation routine to save computation time.
#'
#' @param data
#' An object of class \code{RprobitB_data}.
#' @param normalization
#' An object of class \code{RprobitB_normalization}, which can be created
#' via \code{\link{RprobitB_normalization}}.
#'
#' @return
#' A list of sufficient statistics on the data for Gibbs sampling, containing
#' \itemize{
#'   \item the elements \code{N}, \code{T}, \code{J}, \code{P_f} and \code{P_r} from \code{data},
#'   \item \code{Tvec}, the vector of choice occasions for each decider of length \code{N},
#'   \item \code{csTvec}, a vector of length \code{N} with the cumulated sums of \code{Tvec} starting from \code{0},
#'   \item \code{W}, a list of design matrices differenced with respect to
#'         alternative number \code{normalization$level}
#'         for each decider in each choice occasion with covariates that
#'         are linked to a fixed coefficient (or \code{NA} if \code{P_f = 0}),
#'   \item \code{X}, a list of design matrices differenced with respect to
#'         alternative number \code{normalization$level}
#'         for each decider in each choice occasion with covariates that
#'         are linked to a random coefficient (or \code{NA} if \code{P_r = 0}),
#'   \item \code{y}, a matrix of dimension \code{N} x \code{max(Tvec)} with the observed choices
#'         of deciders in rows and choice occasions in columns, decoded to numeric values with respect
#'         to their appearance in \code{data$alternatives}, where rows are filled with \code{NA} in case
#'         of an unbalanced panel,
#'   \item \code{WkW}, a matrix of dimension \code{P_f^2} x \code{(J-1)^2}, the sum over
#'         Kronecker products of each transposed element in \code{W} with itself,
#'   \item \code{XkX}, a list of length \code{N}, each element is constructed in the same way
#'         as \code{WkW} but with the elements in \code{X} and separately for each decider.
#' }
#'
#' @examples
#' data <- simulate_choices(form = choice ~ v1 | v2 + 0, N = 2, T = 1:2, J = 3, re = "v2")
#' normalization <- RprobitB:::RprobitB_normalization(J = data$J, P_f = data$P_f)
#' RprobitB:::sufficient_statistics(data = data, normalization = normalization)
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

  ### make a copy of 'data'
  data_copy <- data

  ### extract parameters
  N <- data_copy$N
  T <- data_copy$T
  Tvec <- if (length(T) == 1) rep(T, N) else T
  J <- data_copy$J
  P_f <- data_copy$P_f
  P_r <- data_copy$P_r

  ### compute utility differences with respect to 'normalization$level'
  for (n in seq_len(N)) {
    for (t in seq_len(Tvec[n])) {
      data_copy$data[[n]]$X[[t]] <- delta(J, normalization$level) %*% data_copy$data[[n]]$X[[t]]
    }
  }

  ### decode choice to numeric with respect to appearance in 'data_copy$alternatives'
  y <- matrix(0, nrow = N, ncol = max(Tvec))
  for (n in 1:N) {
    y_n <- match(data_copy$data[[n]][[2]], data_copy$alternatives)
    y[n, ] <- c(y_n, rep(NA, max(Tvec) - length(y_n)))
  }

  ### extract covariates linked to fixed ('W') and to random coefficients ('X')
  W <- list()
  X <- list()
  if (P_f > 0 & P_r > 0) {
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        W[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]][, seq_len(P_f), drop = FALSE]
        X[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]][, -seq_len(P_f), drop = FALSE]
      }
    }
  }
  if (P_f > 0 & P_r == 0) {
    X <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        W[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]]
      }
    }
  }
  if (P_f == 0 & P_r > 0) {
    W <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        X[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]]
      }
    }
  }

  ### compute \sum kronecker(t(W_nt),t(W_nt)) for each W_nt in W
  WkW <- NA
  if (P_f > 0) {
    WkW <- matrix(0, nrow = P_f^2, ncol = (J - 1)^2)
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        WkW <- WkW + kronecker(t(W[[sum(Tvec[seq_len(n - 1)]) + t]]), t(W[[sum(Tvec[seq_len(n - 1)]) + t]]))
      }
    }
  }

  ### for each fixed n, compute \sum kronecker(t(X_nt),t(X_nt))
  XkX <- NA
  if (P_r > 0) {
    XkX <- list()
    for (n in seq_len(N)) {
      XnkXn <- matrix(0, nrow = P_r^2, ncol = (J - 1)^2)
      for (t in seq_len(Tvec[n])) {
        XnkXn <- XnkXn + kronecker(t(X[[sum(Tvec[seq_len(n - 1)]) + t]]), t(X[[sum(Tvec[seq_len(n - 1)]) + t]]))
      }
      XkX[[n]] <- XnkXn
    }
  }

  ### build and return 'suff_statistics'
  suff_statistics <- list(
    "N" = N,
    "T" = T,
    "J" = J,
    "P_f" = P_f,
    "P_r" = P_r,
    "Tvec" = Tvec,
    "csTvec" = cumsum(Tvec) - Tvec,
    "W" = W,
    "X" = X,
    "y" = y,
    "WkW" = WkW,
    "XkX" = XkX
  )
  return(suff_statistics)
}

#' Plot class allocation (for \code{P_r = 2} only)
#' @description
#' This function plots the allocation of decision-maker specific coefficient vectors
#' \code{beta} given the allocation vector \code{z}, the class means \code{b},
#' and the class covariance matrices \code{Omega}.
#' @details
#' Only in the two-dimensional case, i.e. only if \code{P_r = 2}.
#' @inheritParams RprobitB_parameter
#' @param ...
#' Optional visualization parameters:
#' \itemize{
#'   \item \code{colors}, a character vector of color specifications,
#'   \item \code{perc}, a numeric between 0 and 1 to draw the \code{perc} percentile
#'         ellipsoids for the underlying Gaussian distributions (\code{perc = 0.95} per default),
#'   \item \code{r}, the current iteration number of the Gibbs sampler to be displayed in the legend,
#'   \item \code{sleep}, the number of seconds to pause after plotting.
#' }
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal
#' @examples
#' b <- matrix(c(-1,1,1,1), ncol = 2)
#' Omega <- matrix(c(0.8,0.5,0.5,1,0.5,-0.2,-0.2,0.3), ncol = 2)
#' z <- rep(1:2, each = 10)
#' beta <- sapply(z, function(z) rmvnorm(mu = b[,z], Sigma = matrix(Omega[,z], ncol = 2)))
#' RprobitB:::plot_class_allocation(beta = beta, z = z, b = b, Omega = Omega,
#'                                  colors = c("red","blue"), perc = 0.5, r = 1)
#' @importFrom mixtools ellipse
#' @importFrom graphics legend

plot_class_allocation <- function(beta, z, b, Omega, ...) {
  m <- as.vector(table(z))
  graphic_pars <- list(...)
  if(!is.null(graphic_pars[["colors"]])){
    colors <- graphic_pars[["colors"]]
  } else {
    colors <- c('black','forestgreen', 'red2', 'orange', 'cornflowerblue',
                'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue',
                'mediumorchid1', 'firebrick4', 'yellowgreen', 'lightsalmon', 'tan3',
                'tan1', 'darkgray', 'wheat4', '#DDAD4B', 'chartreuse',
                'seagreen1', 'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                'darkolivegreen1' , 'tan2', 'tomato3', '#7CE3D8', 'gainsboro')
  }
  plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]))
  points(t(beta), col = colors[z], pch = 19)
  if(!is.null(graphic_pars[["perc"]])){
    perc <- graphic_pars[["perc"]]
  } else {
    perc <- 0.95
  }
  for(c in 1:length(m)){
    mixtools::ellipse(mu = b[,c], sigma = matrix(Omega[,c], ncol = nrow(Omega)/2),
                      alpha = 1 - perc, npoints = 250, col = colors[c])
  }
  if(!is.null(graphic_pars[["r"]])){
    title = paste("Iteration", graphic_pars[["r"]])
  } else {
    title = NULL
  }
  graphics::legend("topleft", legend = paste0("class ", 1:length(m), " (", round(m / sum(m) * 100), "%)"),
                   pch = 19, col = colors[1:length(m)], title = title)
  if(!is.null(graphic_pars[["sleep"]])){
    Sys.sleep(graphic_pars[["sleep"]])
  }
}

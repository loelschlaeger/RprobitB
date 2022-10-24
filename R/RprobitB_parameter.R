#' Define probit model parameters
#'
#' These functions create and validate an object of class
#' \code{RprobitB_parameter}, which contains the parameters of a probit model,
#' see details.
#' \code{simulate_RprobitB_parameters()} simulates missing parameters from the
#' default prior distributions, see \code{\link{RprobitB_prior}}.
#' \code{validate_RprobitB_parameters()} checks the parameters.
#'
#' @param C
#' An \code{integer}, the number (greater or equal 1) of latent classes of
#' decision makers.
#' By default, \code{C = 1}.
#' @param s
#' A \code{numeric} of length \code{C}, the vector of class weights.
#' For identifiability, the vector must be descending.
#' By default, \code{s = rep(1,C)/C}.
#' @param alpha
#' A \code{matrix} of dimension \code{P_f} x \code{C}, the matrix of fixed
#' coefficients.
#' The coefficients for class \code{c} are stored in column \code{c}.
#' @param b
#' A \code{matrix} of dimension \code{P_r} x \code{C}, the matrix of class
#' means.
#' The mean vector for class \code{c} is stored in column \code{c}.
#' @param Omega
#' A \code{matrix} of dimension \code{P_r^2} x \code{C}, the matrix of class
#' covariance matrices.
#' The covariance matrix for class \code{c} is stored as a vector in column
#' \code{c}.
#' @param Sigma
#' A \code{matrix} of dimension \code{J} x \code{J}, the error term covariance
#' matrix.
#' In the ordered probit model (see details), \code{Sigma} can be a
#' \code{matrix} of dimension \code{1} x \code{1} or a single \code{numeric}.
#' @param Sigma_diff
#' A \code{matrix} of dimension \code{J-1} x \code{J-1}, the differenced error
#' term covariance matrix
#' \code{Sigma_diff} is assumed to be differenced with respect to alternative
#' \code{diff_alt}, see details.
#' \code{Sigma_diff} is ignored in case of the ordered probit model
#' (see details) or if \code{Sigma} is specified.
#' @param diff_alt
#' An \code{integer} from \code{1} to \code{J}, the reference alternative for
#' utility differencing that maps \code{Sigma} to \code{Sigma_diff}, see
#' details.
#' By default, \code{diff_alt = 1}.
#' @param beta
#' A \code{matrix} of dimension \code{P_r} x \code{N}, the matrix of the
#' decider-specific coefficient vectors.
#' The coefficient vector for decider \code{n} is stored in column \code{n}.
#' @param z
#' A \code{numeric} of length \code{N}, the vector of the allocation variables.
#' Entry \code{n} of \code{z} is an integer from \code{1} to \code{C} and
#' denotes the allocated class for decider \code{n}.
#' @param d
#' A \code{numeric} of length \code{J-2}, the vector of logarithmic increases of
#' the utility thresholds.
#' Only relevant in the ordered probit model case (see details).
#'
#' @return
#' An \code{RprobitB_parameter} object.
#'
#' @details
#' # The probit model
#' Assume that we know the choices of \eqn{N} deciders choosing between
#' \eqn{J \geq 2} alternatives at each of \eqn{T} choice occasions.
#' Specific to each decider, alternative and choice occasion, we observe \eqn{P}
#' covariates, a linear combination of which eventually explains the latent
#' random utility:
#' \deqn{U_{ntj} = X_{ntj}' \tilde{\beta}_n + \epsilon_{ntj},}
#' \eqn{n=1,\dots,N}, \eqn{t=1,\dots,T}, and \eqn{j=1,\dots,J}.
#' Here, \eqn{X_{ntj}} is a (column) vector of \eqn{P} characteristics specific
#' to alternative \eqn{j} as faced by decider \eqn{n} at choice occasion
#' \eqn{t}, \eqn{\tilde{\beta}_n \in \mathbb{R}^{P}} is the coefficient vector
#' encoding the preferences of \eqn{n}, and
#' \eqn{(\epsilon_{nt:}) = (\epsilon_{nt1},\dots,\epsilon_{ntJ})' \sim
#' \text{MVN}_{J} (0,\Sigma)} is the model's error term vector for \eqn{n} at
#' \eqn{t}.
#'
#' The value \eqn{U_{ntj}} can be interpreted as the decider's utility.
#' It is unobserved by the researcher, but we assume that the deciders know
#' their utilities for each alternative and make a choice which is consistent
#' with utility maximization. Therefore,
#' \deqn{y_{nt} = \operatorname*{argmax}_{j = 1,\dots,J} U_{ntj},}
#' where \eqn{y_{nt}=j} denotes the event that decider \eqn{n} chooses \eqn{j}
#' at her \eqn{t}-th choice occasion.
#'
#' Entries of the decider-specific coefficient vector \eqn{\tilde{\beta}_n} can
#' be fixed across deciders, in which case the coefficient vector is of the form
#' \eqn{\tilde{\beta}_n' = (\alpha', \beta_n')'}, where
#' \eqn{\alpha \in \mathbb{R}^{P_f}} are \eqn{P_f} coefficients that are
#' constant across deciders and \eqn{\beta_n} are \eqn{P_r} decider-specific
#' coefficients, \eqn{P_f + P_r = P}.
#'
#' The decider-specific coefficients are assumed to be realizations of an
#' underlying mixing distribution and to be independent of the characteristics
#' \eqn{X_{ntj}} and the errors \eqn{(\epsilon_{nt:})}.
#' This distribution characterizes heterogeneity among the deciders and allows
#' for individual sensitivities. As mixing distribution, we assume a mixture of
#' \eqn{P_r}-variate Gaussian densities \eqn{\phi_{P_r}} with mean vectors
#' \eqn{b = (b_c)_{c}} and covariance matrices \eqn{\Omega = (\Omega_c)_{c}}
#' using \eqn{C} components:
#' \deqn{\beta_n\mid b,\Omega \sim \sum_{c=1}^{C} s_c \phi_{P_r} (\cdot \mid
#' b_c,\Omega_c).}
#' Here, \eqn{(s_c)_{c}} are weights satisfying \eqn{0 < s_c\leq 1} for
#' \eqn{c=1,\dots,C} and \eqn{\sum_c s_c=1}.
#'
#' One interpretation of the latent class model is obtained by introducing
#' variables \eqn{z=(z_n)_n}, allocating each decision maker \eqn{n} to class
#' \eqn{c} with probability \eqn{s_c}, i.e.,
#' \deqn{\text{Prob}(z_n=c)=s_c \land \beta_n \mid z,b,\Omega \sim
#' \phi_{P_r}(\cdot \mid b_{z_n},\Omega_{z_n}).}
#'
#' # Ordered probit model
#' When the set of choice alternatives is ordered, the probit model has only a
#' single utility
#' \deqn{U_{nt} = X_{nt}' \tilde{\beta}_n + \epsilon_{nt},}
#' \eqn{\epsilon_{nt} \sim \text{MVN}_{1} (0,\Sigma)},
#' per decider \eqn{n} and choice occasion \eqn{t}. The utility can be
#' interpreted as the level of association that \eqn{n} has with the choice
#' question. It falls into discrete categories, which in turn are linked to the
#' ordered alternatives \eqn{j=1,\dots,J}. Formally,
#' \deqn{y_{nt} = \sum_{j = 1,\dots,J} j \cdot I(\gamma_{j-1} < U_{nt} \leq
#' \gamma_{j}),}
#' where \eqn{\gamma_0 = -\infty} and \eqn{\gamma_J = +\infty}. This implies
#' that alternative \eqn{j} is chosen, if the utility falls into the interval
#' \eqn{(\gamma_{j-1}, \gamma_j]}.
#' Monotonicity of the thresholds \eqn{(\gamma_j)_{j=1,\dots,J-1}} is ensured
#' by estimating logarithmic increments \eqn{d_j} with
#' \eqn{\gamma_j = \sum_{i\leq j} \exp{(d_i)}}, \eqn{j=1,\dots,J-1}.
#' For level normalization, we fix \eqn{\gamma_1 = 0}.
#'
#' # Level and scale normalization
#' The probit model is invariant towards the level and scale of utility, hence
#' a transformation is required for identifiability.
#'
#' For level normalization, we take utility differences:
#' \deqn{\tilde{U}_{ntj} = \tilde{X}_{ntj}' \tilde{\beta}_n +
#' \tilde{\epsilon}_{ntj},}
#' where (choosing some alternative \eqn{k \in \{1,\dots,J\}} as the reference,
#' also denoted by \code{diff_alt})
#' \eqn{\tilde{U}_{ntj} = U_{ntj} - U_{ntk}},
#' \eqn{\tilde{X}_{ntj} = X_{ntj} - X_{ntk}}, and
#' \eqn{\tilde{\epsilon}_{ntj} = \epsilon_{ntj} - \epsilon_{ntk}} for
#' \eqn{j\neq k}.
#' The error term differences \eqn{(\tilde{\epsilon}_{nt:})} again are
#' multivariate normally distributed with mean \eqn{0} but transformed
#' covariance matrix \eqn{\tilde{\Sigma}}, also denoted by \code{Sigma_diff}.
#' See \code{\link{diff_Sigma}} for computing \code{Sigma_diff} from
#' \code{Sigma}, and \code{\link{undiff_Sigma}} for the other way around.
#'
#' For level normalization in the ordered probit model, we fix
#' \eqn{\gamma_1 = 0}.
#'
#' For scale normalization, we fix the top left element of \code{Sigma_diff} to
#' \eqn{1} (or \code{Sigma = 1} in the ordered probit case).
#' Other options exist, see \code{\link{transform}}.
#'
#' @export
#' @keywords object

RprobitB_parameter <- function(
    C = 1, s = rep(1,C)/C, alpha = NA, b = NA, Omega = NA, Sigma = NA,
    Sigma_diff = NA, diff_alt = 1, beta = NA, z = NA, d = NA
) {
  stopifnot(identical(C, NA) || is.numeric(C))
  stopifnot(identical(s, NA) || is.numeric(s))
  stopifnot(identical(alpha, NA) || is.numeric(alpha))
  stopifnot(identical(b, NA) || is.numeric(b))
  stopifnot(identical(Omega, NA) || is.numeric(Omega))
  stopifnot(identical(Sigma, NA) || is.numeric(Sigma))
  stopifnot(identical(Sigma_diff, NA) || is.numeric(Sigma_diff))
  stopifnot(identical(diff_alt, NA) || is.numeric(diff_alt))
  stopifnot(identical(beta, NA) || is.numeric(beta))
  stopifnot(identical(z, NA) || is.numeric(z))
  stopifnot(identical(d, NA) || is.numeric(d))
  structure(
    list(
      "C" = C,
      "s" = s,
      "alpha" = alpha,
      "b" = b,
      "Omega" = Omega,
      "Sigma" = Sigma,
      "Sigma_diff" = Sigma_diff,
      "diff_alt" = diff_alt,
      "beta" = beta,
      "z" = z,
      "d" = d
    ),
    class = c("RprobitB_parameter", "list")
  )
}

#' @rdname RprobitB_parameter
#' @param x
#' An \code{RprobitB_parameter} object.

is.RprobitB_parameter <- function(x) {
  inherits(x, "RprobitB_parameter")
}

#' @rdname RprobitB_parameter
#' @inheritParams RprobitB_formula
#' @param J
#' An \code{integer}, the number of choice alternatives.
#' @param N
#' An \code{integer}, the number of deciders.
#' @param seed
#' An \code{integer}, passed to \code{set.seed()} to make the random part
#' reproducible.
#' By default, \code{seed = NULL}, i.e., no seed is set.
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#' @examples
#' (x <- RprobitB_parameter(C = 2))
#' formula <- choice ~ A | B
#' re <- "A"
#' J <- 3
#' N <- 100
#' (x <- simulate_RprobitB_parameter(x, formula = formula, re = re, J = J, N = N, seed = 1))
#' (x <- validate_RprobitB_parameter(x, formula = formula, re = re, J = J, N = N))

simulate_RprobitB_parameter <- function(
    x = RprobitB_parameter(), formula, re  = NULL, ordered = FALSE, J, N,
    seed = NULL
  ) {
  stopifnot(is.RprobitB_parameter(x), is_pos_int(J), is_pos_int(N))
  if (missing(formula)) {
    RprobitB_stop("Please specify the input 'formula'.")
  }
  P_f <- compute_P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
  if (!is.null(seed)) set.seed(seed)
  if (identical(x$alpha, NA) && P_f > 0) {
    alpha_prior <- RprobitB_prior_alpha(P_f)
    alpha #<- rmvnorm(mean = alpha_prior$mean, Sigma = alpha_prior$Sigma)
  }


}

#' @rdname RprobitB_parameter

validate_RprobitB_parameter <- function(
    x = RprobitB_parameter(), formula, re  = NULL, ordered = FALSE, J, N
) {

  P_f <- P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- P_r(formula = formula, re = re, J = J, ordered = ordered)
  if (sample) {
    if (is.null(alpha) && P_f > 0) {
      alpha <- RprobitB_prior("alpha", P_f = P_f)
    }

  } else {

  }

  ### alpha
  if (P_f == 0) {
    alpha <- NA
  } else {
    if (is.null(alpha) && !sample) {
      alpha <- NA
    } else {
      if (is.null(alpha)) {
        alpha <- round(stats::runif(P_f, -3, 3), 1)
      }
      if (length(alpha) != P_f || !is.numeric(alpha)) {
        stop("'alpha' must be a numeric vector of length ", P_f, ".",
             call. = FALSE
        )
      }
      names(alpha) <- create_labels_alpha(P_f)
    }
  }

  ### C, s, b, Omega, z, beta
  if (P_r == 0) {
    C <- NA
    s <- NA
    b <- NA
    Omega <- NA
    z <- NA
    beta <- NA
  } else {

    ### C
    if (!is.null(C)) {
      if (!is.numeric(C) || !C %% 1 == 0 || !C > 0) {
        stop("'C' must be a number greater or equal 1.", call. = FALSE)
      }
    } else {
      C <- 1
    }

    ### s
    if (C == 1) {
      s <- 1
    } else {
      if (is.null(s) && !sample) {
        s <- NA
      } else {
        if (is.null(s)) {
          s <- round(sort(as.vector(rdirichlet(rep(1, C))), decreasing = T), 2)
          s[C] <- 1 - sum(s[-C])
        }
        if (length(s) != C || !is.numeric(s) ||
            abs(sum(s) - 1) > .Machine$double.eps || is.unsorted(rev(s))) {
          stop("'s' must be a non-ascending numeric vector of length ", C,
               " which sums up to 1.", call. = FALSE
          )
        }
        names(s) <- create_labels_s(P_r, C)
      }
    }

    ### b
    if (is.null(b) && !sample) {
      b <- NA
    } else {
      if (is.null(b)) {
        b <- matrix(0, nrow = P_r, ncol = C)
        for (c in 1:C) b[, c] <- round(stats::runif(P_r, -3, 3), 1)
      }
      b <- as.matrix(b)
      if (!is.numeric(b) || nrow(b) != P_r || ncol(b) != C) {
        stop("'b' must be a numeric matrix of dimension ", P_r, " x ", C, ".",
             call. = FALSE
        )
      }
      names(b) <- create_labels_b(P_r, C)
    }

    ### Omega
    if (is.null(Omega) && !sample) {
      Omega <- NA
    } else {
      if (is.null(Omega)) {
        Omega <- matrix(0, nrow = P_r * P_r, ncol = C)
        for (c in 1:C) {
          Omega[, c] <- as.vector(rwishart(P_r, diag(P_r))$W)
        }
      }
      Omega <- as.matrix(Omega)
      if (!is.numeric(Omega) || nrow(Omega) != P_r * P_r ||
          ncol(Omega) != C) {
        stop(
          "'Omega' must be a numeric matrix of dimension ", P_r * P_r, " x ",
          C, ".", call. = FALSE
        )
      }
      for (c in 1:C) {
        if (!is_covariance_matrix(matrix(Omega[, c], nrow = P_r, ncol = P_r))) {
          stop(paste("Column", c, "in 'Omega' builds no covariance matrix."),
               call. = FALSE
          )
        }
      }
      names(Omega) <- create_labels_Omega(P_r, C, cov_sym = TRUE)
    }

    ### z
    if (is.null(z) && !sample) {
      z <- NA
    } else {
      if (is.null(z)) {
        z <- sample(1:C, N, prob = s, replace = TRUE)
      }
      if (length(z) != N || !is.numeric(z) || !all(z %in% 1:C)) {
        stop(
          "'z' must be a numeric vector of length ", N,
          " with elements of value ", paste(seq_len(C), collapse = ", "), ".",
          call. = FALSE
        )
      }
    }

    ### beta
    if (is.null(beta) && !sample) {
      beta <- NA
    } else {
      if (is.null(beta)) {
        beta <- matrix(0, nrow = P_r, ncol = N)
        for (n in seq_len(N)) {
          beta[, n] <- b[, z[n]] +
            t(chol(matrix(Omega[, z[n]], nrow = P_r, ncol = P_r))) %*%
            stats::rnorm(P_r)
        }
      }
      if (!is.numeric(beta) || nrow(beta) != P_r ||
          ncol(beta) != N) {
        stop("'beta' must be a numeric matrix of dimension ", P_r, " x ", N,
             ".", call. = FALSE
        )
      }
    }
  }

  ### Sigma
  if (is.null(Sigma_full) && is.null(Sigma) && !sample) {
    Sigma <- NA
    Sigma_full <- NA
  } else {
    if (ordered) {
      Sigma_full <- NA
      if (is.null(Sigma)) {
        Sigma <- round(runif(1, min = 1, max = 3), 2)
      }
      if (length(Sigma) != 1 || !is.numeric(Sigma) || is.matrix(Sigma)) {
        stop("'Sigma' must be a single numeric value.", call. = FALSE)
      }
      names(Sigma) <- create_labels_Sigma(J, ordered = TRUE)
    } else {
      if (is.null(Sigma)) {
        if (is.null(Sigma_full)) {
          Sigma_full <- rwishart(J, diag(J))$W
        } else {
          Sigma_full <- as.matrix(Sigma_full)
        }
        Sigma <- delta(J, J) %*% Sigma_full %*% t(delta(J, J))
      } else {
        Sigma <- as.matrix(Sigma)
        Sigma_full <- undiff_Sigma(Sigma, i = J)
      }
      if (!(is_covariance_matrix(Sigma) && nrow(Sigma) == J - 1)) {
        stop("'Sigma' is not a differenced covariance matrix of dimension ",
             J - 1, " x ", J - 1, ".",
             call. = FALSE
        )
      }
      if (!(is_covariance_matrix(Sigma_full) && nrow(Sigma_full) == J)) {
        stop("'Sigma_full' is not a covariance matrix of dimension ", J,
             " x ", J, ".",
             call. = FALSE
        )
      }
      names(Sigma) <- create_labels_Sigma(J, cov_sym = TRUE)
      names(Sigma_full) <- create_labels_Sigma(J + 1, cov_sym = TRUE)
    }
  }

  ### d
  if (ordered) {
    if(is.null(d)) {
      d <- round(runif(J-2),2)
    }
    if(length(d) != J-2 || !is.numeric(d)) {
      stop("'d' must be a numeric vector of length ", J-2, ".", call. = FALSE)
    }
    names(d) <- create_labels_d(J, ordered = TRUE)
  } else {
    d <- NA
  }

  ### build and return 'RprobitB_parameter'-object
  out <- list(
    "alpha" = alpha,
    "C" = C,
    "s" = s,
    "b" = b,
    "Omega" = Omega,
    "Sigma" = Sigma,
    "Sigma_full" = Sigma_full,
    "beta" = beta,
    "z" = z,
    "d" = d
  )
  class(out) <- c("RprobitB_parameter", "list")
  return(out)
}

#' @rdname RprobitB_parameter
#' @param ...
#' A \code{character} (vector), the names of model parameters to be printed.
#' By default, all available parameters are printed.
#' @inheritParams print_matrix
#' @exportS3Method

print.RprobitB_parameter <- function(
    x, ..., rowdots = 4, coldots = 4, digits = 2, simplify = FALSE,
    details = !simplify
  ) {
  stopifnot(inherits(x, "RprobitB_parameter"))
  pars <- list(...)
  ind <- if (length(pars) != 0) {
    sapply(pars, function(par) which(names(x) == par))
  } else {
    seq_along(x)
  }
  cat(cli::style_underline("Parameter:\n"))
  for (i in ind) {
    if(!identical(x[[i]], NA)) {
      print_matrix(
        x[[i]], rowdots = rowdots, coldots = coldots, digits = digits,
        label = names(x)[i], simplify = simplify, details = details
      )
      cat("\n")
    }
  }
  return(invisible(x))
}

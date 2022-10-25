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
#' For identifiability, the vector must be decreasing.
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
#' In the ordered probit model (see details), \code{Sigma} is a
#' \code{matrix} of dimension \code{1} x \code{1} (or simply a single
#' \code{numeric}).
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
    C = 1, s = NA, alpha = NA, b = NA, Omega = NA, Sigma = NA,
    Sigma_diff = NA, diff_alt = 1, beta = NA, z = NA, d = NA
) {
  stopifnot(identical(C, NA) || is_pos_int(C))
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
#' (x <- simulate_RprobitB_parameter(x, formula = formula, re = re, J = J, N = N))
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
  if (identical(x$s, NA)) {
    s_prior <- RprobitB_prior_s(C = x$C)
    x$s <- sort(
      rdirichlet(
        concentration = s_prior$s_prior_concentration
      ),
      decreasing = TRUE
    )
  }
  if (identical(x$alpha, NA) && P_f > 0) {
    alpha_prior <- RprobitB_prior_alpha(P_f = P_f)
    x$alpha <- do.call(
      cbind,
      replicate(
        x$C,
        rmvnorm(
          mean = alpha_prior$alpha_prior_mean,
          Sigma = alpha_prior$alpha_prior_Sigma
        ),
        simplify = FALSE
      )
    )
  }
  if (identical(x$b, NA) && P_r > 0) {
    b_prior <- RprobitB_prior_b(P_r = P_r)
    x$b <- do.call(
      cbind,
      replicate(
        x$C,
        rmvnorm(
          mean = b_prior$b_prior_mean,
          Sigma = b_prior$b_prior_Sigma
        ),
        simplify = FALSE
      )
    )
  }
  if (identical(x$Omega, NA) && P_r > 0) {
    Omega_prior <- RprobitB_prior_Omega(P_r = P_r)
    x$Omega <- do.call(
      cbind,
      replicate(
        x$C,
        rwishart(
          df = Omega_prior$Omega_prior_df,
          scale = Omega_prior$Omega_prior_scale,
          inv = TRUE
        ),
        simplify = FALSE
      )
    )
  }
  if (ordered) {
    x$Sigma_diff <- NA
    if (identical(x$Sigma, NA)) {
      Sigma_prior <- RprobitB_prior_Sigma(ordered = TRUE, J = J)
      x$Sigma <- rwishart(
        df = Sigma_prior$Sigma_prior_df,
        scale = Sigma_prior$Sigma_prior_scale,
        inv = TRUE
      )
    }
  } else {
    if (identical(x$Sigma, NA)) {
      if (identical(x$Sigma_diff, NA)) {
        Sigma_diff_prior <- RprobitB_prior_Sigma_diff(ordered = FALSE, J = J)
        x$Sigma_diff <- rwishart(
          df = Sigma_diff_prior$Sigma_diff_prior_df,
          scale = Sigma_diff_prior$Sigma_diff_prior_scale,
          inv = TRUE
        )
      }
      x$Sigma <- undiff_Sigma(x$Sigma_diff, diff_alt = x$diff_alt)
    } else {
      x$Sigma_diff <- diff_Sigma(x$Sigma, diff_alt = x$diff_alt)
    }
  }
  if (identical(x$z, NA)) {
    x$z <- sample.int(x$C, size = N, replace = TRUE, prob = x$s)
  }
  if (identical(x$beta, NA) && P_r > 0) {
    x$beta <- do.call(
      cbind,
      lapply(x$z, function(c) {
        rmvnorm(
          mean = x$b[,c],
          Sigma = matrix(x$Omega[,c], nrow = P_r, ncol = P_r)
        )
      })
    )
  }
  if (ordered) {
    d_prior <- RprobitB_prior_d(ordered = TRUE, J = J)
    x$d <- rmvnorm(
      mean = d_prior$d_prior_mean,
      Sigma = d_prior$d_prior_Sigma
    )
  } else {
    x$d <- NA
  }
  validate_RprobitB_parameter(
    x = x, formula = formula, re = re, ordered = ordered, J = J, N = N
  )
}

#' @rdname RprobitB_parameter
#' @importFrom glue glue glue_collapse

validate_RprobitB_parameter <- function(
    x = RprobitB_parameter(), formula, re  = NULL, ordered = FALSE, J, N
) {
  stopifnot(is.RprobitB_parameter(x), is_pos_int(J), is_pos_int(N))
  if (missing(formula)) {
    RprobitB_stop("Please specify the input 'formula'.")
  }
  P_f <- compute_P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
  ### check C
  if (!is_pos_int(x$C)) {
    RprobitB_stop(
      "C is expected to be a positive integer.",
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$C}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  ### check s
  if (x$C == 1) x$s <- 1
  if (length(x$s) != x$C || !is.numeric(x$s) ||
      abs(sum(x$s) - 1) > .Machine$double.eps || is.unsorted(rev(x$s))) {
    RprobitB_stop(
      glue::glue(
        "s is expected to be a descending numeric vector of length {C} which sums up to 1."
      ),
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$s}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  ### check alpha
  if (P_f == 1) x$alpha <- matrix(x$alpha, nrow = 1, ncol = x$C)
  if (x$C == 1) x$alpha <- matrix(x$alpha, nrow = P_f, ncol = 1)
  if (!is.numeric(x$s) || !is.matrix(x$alpha) || nrow(x$alpha) != P_f ||
      ncol(x$alpha) != x$C) {
    RprobitB_stop(
      glue::glue(
        "alpha is expected to be a numeric matrix of dimension {P_f} x {x$C}."
      ),
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$alpha}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  ### check b
  if (P_r == 1) x$b <- matrix(x$b, nrow = 1, ncol = x$C)
  if (x$C == 1) x$b <- matrix(x$b, nrow = P_r, ncol = 1)
  if (!is.numeric(x$b) || !is.matrix(x$b) || nrow(x$b) != P_r || ncol(x$b) != x$C) {
    RprobitB_stop(
      glue::glue(
        "b is expected to be a numeric matrix of dimension {P_r} x {x$C}."
      ),
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$b}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  ### check Omega
  if (P_r == 1) x$Omega <- matrix(x$Omega, nrow = 1, ncol = x$C)
  if (x$C == 1) x$Omega <- matrix(x$Omega, nrow = P_r^2, ncol = 1)
  if (!is.numeric(x$Omega) || !is.matrix(x$Omega) || nrow(x$Omega) != P_r^2 ||
      ncol(x$Omega) != x$C) {
    RprobitB_stop(
      glue::glue(
        "Omega is expected to be a numeric matrix of dimension {P_r^2} x {x$C}."
      ),
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$Omega}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  for (c in 1:x$C) {
    if (!is_cov_matrix(matrix(x$Omega[,c], nrow = P_r, ncol = P_r))) {
      RprobitB_stop(
        glue::glue(
          "Column {c} in Omega is expected to be a proper covariance matrix."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{x$Omega[,c]}'"),
          sep = " ",
          width = getOption("width") - 3
        ),
        "Check it with 'is_cov_matrix()'."
      )
    }
  }
  ### check diff_alt
  if (!is_pos_int(x$diff_alt) || x$diff_alt > J) {
    RprobitB_stop(
      glue::glue(
        "diff_alt is expected to be a positive integer smaller or equal {J}."
      ),
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$diff_alt}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  ### check Sigma / Sigma_diff
  if (ordered) {
    x$Sigma_diff <- NA
    x$Sigma <- matrix(x$Sigma)
    if (!is.numeric(x$Sigma) || !is.matrix(x$Sigma) || nrow(x$Sigma) != 1 ||
        ncol(x$Sigma) != 1 || x$Sigma[1,1] <= 0) {
      RprobitB_stop(
        glue::glue(
          "Sigma is expected to be a numeric 1 x 1 matrix with a positive entry."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{x$Sigma}'"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    if (!is.numeric(x$Sigma) || !is.matrix(x$Sigma) || nrow(x$Sigma) != J ||
        ncol(x$Sigma) != J) {
      RprobitB_stop(
        glue::glue(
          "Sigma is expected to be a numeric matrix of dimension {J} x {J}."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{x$Sigma}'"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
    if (!is_cov_matrix(x$Sigma)) {
      RprobitB_stop(
        glue::glue(
          "Sigma is expected to be a proper covariance matrix."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{x$Sigma}'"),
          sep = " ",
          width = getOption("width") - 3
        ),
        "Check it with 'is_cov_matrix()'."
      )
    }
    if (!is.numeric(x$Sigma_diff) || !is.matrix(x$Sigma_diff) ||
        nrow(x$Sigma_diff) != J-1 || ncol(x$Sigma_diff) != J-1) {
      RprobitB_stop(
        glue::glue(
          "Sigma_diff is expected to be a numeric matrix of dimension {J-1} x {J-1}."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{x$Sigma_diff}'"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
    if (!is_cov_matrix(x$Sigma_diff)) {
      RprobitB_stop(
        glue::glue(
          "Sigma_diff is expected to be a proper covariance matrix."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{x$Sigma_diff}'"),
          sep = " ",
          width = getOption("width") - 3
        ),
        "Check it with 'is_cov_matrix()'."
      )
    }
    if (!all.equal(diff_Sigma(x$Sigma, diff_alt = x$diff_alt), x$Sigma_diff)) {
      RprobitB_stop(
        glue::glue(
          "Differencing Sigma with respect to alternative {x$diff_alt} is expected to yield Sigma_diff."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{diff_Sigma(x$Sigma, diff_alt = x$diff_alt)}'"),
          sep = " ",
          width = getOption("width") - 3
        ),
        "Check it with 'diff_Sigma(Sigma, diff_alt = {x$diff_alt})'."
      )
    }
  }
  ### check beta
  if (P_r == 1) x$beta <- matrix(x$beta, nrow = 1, ncol = N)
  if (N == 1) x$beta <- matrix(x$beta, nrow = P_r, ncol = 1)
  if (!is.numeric(x$beta) || !is.matrix(x$beta) || nrow(x$beta) != P_r ||
      ncol(x$beta) != N) {
    RprobitB_stop(
      glue::glue(
        "beta is expected to be a numeric matrix of dimension {P_r} x {N}."
      ),
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$beta}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  ### check z
  if (!is.vector(x$z) || length(x$z) != N || !is.numeric(x$z) ||
      !all(x$z %in% 1:x$C)) {
    RprobitB_stop(
      glue::glue(
        "z is expected to be an integer vector with values {paste(seq.int(x$C), collapse = ", ")}."
      ),
      "Instead, it is (collapsed):",
      glue::glue_collapse(
        glue::glue("'{x$z}'"),
        sep = " ",
        width = getOption("width") - 3
      )
    )
  }
  ### check d
  if (ordered) {
    if (!is.vector(x$d) || length(x$d) != J-2 || !is.numeric(x$d)) {
      RprobitB_stop(
        glue::glue(
          "d is expected to be a numeric vector of length {J-2}."
        ),
        "Instead, it is (collapsed):",
        glue::glue_collapse(
          glue::glue("'{x$d}'"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    x$d <- NA
  }
  return(x)
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

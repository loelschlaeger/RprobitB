#' Define probit model prior
#'
#' This function defines an object of class \code{RprobitB_prior}, which
#' contains the prior specification for a probit model.
#'
#' @inheritParams RprobitB_formula
#' @inheritDotParams RprobitB_prior_alpha
# TODO #' @inheritDotParams RprobitB_prior_s
# TODO #' @inheritDotParams RprobitB_prior_b
# TODO #' @inheritDotParams RprobitB_prior_Omega
# TODO #' @inheritDotParams RprobitB_prior_Sigma
# TODO #' @inheritDotParams RprobitB_prior_d
#'
#' @details
#' # Model priors
#'
#' ## Conjugate priors
#' A priori, the following
#' \href{https://en.wikipedia.org/wiki/Conjugate_prior}{conjugate prior distributions}
#' are assumed for the model parameters:
#' * \code{alpha ~ Normal(mean = alpha_prior_mean, Sigma = alpha_prior_Sigma)}
#' * \code{s ~ Dirichlet(concentration = s_prior_concentration)}
#' * \code{b ~ Normal(mean = b_prior_mean, Sigma = b_prior_Sigma)}
#' * \code{Omega ~ Inverse-Wishart(df = Omega_prior_df, scale = Omega_prior_scale)}
#' * \code{Sigma ~ Inverse-Wishart(df = Sigma_prior_df, scale = Sigma_prior_scale)}
#' * \code{d ~ Normal(mean = d_prior_mean, Sigma = d_prior_Sigma)}
#'
#' ## Default parameters for the conjugate priors
#' By default, the following parameters are set for the conjugate priors:
#' * \code{alpha}
#'   - \code{alpha_prior_mean = numeric(P_f)}
#'   - \code{alpha_prior_Sigma = 10*diag(P_f)}
#' * \code{s}
#'   - \code{s_prior_concentration = rep(1, C)}
#' * \code{b}
#'   - \code{b_prior_mean = numeric(P_r)}
#'   - \code{b_prior_Sigma = 10*diag(P_r)}
#' * \code{Omega}
#'   - \code{Omega_prior_df = P_r + 2}
#'   - \code{Omega_prior_scale = diag(P_r)}
#' * \code{Sigma}
#'   - \code{Sigma_prior_df = if (ordered) 4 else J + 1}
#'   - \code{Sigma_prior_scale = if (ordered) diag(1) else diag(J-1)}
#' * \code{d}
#'   - \code{d_prior_mean = numeric(J - 2)}
#'   - \code{d_prior_Sigma = diag(J - 2)}
#' These parameters can be overwritten by submitting eponymous inputs.
#'
#' ## Custom priors
#' By default, conjugate priors are used, but custom priors can be
#' specified as well. To specify a custom prior for parameter \code{<par>},
#' use the input \code{<par>_prior_custom}, which should compute the density
#' of the prior (or a value that is proportional), see \sQuote{Examples}.
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#'
#' @return
#' An \code{RprobitB_prior} object.
#'
#' @examples
#' ### default conjugate prior distributions
#' RprobitB_prior(formula = choice ~ A, J = 2)
#'
#' ### conjugate prior distributions with custom parameters
#' RprobitB_prior(
#'   formula = choice ~ A | B + 0, re = "A", J = 4, C = 2,
#'   alpha_prior_mean = c(-10, 0, 10),
#'   s_prior_concentration = c(1, 2)
#' )
#'
#' ### custom prior distributions
#' RprobitB_prior(
#'   formula = choice ~ A | B | C, re = "C", J = 3,
#'   alpha_prior_custom = function(x) {
#'     dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
#'   }
#' )
#'
#' @keywords specification
#'
#' @export

RprobitB_prior <- function(formula, re = NULL, J, C = 1, ordered = FALSE, ...) {
  if (missing(formula)) {
    RprobitB_stop("Please specify the input 'formula'.")
  }
  if (missing(J)) {
    RprobitB_stop("Please specify the input 'J'.")
  }
  P_f <- P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- P_r(formula = formula, re = re, J = J, ordered = ordered)
  structure(
    list(
      "alpha" = RprobitB_prior_alpha(P_f = P_f, ...),
      "s" = RprobitB_prior_s(C = C, ...),
      "b" = RprobitB_prior_b(P_r = P_r, ...),
      "Omega" = RprobitB_prior_Omega(P_r = P_r, ...),
      "Sigma" = RprobitB_prior_Sigma(J = J, ordered = ordered, ...),
      "d" = RprobitB_prior_d(J = J, ordered = ordered, ...)
    ),
    class = c("RprobitB_prior", "list")
  )
}

#' @rdname RprobitB_prior
#' @param x
#' An \code{RprobitB_prior} object.

is.RprobitB_prior <- function(x) {
  inherits(x, "RprobitB_prior")
}

#' @rdname RprobitB_prior
#' @exportS3Method
#' @importFrom cli style_underline

print.RprobitB_prior <- function(x, ...) {
  stopifnot(inherits(x, "RprobitB_prior"))
  cat(cli::style_underline("Priors:\n"))
  lapply(x, print)
}

#' Define \code{alpha} prior
#'
#' This function defines the prior distributions for the probit model parameter
#' \code{alpha}.
#'
#' @param P_f
#' An \code{integer}, the number of fixed model effects.
#' Can be computed via the function \code{\link{P_f}}.
#' @param alpha_prior_mean
#' A \code{numeric} of length \code{P_f}, the mean vector for the conjugate
#' normal prior distribution of \code{alpha}.
#' By default, \code{alpha_prior_mean = numeric(P_f)}.
#' @param alpha_prior_Sigma
#' A \code{matrix} of dimension \code{P_f} x \code{P_f}, the covariance
#' matrix for the conjugate normal prior distribution of \code{alpha}.
#' By default, \code{alpha_prior_Sigma = 10 * diag(P_f)}.
#' @param alpha_prior_custom
#' A \code{function}, a custom prior density function for \code{alpha},
#' see details.
#' By default, \code{alpha_prior_custom = NULL}, i.e., no custom prior.
#' @param alpha_prior_custom_test_par
#' A \code{numeric} of length \code{P_f}, a test input for the custom prior
#' density function \code{alpha_prior_custom}.
#' By default, \code{alpha_prior_custom_test_par = numeric(P_f)}.
#' Ignored if \code{alpha_prior_custom = NULL}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An \code{RprobitB_prior_alpha} object or \code{NA} if \code{P_f = 0}.
#'
#' @examples
#' ### conjugate prior: alpha ~ Normal(0, 10*I)
#' RprobitB_prior_alpha(
#'   P_f = 2,
#'   alpha_prior_mean = numeric(P_f),
#'   alpha_prior_Sigma = 10 * diag(P_f)
#' )
#'
#' ### custom prior: alpha_1 ~ Normal(0,1), alpha_2 ~ Uniform(0,1)
#' RprobitB_prior_alpha(
#'   P_f = 2,
#'   alpha_prior_custom = function(x) {
#'     dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
#'   }
#' )
#'
#' @importFrom glue glue
#' @importFrom stats dnorm dunif
#'
#' @keywords internal

RprobitB_prior_alpha <- function(
    P_f, alpha_prior_mean = numeric(P_f), alpha_prior_Sigma = 10 * diag(P_f),
    alpha_prior_custom = NA, alpha_prior_custom_test_par = numeric(P_f)
  ) {
  if (P_f == 0) {
    return (NA)
  } else if (!is.na(alpha_prior_custom)) {
    conjugate <- FALSE
    alpha_prior_mean <- NA
    alpha_prior_Sigma <- NA
    if (!is.function(alpha_prior_custom)) {
      RprobitB_stop(
        "Custom prior for alpha is misspecified.",
        "'alpha_prior_custom' should be a function.",
        glue::glue("Instead, it is of class {class(alpha_prior_custom)}.")
      )
    }
    alpha_prior_custom_test <- try(
      alpha_prior_custom(alpha_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(alpha_prior_custom_test) &&
          alpha_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for alpha is misspecified.",
        glue::glue("'alpha_prior_custom(alpha_prior_custom_test_par)'",
                   "should return density value."),
        glue::glue("Instead, it returned {alpha_prior_custom_test}.")
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(alpha_prior_mean)) {
      RprobitB_stop(
        "Mean vector for conjugate alpha prior is misspecified.",
        "'alpha_prior_mean' should be a numeric vector.",
        glue::glue("Instead, it is of class '{class(alpha_prior_mean)}'.")
      )
    }
    if (length(alpha_prior_mean) != P_f) {
      RprobitB_stop(
        "Mean vector for conjugate alpha prior is misspecified.",
        glue::glue("'alpha_prior_mean' should have length {P_f}."),
        glue::glue("Instead, it has length {length(alpha_prior_mean)}.")
      )
    }
    if (!is_cov_matrix(alpha_prior_Sigma)) {
      RprobitB_stop(
        "Input 'alpha_prior_Sigma' for conjugate alpha prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_cov_matrix()'."
      )
    }
    if (any(dim(alpha_prior_Sigma) != P_f)) {
      RprobitB_stop(
        "Covariance matrix for conjugate alpha prior is misspecified.",
        glue::glue("'alpha_prior_Sigma' should have dimension {P_f}."),
        glue::glue("Instead, it has dimension {dim(alpha_prior_Sigma)[1]}.")
      )
    }
  }
  structure(
    list(
      "conjugate" = conjugate,
      "alpha_prior_mean" = alpha_prior_mean,
      "alpha_prior_Sigma" = alpha_prior_Sigma,
      "alpha_prior_custom" = alpha_prior_custom
    ),
    class = c("RprobitB_prior_alpha", "list")
  )
}

#' @rdname RprobitB_prior_alpha
#' @param x
#' An \code{RprobitB_prior_alpha} object.

is.RprobitB_prior_alpha <- function(x) {
  inherits(x, "RprobitB_prior_alpha")
}

#' @rdname RprobitB_prior_alpha
#' @param ...
#' Not used.
#' @exportS3Method

print.RprobitB_prior_alpha <- function (x, ...) {
  stopifnot(is.RprobitB_prior_alpha(x))
  if (x$conjugate) {
    # TODO use print_matrix
    cat("alpha ~ Normal( mean =", x$alpha_prior_mean, ", Sigma =" ,
        x$alpha_prior_Sigma, ")")
  } else {
    cat("alpha ~", function_body(x$alpha_prior_custom))
  }
}


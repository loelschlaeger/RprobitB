#' Define model prior
#'
#' @description
#' This function defines the prior distributions for a probit model.
#'
#' @inheritParams new_RprobitB_formula
#' @param J
#' An integer, the number of choice alternatives.
#' @param C
#' An integer, the number of latent classes.
#' @inheritDotParams RprobitB_prior_alpha
# #' @inheritDotParams RprobitB_prior_s
# #' @inheritDotParams RprobitB_prior_b
# #' @inheritDotParams RprobitB_prior_Omega
# #' @inheritDotParams RprobitB_prior_Sigma
# #' @inheritDotParams RprobitB_prior_d
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
#' Per default, the following parameters are set for the conjugate priors:
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
#' Per default, conjugate priors are used, but custom priors can be
#' specified as well. To specify a custom prior for parameter \code{<par>},
#' use the input \code{<par>_prior_custom}, which should compute the density
#' of the prior at any point, see \sQuote{Examples}.
#'
#' @inheritSection new_RprobitB_formula Details of model specification
#' @inheritSection new_RprobitB_formula Details of random effects
#'
#' @return
#' An object of class \code{RprobitB_prior}.
#'
#' @examples
#' ### default conjugate prior distributions
#' RprobitB_prior(formula = choice ~ A, J = 2)
#'
#' ### conjugate prior distributions with custom parameters
#' RprobitB_prior(
#'   formula = choice ~ A | B | C, re = "C", J = 3, C = 2,
#'   alpha_prior_mean = numeric(5),
#'   s_prior_concentration = c(1,2)
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
#' @keywords
#' specification
#'
#' @export

RprobitB_prior <- function(formula, re = NULL, J, C = 1, ordered = FALSE, ...) {
  P_f <- P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- P_r(formula = formula, re = re, J = J, ordered = ordered)
  structure(
    list(
      "alpha" = RprobitB_prior_alpha(P_f = P_f, ...)
      #"s" = RprobitB_prior_s(C = C, ...),
      #"b" = RprobitB_prior_b(P_r = P_r, ...),
      #"Omega" = RprobitB_prior_Omega(P_r = P_r, ...),
      #"Sigma" = RprobitB_prior_Sigma(J = J, ordered = ordered, ...),
      #"d" = RprobitB_prior_d(J = J, ordered = ordered, ...)
    ),
    class = c("RprobitB_prior", "list")
  )
}

#' @noRd
#' @exportS3Method
#' @importFrom cli style_underline

print.RprobitB_prior <- function(x, ...) {
  stopifnot(inherits(x, "RprobitB_prior"))
  cat(cli::style_underline("Priors:\n"))
  lapply(x, print)
}

#' Prior for \code{alpha}
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{alpha}.
#'
#' @param P_f
#' An integer, the number of fixed model effects.
#' @param alpha_prior_mean
#' A numeric vector of length \code{P_f}, the mean vector for the conjugate
#' normal prior distribution of \code{alpha}.
#' @param alpha_prior_Sigma
#' A covariance matrix of dimension \code{P_f} x \code{P_f}, the covariance
#' matrix for the conjugate normal prior distribution of \code{alpha}.
#' @param alpha_prior_custom
#' A function, a custom prior density function for \code{alpha}.
#' The details of specifying a custom prior are given under \sQuote{Details}.
#' @param alpha_prior_custom_test_par
#' A numeric vector of length \code{P_f}, a test input for the custom prior
#' density function for \code{alpha}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An object of class \code{RprobitB_prior_alpha}.
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
    alpha_prior_custom = NULL, alpha_prior_custom_test_par = numeric(P_f)
  ) {
  if (P_f == 0) {
    return (NULL)
  } else if (!is.null(alpha_prior_custom)) {
    conjugate <- FALSE
    alpha_prior_mean <- NULL
    alpha_prior_Sigma <- NULL
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
    if (!is_sn(alpha_prior_custom_test)) {
      RprobitB_stop(
        "Custom prior for alpha is misspecified.",
        glue::glue("'alpha_prior_custom(alpha_prior_custom_test_par)' should return density value."),
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
    if (!is_cov(alpha_prior_Sigma) || any(dim(alpha_prior_Sigma) != P_f)) {
      RprobitB_stop(
        "alpha_prior_Sigma has wrong dimension" # TODO
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
#' @param x An object of class \code{RprobitB_prior_alpha}.
#' @param ... Ignored.
#' @exportS3Method

print.RprobitB_prior_alpha <- function (x, ...) {
  if (x$conjugate) {
    cat("alpha ~ Normal( mean =", x$alpha_prior_mean, ", Sigma =" , x$alpha_prior_Sigma, ")")
  } else {
    cat("alpha ~", function_body(x$alpha_prior_custom))
  }
}


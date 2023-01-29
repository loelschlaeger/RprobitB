#' Define probit model prior
#'
#' @description
#' This function constructs an object of class \code{\link{RprobitB_prior}},
#' which contains the prior specification for a probit model.
#'
#' @inheritParams RprobitB_formula
#' @inheritDotParams RprobitB_prior_alpha
#' @inheritDotParams RprobitB_prior_s
#' @inheritDotParams RprobitB_prior_b
#' @inheritDotParams RprobitB_prior_Omega
#' @inheritDotParams RprobitB_prior_Sigma
#' @inheritDotParams RprobitB_prior_Sigma_diff
#' @inheritDotParams RprobitB_prior_d
#'
#' @details
#' # Model priors
#'
#' ## Conjugate priors
#' A priori, the following
#' \href{https://en.wikipedia.org/wiki/Conjugate_prior}{conjugate prior distributions}
#' are assumed for the model parameters:
#' * Only if \code{P_f > 0}:
#'   \code{alpha ~ Normal(mean = alpha_prior_mean, Sigma = alpha_prior_Sigma)}
#' * \code{s ~ Dirichlet(concentration = s_prior_concentration)}
#' * Only if \code{P_r > 0}:
#'   \code{b ~ Normal(mean = b_prior_mean, Sigma = b_prior_Sigma)}
#' * Only if \code{P_r > 0}:
#'   \code{Omega ~ Inverse-Wishart(df = Omega_prior_df, scale = Omega_prior_scale)}
#' * Only in the ordered case (\code{ordered = TRUE}):
#'   \code{Sigma ~ Inverse-Wishart(df = Sigma_prior_df, scale = Sigma_prior_scale)}
#' * Only in the unordered case (\code{ordered = FALSE}):
#'   \code{Sigma_diff ~ Inverse-Wishart(df = Sigma_diff_prior_df, scale = Sigma_diff_prior_scale)}
#' * Only in the ordered case (\code{ordered = TRUE}):
#'   \code{d ~ Normal(mean = d_prior_mean, Sigma = d_prior_Sigma)}
#'
#' ## Default parameters for the conjugate priors
#' By default, the following parameters are set for the conjugate priors:
#' * \code{alpha} (only if \code{P_f > 0})
#'   - \code{alpha_prior_mean = numeric(P_f)}
#'   - \code{alpha_prior_Sigma = 10*diag(P_f)}
#' * \code{s}
#'   - \code{s_prior_concentration = rep(1, C)}
#' * \code{b} (only if \code{P_r > 0})
#'   - \code{b_prior_mean = numeric(P_r)}
#'   - \code{b_prior_Sigma = 10*diag(P_r)}
#' * \code{Omega} (only if \code{P_r > 0})
#'   - \code{Omega_prior_df = P_r + 2}
#'   - \code{Omega_prior_scale = diag(P_r)}
#' * \code{Sigma} (only if \code{ordered = TRUE})
#'   - \code{Sigma_prior_df = 3}
#'   - \code{Sigma_prior_scale = 1}
#' * \code{Sigma_diff} (only if \code{ordered = FALSE})
#'   - \code{Sigma_diff_prior_df = J + 1}
#'   - \code{Sigma_diff_prior_scale = diag(J-1)}
#' * \code{d} (only if \code{ordered = TRUE})
#'   - \code{d_prior_mean = numeric(J - 2)}
#'   - \code{d_prior_Sigma = diag(J - 2)}
#' These parameters can be overwritten by submitting eponymous inputs.
#'
#' ## Custom priors
#' By default, conjugate priors are applied, but custom priors can be
#' specified as well. To specify a custom prior for parameter \code{<par>},
#' use the input \code{<par>_prior_custom}, which should compute the density
#' of the prior (or a value that is proportional), see \sQuote{Examples}.
#' The following density functions (implemented in \{RprobitB\}) can be helpful:
#' * \code{\link{ddirichlet}}
#' * \code{\link{dmvnorm}}
#' * \code{\link{dwishart}}
#'
#' @inheritSection RprobitB_formula Model formula
#' @inheritSection RprobitB_formula Random effects
#'
#' @return
#' An \code{\link{RprobitB_prior}} object.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{alpha}}{The \code{\link{RprobitB_prior_alpha}} object.}
#'   \item{\code{s}}{The \code{\link{RprobitB_prior_s}} object.}
#'   \item{\code{b}}{The \code{\link{RprobitB_prior_b}} object.}
#'   \item{\code{Omega}}{The \code{\link{RprobitB_prior_Omega}} object.}
#'   \item{\code{Sigma}}{The \code{\link{RprobitB_prior_Sigma}} object.}
#'   \item{\code{Sigma_diff}}{The \code{\link{RprobitB_prior_Sigma_diff}} object.}
#'   \item{\code{d}}{The \code{\link{RprobitB_prior_d}} object.}
#' }
#'
#' @examples
#' ### default conjugate prior distributions
#' RprobitB_prior(formula = choice ~ A, J = 2)
#'
#' ### conjugate prior distributions with custom parameters
#' RprobitB_prior(
#'   formula = choice ~ A | B + 0, re = "A", J = 4, C = 2,
#'   alpha_prior_mean = c(-10, 0, 10),
#'   s_prior_concentration = c(1, 2),
#'   b_prior_Sigma = matrix(2)
#' )
#'
#' ### custom prior distributions
#' RprobitB_prior(
#'   formula = choice ~ A | B | C, re = "C", J = 3,
#'   alpha_prior_custom = function(x) {
#'     stats::dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
#'   }
#' )
#'
#' @keywords object
#'
#' @export

RprobitB_prior <- function(formula, re = NULL, J, C = 1, ordered = FALSE, ...) {
  if (missing(formula)) {
    RprobitB_stop(
      "Please specify the input 'formula'.",
      "See the function documentation for details."
    )
  }
  if (missing(J)) {
    RprobitB_stop(
      "Please specify the input 'J'.",
      "It should be the number of alternatives."
    )
  }
  if (!is_positive_integer(C)) {
    RprobitB_stop(
      "Input 'C' is misspecified.",
      "It should be the number (greater or equal 1) of latent classes.",
      "Setting `C = 1` is equivalent to having no latent classes."
    )
  }
  P_f <- compute_P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
  args <- list(...)
  args_alpha <- Filter(
    Negate(is.null),
    args[c("alpha_prior_mean", "alpha_prior_Sigma", "alpha_prior_custom",
           "alpha_prior_custom_test_par")]
  )
  args_s <- Filter(
    Negate(is.null),
    args[c("s_prior_concentration", "s_prior_custom",
           "s_prior_custom_test_par")]
  )
  args_b <- Filter(
    Negate(is.null),
    args[c("b_prior_mean", "b_prior_Sigma", "b_prior_custom",
           "b_prior_custom_test_par")]
  )
  args_Omega <- Filter(
    Negate(is.null),
    args[c("Omega_prior_df", "Omega_prior_scale", "Omega_prior_custom",
           "Omega_prior_custom_test_par")]
  )
  args_Sigma <- Filter(
    Negate(is.null),
    args[c("Sigma_prior_df", "Sigma_prior_scale", "Sigma_prior_custom",
           "Sigma_prior_custom_test_par")]
  )
  args_Sigma_diff <- Filter(
    Negate(is.null),
    args[c("Sigma_diff_prior_df", "Sigma_diff_prior_scale",
           "Sigma_diff_prior_custom", "Sigma_diff_prior_custom_test_par")]
  )
  args_d <- Filter(
    Negate(is.null),
    args[c("d_prior_mean", "d_prior_Sigma", "d_prior_custom",
           "d_prior_custom_test_par")]
  )
  structure(
    list(
      "alpha" = do.call(RprobitB_prior_alpha, c(list(P_f = P_f), args_alpha)),
      "s" = do.call(RprobitB_prior_s, c(list(C = C), args_s)),
      "b" = do.call(RprobitB_prior_b, c(list(P_r = P_r), args_b)),
      "Omega" = do.call(RprobitB_prior_Omega, c(list(P_r = P_r), args_Omega)),
      "Sigma" = do.call(RprobitB_prior_Sigma,
                        c(list(J = J, ordered = ordered), args_Sigma)),
      "Sigma_diff" = do.call(RprobitB_prior_Sigma_diff,
                             c(list(J = J, ordered = ordered), args_Sigma_diff)),
      "d" = do.call(RprobitB_prior_d, c(list(J = J, ordered = ordered), args_d))
    ),
    class = c("RprobitB_prior", "list")
  )
}

#' @rdname RprobitB_prior
#' @param x
#' An \code{\link{RprobitB_prior}} object.

is.RprobitB_prior <- function(x) {
  inherits(x, "RprobitB_prior")
}

#' @rdname RprobitB_prior
#' @exportS3Method
#' @importFrom cli style_underline
#' @param ...
#' Currently not used.

print.RprobitB_prior <- function(x, ...) {
  if (!is.RprobitB_prior(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior`."
    )
  }
  cat(cli::style_underline("Priors:\n"))
  lapply(x, function(y) if(!identical(y, NA)) {print(y); cat("\n")})
  invisible()
}

#' Define \code{alpha} prior
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{alpha}. Only relevant if \code{P_f > 0}.
#'
#' @param P_f
#' An \code{integer}, the number of fixed model effects.
#' Can be computed via the function \code{\link{compute_P_f}}.
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
#' An \code{\link{RprobitB_prior_alpha}} object or \code{NA} if \code{P_f = 0}.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{conjugate}}{Is the prior conjugated?}
#'   \item{\code{alpha_prior_mean}}{The prior mean vector.}
#'   \item{\code{alpha_prior_Sigma}}{The prior covariance matrix.}
#'   \item{\code{alpha_prior_custom}}{The custom prior function.}
#' }
#'
#' @examples
#' \dontrun{
#' ### conjugate prior: alpha ~ Normal(0, 10*I)
#' RprobitB_prior_alpha(P_f = 2)
#'
#' ### custom prior: alpha_1 ~ Normal(0,1), alpha_2 ~ Uniform(0,1)
#' RprobitB_prior_alpha(
#'   P_f = 2,
#'   alpha_prior_custom = function(x) {
#'     stats::dnorm(x[1]) * stats::dunif(x[2])
#'   }
#' )
#' }
#'
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal object

RprobitB_prior_alpha <- function(
    P_f, alpha_prior_mean = numeric(P_f), alpha_prior_Sigma = 10 * diag(P_f),
    alpha_prior_custom = NA, alpha_prior_custom_test_par = numeric(P_f)
  ) {
  if (P_f == 0) {
    return (NA)
  } else if (!identical(alpha_prior_custom, NA)) {
    conjugate <- FALSE
    alpha_prior_mean <- NA
    alpha_prior_Sigma <- NA
    if (!is.function(alpha_prior_custom)) {
      RprobitB_stop(
        "Custom prior for 'alpha' is misspecified.",
        "'alpha_prior_custom' should be a `function`."
      )
    }
    if (!(is.numeric(alpha_prior_custom_test_par) &&
          length(alpha_prior_custom_test_par) == P_f)) {
      RprobitB_stop(
        glue::glue(
          "'alpha_prior_custom_test_par' should be a `numeric` vector ",
          "of length {P_f}."
        )
      )
    }
    alpha_prior_custom_test <- try(
      alpha_prior_custom(alpha_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(alpha_prior_custom_test) &&
          alpha_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for 'alpha' is misspecified.",
        glue::glue(
          "The call 'alpha_prior_custom(c({paste(alpha_prior_custom_test_par, collapse = ', ')}))' ",
          "should return a single density value."
        ),
        "Instead, it returned (collapsed):",
        glue::glue_collapse(
          glue::glue("{alpha_prior_custom_test}"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(alpha_prior_mean)) {
      RprobitB_stop(
        "Mean vector for conjugate alpha prior is misspecified.",
        "'alpha_prior_mean' should be a `numeric` vector."
      )
    }
    if (length(alpha_prior_mean) != P_f) {
      RprobitB_stop(
        "Mean vector for conjugate alpha prior is misspecified.",
        glue::glue("'alpha_prior_mean' should have length {P_f}."),
        glue::glue("Instead, it has length {length(alpha_prior_mean)}.")
      )
    }
    if (!is_covariance_matrix(alpha_prior_Sigma)) {
      RprobitB_stop(
        "Input 'alpha_prior_Sigma' for conjugate alpha prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_covariance_matrix(alpha_prior_Sigma)'."
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
#' An \code{\link{RprobitB_prior_alpha}} object.

is.RprobitB_prior_alpha <- function(x) {
  inherits(x, "RprobitB_prior_alpha")
}

#' @rdname RprobitB_prior_alpha
#' @inheritParams print_matrix
#' @param ...
#' Currently not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_alpha <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
  ) {
  if (!is.RprobitB_prior_alpha(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior_alpha`."
    )
  }
  cat(crayon::underline("alpha prior"))
  cat(" : ")
  if (x$conjugate) {
    cat("Normal (conjugate)\n")
    print_matrix(
      x$alpha_prior_mean, rowdots = rowdots, coldots = coldots, digits = digits,
      label = "mean", simplify = simplify, details = details
    )
    cat("\n")
    print_matrix(
      x$alpha_prior_Sigma, rowdots = rowdots, coldots = coldots,
      digits = digits, label = "Sigma", simplify = simplify, details = details
    )
  } else {
    cat("custom\n")
    cat("alpha ~", function_body(x$alpha_prior_custom))
  }
  invisible()
}

#' Define \code{s} prior
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{s}. Only relevant if \code{C > 1}.
#'
#' @inheritParams RprobitB_parameter
#' @param s_prior_concentration
#' A \code{numeric} of length \code{C}, the concentration vector for the
#' conjugate Dirichlet prior distribution of \code{s}.
#' By default, \code{s_prior_concentration = rep(1, C)}.
#' @param s_prior_custom
#' A \code{function}, a custom prior density function for \code{s},
#' see details.
#' By default, \code{s_prior_custom = NULL}, i.e., no custom prior.
#' @param s_prior_custom_test_par
#' A \code{numeric} of length \code{C}, a test input for the custom prior
#' density function \code{s_prior_custom}.
#' By default, \code{s_prior_custom_test_par = numeric(C)}.
#' Ignored if \code{s_prior_custom = NULL}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An \code{\link{RprobitB_prior_s}} object or \code{NA} if \code{C = 1}.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{conjugate}}{Is the prior conjugated?}
#'   \item{\code{s_prior_concentration}}{The prior concentration vector.}
#'   \item{\code{c_prior_custom}}{The custom prior function.}
#' }
#'
#' @examples
#' \dontrun{
#' ### conjugate prior: s ~ Dirichlet(rep(1, C))
#' RprobitB_prior_s(C = 2)
#'
#' ### custom prior: s_1 ~ Uniform(0.5,1), s_2 := 1 - s_1
#' RprobitB_prior_s(
#'   C = 2,
#'   s_prior_custom = function(x) {
#'     stats::dunif(x[1], min = 0.5, max = 1) * (x[2] == 1 - x[1])
#'   },
#'   s_prior_custom_test_par = c(0.6, 0.4)
#' )
#' }
#'
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal object

RprobitB_prior_s <- function(
    C = 1, s_prior_concentration = rep(1, C),
    s_prior_custom = NA, s_prior_custom_test_par = numeric(C)
) {
  if (C == 1) {
    return (NA)
  } else if (!identical(s_prior_custom, NA)) {
    conjugate <- FALSE
    s_prior_concentration <- NA
    if (!is.function(s_prior_custom)) {
      RprobitB_stop(
        "Custom prior for 's' is misspecified.",
        "'s_prior_custom' should be a `function`."
      )
    }
    if (!(is.numeric(s_prior_custom_test_par) &&
          length(s_prior_custom_test_par) == C)) {
      RprobitB_stop(
        glue::glue(
          "'s_prior_custom_test_par' should be a `numeric` vector ",
          "of length {C}."
        )
      )
    }
    s_prior_custom_test <- try(
      s_prior_custom(s_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(s_prior_custom_test) && s_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for 's' is misspecified.",
        glue::glue(
          "The call 's_prior_custom(c({paste(s_prior_custom_test_par, collapse = ', ')}))' ",
          "should return a single density value."
        ),
        "Instead, it returned (collapsed):",
        glue::glue_collapse(
          glue::glue("{s_prior_custom_test}"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(s_prior_concentration)) {
      RprobitB_stop(
        "Concentration vector for conjugate s prior is misspecified.",
        "'s_prior_concentration' should be a `numeric` vector."
      )
    }
    if (length(s_prior_concentration) != C) {
      RprobitB_stop(
        "Concentration vector for conjugate s prior is misspecified.",
        glue::glue("'s_prior_concentration' should have length {C}."),
        glue::glue("Instead, it has length {length(s_prior_concentration)}.")
      )
    }
  }
  structure(
    list(
      "conjugate" = conjugate,
      "s_prior_concentration" = s_prior_concentration,
      "s_prior_custom" = s_prior_custom
    ),
    class = c("RprobitB_prior_s", "list")
  )
}

#' @rdname RprobitB_prior_s
#' @param x
#' An \code{\link{RprobitB_prior_s}} object.

is.RprobitB_prior_s <- function(x) {
  inherits(x, "RprobitB_prior_s")
}

#' @rdname RprobitB_prior_s
#' @inheritParams print_matrix
#' @param ...
#' Currently not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_s <- function (
    x, coldots = 4, digits = 2, simplify = TRUE, details = FALSE, ...
) {
  if (!is.RprobitB_prior_s(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior_s`."
    )
  }
  cat(crayon::underline("s prior"))
  cat(" : ")
  if (x$conjugate) {
    cat("Dirichlet (conjugate)\n")
    print_matrix(
      x$s_prior_concentration, coldots = coldots, digits = digits,
      label = "concentration", simplify = simplify, details = details
    )
  } else {
    cat("custom\n")
    cat("s ~", function_body(x$s_prior_custom))
  }
  invisible()
}

#' Define \code{b} prior
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{b}. Only relevant if \code{P_r > 0}.
#'
#' @param P_r
#' An \code{integer}, the number of random model effects.
#' Can be computed via the function \code{\link{compute_P_r}}.
#' @param b_prior_mean
#' A \code{numeric} of length \code{P_r}, the mean vector for the conjugate
#' normal prior distribution of \code{b}.
#' By default, \code{b_prior_mean = numeric(P_r)}.
#' @param b_prior_Sigma
#' A \code{matrix} of dimension \code{P_r} x \code{P_r}, the covariance
#' matrix for the conjugate normal prior distribution of \code{b}.
#' By default, \code{b_prior_Sigma = 10 * diag(P_r)}.
#' @param b_prior_custom
#' A \code{function}, a custom prior density function for \code{b},
#' see details.
#' By default, \code{b_prior_custom = NULL}, i.e., no custom prior.
#' @param b_prior_custom_test_par
#' A \code{numeric} of length \code{P_r}, a test input for the custom prior
#' density function \code{b_prior_custom}.
#' By default, \code{b_prior_custom_test_par = numeric(P_r)}.
#' Ignored if \code{b_prior_custom = NULL}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An \code{\link{RprobitB_prior_b}} object or \code{NA} if \code{P_r = 0}.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{conjugate}}{Is the prior conjugated?}
#'   \item{\code{b_prior_mean}}{The prior mean vector.}
#'   \item{\code{b_prior_Sigma}}{The prior covariance matrix.}
#'   \item{\code{b_prior_custom}}{The custom prior function.}
#' }
#'
#' @examples
#' \dontrun{
#' ### conjugate prior: b ~ Normal(0, 10*I)
#' RprobitB_prior_b(P_r = 2)
#'
#' ### custom prior: b_1 ~ Normal(0,1), b_2 ~ Uniform(0,1)
#' RprobitB_prior_b(
#'   P_r = 2,
#'   b_prior_custom = function(x) {
#'     stats::dnorm(x[1]) * stats::dunif(x[2])
#'   }
#' )
#' }
#'
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal object

RprobitB_prior_b <- function(
    P_r, b_prior_mean = numeric(P_r), b_prior_Sigma = 10 * diag(P_r),
    b_prior_custom = NA, b_prior_custom_test_par = numeric(P_r)
) {
  if (P_r == 0) {
    return (NA)
  } else if (!identical(b_prior_custom, NA)) {
    conjugate <- FALSE
    b_prior_mean <- NA
    b_prior_Sigma <- NA
    if (!is.function(b_prior_custom)) {
      RprobitB_stop(
        "Custom prior for 'b' is misspecified.",
        "'b_prior_custom' should be a `function`."
      )
    }
    if (!(is.numeric(b_prior_custom_test_par) &&
          length(b_prior_custom_test_par) == P_r)) {
      RprobitB_stop(
        glue::glue(
          "'b_prior_custom_test_par' should be a `numeric` vector ",
          "of length {P_r}."
        )
      )
    }
    b_prior_custom_test <- try(
      b_prior_custom(b_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(b_prior_custom_test) &&
          b_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for 'b' is misspecified.",
        glue::glue(
          "The call 'b_prior_custom(c({paste(b_prior_custom_test_par, collapse = ', ')}))' ",
          "should return a single density value."
        ),
        "Instead, it returned (collapsed):",
        glue::glue_collapse(
          glue::glue("{b_prior_custom_test}"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(b_prior_mean)) {
      RprobitB_stop(
        "Mean vector for conjugate b prior is misspecified.",
        "'b_prior_mean' should be a `numeric` vector."
      )
    }
    if (length(b_prior_mean) != P_r) {
      RprobitB_stop(
        "Mean vector for conjugate b prior is misspecified.",
        glue::glue("'b_prior_mean' should have length {P_r}."),
        glue::glue("Instead, it has length {length(b_prior_mean)}.")
      )
    }
    if (!is_covariance_matrix(b_prior_Sigma)) {
      RprobitB_stop(
        "Input 'b_prior_Sigma' for conjugate b prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_covariance_matrix()'."
      )
    }
    if (any(dim(b_prior_Sigma) != P_r)) {
      RprobitB_stop(
        "Covariance matrix for conjugate b prior is misspecified.",
        glue::glue("'b_prior_Sigma' should have dimension {P_r}."),
        glue::glue("Instead, it has dimension {dim(b_prior_Sigma)[1]}.")
      )
    }
  }
  structure(
    list(
      "conjugate" = conjugate,
      "b_prior_mean" = b_prior_mean,
      "b_prior_Sigma" = b_prior_Sigma,
      "b_prior_custom" = b_prior_custom
    ),
    class = c("RprobitB_prior_b", "list")
  )
}

#' @rdname RprobitB_prior_b
#' @param x
#' An \code{\link{RprobitB_prior_b}} object.

is.RprobitB_prior_b <- function(x) {
  inherits(x, "RprobitB_prior_b")
}

#' @rdname RprobitB_prior_b
#' @inheritParams print_matrix
#' @param ...
#' Currently not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_b <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
) {
  if (!is.RprobitB_prior_b(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior_b`."
    )
  }
  cat(crayon::underline("b prior"))
  cat(" : ")
  if (x$conjugate) {
    cat("Normal (conjugate)\n")
    print_matrix(
      x$b_prior_mean, rowdots = rowdots, coldots = coldots, digits = digits,
      label = "mean", simplify = simplify, details = details
    )
    cat("\n")
    print_matrix(
      x$b_prior_Sigma, rowdots = rowdots, coldots = coldots,
      digits = digits, label = "Sigma", simplify = simplify, details = details
    )
  } else {
    cat("custom\n")
    cat("b ~", function_body(x$b_prior_custom))
  }
  invisible()
}

#' Define \code{Omega} prior
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{Omega}. Only relevant if \code{P_r > 0}.
#'
#' @inheritParams RprobitB_prior_b
#' @param Omega_prior_df
#' An \code{integer}, the degrees of freedom for the conjugate
#' Inverse-Wishart prior distribution of \code{Omega}.
#' It must be greater or equal than \code{P_r + 2} (to ensure a proper prior).
#' By default, \code{Omega_prior_df = P_r + 2}.
#' @param Omega_prior_scale
#' A \code{matrix} of dimension \code{P_r} x \code{P_r}, the scale
#' matrix for the conjugate Inverse-Wishart prior distribution of \code{Omega}.
#' By default, \code{Omega_prior_scale = diag(P_r)}.
#' @param Omega_prior_custom
#' A \code{function}, a custom prior density function for \code{Omega},
#' see details.
#' By default, \code{Omega_prior_custom = NULL}, i.e., no custom prior.
#' @param Omega_prior_custom_test_par
#' A \code{matrix} of dimension \code{P_r} x \code{P_r}, a test input for the
#' custom prior density function \code{Omega_prior_custom}.
#' By default, \code{Omega_prior_custom_test_par = diag(P_r)}.
#' Ignored if \code{Omega_prior_custom = NULL}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An \code{\link{RprobitB_prior_Omega}} object or \code{NA} if \code{P_r = 0}.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{conjugate}}{Is the prior conjugated?}
#'   \item{\code{Omega_prior_df}}{The prior degrees of freedom.}
#'   \item{\code{Omega_prior_scale}}{The prior scale matrix.}
#'   \item{\code{Omega_prior_custom}}{The custom prior function.}
#' }
#'
#' @examples
#' \dontrun{
#' ### conjugate prior: Omega ~ Inverse-Wishart(P_r + 2, I)
#' RprobitB_prior_Omega(P_r = 2)
#'
#' ### custom prior: Omega ~ Inverse-Wishart(P_r + 2, I), Omega_11 = Omega_22
#' RprobitB_prior_Omega(
#'   P_r = 2,
#'   Omega_prior_custom = function(x) {
#'     dwishart(x, df = 4, scale = diag(2), inv = TRUE) * (x[1,1] == x[2,2])
#'   }
#' )
#' }
#'
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal object

RprobitB_prior_Omega <- function(
    P_r, Omega_prior_df = P_r + 2, Omega_prior_scale = diag(P_r),
    Omega_prior_custom = NA, Omega_prior_custom_test_par = diag(P_r)
) {
  if (P_r == 0) {
    return (NA)
  } else if (!identical(Omega_prior_custom, NA)) {
    conjugate <- FALSE
    Omega_prior_df <- NA
    Omega_prior_scale <- NA
    if (!(is.numeric(Omega_prior_custom_test_par) &&
          is.matrix(Omega_prior_custom_test_par) &&
          all(dim(Omega_prior_custom_test_par) == P_r))) {
      RprobitB_stop(
        glue::glue(
          "'Omega_prior_custom_test_par' should be a `matrix` ",
          "of dimension {P_r} x {P_r}."
        )
      )
    }
    if (!is.function(Omega_prior_custom)) {
      RprobitB_stop(
        "Custom prior for 'Omega' is misspecified.",
        "'Omega_prior_custom' should be a `function`."
      )
    }
    Omega_prior_custom_test <- try(
      Omega_prior_custom(Omega_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(Omega_prior_custom_test) &&
          Omega_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for 'Omega' is misspecified.",
        glue::glue(
          "The call 'Omega_prior_custom(<Omega_prior_custom_test_par>)' ",
          "should return a single density value."
        ),
        "Instead, it returned (collapsed):",
        glue::glue_collapse(
          glue::glue("{Omega_prior_custom_test}S"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is_positive_integer(Omega_prior_df)) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Omega prior is misspecified.",
        "'Omega_prior_df' should be a positive `integer`."
      )
    }
    if (Omega_prior_df < P_r + 2) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Omega prior is misspecified.",
        glue::glue("'Omega_prior_df' should be greater or equal {P_r+2}."),
        glue::glue("Instead, it is {Omega_prior_df}.")
      )
    }
    if (!is_covariance_matrix(Omega_prior_scale)) {
      RprobitB_stop(
        "Input 'Omega_prior_scale' for conjugate Omega prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_covariance_matrix()'."
      )
    }
    if (any(dim(Omega_prior_scale) != P_r)) {
      RprobitB_stop(
        "Covariance matrix for conjugate Omega prior is misspecified.",
        glue::glue("'Omega_prior_scale' should have dimension {P_r}."),
        glue::glue("Instead, it has dimension {dim(Omega_prior_scale)[1]}.")
      )
    }
  }
  structure(
    list(
      "conjugate" = conjugate,
      "Omega_prior_df" = Omega_prior_df,
      "Omega_prior_scale" = Omega_prior_scale,
      "Omega_prior_custom" = Omega_prior_custom
    ),
    class = c("RprobitB_prior_Omega", "list")
  )
}

#' @rdname RprobitB_prior_Omega
#' @param x
#' An \code{\link{RprobitB_prior_Omega}} object.

is.RprobitB_prior_Omega <- function(x) {
  inherits(x, "RprobitB_prior_Omega")
}

#' @rdname RprobitB_prior_Omega
#' @inheritParams print_matrix
#' @param ...
#' Currently not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_Omega <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
) {
  if (!is.RprobitB_prior_Omega(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior_Omega`."
    )
  }
  cat(crayon::underline("Omega prior"))
  cat(" : ")
  if (x$conjugate) {
    cat("Inverse-Wishart (conjugate)\n")
    print_matrix(
      x$Omega_prior_df, rowdots = rowdots, coldots = coldots, digits = digits,
      label = "degrees of freedom", simplify = simplify, details = details
    )
    cat("\n")
    print_matrix(
      x$Omega_prior_scale, rowdots = rowdots, coldots = coldots,
      digits = digits, label = "scale", simplify = simplify, details = details
    )
  } else {
    cat("custom\n")
    cat("Omega ~", function_body(x$Omega_prior_custom))
  }
  invisible()
}

#' Define \code{Sigma} prior
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{Sigma}. Only relevant if \code{ordered = TRUE}.
#'
#' @inheritParams RprobitB_alternatives
#' @inheritParams RprobitB_parameter
#' @param Sigma_prior_df
#' An \code{integer}, the degrees of freedom for the conjugate
#' Inverse-Wishart prior distribution of \code{Sigma}.
#' It must be greater or equal than \code{3} (to ensure a proper prior).
#' By default, \code{Sigma_prior_df = 3}.
#' @param Sigma_prior_scale
#' A \code{matrix} of dimension \code{1} x \code{1}, the scale
#' matrix for the conjugate Inverse-Wishart prior distribution of
#' \code{Sigma}.
#' Can also be a single \code{numeric}.
#' By default, \code{Sigma_prior_scale = 1}.
#' @param Sigma_prior_custom
#' A \code{function}, a custom prior density function for \code{Sigma},
#' see details.
#' By default, \code{Sigma_prior_custom = NULL}, i.e., no custom prior.
#' @param Sigma_prior_custom_test_par
#' A \code{matrix} of dimension \code{1} x \code{1}, a test input for
#' the custom prior density function \code{Sigma_prior_custom}.
#' Can also be a single \code{numeric}.
#' By default, \code{Sigma_prior_custom_test_par = matrix(1)}.
#' Ignored if \code{Sigma_prior_custom = NULL}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An \code{\link{RprobitB_prior_Sigma}} object or \code{NA} if
#' \code{ordered = FALSE}.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{conjugate}}{Is the prior conjugated?}
#'   \item{\code{Sigma_prior_df}}{The prior degrees of freedom.}
#'   \item{\code{Sigma_prior_scale}}{The prior scale matrix.}
#'   \item{\code{Sigma_prior_custom}}{The custom prior function.}
#' }
#'
#' @examples
#' \dontrun{
#' ### conjugate prior: Sigma ~ Inverse-Wishart(3, 1)
#' RprobitB_prior_Sigma(ordered = TRUE, J = 4)
#'
#' ### custom prior: Sigma ~ Uniform(0,1)
#' RprobitB_prior_Sigma(
#'   ordered = TRUE,
#'   J = 4,
#'   Sigma_prior_custom = function(x) dunif(x)
#' )
#' }
#'
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal object

RprobitB_prior_Sigma <- function(
    ordered = FALSE, J, Sigma_prior_df = 3, Sigma_prior_scale = 1,
    Sigma_prior_custom = NA, Sigma_prior_custom_test_par = matrix(1)
) {
  if (!ordered) {
    return(NA)
  } else if (!identical(Sigma_prior_custom, NA)) {
    conjugate <- FALSE
    Sigma_prior_df <- NA
    Sigma_prior_scale <- NA
    Sigma_prior_custom_test_par <- as.matrix(Sigma_prior_custom_test_par)
    if (!(is.numeric(Sigma_prior_custom_test_par) &&
          is.matrix(Sigma_prior_custom_test_par) &&
          all(dim(Sigma_prior_custom_test_par) == 1))) {
      RprobitB_stop(
        glue::glue(
          "'Sigma_prior_custom_test_par' should be a `matrix` ",
          "of dimension 1 x 1."
        )
      )
    }
    if (!is.function(Sigma_prior_custom)) {
      RprobitB_stop(
        "Custom prior for 'Sigma' is misspecified.",
        "'Sigma_prior_custom' should be a `function`."
      )
    }
    Sigma_prior_custom_test <- try(
      Sigma_prior_custom(Sigma_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(Sigma_prior_custom_test) &&
          Sigma_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for 'Sigma' is misspecified.",
        glue::glue(
          "The call 'Sigma_prior_custom(<Sigma_prior_custom_test_par>)' ",
          "should return a single density value."
        ),
        "Instead, it returned (collapsed):",
        glue::glue_collapse(
          glue::glue("{Sigma_prior_custom_test}"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    conjugate <- TRUE
    Sigma_prior_scale <- as.matrix(Sigma_prior_scale)
    if (!is_positive_integer(Sigma_prior_df)) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Sigma prior is misspecified.",
        "'Sigma_prior_df' should be a positive `integer`."
      )
    }
    if (Sigma_prior_df < 3) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Sigma prior is misspecified.",
        glue::glue("'Sigma_prior_df' should be greater or equal 3."),
        glue::glue("Instead, it is {Sigma_prior_df}.")
      )
    }
    if (!is_covariance_matrix(Sigma_prior_scale)) {
      RprobitB_stop(
        "Input 'Sigma_prior_scale' for conjugate Sigma prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_covariance_matrix()'."
      )
    }
    if (any(dim(Sigma_prior_scale) != 1)) {
      RprobitB_stop(
        "Covariance matrix for conjugate Sigma prior is misspecified.",
        glue::glue("'Sigma_prior_scale' should have dimension 1."),
        glue::glue("Instead, it has dimension {dim(Sigma_prior_scale)[1]}.")
      )
    }
  }
  structure(
    list(
      "conjugate" = conjugate,
      "Sigma_prior_df" = Sigma_prior_df,
      "Sigma_prior_scale" = Sigma_prior_scale,
      "Sigma_prior_custom" = Sigma_prior_custom
    ),
    class = c("RprobitB_prior_Sigma", "list")
  )
}

#' @rdname RprobitB_prior_Sigma
#' @param x
#' An \code{\link{RprobitB_prior_Sigma}} object.

is.RprobitB_prior_Sigma <- function(x) {
  inherits(x, "RprobitB_prior_Sigma")
}

#' @rdname RprobitB_prior_Sigma
#' @inheritParams print_matrix
#' @param ...
#' Currently not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_Sigma <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
) {
  if (!is.RprobitB_prior_Sigma(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior_Sigma`."
    )
  }
  cat(crayon::underline("Sigma prior"))
  cat(" : ")
  if (x$conjugate) {
    cat("Inverse-Wishart (conjugate)\n")
    print_matrix(
      x$Sigma_prior_df, rowdots = rowdots, coldots = coldots, digits = digits,
      label = "degrees of freedom", simplify = simplify, details = details
    )
    cat("\n")
    print_matrix(
      x$Sigma_prior_scale, rowdots = rowdots, coldots = coldots,
      digits = digits, label = "scale", simplify = simplify, details = details
    )
  } else {
    cat("custom\n")
    cat("Sigma ~", function_body(x$Sigma_prior_custom))
  }
  invisible()
}

#' Define \code{Sigma_diff} prior
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{Sigma_diff}. Only relevant if \code{ordered = FALSE}.
#'
#' @inheritParams RprobitB_alternatives
#' @inheritParams RprobitB_parameter
#' @param Sigma_diff_prior_df
#' An \code{integer}, the degrees of freedom for the conjugate
#' Inverse-Wishart prior distribution of \code{Sigma_diff}.
#' It must be greater or equal than \code{J + 1} (to ensure a proper prior).
#' By default, \code{Sigma_diff_prior_df = J + 1}.
#' @param Sigma_diff_prior_scale
#' A \code{matrix} of dimension \code{(J-1)} x \code{(J-1)}, the scale
#' matrix for the conjugate Inverse-Wishart prior distribution of
#' \code{Sigma_diff}.
#' By default, \code{Sigma_diff_prior_scale = diag(J-1)}.
#' @param Sigma_diff_prior_custom
#' A \code{function}, a custom prior density function for \code{Sigma_diff},
#' see details.
#' By default, \code{Sigma_diff_prior_custom = NULL}, i.e., no custom prior.
#' @param Sigma_diff_prior_custom_test_par
#' A \code{matrix} of dimension \code{(J-1)} x \code{(J-1)}, a test input for
#' the custom prior density function \code{Sigma_diff_prior_custom}.
#' By default, \code{Sigma_diff_prior_custom_test_par = diag(J-1)}.
#' Ignored if \code{Sigma_diff_prior_custom = NULL}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An \code{\link{RprobitB_prior_Sigma_diff}} object or \code{NA} if
#' \code{ordered = TRUE}.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{conjugate}}{Is the prior conjugated?}
#'   \item{\code{Sigma_diff_prior_df}}{The prior degrees of freedom.}
#'   \item{\code{Sigma_diff_prior_scale}}{The prior scale matrix.}
#'   \item{\code{Sigma_diff_prior_custom}}{The custom prior function.}
#' }
#'
#' @examples
#' \dontrun{
#' ### conjugate prior: Sigma_diff ~ Inverse-Wishart(J + 1, I)
#' RprobitB_prior_Sigma_diff(J = 3)
#'
#' ### custom prior: Sigma_diff ~ Inverse-Wishart(J + 1, I), Sigma_diff diagonal
#' RprobitB_prior_Sigma_diff(
#'   J = 3,
#'   Sigma_diff_prior_custom = function(x) {
#'     dwishart(x, df = 4, scale = diag(2), inv = TRUE) *
#'     all(x[row(x) != col(x)] == 0)
#'   }
#' )
#' }
#'
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal object

RprobitB_prior_Sigma_diff <- function(
    ordered = FALSE, J, Sigma_diff_prior_df = J + 1, Sigma_diff_prior_scale = diag(J-1),
    Sigma_diff_prior_custom = NA, Sigma_diff_prior_custom_test_par = diag(J-1)
) {
  if (ordered) {
    return(NA)
  } else if (!identical(Sigma_diff_prior_custom, NA)) {
    conjugate <- FALSE
    Sigma_diff_prior_df <- NA
    Sigma_diff_prior_scale <- NA
    if (!(is.numeric(Sigma_diff_prior_custom_test_par) &&
          is.matrix(Sigma_diff_prior_custom_test_par) &&
          all(dim(Sigma_diff_prior_custom_test_par) == J - 1))) {
      RprobitB_stop(
        glue::glue(
          "'Sigma_diff_prior_custom_test_par' should be a `matrix` ",
          "of dimension {J-1} x {J-1}."
        )
      )
    }
    if (!is.function(Sigma_diff_prior_custom)) {
      RprobitB_stop(
        "Custom prior for 'Sigma_diff' is misspecified.",
        "'Sigma_diff_prior_custom' should be a `function`."
      )
    }
    Sigma_diff_prior_custom_test <- try(
      Sigma_diff_prior_custom(Sigma_diff_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(Sigma_diff_prior_custom_test) &&
          Sigma_diff_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for 'Sigma_diff' is misspecified.",
        glue::glue(
          "The call 'Sigma_diff_prior_custom(<Sigma_diff_prior_custom_test_par>)' ",
          "should return a single density value."
        ),
        "Instead, it returned (collapsed):",
        glue::glue_collapse(
          glue::glue("{Sigma_diff_prior_custom_test}"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is_positive_integer(Sigma_diff_prior_df)) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Sigma_diff prior is misspecified.",
        "'Sigma_diff_prior_df' should be a positive `integer`."
      )
    }
    if (Sigma_diff_prior_df < J + 1) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Sigma_diff prior is misspecified.",
        glue::glue("'Sigma_diff_prior_df' should be greater or equal {J+1}."),
        glue::glue("Instead, it is {Sigma_diff_prior_df}.")
      )
    }
    if (!is_covariance_matrix(Sigma_diff_prior_scale)) {
      RprobitB_stop(
        "Input 'Sigma_diff_prior_scale' for conjugate Sigma_diff prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_covariance_matrix()'."
      )
    }
    if (any(dim(Sigma_diff_prior_scale) != (J-1))) {
      RprobitB_stop(
        "Covariance matrix for conjugate Sigma_diff prior is misspecified.",
        glue::glue("'Sigma_diff_prior_scale' should have dimension {J-1}."),
        glue::glue("Instead, it has dimension {dim(Sigma_diff_prior_scale)[1]}.")
      )
    }
  }
  structure(
    list(
      "conjugate" = conjugate,
      "Sigma_diff_prior_df" = Sigma_diff_prior_df,
      "Sigma_diff_prior_scale" = Sigma_diff_prior_scale,
      "Sigma_diff_prior_custom" = Sigma_diff_prior_custom
    ),
    class = c("RprobitB_prior_Sigma_diff", "list")
  )
}

#' @rdname RprobitB_prior_Sigma_diff
#' @param x
#' An \code{\link{RprobitB_prior_Sigma_diff}} object.

is.RprobitB_prior_Sigma_diff <- function(x) {
  inherits(x, "RprobitB_prior_Sigma_diff")
}

#' @rdname RprobitB_prior_Sigma_diff
#' @inheritParams print_matrix
#' @param ...
#' Currently not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_Sigma_diff <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
) {
  if (!is.RprobitB_prior_Sigma_diff(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior_Sigma_diff`."
    )
  }
  cat(crayon::underline("Sigma_diff prior"))
  cat(" : ")
  if (x$conjugate) {
    cat("Inverse-Wishart (conjugate)\n")
    print_matrix(
      x$Sigma_diff_prior_df, rowdots = rowdots, coldots = coldots, digits = digits,
      label = "degrees of freedom", simplify = simplify, details = details
    )
    cat("\n")
    print_matrix(
      x$Sigma_diff_prior_scale, rowdots = rowdots, coldots = coldots,
      digits = digits, label = "scale", simplify = simplify, details = details
    )
  } else {
    cat("custom\n")
    cat("Sigma_diff ~", function_body(x$Sigma_diff_prior_custom))
  }
  invisible()
}

#' Define \code{d} prior
#'
#' @description
#' This function defines the prior distributions for the probit model parameter
#' \code{d}. Only relevant if \code{ordered = TRUE}.
#'
#' @inheritParams RprobitB_alternatives
#' @inheritParams RprobitB_parameter
#' @param d_prior_mean
#' A \code{numeric} of length \code{J-2}, the mean vector for the conjugate
#' normal prior distribution of \code{d}.
#' By default, \code{d_prior_mean = numeric(J-2)}.
#' @param d_prior_Sigma
#' A \code{matrix} of dimension \code{(J-2)} x \code{(J-2)}, the covariance
#' matrix for the conjugate normal prior distribution of \code{d}.
#' By default, \code{d_prior_Sigma = 10 * diag(J-2)}.
#' @param d_prior_custom
#' A \code{function}, a custom prior density function for \code{d},
#' see details.
#' By default, \code{d_prior_custom = NULL}, i.e., no custom prior.
#' @param d_prior_custom_test_par
#' A \code{numeric} of length \code{J-2}, a test input for the custom prior
#' density function \code{d_prior_custom}.
#' By default, \code{d_prior_custom_test_par = numeric(J-2)}.
#' Ignored if \code{d_prior_custom = NULL}.
#'
#' @inheritSection RprobitB_prior Model priors
#'
#' @return
#' An \code{\link{RprobitB_prior_d}} object or \code{NA} if
#' \code{ordered = FALSE}.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{conjugate}}{Is the prior conjugated?}
#'   \item{\code{d_prior_mean}}{The prior mean vector.}
#'   \item{\code{d_prior_Sigma}}{The prior covariance matrix.}
#'   \item{\code{d_prior_custom}}{The custom prior function.}
#' }
#'
#' @examples
#' \dontrun{
#' ### conjugate prior: d ~ Normal(0, 10*I)
#' RprobitB_prior_d(ordered = TRUE, J = 4)
#'
#' ### custom prior: d_1 ~ Normal(0,1), d_2 ~ Uniform(0,1)
#' RprobitB_prior_d(
#'   ordered = TRUE,
#'   J = 4,
#'   d_prior_custom = function(x) {
#'     stats::dnorm(x[1]) * stats::dunif(x[2])
#'   }
#' )
#' }
#'
#' @importFrom glue glue glue_collapse
#'
#' @keywords internal object

RprobitB_prior_d <- function(
    ordered = FALSE, J, d_prior_mean = numeric(J-2), d_prior_Sigma = 10 * diag(J-2),
    d_prior_custom = NA, d_prior_custom_test_par = numeric(J-2)
) {
  if (!ordered) {
    return (NA)
  } else if (!identical(d_prior_custom, NA)) {
    conjugate <- FALSE
    d_prior_mean <- NA
    d_prior_Sigma <- NA
    if (!is.function(d_prior_custom)) {
      RprobitB_stop(
        "Custom prior for 'd' is misspecified.",
        "'d_prior_custom' should be a `function`."
      )
    }
    if (!(is.numeric(d_prior_custom_test_par) &&
          length(d_prior_custom_test_par) == J-2)) {
      RprobitB_stop(
        glue::glue(
          "'d_prior_custom_test_par' should be a `numeric` vector ",
          "of length {J-2}."
        )
      )
    }
    d_prior_custom_test <- try(
      d_prior_custom(d_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(d_prior_custom_test) &&
          d_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for 'd' is misspecified.",
        glue::glue(
          "The call 'd_prior_custom(c({paste(d_prior_custom_test_par, collapse = ', ')}))' ",
          "should return a single density value."
        ),
        "Instead, it returned (collapsed):",
        glue::glue_collapse(
          glue::glue("{d_prior_custom_test}"),
          sep = " ",
          width = getOption("width") - 3
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(d_prior_mean)) {
      RprobitB_stop(
        "Mean vector for conjugate d prior is misspecified.",
        "'d_prior_mean' should be a `numeric` vector."
      )
    }
    if (length(d_prior_mean) != (J-2)) {
      RprobitB_stop(
        "Mean vector for conjugate d prior is misspecified.",
        glue::glue("'d_prior_mean' should have length {J-2}."),
        glue::glue("Instead, it has length {length(d_prior_mean)}.")
      )
    }
    if (!is_covariance_matrix(d_prior_Sigma)) {
      RprobitB_stop(
        "Input 'd_prior_Sigma' for conjugate d prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_covariance_matrix()'."
      )
    }
    if (any(dim(d_prior_Sigma) != (J-2))) {
      RprobitB_stop(
        "Covariance matrix for conjugate d prior is misspecified.",
        glue::glue("'d_prior_Sigma' should have dimension {J-2}."),
        glue::glue("Instead, it has dimension {dim(d_prior_Sigma)[1]}.")
      )
    }
  }
  structure(
    list(
      "conjugate" = conjugate,
      "d_prior_mean" = d_prior_mean,
      "d_prior_Sigma" = d_prior_Sigma,
      "d_prior_custom" = d_prior_custom
    ),
    class = c("RprobitB_prior_d", "list")
  )
}

#' @rdname RprobitB_prior_d
#' @param x
#' An \code{\link{RprobitB_prior_d}} object.

is.RprobitB_prior_d <- function(x) {
  inherits(x, "RprobitB_prior_d")
}

#' @rdname RprobitB_prior_d
#' @inheritParams print_matrix
#' @param ...
#' Currently not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_d <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
) {
  if (!is.RprobitB_prior_d(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_prior_d`."
    )
  }
  cat(crayon::underline("d prior"))
  cat(" : ")
  if (x$conjugate) {
    cat("Normal (conjugate)\n")
    print_matrix(
      x$d_prior_mean, rowdots = rowdots, coldots = coldots, digits = digits,
      label = "mean", simplify = simplify, details = details
    )
    cat("\n")
    print_matrix(
      x$d_prior_Sigma, rowdots = rowdots, coldots = coldots,
      digits = digits, label = "Sigma", simplify = simplify, details = details
    )
  } else {
    cat("custom\n")
    cat("d ~", function_body(x$d_prior_custom))
  }
  invisible()
}

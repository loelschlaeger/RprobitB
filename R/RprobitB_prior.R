#' Define probit model prior
#'
#' This function defines an object of class \code{RprobitB_prior}, which
#' contains the prior specification for a probit model.
#'
#' @inheritParams RprobitB_formula
#' @inheritDotParams RprobitB_prior_alpha
#' @inheritDotParams RprobitB_prior_s
#' @inheritDotParams RprobitB_prior_b
#' @inheritDotParams RprobitB_prior_Omega
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
#' The following density functions (implemented in {RprobitB}) could be helpful:
#' * \code{\link{ddirichlet}}
#' * \code{\link{dmvnorm}}
#' * \code{\link{dwishart}}
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
#'     stats::dnorm(x[1], mean = 0, sd = 1) * dunif(x[2], min = 0, max = 1)
#'   }
#' )
#'
#' @keywords specification
#' @export

RprobitB_prior <- function(formula, re = NULL, J, C = 1, ordered = FALSE, ...) {
  if (missing(formula)) {
    RprobitB_stop("Please specify the input 'formula'.")
  }
  if (missing(J)) {
    RprobitB_stop("Please specify the input 'J'.")
  }
  P_f <- compute_P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
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
#' An \code{RprobitB_prior_alpha} object or \code{NA} if \code{P_f = 0}.
#'
#' @examples
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
#'
#' @importFrom glue glue
#' @keywords internal

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
        "Custom prior for alpha is misspecified.",
        "'alpha_prior_custom' should be a function.",
        glue::glue("Instead, it is of class {class(alpha_prior_custom)[1]}.")
      )
    }
    alpha_prior_custom_test <- try(
      alpha_prior_custom(alpha_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(alpha_prior_custom_test) &&
          alpha_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for alpha is misspecified.",
        glue::glue("'alpha_prior_custom(alpha_prior_custom_test_par)' ",
                   "should return density value."),
        paste(
          "Instead, it returned (collapsed):",
          glue::glue_collapse(
            glue::glue("'{alpha_prior_custom_test}'"),
            sep = " "
          )
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(alpha_prior_mean)) {
      RprobitB_stop(
        "Mean vector for conjugate alpha prior is misspecified.",
        "'alpha_prior_mean' should be a numeric vector.",
        glue::glue("Instead, it is of class '{class(alpha_prior_mean)[1]}'.")
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
#' @inheritParams print_matrix
#' @param ...
#' Not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_alpha <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
  ) {
  stopifnot(is.RprobitB_prior_alpha(x))
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
}

#' Define \code{s} prior
#'
#' This function defines the prior distributions for the probit model parameter
#' \code{s}.
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
#' An \code{RprobitB_prior_s} object.
#'
#' @examples
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
#'
#' @importFrom glue glue
#' @keywords internal

RprobitB_prior_s <- function(
    C = 1, s_prior_concentration = rep(1, C),
    s_prior_custom = NA, s_prior_custom_test_par = numeric(C)
) {
  if (!identical(s_prior_custom, NA)) {
    conjugate <- FALSE
    s_prior_concentration <- NA
    if (!is.function(s_prior_custom)) {
      RprobitB_stop(
        "Custom prior for s is misspecified.",
        "'s_prior_custom' should be a function.",
        glue::glue("Instead, it is of class {class(s_prior_custom)[1]}.")
      )
    }
    s_prior_custom_test <- try(
      s_prior_custom(s_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(s_prior_custom_test) && s_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for s is misspecified.",
        glue::glue("'s_prior_custom(s_prior_custom_test_par)' ",
                   "should return density value."),
        paste(
          "Instead, it returned (collapsed):",
          glue::glue_collapse(
            glue::glue("'{s_prior_custom_test}'"),
            sep = " "
          )
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(s_prior_concentration)) {
      RprobitB_stop(
        "Concentration vector for conjugate s prior is misspecified.",
        "'s_prior_concentration' should be a numeric vector.",
        glue::glue("Instead, it is of class ",
                   "'{class(s_prior_concentration)[1]}'.")
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
#' An \code{RprobitB_prior_s} object.

is.RprobitB_prior_s <- function(x) {
  inherits(x, "RprobitB_prior_s")
}

#' @rdname RprobitB_prior_s
#' @inheritParams print_matrix
#' @param ...
#' Not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_s <- function (
    x, coldots = 4, digits = 2, simplify = TRUE, details = FALSE, ...
) {
  stopifnot(is.RprobitB_prior_s(x))
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
}

#' Define \code{b} prior
#'
#' This function defines the prior distributions for the probit model parameter
#' \code{b}.
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
#' An \code{RprobitB_prior_b} object or \code{NA} if \code{P_r = 0}.
#'
#' @examples
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
#'
#' @importFrom glue glue
#' @keywords internal

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
        "Custom prior for b is misspecified.",
        "'b_prior_custom' should be a function.",
        glue::glue("Instead, it is of class {class(b_prior_custom)[1]}.")
      )
    }
    b_prior_custom_test <- try(
      b_prior_custom(b_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(b_prior_custom_test) &&
          b_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for b is misspecified.",
        glue::glue("'b_prior_custom(b_prior_custom_test_par)' ",
                   "should return density value."),
        paste(
          "Instead, it returned (collapsed):",
          glue::glue_collapse(
            glue::glue("'{b_prior_custom_test}'"),
            sep = " "
          )
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is.numeric(b_prior_mean)) {
      RprobitB_stop(
        "Mean vector for conjugate b prior is misspecified.",
        "'b_prior_mean' should be a numeric vector.",
        glue::glue("Instead, it is of class '{class(b_prior_mean)[1]}'.")
      )
    }
    if (length(b_prior_mean) != P_r) {
      RprobitB_stop(
        "Mean vector for conjugate b prior is misspecified.",
        glue::glue("'b_prior_mean' should have length {P_r}."),
        glue::glue("Instead, it has length {length(b_prior_mean)}.")
      )
    }
    if (!is_cov_matrix(b_prior_Sigma)) {
      RprobitB_stop(
        "Input 'b_prior_Sigma' for conjugate b prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_cov_matrix()'."
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
#' An \code{RprobitB_prior_b} object.

is.RprobitB_prior_b <- function(x) {
  inherits(x, "RprobitB_prior_b")
}

#' @rdname RprobitB_prior_b
#' @inheritParams print_matrix
#' @param ...
#' Not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_b <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
) {
  stopifnot(is.RprobitB_prior_b(x))
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
}

#' Define \code{Omega} prior
#'
#' This function defines the prior distributions for the probit model parameter
#' \code{Omega}.
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
#' An \code{RprobitB_prior_Omega} object or \code{NA} if \code{P_r = 0}.
#'
#' @examples
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
#'
#' @importFrom glue glue
#' @keywords internal

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
    if (!is.function(Omega_prior_custom)) {
      RprobitB_stop(
        "Custom prior for Omega is misspecified.",
        "'Omega_prior_custom' should be a function.",
        glue::glue("Instead, it is of class '{class(Omega_prior_custom)[1]}'.")
      )
    }
    Omega_prior_custom_test <- try(
      Omega_prior_custom(Omega_prior_custom_test_par), silent = TRUE
    )
    if (!(is_single_numeric(Omega_prior_custom_test) &&
          Omega_prior_custom_test >= 0)) {
      RprobitB_stop(
        "Custom prior for Omega is misspecified.",
        glue::glue("'Omega_prior_custom(Omega_prior_custom_test_par)' ",
                   "should return single density value."),
        paste(
          "Instead, it returned (collapsed):",
          glue::glue_collapse(
            glue::glue("'{Omega_prior_custom_test}'"),
            sep = " "
          )
        )
      )
    }
  } else {
    conjugate <- TRUE
    if (!is_pos_int(Omega_prior_df)) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Omega prior is misspecified.",
        "'Omega_prior_df' should be a positive integer.",
        glue::glue("Instead, it is of class '{class(Omega_prior_df)[1]}'.")
      )
    }
    if (Omega_prior_df < P_r + 2) {
      RprobitB_stop(
        "Degrees of freedom for conjugate Omega prior is misspecified.",
        glue::glue("'Omega_prior_df' should be greater or equal {P_r+2}."),
        glue::glue("Instead, it is {length(Omega_prior_df)}.")
      )
    }
    if (!is_cov_matrix(Omega_prior_scale)) {
      RprobitB_stop(
        "Input 'Omega_prior_scale' for conjugate Omega prior is misspecified.",
        "It is not a proper covariance matrix.",
        "Check it with 'is_cov_matrix()'."
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
#' An \code{RprobitB_prior_Omega} object.

is.RprobitB_prior_Omega <- function(x) {
  inherits(x, "RprobitB_prior_Omega")
}

#' @rdname RprobitB_prior_Omega
#' @inheritParams print_matrix
#' @param ...
#' Not used.
#' @exportS3Method
#' @importFrom crayon underline

print.RprobitB_prior_Omega <- function (
    x, rowdots = 4, coldots = 4, digits = 2, simplify = TRUE, details = FALSE,
    ...
) {
  stopifnot(is.RprobitB_prior_Omega(x))
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
}

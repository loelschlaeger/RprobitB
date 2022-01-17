#' Return choice probabilities of an \code{RprobitB_fit}.
#'
#' @description
#' This function returns the choice probabilities of an \code{RprobitB_fit}.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param data
#' Either \code{NULL} or an object of class \code{RprobitB_data}. In the former
#' case, choice probabilities are computed for the data that was used for model
#' fitting. Alternatively, a new data set can be provided.
#' @param at_true
#' If \code{TRUE}, choice probabilities are computed at the true parameter
#' values (if they are available).
#'
#' @return
#' A data frame of choice probabilities with choice situations in rows and
#' alternatives in columns. The first two columns are the decider identifier
#' \code{"id"} and the choice situation identifier \code{"idc"}.
#'
#' @examples
#' data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#' x <- mcmc(data)
#' choice_probabilities(x)
#'
#' @export

choice_probabilities <- function(x, data = NULL, at_true = FALSE) {

  ### extract parameters
  if (at_true) {
    parameter <- x$data$true_parameter
    if (is.null(parameter)) {
      stop("True parameters are not available.")
    }
  } else {
    parameter <- point_estimates(x, FUN = mean)
  }

  ### choose data
  if (is.null(data)) {
    data <- x$data
  }
  if (class(data) != "RprobitB_data") {
    stop("'data' is not of class 'RprobitB_data'.")
  }

  ### compute probabilities
  probabilities <- matrix(NA, nrow = 0, ncol = data$J)
  for (n in 1:data$N) {
    for (t in 1:data$T[n]) {
      P_nt <- compute_choice_probabilities(
        X = data$data[[n]]$X[[t]],
        alternatives = 1:data$J,
        parameter = parameter
      )
      probabilities <- rbind(probabilities, P_nt)
    }
  }
  probabilities <- as.data.frame(probabilities)

  ### add decision maker ids
  probabilities <- cbind(data$choice_data$id, data$choice_data$idc, probabilities)
  colnames(probabilities) <- c("id", "idc", data$alternatives)
  rownames(probabilities) <- NULL

  ### return probabilities
  out <- as.data.frame(probabilities)
  return(out)
}

#' Compute probit choice probabilities for a single choice situation.
#'
#' @description
#' This function computes the probit choice probabilities for a single choice
#' situation with \code{J} alternatives.
#'
#' @param X
#' A matrix of covariates with \code{J} rows and \code{P_f + P_r} columns, where
#' the first \code{P_f} columns are connected to fixed coefficients and the last
#' \code{P_r} columns are connected to random coefficients.
#' @param alternatives
#' A vector with unique integers from \code{1} to \code{J}, indicating the
#' alternatives for which choice probabilities are to be computed.
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#'
#' @return
#' A probability vector of length \code{length(alternatives)}.
#'
#' @keywords
#' internal

compute_choice_probabilities <- function(X, alternatives, parameter) {

  ### unpack and check inputs
  if (class(parameter) != "RprobitB_parameter") {
    stop("'parameter' is not of class 'RprobitB_parameter.")
  }
  alpha <- parameter$alpha
  s <- ifelse(is.na(parameter$s), 1, parameter$s)
  b <- parameter$b
  Omega <- parameter$Omega
  Sigma_full <- parameter$Sigma_full
  P_f <- ifelse(any(is.na(alpha)), 0, length(alpha))
  P_r <- ifelse(any(is.na(parameter$s)), 0, nrow(parameter$b))
  J <- nrow(Sigma_full)

  ### check inputs
  if (!(is.numeric(alternatives) && identical(alternatives, unique(alternatives)) &&
    length(setdiff(alternatives, 1:J)) == 0)) {
    stop("'alternatives' must be a vector with unique integers from 1 to 'J'.")
  }
  if (P_f > 0 || P_r > 0) {
    if (!is.matrix(X)) {
      stop("'X' must be a matrix.")
    }
    if (ncol(X) != (P_f + P_r)) {
      stop("'X' must have 'P_f'+'P_r' columns.")
    }
    if (nrow(X) != J) {
      stop("'X' must have 'J' columns.")
    }
  }

  ### compute choice probabilities
  probabilities <- rep(NA, J)
  for (j in alternatives) {
    if (P_f > 0) {
      if (P_r > 0) {
        probabilities[j] <- ccp_pfpr(j, J, Sigma_full, X, alpha, b, Omega, s, P_f, P_r)
      }
      if (P_r == 0) {
        probabilities[j] <- ccp_pf(j, J, Sigma_full, X, alpha)
      }
    }
    if (P_f == 0) {
      if (P_r > 0) {
        probabilities[j] <- ccp_pr(j, J, Sigma_full, X, b, Omega, s, P_r)
      }
      if (P_r == 0) {
        probabilities[j] <- ccp(j, J, Sigma_full)
      }
    }
  }

  ### check if probabilities sum to 1
  if (identical(alternatives, 1:J)) {
    if (abs(sum(probabilities) - 1) > sqrt(.Machine$double.eps)) {
      warning("probabilities do not sum to 1.")
    }
  }

  ### return probabilities
  return(probabilities)
}

#' Compute probit choice probability in case of \code{P_f = 0} and \code{P_r = 0}.
#'
#' @description
#' This function computes the probit choice probability for alternative \code{j}
#' in case of \code{P_f = 0} and \code{P_r = 0}.
#'
#' @inheritParams ccp_pfpr
#'
#' @return
#' A probability.
#'
#' @keywords
#' internal
#'
#' @noRd

ccp <- function(j, J, Sigma_full) {
  mvtnorm::pmvnorm(
    lower = rep(-Inf, J - 1),
    upper = rep(0, J - 1),
    mean = rep(0, J - 1),
    sigma = delta(J, j) %*% Sigma_full %*% t(delta(J, j))
  )[1]
}

#' Compute probit choice probability in case of \code{P_f > 0} and \code{P_r = 0}.
#'
#' @description
#' This function computes the probit choice probability for alternative \code{j}
#' in case of \code{P_f > 0} and \code{P_r = 0}.
#'
#' @inheritParams ccp_pfpr
#'
#' @return
#' A probability.
#'
#' @keywords
#' internal
#'
#' @noRd

ccp_pf <- function(j, J, Sigma_full, X, alpha) {
  mvtnorm::pmvnorm(
    lower = rep(-Inf, J - 1),
    upper = as.vector(-delta(J, j) %*% X %*% alpha),
    mean = rep(0, J - 1),
    sigma = delta(J, j) %*% Sigma_full %*% t(delta(J, j))
  )[1]
}

#' Compute probit choice probability in case of \code{P_f = 0} and \code{P_r > 0}.
#'
#' @description
#' This function computes the probit choice probability for alternative \code{j}
#' in case of \code{P_f = 0} and \code{P_r > 0}.
#'
#' @inheritParams ccp_pfpr
#'
#' @return
#' A probability.
#'
#' @keywords
#' internal
#'
#' @noRd

ccp_pr <- function(j, J, Sigma_full, X, b, Omega, s, P_r) {
  out <- 0
  for (c in seq_along(s)) {
    out <- out + s[c] * mvtnorm::pmvnorm(
      lower = rep(-Inf, J - 1),
      upper = as.vector(-delta(J, j) %*% X %*% b[, c]),
      mean = rep(0, J - 1),
      sigma = delta(J, j) %*%
        (X %*% matrix(Omega[, c], P_r, P_r) %*% t(X) + Sigma_full) %*%
        t(delta(J, j))
    )[1]
  }
  return(out)
}

#' Compute probit choice probability in case of \code{P_f > 0} and \code{P_r > 0}.
#'
#' @description
#' This function computes the probit choice probability for alternative \code{j}
#' in case of \code{P_f > 0} and \code{P_r > 0}.
#'
#' @param j
#' An integer between 1 and \code{J}.
#' @inheritParams compute_choice_probabilities
#' @param X
#' A matrix of covariates with \code{J} rows and \code{P_f + P_r} columns, where
#' the first \code{P_f} columns are connected to fixed coefficients and the last
#' \code{P_r} columns are connected to random coefficients.
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#'
#' @return
#' A probability.
#'
#' @keywords
#' internal
#'
#' @noRd

ccp_pfpr <- function(j, J, Sigma_full, X, alpha, b, Omega, s, P_f, P_r) {
  out <- 0
  for (c in seq_along(s)) {
    out <- out + s[c] * mvtnorm::pmvnorm(
      lower = rep(-Inf, J - 1),
      upper = as.vector(-delta(J, j) %*% X %*% c(alpha, b[, c])),
      mean = rep(0, J - 1),
      sigma = delta(J, j) %*%
        (X[, -(1:P_f)] %*% matrix(Omega[, c], P_r, P_r) %*% t(X[, -(1:P_f)]) +
          Sigma_full) %*% t(delta(J, j))
    )[1]
  }
  return(out)
}

#' Compute choice probabilities
#'
#' @description
#' This function returns the choice probabilities of an \code{RprobitB_fit}
#' object.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param data
#' Either \code{NULL} or an object of class \code{RprobitB_data}. In the former
#' case, choice probabilities are computed for the data that was used for model
#' fitting. Alternatively, a new data set can be provided.
#' @param par_set
#' Specifying the parameter set for calculation and either
#' \itemize{
#'   \item a function that computes a posterior point estimate (the default is
#'         \code{mean()}),
#'   \item \code{"true"} to select the true parameter set,
#'   \item an object of class \code{RprobitB_parameter}.
#' }
#'
#' @return
#' A data frame of choice probabilities with choice situations in rows and
#' alternatives in columns. The first two columns are the decider identifier
#' \code{"id"} and the choice situation identifier \code{"idc"}.
#'
#' @examples
#' data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#' x <- fit_model(data)
#' choice_probabilities(x)
#'
#' @export

choice_probabilities <- function(x, data = NULL, par_set = mean) {
  ### specify parameter set
  if (is.function(par_set)) {
    parameter <- point_estimates(x, FUN = par_set)
  } else if (identical(par_set, "true")) {
    parameter <- x$data$true_parameter
    if (is.null(parameter)) {
      stop("True parameters are not available.",
           call. = FALSE
      )
    }
  } else if (inherits(par_set, "RprobitB_parameter")) {
    parameter <- par_set
  } else {
    stop(
      paste(
        "'par_set' must be either a function, 'true' or an",
        "'RprobitB_parameter' object."
      ),
      call. = FALSE
    )
  }

  ### choose data
  if (is.null(data)) {
    data <- x$data
  }
  if (!inherits(data, "RprobitB_data")) {
    stop("'data' is not of class 'RprobitB_data'.",
         call. = FALSE
    )
  }

  ### define progress bar
  pb <- RprobitB_pb(
    title = "Computing choice probabilities",
    total = data$N,
    tail = "deciders"
  )

  ### compute probabilities
  probabilities <- matrix(NA_real_, nrow = 0, ncol = data$J)
  for (n in 1:data$N) {
    RprobitB_pb_tick(pb)
    for (t in 1:data$T[n]) {
      P_nt <- compute_choice_probabilities(
        X = data$data[[n]]$X[[t]],
        alternatives = 1:data$J,
        parameter = parameter,
        ordered = x$data$ordered
      )
      probabilities <- rbind(probabilities, P_nt)
    }
  }
  probabilities <- as.data.frame(probabilities)

  ### add decision maker ids
  probabilities <- cbind(
    data$choice_data[[data$res_var_names[["id"]]]],
    data$choice_data[[data$res_var_names[["idc"]]]],
    probabilities
  )
  colnames(probabilities) <- c(
    x$data$res_var_names[["id"]],
    x$data$res_var_names[["idc"]],
    data$alternatives
  )
  rownames(probabilities) <- NULL

  ### return probabilities
  out <- as.data.frame(probabilities)
  return(out)
}

#' Compute probit choice probabilities
#'
#' @description
#' This is a helper function for \code{\link{choice_probabilities}} and computes
#' the probit choice probabilities for a single choice situation with \code{J}
#' alternatives.
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
#' @inheritParams RprobitB_data
#'
#' @return
#' A probability vector of length \code{length(alternatives)}.
#'
#' @keywords internal

compute_choice_probabilities <- function(
    X, alternatives, parameter, ordered = FALSE) {
  ### unpack and check inputs
  if (!inherits(parameter, "RprobitB_parameter")) {
    stop("'parameter' is not of class 'RprobitB_parameter.",
         call. = FALSE
    )
  }
  alpha <- parameter$alpha
  s <- ifelse(is.na(parameter$s), 1, parameter$s)
  b <- parameter$b
  Omega <- parameter$Omega
  P_f <- ifelse(anyNA(alpha), 0, length(alpha))
  P_r <- ifelse(anyNA(parameter$s), 0, nrow(parameter$b))
  if (ordered) {
    Sigma <- parameter$Sigma
    d <- parameter$d
    gamma <- as.vector(d_to_gamma(d))
    J <- length(d) + 2
  } else {
    Sigma_full <- parameter$Sigma_full
    J <- nrow(Sigma_full)
  }

  ### check inputs
  if (!(is.numeric(alternatives) &&
        identical(alternatives, unique(alternatives)) &&
        length(setdiff(alternatives, 1:J)) == 0)) {
    stop("'alternatives' must be a vector with unique integers from 1 to 'J'.",
         call. = FALSE
    )
  }
  if (P_f > 0 || P_r > 0) {
    if (!is.matrix(X)) {
      stop("'X' must be a matrix.",
           call. = FALSE
      )
    }
    if (ncol(X) != (P_f + P_r)) {
      stop("'X' must have 'P_f'+'P_r' columns.",
           call. = FALSE
      )
    }
    if (!ordered && nrow(X) != J) {
      stop("'X' must have 'J' columns.",
           call. = FALSE
      )
    }
  }

  ### compute choice probabilities
  probabilities <- rep(NA_real_, J)
  for (j in alternatives) {
    if (ordered) {
      ub <- gamma[j + 1]
      lb <- gamma[j]
      if (P_f > 0) {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                mu <- X %*% c(alpha, b[, c])
                sd <- sqrt(X[, -(1:P_f)] %*% matrix(Omega[, c], P_r, P_r) %*%
                             t(X[, -(1:P_f)]) + Sigma)
                s[c] * (stats::pnorm(q = ub - mu, mean = 0, sd = sd) -
                          stats::pnorm(q = lb - mu, mean = 0, sd = sd))
              }
            )
          )
        } else {
          mu <- X %*% alpha
          sd <- sqrt(Sigma)
          probabilities[j] <- pnorm(q = ub - mu, mean = 0, sd = sd) -
            pnorm(q = lb - mu, mean = 0, sd = sd)
        }
      } else {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                mu <- X %*% b[, c]
                sd <- sqrt(X %*% matrix(Omega[, c], P_r, P_r) %*%
                             t(X) + Sigma)
                s[c] * (pnorm(q = ub - mu, mean = 0, sd = sd) -
                          pnorm(q = lb - mu, mean = 0, sd = sd))
              }
            )
          )
        } else {
          mu <- 0
          sd <- sqrt(Sigma)
          probabilities[j] <- pnorm(q = ub - mu, mean = 0, sd = sd) -
            pnorm(q = lb - mu, mean = 0, sd = sd)
        }
      }
    } else {
      delta_j <- oeli::delta(ref = j, dim = J)
      if (P_f > 0) {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                s[c] * oeli::pmvnorm_cpp(
                  x = as.vector(-delta_j %*% X %*% c(alpha, b[, c])),
                  mean = rep(0, J - 1),
                  Sigma = delta_j %*%
                    (X[, -(1:P_f)] %*% matrix(Omega[, c], P_r, P_r) %*%
                       t(X[, -(1:P_f)]) + Sigma_full) %*% t(delta_j),
                  abseps = 0.01
                )
              }
            )
          )
        } else {
          probabilities[j] <- oeli::pmvnorm_cpp(
            x = as.vector(-delta_j %*% X %*% alpha),
            mean = rep(0, J - 1),
            Sigma = delta_j %*% Sigma_full %*% t(delta_j),
            abseps = 0.01
          )
        }
      } else {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                s[c] * oeli::pmvnorm_cpp(
                  x = as.vector(-delta_j %*% X %*% b[, c]),
                  mean = rep(0, J - 1),
                  Sigma = delta_j %*%
                    (X %*% matrix(Omega[, c], P_r, P_r) %*% t(X) + Sigma_full) %*%
                    t(delta_j),
                  abseps = 0.01
                )
              }
            )
          )
        } else {
          probabilities[j] <- oeli::pmvnorm_cpp(
            x = rep(0, J - 1),
            mean = rep(0, J - 1),
            Sigma = delta_j %*% Sigma_full %*% t(delta_j),
            abseps = 0.01
          )
        }
      }
    }
  }

  ### return probabilities
  return(probabilities)
}

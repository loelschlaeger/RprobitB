#' Matrix difference operator
#'
#' @description
#' This function creates the difference operator matrix \code{delta} for
#' subtracting a matrix row from the other matrix rows.
#'
#' @details
#' Given a matrix \code{x} with \code{J} rows, then \code{delta(i,J) %*% x}
#' computes differences with respect to row \code{i}.
#'
#' @param J
#' The number of matrix rows.
#' @param i
#' The row number to which respect differences are computed.
#'
#' @return
#' A matrix with \code{J-1} rows.
#'
#' @examples
#' J <- 2
#' x <- matrix(1, nrow = J, ncol = 2)
#' RprobitB:::delta(J, 1) %*% x
#'
#' @export
#'
#' @keywords
#' internal utils

delta <- function(J, i) {
  stopifnot(is.numeric(J), J %% 1 == 0, J >= 2)
  stopifnot(is.numeric(i), i %% 1 == 0, i >= 1)
  stopifnot(J >= 2)
  stopifnot(J >= i)
  if (i == 1) {
    Delta <- cbind(-1, diag(J - 1))
  } else if (i == J) {
    Delta <- cbind(diag(J - 1), -1)
  } else {
    Delta <- cbind(diag(J - 1)[, 1:(i - 1)], -1, diag(J - 1)[, i:(J - 1)])
  }
  return(Delta)
}

#' Matrix difference operator for ranked vectors
#'
#' @description
#' This function creates the difference operator matrix for differencing ranked
#' vector elements such that the resulting vector is negative.
#'
#' @param ranking
#' A numeric vector of the ranking in decreasing order.
#'
#' @return
#' A matrix of dimension \code{length(rank)-1} x \code{length(rank)}.
#'
#' @examples
#' x <- c(-1, 5, 10, 2)
#' ranking <- order(x, decreasing = TRUE)
#' M(ranking) %*% x
#'
#' @export
#'
#' @keywords
#' internal utils

M <- function(ranking) {
  J <- length(ranking)
  out <- matrix(0, nrow = J - 1, ncol = J)
  for (i in 1:(J - 1)) {
    out[i, ranking[i]] <- -1
    out[i, ranking[i + 1]] <- 1
  }
  return(out)
}

#' Compute Gelman-Rubin statistic
#'
#' @description
#' This function computes the Gelman-Rubin statistic \code{R_hat}.
#'
#' @references
#' <https://bookdown.org/rdpeng/advstatcomp/monitoring-convergence.html>
#'
#' @param samples
#' A vector or a matrix of samples from a Markov chain, e.g. Gibbs samples.
#' If \code{samples} is a matrix, each column gives the samples for a separate
#' run.
#' @param parts
#' The number of parts to divide each chain into sub-chains.
#'
#' @return
#' A numeric value, the Gelman-Rubin statistic.
#'
#' @examples
#' no_chains <- 2
#' length_chains <- 1e3
#' samples <- matrix(NA_real_, length_chains, no_chains)
#' samples[1, ] <- 1
#' Gamma <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)
#' for (c in 1:no_chains) {
#'   for (t in 2:length_chains) {
#'     samples[t, c] <- sample(1:2, 1, prob = Gamma[samples[t - 1, c], ])
#'   }
#' }
#' R_hat(samples)
#'
#' @keywords
#' utils
#'
#' @export
#'
#' @importFrom stats var

R_hat <- function(samples, parts = 2) {
  ### divide chains into parts
  samples <- as.matrix(samples)
  no_chains <- ncol(samples)
  length_chains <- nrow(samples)
  sub_chains <- list()
  for (c in 1:no_chains) {
    sub_chains <- c(
      sub_chains,
      split(samples[, c], cut(1:length_chains, parts))
    )
  }

  ### compute and return the Gelman-Rubin statistic
  L <- length_chains / parts
  chain_means <- sapply(sub_chains, mean)
  grand_mean <- mean(chain_means)
  B <- 1 / (parts - 1) * sum((chain_means - grand_mean)^2)
  chain_variances <- sapply(sub_chains, stats::var)
  W <- sum(chain_variances) / parts
  R_hat <- ((L - 1) / L * W + B) / W
  return(R_hat)
}

#' Check covariance matrix properties
#'
#' @description
#' This function checks if the input is a proper covariance matrix, i.e. a
#' symmetric, numeric matrix with non-negative eigenvalues.
#'
#' @param x
#' A matrix.
#'
#' @return
#' A boolean, \code{TRUE} if \code{x} is a proper covariance matrix.
#'
#' @examples
#' x <- diag(2)
#' RprobitB:::is_covariance_matrix(x)
#' @keywords
#' internal utils

is_covariance_matrix <- function(x) {
  is.matrix(x) &&
    is.numeric(x) &&
    ncol(x) == nrow(x) &&
    all(abs(x - t(x)) < sqrt(.Machine$double.eps)) &&
    all(eigen(x)$value > -sqrt(.Machine$double.eps))
}

#' Print abbreviated matrices and vectors
#'
#' @description
#' This function prints abbreviated matrices and vectors.
#'
#' @references
#' This function is a modified version of the \code{pprint()} function from the
#' \code{ramify} R package.
#'
#' @param x
#' A (numeric or character) matrix or a vector.
#' @param rowdots
#' The row number which is replaced by dots.
#' @param coldots
#' The column number which is replaced by dots.
#' @param digits
#' If \code{x} is numeric, sets the number of decimal places.
#' @param name
#' Either \code{NULL} or a label for \code{x}. Only printed if
#' \code{desc = TRUE}.
#' @param desc
#' Set to \code{TRUE} to print the name and the dimension of \code{x}.
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @examples
#' RprobitB:::pprint(x = 1, name = "single integer")
#' RprobitB:::pprint(x = LETTERS[1:26], name = "letters")
#' RprobitB:::pprint(
#'   x = matrix(rnorm(100), ncol = 1),
#'   name = "single column matrix"
#' )
#' RprobitB:::pprint(x = matrix(1:100, nrow = 1), name = "single row matrix")
#' RprobitB:::pprint(x = matrix(LETTERS[1:24], ncol = 6), name = "big matrix")
#'
#' @keywords
#' internal utils

pprint <- function(x, rowdots = 4, coldots = 4, digits = 4, name = NULL,
                   desc = TRUE) {
  ### helper function
  add_dots <- function(x, pos = 3) {
    if (length(x) > pos) {
      c(x[seq_len(pos - 1)], "...", x[length(x)])
    } else {
      x
    }
  }

  ### distinguish between single numbers, vectors and matrices
  if (length(x) == 1) {
    if (desc) if (!is.null(name)) cat(name, ": ")
    cat(x)
  } else if (!is.matrix(x)) {
    if (desc) {
      if (!is.null(name)) {
        cat(name, ":", typeof(x), "vector of length", length(x), "\n\n")
      }
    }
    res <- if (is.numeric(x)) round(x, digits) else x
    cat(noquote(add_dots(res, coldots)))
  } else {
    ### row labels
    row_labels <- if (is.null(rownames(x))) {
      paste0("[", seq_len(nrow(x)), ",]")
    } else {
      rownames(x)
    }

    ### columns labels
    col_labels <- if (is.null(colnames(x))) {
      paste0("[,", seq_len(ncol(x)), "]")
    } else {
      colnames(x)
    }

    ### adjust values for 'coldots' and 'rowdots'
    coldots <- max(1, min(ncol(x) - 1, coldots))
    rowdots <- max(1, min(nrow(x) - 1, rowdots))

    ### omit all values that will not be printed
    x2 <- if (nrow(x) == 1) {
      cbind(x[1, 1:coldots, drop = FALSE], x[1, ncol(x), drop = FALSE])
    } else if (ncol(x) == 1) {
      rbind(x[1:rowdots, 1, drop = FALSE], x[nrow(x), 1, drop = FALSE])
    } else {
      rbind(
        cbind(
          x[1:rowdots, 1:coldots, drop = FALSE],
          x[1:rowdots, ncol(x), drop = FALSE]
        ),
        cbind(
          x[nrow(x), 1:coldots, drop = FALSE],
          x[nrow(x), ncol(x), drop = FALSE]
        )
      )
    }

    ### convert to character matrix
    charx <- if (typeof(x2) == "character") {
      x2
    } else if (typeof(x2) %in% c("integer", "logical")) {
      as.character(x2)
    } else {
      sprintf(paste0("%.", digits, "f"), x2)
    }
    dim(charx) <- dim(x2)

    ### Case 1: rows and columns do not have dots
    if (nrow(x) <= rowdots + 1 && ncol(x) <= coldots + 1) {
      res <- x
    }

    ### Case 2: rows have dots, columns do not
    if (nrow(x) > rowdots + 1 && ncol(x) <= coldots + 1) {
      res <- rbind(
        as.matrix(charx[seq_len(rowdots - 1), ]),
        rep("...", ncol(charx)),
        charx[nrow(charx), ]
      )
      row_labels <- add_dots(row_labels, pos = rowdots)
    }

    ### Case 3: rows do not have dots, columns have dots
    if (nrow(x) <= rowdots + 1 && ncol(x) > coldots + 1) {
      res <- t(apply(charx, 1, add_dots, pos = coldots))
      col_labels <- add_dots(col_labels, pos = coldots)
    }

    ### Case 4: rows and columns have dots
    if (nrow(x) > rowdots + 1 && ncol(x) > coldots + 1) {
      smallx <- t(apply(charx[seq_len(rowdots - 1), ], 1, add_dots,
        pos = coldots
      ))
      res <- rbind(
        smallx,
        rep("...", ncol(smallx)),
        add_dots(charx[nrow(charx), ], pos = coldots)
      )
      row_labels <- add_dots(row_labels, pos = rowdots)
      col_labels <- add_dots(col_labels, pos = coldots)
    }

    ### print matrix
    if (desc) {
      if (!is.null(name)) {
        cat(name, ": ")
      }
      cat(
        paste(dim(x), collapse = " x "), "matrix of", paste0(typeof(x), "s"),
        "\n\n"
      )
    }
    prmatrix(res,
      rowlab = row_labels, collab = col_labels, quote = FALSE,
      right = TRUE
    )
  }

  ### return 'x' invisibly
  return(invisible(x))
}

#' Permutations of a vector
#'
#' @description
#' This function returns all permutations of a given vector.
#'
#' @references
#' This function is a modified version of
#' <https://stackoverflow.com/a/20199902/15157768>.
#'
#' @param x
#' A vector.
#'
#' @return
#' A list of all permutations of \code{x}.
#'
#' @examples
#' RprobitB:::permutations(x = c("a", "b", "c"))
#'
#' @keywords
#' internal utils

permutations <- function(x) {
  perm_index <- function(n) {
    if (n == 1) {
      return(matrix(1))
    } else {
      sp <- perm_index(n - 1)
      p <- nrow(sp)
      A <- matrix(nrow = n * p, ncol = n)
      for (i in 1:n) {
        A[(i - 1) * p + 1:p, ] <- cbind(i, sp + (sp >= i))
      }
      return(A)
    }
  }
  p <- perm_index(length(x))
  out <- list()
  for (i in 1:nrow(p)) {
    out <- c(out, list(x[p[i, ]]))
  }
  return(out)
}

#' Check boolean
#'
#' This function checks whether the input is a single boolean.
#'
#' @param x
#' Any element.
#'
#' @return
#' A \code{logical}, \code{TRUE} if \code{x} is either \code{TRUE} or
#' \code{FALSE}, \code{FALSE} else.
#'
#' @examples
#' RprobitB:::is_bool("TRUE")
#' RprobitB:::is_bool(FALSE)
#'
#' @keywords internal utils

is_bool <- function(x) {
  length(x) == 1 && (isTRUE(x) || isFALSE(x))
}

#' Check single numeric
#'
#' This function checks whether the input is a single numeric value.
#'
#' @param x
#' Any element.
#'
#' @return
#' A \code{logical}, \code{TRUE} if \code{x} is a single numeric, \code{FALSE}
#' else.
#'
#' @examples
#' RprobitB:::is_single_numeric(1)
#' RprobitB:::is_single_numeric("1")
#' RprobitB:::is_single_numeric(NA_real_)
#'
#' @keywords internal utils

is_single_numeric <- function(i) {
  is.numeric(i) && length(i) == 1 && !is.na(i)
}

#' Check positive integer
#'
#' This function checks whether the input is a single positive integer.
#'
#' @param x
#' Any element.
#'
#' @return
#' A \code{logical}, \code{TRUE} if \code{i} is a single positive integer,
#' \code{FALSE} else.
#'
#' @examples
#' RprobitB:::is_pos_int(-1.5)
#' RprobitB:::is_pos_int(1)
#'
#' @keywords internal utils

is_pos_int <- function(i) {
  is_single_numeric(i) && i %% 1 == 0 && i > 0
}

#' Check covariance matrix
#'
#' This function checks whether the input is a proper covariance matrix.
#'
#' @details
#' A proper covariance matrix is a square, symmetric, numeric matrix with
#' non-negative eigenvalues.
#'
#' @param x
#' A \code{matrix}.
#' @param tol
#' A \code{numeric}, a numeric tolerance value.
#' Per default, \code{tol = sqrt(.Machine$double.eps)}.
#'
#' @return
#' A \code{logical}, \code{TRUE} if \code{x} is a proper covariance matrix,
#' \code{FALSE} else.
#'
#' @examples
#' x <- RprobitB:::sample_cov_matrix(dim = 3)
#' RprobitB:::is_cov_matrix(x)
#'
#' @keywords internal utils

is_cov_matrix <- function(x, tol = sqrt(.Machine$double.eps)) {
  is.matrix(x) && is.numeric(x) && ncol(x) == nrow(x) &&
    all(abs(x - t(x)) < tol) && all(eigen(x)$value > -tol)
}

#' Sample covariance matrix
#'
#' This function samples a covariance matrix from a Wishart distribution.
#'
#' @param dim
#' An \code{integer}, the matrix dimension.
#' @param df
#' An \code{integer}, the degrees of freedom.
#' Must be at least \code{dim}.
#' Per default, \code{df = dim}.
#' @param scale
#' A \code{matrix}, the scale matrix.
#' Must be a covariance matrix.
#' Per default, \code{scale = diag(dim)}.
#'
#' @return
#' A \code{matrix}, a covariance matrix.
#'
#' @examples
#' RprobitB:::sample_cov_matrix(dim = 3)
#'
#' @keywords internal utils

sample_cov_matrix <- function(dim, df = dim, scale = diag(dim)) {
  stopifnot(is_pos_int(dim), is_pos_int(df), is_cov_matrix(scale))
  rwishart(df = df, scale = scale, inv = FALSE)
}

#' Extract function body as character
#'
#' This function extracts the body of a function as a character.
#'
#' @param fun
#' A \code{function}.
#' @param braces
#' A \code{logical}, if \code{FALSE} (default) removes \code{"{"} and \code{"}"}
#' at start and end (if any).
#' @param nchar
#' An \code{integer}, the maximum number of characters before abbreviation.
#' Must be at least \code{3}.
#' By default, \code{nchar = getOption("width")}.
#'
#' @return
#' A \code{character}, the body of \code{f}.
#'
#' @examples
#' fun <- RprobitB:::is_cov_matrix
#' RprobitB:::function_body(fun)
#' RprobitB:::function_body(fun, braces = TRUE)
#' RprobitB:::function_body(fun, nchar = 30)
#'
#' @keywords utils

function_body <- function(
    fun, braces = FALSE, nchar = getOption("width") - 4
  ) {
  stopifnot(is.function(fun), is_bool(braces))
  nchar <- as.integer(nchar)
  stopifnot(nchar >= 3)
  out <- deparse1(body(fun))
  if (!braces) out <- gsub("^[{]|[}]$","", out)
  out <- trimws(gsub("\\s+", " ", out))
  if (nchar(out) > nchar) out <- paste0(strtrim(out, nchar - 3), '...')
  out
}

#' Print (abbreviated) matrices
#'
#' This function prints (abbreviated) matrices.
#'
#' @references
#' This function is a modified version of \code{\link[ramify]{pprint}}.
#'
#' @param x
#' A \code{numeric} or \code{character} (vector or matrix).
#' @param rowdots
#' An \code{integer}, the row number which is replaced by \code{...}.
#' By default, \code{rowdots = 4}.
#' @param coldots
#' An \code{integer}, the column number which is replaced by \code{...}.
#' By default, \code{coldots = 4}.
#' @param digits
#' An \code{integer}, the number of printed decimal places.
#' Only relevant if input \code{x} is numeric.
#' By default, \code{digits = 2}.
#' @param label
#' A \code{character}, a label for \code{x}.
#' Only printed if \code{simplify = FALSE}.
#' By default, \code{label = NULL}, i.e., no label.
#' @param simplify
#' A \code{logical}, set to \code{TRUE} to simplify the output.
#' By default, \code{simplify = FALSE}.
#' @param details
#' A \code{logical}, set to \code{TRUE} to print the type and
#' dimension of \code{x}.
#' By default, \code{details = !simplify}.
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @examples
#' print_matrix(x = 1, label = "single numeric")
#' print_matrix(x = LETTERS[1:26], label = "letters")
#' print_matrix(x = 1:3, coldots = 2)
#' print_matrix(x = matrix(rnorm(99), ncol = 1), label = "single column matrix")
#' print_matrix(x = matrix(1:100, nrow = 1), label = "single row matrix")
#' print_matrix(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix")
#' print_matrix(x = diag(5), coldots = 2, rowdots = 2, simplify = TRUE)
#'
#' @export
#' @importFrom crayon italic bold
#' @keywords internal utils

print_matrix <- function(
    x, rowdots = 4, coldots = 4, digits = 2, label = NULL, simplify = FALSE,
    details = !simplify
) {
  stopifnot(is.numeric(x) || is.character(x))
  stopifnot(is_pos_int(rowdots), is_pos_int(coldots), is_single_numeric(digits))
  stopifnot(is_bool(details), is_bool(simplify))
  if (!is.null(label)) {
    label <- as.character(label)
    stopifnot(length(label) == 1)
  }
  add_dots <- function(x, pos) {
    if (length(x) > pos) c(x[seq_len(pos-1)], "...", x[length(x)]) else x
  }
  if (is.numeric(x)) x <- round(x, digits)
  if (!is.null(label)) cat(crayon::italic(label), ": ")
  if (length(x) == 1){
    cat(x)
  } else if (!is.matrix(x)) {
    if (details) {
      cat(typeof(x), "vector of length", crayon::bold(length(x)), "\n")
    }
    cat(noquote(add_dots(x, coldots)))
  } else {
    row_labs <- if (is.null(rownames(x))) {
      paste0("[", seq_len(nrow(x)), ",]")
    } else {
      rownames(x)
    }
    col_labs <- if (is.null(colnames(x))) {
      paste0("[,", seq_len(ncol(x)), "]")
    } else {
      colnames(x)
    }
    coldots <- max(1, min(ncol(x) - 1, coldots))
    rowdots <- max(1, min(nrow(x) - 1, rowdots))
    if (coldots == 1 && rowdots == 1) {
      if (nrow(x) <= 2 && ncol(x) <= 2) {
        res <- x
      } else {
        res <- if (nrow(x) == 1) {
          matrix(c("...", x[1, ncol(x)]), 1, 2)
        } else if (ncol(x) == 1) {
          matrix(c("...", x[nrow(x), 1]), 2, 1)
        } else {
          matrix(c("...", "...", "...", x[nrow(x), ncol(x)]), 2, 2)
        }
        row_labs <- add_dots(row_labs, 1)
        col_labs <- add_dots(col_labs, 1)
      }
    } else {
      x2 <- if(nrow(x) == 1) {
        cbind(x[1, 1:coldots, drop = FALSE], x[1, ncol(x), drop = FALSE])
      } else if(ncol(x) == 1) {
        rbind(x[1:rowdots, 1, drop = FALSE], x[nrow(x), 1, drop = FALSE])
      } else {
        rbind(cbind(x[1:rowdots, 1:coldots, drop = FALSE],
                    x[1:rowdots, ncol(x), drop = FALSE]),
              cbind(x[nrow(x), 1:coldots, drop = FALSE],
                    x[nrow(x), ncol(x), drop = FALSE]))
      }
      charx <- as.character(x2)
      dim(charx) <- dim(x2)
      if (nrow(x) <= rowdots + 1 && ncol(x) <= coldots + 1) {
        res <- charx
      } else if (nrow(x) > rowdots + 1 && ncol(x) <= coldots + 1) {
        res <- rbind(
          as.matrix(charx[seq_len(rowdots - 1), ]),
          rep("...", ncol(charx)),
          charx[nrow(charx), ]
        )
        row_labs <- add_dots(row_labs, pos = rowdots)
      } else if (nrow(x) <= rowdots + 1 && ncol(x) > coldots + 1) {
        res <- t(apply(charx, 1, add_dots, pos = coldots))
        col_labs <- add_dots(col_labs, pos = coldots)
      } else if (nrow(x) > rowdots + 1 && ncol(x) > coldots + 1) {
        smallx <- t(apply(charx[seq_len(rowdots - 1), , drop = FALSE], 1,
                          add_dots, pos = coldots))
        res <- rbind(
          smallx,
          rep("...", ncol(smallx)),
          add_dots(charx[nrow(charx), ], pos = coldots)
        )
        row_labs <- add_dots(row_labs, pos = rowdots)
        col_labs <- add_dots(col_labs, pos = coldots)
      }
    }
    if (details) {
      cat(crayon::bold(paste(dim(x), collapse = " x ")), "matrix of",
          paste0(typeof(x), "s"), "\n")
    }
    if (simplify) {
      cat(paste("[", paste(apply(res, 1, paste, collapse = " "),
                           collapse = "; "), "]"))
    } else {
      prmatrix(res, rowlab = row_labs, collab = col_labs, quote = FALSE,
               right = TRUE)
    }
  }
  return(invisible(x))
}

#' Build permutations
#'
#' This function creates all permutations of a given vector.
#'
#' @references
#' Modified version of <https://stackoverflow.com/a/20199902/15157768>.
#'
#' @param x
#' Any \code{vector}.
#'
#' @return
#' A \code{list} of all permutations of \code{x}.
#'
#' @examples
#' RprobitB:::permutations(1:3)
#' RprobitB:::permutations(LETTERS[1:3])
#'
#' @keywords internal utils

permutations <- function(x){
  stopifnot(is.vector(x))
  perm_index <- function(n){
    if (n == 1) {
      return(matrix(1))
    } else {
      sp <- perm_index(n-1)
      p <- nrow(sp)
      A <- matrix(nrow = n*p, ncol = n)
      for (i in 1:n) {
        A[(i-1)*p+1:p,] <- cbind(i, sp + (sp >= i))
      }
      return(A)
    }
  }
  p <- perm_index(length(x))
  apply(p, 1, function(p) x[p], simplify = FALSE)
}



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
  rwishart(nu = df, V = scale)$W # TODO: rename args nu and V in rwishart
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


### TODO Not touched from here


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
#' RprobitB:::permutations(x = c("a","b","c"))
#'
#' @keywords
#' internal utils

permutations <- function(x){
  perm_index <- function(n){
    if(n==1){
      return(matrix(1))
    } else {
      sp <- perm_index(n-1)
      p <- nrow(sp)
      A <- matrix(nrow=n*p,ncol=n)
      for(i in 1:n){
        A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
      }
      return(A)
    }
  }
  p <- perm_index(length(x))
  out <- list()
  for(i in 1:nrow(p)) {
    out <- c(out, list(x[p[i,]]))
  }
  return(out)
}



#' Print abbreviated vectors and matrices
#'
#' @description
#' This function prints abbreviated vectors and matrices.
#'
#' @references
#' This function is a modified version of \code{\link[ramify]{pprint}}.
#'
#' @param x
#' A (numeric or character) vector or matrix.
#' @param rowdots
#' The row number which is replaced by \code{...}.
#' @param coldots
#' The column number which is replaced by \code{...}.
#' @param digits
#' The number of decimal places, if \code{x} is numeric.
#' @param label
#' A character, a label for \code{x}.
#' Only printed if \code{details = TRUE} and \code{onerow = FALSE}.
#' @param details
#' A boolean, set to \code{TRUE} (default) to print the name and the dimension
#' of \code{x}.
#' @param onerow
#' A boolean, set to \code{TRUE} to print \code{x} in a single row.
#'
#' @return
#' Invisibly returns \code{x}.
#'
#' @examples
#' pprint(x = 1, label = "single numeric")
#' pprint(x = LETTERS[1:26], label = "letters")
#' pprint(x = 1:3, coldots = 2)
#' pprint(x = matrix(rnorm(100), ncol = 1), label = "single column matrix")
#' pprint(x = matrix(1:100, nrow = 1), label = "single row matrix")
#' pprint(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix")
#' pprint(x = diag(5), coldots = 2, rowdots = 2, onerow = TRUE)
#'
#' @export
#'
#' @importFrom crayon italic bold
#'
#' @keywords
#' internal utils

pprint <- function(
    x, rowdots = 4, coldots = 4, digits = 4, label = NULL, details = TRUE,
    onerow = FALSE
  ) {
  stopifnot(is_int(rowdots), is_int(coldots), is_sn(digits))
  stopifnot(is_bool(details), is_bool(onerow))
  if (onerow) details <- FALSE
  if (!is.null(label)) {
    label <- as.character(label)
    stopifnot(length(label) == 1)
  }
  add_dots <- function(x, pos) {
    if (length(x) > pos) c(x[seq_len(pos-1)], "...", x[length(x)]) else x
  }
  if (length(x) == 1){
    if(details && !is.null(label)) cat(crayon::italic(label), ": ")
    cat(x)
  } else if (!is.matrix(x)) {
    if(details) if(!is.null(label))
      cat(crayon::italic(label), ":", typeof(x), "vector of length",
          crayon::bold(length(x)), "\n")
    res <- if(is.numeric(x)) round(x,digits) else x
    cat(noquote(add_dots(res, coldots)))
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
    coldots <- max(1, min(ncol(x)-1, coldots))
    rowdots <- max(1, min(nrow(x)-1, rowdots))
    if (coldots == 1 && rowdots == 1) {
      res <- matrix(c("...", "...", "...", x[nrow(x), ncol(x)]), nrow = 2, ncol = 2)
      row_labs <- add_dots(row_labs, 1)
      col_labs <- add_dots(col_labs, 1)
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
      charx <- if (typeof(x2) == "character") {
        x2
      } else if (typeof(x2) %in% c("integer", "logical")) {
        as.character(x2)
      } else {
        sprintf(paste0("%.", digits, "f"), x2)
      }
      dim(charx) <- dim(x2)
      if (nrow(x) <= rowdots + 1 && ncol(x) <= coldots + 1) {
        res <- x
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
    if (details){
      if (!is.null(label)) cat(crayon::italic(label), ": ")
      cat(crayon::bold(paste(dim(x), collapse = " x ")), "matrix of",
          paste0(typeof(x), "s"), "\n")
    }
    if (onerow) {
      cat(paste("[", paste(apply(res, 1, paste, collapse = " "), collapse = "; "), "]"))
    } else {
      prmatrix(res, rowlab = row_labs, collab = col_labs, quote = FALSE, right = TRUE)
    }
  }
  return(invisible(x))
}

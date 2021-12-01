#' Print method for \code{RprobitB_gibbs_samples_statistics}.
#' @description
#' This function is the print method for an object of class
#' \code{RprobitB_gibbs_samples_statistics}.
#' @param x
#' An object of class \code{RprobitB_gibbs_samples_statistics}.
#' @param true
#' Either \code{NULL} or an object of class \code{RprobitB_true_parameter}.
#' @inheritParams print.summary.RprobitB_model
#' @param ...
#' Ignored.

print.RprobitB_gibbs_samples_statistics <- function(x, true = NULL, digits = 2, ...) {

  ### check inputs
  if (class(x) != "RprobitB_gibbs_samples_statistics") {
    stop("'x' must be of class 'RprobitB_gibbs_samples_statistics'.")
  }
  if (!is.null(true)) {
    if (class(true) != "RprobitB_parameter") {
      stop("'true' must be of class 'RprobitB_parameter'.")
    }
  }
  if (!(is.numeric(digits) && digits >= 0)) {
    stop("'digits' must a non-negative number.")
  }

  ### print statistics
  cols <- colnames(x[[1]])
  if (length(cols) > 0) {

    ### determine cell width
    cw <- max(digits + 5, max(nchar(cols)) + 1)

    ### print header of table
    cat("Parameter statistics:\n")
    header <- sprintf("%6s", " ")
    if (!is.null(true)) {
      header <- paste0(header, sprintf(paste0("%", cw + 1, "s"), "true"))
    }
    for (header_element in cols) {
      header <- paste0(
        header,
        sprintf(paste0("%", cw + 1, "s"), header_element)
      )
    }
    cat(header)

    ### print table elements
    order_of_parameters <- c("alpha", "s", "b", "Omega", "Sigma")
    for (par_name in intersect(order_of_parameters, names(x))) {
      out <- x[[par_name]]
      if (!is.null(true)) {
        true_par_name <- true[[par_name]][rownames(out)]
        rownames_true <- names(true_par_name)[!is.na(names(true_par_name))]
        rownames_all <- union(rownames(out), rownames_true)
        out <- cbind(true_par_name, out)
        rownames(out) <- rownames_all
      }
      out <- round(out, digits)
      colnames(out) <- rep(sprintf(paste0("%", cw, "s"), " "), ncol(out))
      rownames(out) <- sprintf("%6s", rownames(out))
      writeLines(paste("\n", par_name))
      print(formatC(out,
        format = "f", digits = digits, width = cw,
        flag = ""
      ), quote = FALSE)
    }
  }
  return(invisible(x))
}

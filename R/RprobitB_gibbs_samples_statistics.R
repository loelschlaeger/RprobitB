#' Create object of class \code{RprobitB_gibbs_samples_statistics}
#'
#' @description
#' This function creates an object of class \code{RprobitB_gibbs_samples_statistics}.
#'
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param FUN
#' A (preferably named) list of functions that compute parameter statistics
#' from the Gibbs samples, i.e.
#' \itemize{
#'   \item \code{mean} for the mean,
#'   \item \code{sd} for the standard deviation,
#'   \item \code{min} for the minimum,
#'   \item \code{max} for the maximum,
#'   \item \code{median} for the median,
#'   \item \code{function(x) quantile(x, p)} for the \code{p}th quantile,
#'   \item \code{R_hat} for the Gelman-Rubin statistic.
#' }
#'
#' @return
#' An object of class \code{RprobitB_gibbs_samples_statistics}, which is a list
#' of statistics from \code{gibbs_samples} obtained by applying the elements of
#' \code{FUN}.
#'
#' @keywords
#' constructor

RprobitB_gibbs_samples_statistics <- function(gibbs_samples, FUN) {

  ### check inputs
  if (class(gibbs_samples) != "RprobitB_gibbs_samples") {
    stop("'gibbs_samples' must be of class 'RprobitB_gibbs_samples'.")
  }
  for (i in seq_len(length(FUN))) {
    if (class(FUN[[i]]) != "function") {
      stop("Element ", i, " in 'FUN' is not of class 'function'.")
    }
    if (is.null(names(FUN)[i]) || names(FUN)[i] == "") {
      names(FUN)[i] <- paste0("FUN", i)
    }
  }
  if (any(sapply(FUN, class) != "function")) {
    stop("Not all elements of 'FUN' are functions.")
  }

  ### build 'RprobitB_gibbs_sample_statistics'
  statistics <- list()
  for (par in names(gibbs_samples[["gibbs_samples_nbt"]])) {
    statistics[[par]] <- matrix(
      NA,
      nrow = ncol(gibbs_samples[["gibbs_samples_nbt"]][[par]]), ncol = 0,
      dimnames = list(colnames(gibbs_samples[["gibbs_samples_nbt"]][[par]]))
    )
    for (i in seq_len(length(FUN))) {
      fun <- FUN[[i]]
      values <- apply(gibbs_samples[["gibbs_samples_nbt"]][[par]], 2, fun,
        simplify = FALSE
      )
      nvalue <- length(values[[1]])
      labels <- colnames(gibbs_samples[["gibbs_samples_nbt"]][[par]])
      fun_name <- if (nvalue == 1) {
        names(FUN[i])
      } else {
        paste(names(FUN[i]), seq_len(nvalue), sep = "_", recycle0 = TRUE)
      }
      append <- matrix(NA,
        nrow = length(values), ncol = nvalue,
        dimnames = list(labels, fun_name)
      )
      for (j in seq_len(length(values))) {
        append[j, ] <- values[[j]]
      }
      statistics[[par]] <- cbind(statistics[[par]], append)
    }
  }

  ### return
  class(statistics) <- "RprobitB_gibbs_samples_statistics"
  return(statistics)
}

#' @param x
#' An object of class \code{RprobitB_gibbs_samples_statistics}.
#' @param true
#' Either \code{NULL} or an object of class \code{RprobitB_true_parameter}.
#' @inheritParams print.summary.RprobitB_fit
#' @param ...
#' Ignored.
#'
#' @noRd
#'
#' @export
#' @importFrom crayon underline

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
    cat(crayon::underline("Gibbs sample statistics\n"))
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

#' Filter Gibbs samples
#'
#' @description
#' This is a helper function that filters Gibbs samples.
#'
#' @param x
#' An object of class \code{RprobitB_gibbs_samples}.
#' @inheritParams parameter_labels
#'
#' @return
#' An object of class \code{RprobitB_gibbs_samples} filtered by the labels of
#' \code{parameter_labels(P_f, P_r, J, C, cov_sym, keep_par, drop_par)}.
#'
#' @keywords
#' internal

filter_gibbs_samples <- function(x, P_f, P_r, J, C, cov_sym,
                                 keep_par = c("s", "alpha", "b", "Omega", "Sigma"),
                                 drop_par = NULL) {
  labels <- parameter_labels(P_f, P_r, J, C, cov_sym, keep_par, drop_par)
  for (gs in names(x)) {
    for (par in names(x[[gs]])) {
      if (!par %in% names(labels)) {
        x[[gs]][[par]] <- NULL
      } else {
        cols <- intersect(colnames(x[[gs]][[par]]), labels[[par]])
        x[[gs]][[par]] <- x[[gs]][[par]][, cols, drop = FALSE]
      }
      x[[gs]] <- x[[gs]][lengths(x[[gs]]) != 0]
    }
  }
  return(x)
}

#' Create object of class \code{RprobitB_gibbs_samples_statistics}
#'
#' @description
#' This function creates an object of class
#' \code{RprobitB_gibbs_samples_statistics}.
#'
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}, which generally is located
#' as object \code{gibbs_samples} in an \code{RprobitB_model} object.
#' @param FUN
#' A (preferably named) list of functions that compute parameter statistics
#' from the Gibbs samples, for example
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
#' internal

RprobitB_gibbs_samples_statistics <- function(
    gibbs_samples, FUN = list("mean" = mean)) {
  ### check inputs
  if (!inherits(gibbs_samples, "RprobitB_gibbs_samples")) {
    stop("'gibbs_samples' must be of class 'RprobitB_gibbs_samples'.",
         call. = FALSE
    )
  }
  for (i in seq_len(length(FUN))) {
    if (!is.function(FUN[[i]])) {
      stop("Element ", i, " in 'FUN' is not of class 'function'.",
           call. = FALSE
      )
    }
    if (is.null(names(FUN)[i]) || names(FUN)[i] == "") {
      names(FUN)[i] <- paste0("FUN", i)
    }
  }
  if (any(sapply(FUN, class) != "function")) {
    stop("Not all elements of 'FUN' are functions.",
         call. = FALSE
    )
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
      append <- matrix(NA_real_,
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
#' Either \code{NULL} or an object of class \code{RprobitB_parameter}.
#' @inheritParams print.summary.RprobitB_fit
#' @param ...
#' Ignored.
#'
#' @noRd
#'
#' @export

print.RprobitB_gibbs_samples_statistics <- function(
    x, true = NULL, digits = 2, ...) {
  ### check inputs
  if (!inherits(x, "RprobitB_gibbs_samples_statistics")) {
    stop("'x' must be of class 'RprobitB_gibbs_samples_statistics'.",
         call. = FALSE
    )
  }
  if (!is.null(true)) {
    if (!inherits(true, "RprobitB_parameter")) {
      stop("'true' must be of class 'RprobitB_parameter'.",
           call. = FALSE
      )
    }
  }
  if (!(is.numeric(digits) && digits >= 0)) {
    stop("'digits' must a non-negative number.",
         call. = FALSE
    )
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

    ### determine order of parameters
    order_of_parameters <- c("alpha", "s", "b", "Omega", "Sigma", "d")

    ### ignore 's' if it is trivial
    if ("s" %in% names(x)) {
      if ((is.null(true) || true$C == 1) && nrow(x[["s"]]) == 1) {
        x[["s"]] <- NULL
      }
    }

    ### print table elements
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


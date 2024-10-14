#' Create object of class `RprobitB_latent_classes`
#'
#' @description
#' This function creates an object of class `RprobitB_latent_classes` which
#' defines the number of latent classes and their updating scheme.
#' The `RprobitB_latent_classes` object generated
#' by this function is only of relevance if the model possesses at least one
#' random coefficient, i.e. if `P_r>0`.
#'
#' @details
#' ## Why update latent classes?
#' In order not to have to specify the number of latent classes before
#' estimation.
#'
#' ## What options to update latent classes exist?
#' Currently two updating schemes are implemented, weight-based and
#' via a Dirichlet process, see
#' [the vignette on modeling heterogeneity](https://loelschlaeger.de/RprobitB/articles/v04_modeling_heterogeneity.html).
#'
#' ## What is the default behavior?
#' One latent class without updates is specified per default. Print an
#' \code{RprobitB_latent_classes}-object to see a summary of all relevant
#' (default) parameter settings.
#'
#' ## Why is \code{Cmax} required?
#' The implementation requires an upper bound on the number of latent classes
#' for saving the Gibbs samples. However, this is not a restriction since the
#' number of latent classes is bounded by the number of deciders in any case.
#' A plot method for visualizing the sequence of class numbers after estimation
#' and can be used to check if \code{Cmax} was reached, see
#' \code{\link{plot.RprobitB_fit}}.
#'
#' @param latent_classes
#' Either \code{NULL} (for no latent classes) or a list of parameters specifying
#' the number of latent classes and their updating scheme:
#' \itemize{
#'   \item \code{C}: The fixed number (greater or equal 1) of latent classes,
#'         which is set to 1 per default. If either \code{weight_update = TRUE}
#'         or \code{dp_update = TRUE} (i.e. if classes are updated), \code{C}
#'         equals the initial number of latent classes.
#'   \item \code{weight_update}: A boolean, set to \code{TRUE} to weight-based
#'         update the latent classes. See ... for details.
#'   \item \code{dp_update}: A boolean, set to \code{TRUE} to update the latent
#'         classes based on a Dirichlet process. See ... for details.
#'   \item \code{Cmax}: The maximum number of latent classes.
#'   \item \code{buffer}: The number of iterations to wait before a next
#'         weight-based update of the latent classes.
#'   \item \code{epsmin}: The threshold weight (between 0 and 1) for removing
#'         a latent class in the weight-based updating scheme.
#'   \item \code{epsmax}: The threshold weight (between 0 and 1) for splitting
#'         a latent class in the weight-based updating scheme.
#'   \item \code{distmin}: The (non-negative) threshold in class mean difference
#'         for joining two latent classes in the weight-based updating scheme.
#' }
#'
#' @return
#' An object of class \code{RprobitB_latent_classes}.
#'
#' @examples
#' ### default setting
#' RprobitB:::RprobitB_latent_classes()
#'
#' ### setting for a fixed number of two latent classes
#' RprobitB:::RprobitB_latent_classes(list(C = 2))
#'
#' ### setting for weight-based on Dirichlet process-based updates
#' RprobitB:::RprobitB_latent_classes(
#'   list("weight_update" = TRUE, "dp_update" = TRUE)
#' )
#'
#' @keywords
#' internal

RprobitB_latent_classes <- function(latent_classes = NULL) {
  ### check if 'latent_classes' is 'NULL' or a list
  if (!is.null(latent_classes)) {
    if (!is.list(latent_classes)) {
      stop("'latent_classes' must be either a list or 'NULL'.",
           call. = FALSE
      )
    }
  } else {
    latent_classes <- list()
  }

  ### set default number of latent classes to 1
  if (is.null(latent_classes[["C"]])) {
    latent_classes[["C"]] <- 1
  }

  ### determine whether latent classes should be weight-based updated
  latent_classes[["weight_update"]] <-
    ifelse(!isTRUE(latent_classes[["weight_update"]]) &&
             !isFALSE(latent_classes[["weight_update"]]),
           FALSE, latent_classes[["weight_update"]]
    )

  ### determine whether latent classes should be DP-based updated
  latent_classes[["dp_update"]] <-
    ifelse(!isTRUE(latent_classes[["dp_update"]]) &&
             !isFALSE(latent_classes[["dp_update"]]),
           FALSE, latent_classes[["dp_update"]]
    )

  if (!latent_classes[["weight_update"]] && !latent_classes[["dp_update"]]) {
    ### remove other parameters in case of no updates
    latent_classes <- list(
      "C" = latent_classes[["C"]],
      "weight_update" = FALSE,
      "dp_update" = FALSE
    )
  } else {
    ### specify updating parameters
    if (is.null(latent_classes[["Cmax"]])) {
      latent_classes[["Cmax"]] <- max(10, latent_classes[["C"]])
    }

    ### set missing parameters to default values
    if (is.null(latent_classes[["buffer"]])) {
      latent_classes[["buffer"]] <- 100
    }
    if (is.null(latent_classes[["epsmin"]])) {
      latent_classes[["epsmin"]] <- 0.01
    }
    if (is.null(latent_classes[["epsmax"]])) {
      latent_classes[["epsmax"]] <- 0.99
    }
    if (is.null(latent_classes[["distmin"]])) {
      latent_classes[["distmin"]] <- 0.1
    }

    ### remove redundant parameters
    req_names <- c("C", "weight_update", "dp_update", "Cmax")
    if (latent_classes[["weight_update"]]) {
      req_names <- c(req_names, "buffer", "epsmin", "epsmax", "distmin")
    }
    latent_classes[!names(latent_classes) %in% req_names] <- NULL
  }

  ### check 'latent_classes'
  if (!is.numeric(latent_classes$C) || !latent_classes$C %% 1 == 0 ||
      !latent_classes$C > 0) {
    stop("'latent_classes$C' must be a positive integer.",
         call. = FALSE
    )
  }
  if (latent_classes[["weight_update"]] || latent_classes[["dp_update"]]) {
    if (!is.numeric(latent_classes$Cmax) || !latent_classes$Cmax %% 1 == 0 ||
        !latent_classes$Cmax > 0) {
      stop("'latent_classes$Cmax' must be a positive integer.",
           call. = FALSE
      )
    }
  }
  if (latent_classes[["weight_update"]]) {
    if (!is.numeric(latent_classes$buffer) ||
        !latent_classes$buffer %% 1 == 0 ||
        !latent_classes$buffer > 0) {
      stop("'latent_classes$buffer' must be a positive integer.",
           call. = FALSE
      )
    }
    if (!is.numeric(latent_classes$epsmin) || !latent_classes$epsmin <= 1 ||
        !latent_classes$epsmin >= 0) {
      stop("'latent_classes$epsmin' must be a numeric between 0 and 1.",
           call. = FALSE
      )
    }
    if (!is.numeric(latent_classes$epsmax) || !latent_classes$epsmax <= 1 ||
        !latent_classes$epsmax >= 0 ||
        !latent_classes$epsmin < latent_classes$epsmax) {
      stop(
        "'latent_classes$epsmax' must be a numeric between 0 and 1 and",
        "greater than 'latent_classes$epsmin'.",
        call. = FALSE
      )
    }
    if (!is.numeric(latent_classes$distmin) || !0 <= latent_classes$distmin) {
      stop("'latent_classes$distmin' must be a non-negative numeric value.",
           call. = FALSE
      )
    }
  }

  ### add boolean for class update
  if (latent_classes[["weight_update"]] || latent_classes[["dp_update"]]) {
    latent_classes[["class_update"]] <- TRUE
  } else {
    latent_classes[["class_update"]] <- FALSE
  }

  ### add class to 'latent_classes'
  class(latent_classes) <- "RprobitB_latent_classes"

  ### return latent_classes
  return(latent_classes)
}

#' @rdname RprobitB_latent_classes
#' @exportS3Method

print.RprobitB_latent_classes <- function(x, ...) {
  cat(crayon::underline("Latent classes\n"))
  if (!x[["class_update"]]) {
    cat("C =", x$C, "\n")
  } else {
    cat("DP-based update:", x[["dp_update"]], "\n")
    cat("Weight-based update:", x[["weight_update"]], "\n")
    cat("Initial classes:", x$C, "\n")
    cat("Maximum classes:", x$Cmax, "\n")
    if (x[["weight_update"]]) {
      cat("Updating buffer:", x$buffer, "\n")
      cat("Minimum class weight:", x$epsmin, "\n")
      cat("Maximum class weight:", x$epsmax, "\n")
      cat("Mimumum class distance:", x$distmin, "\n")
    }
  }
  return(invisible(x))
}

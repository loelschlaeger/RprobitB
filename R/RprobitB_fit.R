#' Create object of class \code{RprobitB_model}.
#' @description
#' This function creates an object of class \code{RprobitB_model}.
#' @inheritParams mcmc
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param classification
#' The allocation variable of the estimated latent classes.
#' @return
#' An object of class \code{RprobitB_model}, i.e. a list with the arguments of
#' this function as elements.
#' @keywords
#' s3

RprobitB_model <- function(data, normalization, R, B, Q, latent_classes, prior,
                           gibbs_samples, classification) {

  ### check inputs
  stopifnot(inherits(data, "RprobitB_data"))
  stopifnot(inherits(normalization, "RprobitB_normalization"))
  stopifnot(is.numeric(R), R %% 1 == 0, R > 0)
  stopifnot(is.numeric(B), B %% 1 == 0, B > 0)
  stopifnot(is.numeric(Q), Q %% 1 == 0, Q > 0)
  stopifnot(inherits(latent_classes, "RprobitB_latent_classes"))
  stopifnot(inherits(prior, "RprobitB_prior"))
  stopifnot(inherits(gibbs_samples, "RprobitB_gibbs_samples"))

  ### create and return object of class "RprobitB_model"
  out <- list(
    "data" = data,
    "normalization" = normalization,
    "R" = R,
    "B" = B,
    "Q" = Q,
    "latent_classes" = latent_classes,
    "prior" = prior,
    "gibbs_samples" = gibbs_samples,
    "classification" = classification
  )
  class(out) <- "RprobitB_model"
  return(out)
}

#' Print method for \code{RprobitB_model}.
#' @description
#' This function is the print method for an object of class \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param ...
#' Ignored.
#' @noRd

print.RprobitB_model <- function(x, ...) {
  cat("Probit model '", deparse1(x$data$form), "'.\n", sep = "")
  return(invisible(x))
}

#' Summary method for \code{RprobitB_model}.
#' @description
#' This function is the summary method for an object of class
#' \code{RprobitB_model}.
#' @param object
#' An object of class \code{RprobitB_model}.
#' @inheritParams RprobitB_gibbs_samples_statistics
#' @param ...
#' Ignorded.
#' @noRd

summary.RprobitB_model <- function(object, FUN = c(
                                     "mean" = mean, "sd" = sd,
                                     "R^" = R_hat
                                   ), ...) {

  ### check class of 'object'
  if (!inherits(object, "RprobitB_model")) {
    stop("Not of class 'RprobitB_model'.")
  }

  ### compute statistics from 'gibbs_samples'
  if (object$data$simulated) {
    C <- max(object$latent_classes$C, object$data$true_parameter$C)
  } else {
    C <- object$latent_classes$C
  }
  statistics <- RprobitB_gibbs_samples_statistics(
    gibbs_samples = filter_gibbs_samples(
      object$gibbs_samples,
      P_f = object$data$P_f, P_r = object$data$P_r,
      J = object$data$J, C = C, cov_sym = FALSE, drop_par = NULL
    ),
    FUN = FUN
  )

  ### build 'summary.RprobitB_model' object
  out <- list(
    "form" = object$data$form,
    "R" = object$R,
    "B" = object$B,
    "Q" = object$Q,
    "P_f" = object$data$P_f,
    "P_r" = object$data$P_r,
    "linear_coeffs" = object$data$linear_coeffs,
    "J" = object$data$J,
    "alternatives" = object$data$alternatives,
    "normalization" = object$normalization,
    "latent_classes" = object$latent_classes,
    "prior" = object$prior,
    "statistics" = statistics,
    "simulated" = object$data$simulated,
    "true_parameter" = object$data$true_parameter
  )
  class(out) <- "summary.RprobitB_model"

  ### return 'summary.RprobitB_model' object
  return(out)
}

#' Print method for the summary of \code{RprobitB_model}.
#' @description
#' This function is the print method for an object of class
#' \code{summary.RprobitB_model}.
#' @param x
#' An object of class \code{summary.RprobitB_model}.
#' @param digits
#' The number of printed decimal places.
#' @param ...
#' Ignored.
#' @noRd

print.summary.RprobitB_model <- function(x, digits = 2, ...) {
  cat("Probit model '", deparse1(x$form), "'.\n\n", sep = "")

  ### summary of model
  cat("MCMC settings:\n")
  cat("- R:", x$R, "\n")
  cat("- B:", x$B, "\n")
  cat("- Q:", x$Q, "\n")
  cat("\n")

  ### summary of normalization
  print(x$normalization)
  cat("\n")

  ### legend of alternatives
  cat("Legend of alternatives:\n")
  print(data.frame("name" = x$alternatives))
  cat("\n")

  ### legend of linear coefficients
  cat("Legend of linear coefficients:\n")
  print(x$linear_coeffs)
  cat("\n")

  ### legend of latent classes
  if (x$P_r > 0) {
    print(x$latent_classes)
    cat("\n")
  }

  ### overview of estimates
  print(x = x$statistics, true = x$true_parameter, digits = digits)

  ### return 'x' invisibly
  return(invisible(x))
}

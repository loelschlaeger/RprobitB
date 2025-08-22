#' Create object of class \code{RprobitB_fit}
#'
#' @description
#' This function creates an object of class \code{RprobitB_fit}.
#'
#' @inheritParams fit_model
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param class_sequence
#' The sequence of class numbers during Gibbs sampling of length \code{R}.
#' @param comp_time
#' The time spent for Gibbs sampling.
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @keywords internal

RprobitB_fit <- function(
    data, scale, level, normalization, R, B, Q, latent_classes, prior,
    gibbs_samples, class_sequence, comp_time
  ) {
  ### check inputs
  stopifnot(inherits(data, "RprobitB_data"))
  stopifnot(is.character("scale"))
  stopifnot(is.character("level"))
  stopifnot(inherits(normalization, "RprobitB_normalization"))
  stopifnot(is.numeric(R), R %% 1 == 0, R > 0)
  stopifnot(is.numeric(B), B %% 1 == 0, B > 0)
  stopifnot(is.numeric(Q), Q %% 1 == 0, Q > 0)
  stopifnot(inherits(latent_classes, "RprobitB_latent_classes"))
  stopifnot(is.list(prior))
  stopifnot(inherits(gibbs_samples, "RprobitB_gibbs_samples"))
  stopifnot(inherits(comp_time, "difftime"))

  ### create and return object of class "RprobitB_fit"
  out <- list(
    "data" = data,
    "scale" = scale,
    "level" = level,
    "normalization" = normalization,
    "R" = R,
    "B" = B,
    "Q" = Q,
    "latent_classes" = latent_classes,
    "prior" = prior,
    "gibbs_samples" = gibbs_samples,
    "class_sequence" = class_sequence,
    "comp_time" = comp_time
  )
  class(out) <- "RprobitB_fit"
  return(out)
}

#' @rdname RprobitB_fit
#' @export

print.RprobitB_fit <- function(x, ...) {
  cat("Probit model '", deparse1(x$data$form), "'.\n", sep = "")
  return(invisible(x))
}

#' @rdname RprobitB_fit
#' @exportS3Method

summary.RprobitB_fit <- function(
  object, FUN = c("mean" = mean, "sd" = stats::sd, "R^" = R_hat), ...
) {
  ### check class of 'object'
  if (!inherits(object, "RprobitB_fit")) {
    stop("Not of class 'RprobitB_fit'.",
         call. = FALSE
    )
  }

  ### compute statistics from 'gibbs_samples'
  statistics <- RprobitB_gibbs_samples_statistics(
    gibbs_samples = filter_gibbs_samples(
      x = object$gibbs_samples,
      P_f = object$data$P_f,
      P_r = object$data$P_r,
      J = object$data$J,
      C = ifelse(
        object$data$simulated,
        max(object$latent_classes$C, object$data$true_parameter$C),
        object$latent_classes$C
      ),
      ordered = object$data$ordered,
      cov_sym = FALSE,
      drop_par = NULL,
    ),
    FUN = FUN
  )

  ### build 'summary.RprobitB_fit' object
  out <- list(
    "form" = object$data$form,
    "R" = object$R,
    "B" = object$B,
    "Q" = object$Q,
    "P_f" = object$data$P_f,
    "P_r" = object$data$P_r,
    "linear_coefs" = object$data$linear_coefs,
    "J" = object$data$J,
    "alternatives" = object$data$alternatives,
    "normalization" = object$normalization,
    "latent_classes" = object$latent_classes,
    "prior" = object$prior,
    "statistics" = statistics,
    "simulated" = object$data$simulated,
    "true_parameter" = object$data$true_parameter
  )
  class(out) <- "summary.RprobitB_fit"

  ### return 'summary.RprobitB_fit' object
  return(out)
}

#' @rdname RprobitB_fit
#' @exportS3Method

print.summary.RprobitB_fit <- function(x, digits = 2, ...) {
  cat(crayon::underline("Probit model\n"))
  cat("Formula:", deparse1(x$form), "\n")
  cat(paste0("R: ", x$R, ", B: ", x$B, ", Q: ", x$Q, "\n"))
  print(x$normalization)
  cat("\n")
  if (x$P_r > 0) {
    print(x$latent_classes)
    cat("\n")
  }

  ### overview of estimates
  print(x = x$statistics, true = x$true_parameter, digits = digits)

  ### return 'x' invisibly
  return(invisible(x))
}

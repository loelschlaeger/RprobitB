#' Transform fitted probit model
#'
#' @description
#' Given an object of class \code{RprobitB_fit}, this function can:
#' \itemize{
#'   \item change the length \code{B} of the burn-in period,
#'   \item change the the thinning factor \code{Q} of the Gibbs samples,
#'   \item change the utility \code{scale}.
#' }
#'
#' @details
#' See the vignette "Model fitting" for more details:
#' \code{vignette("v03_model_fitting", package = "RprobitB")}.
#'
#' @param _data
#' An object of class \code{\link{RprobitB_fit}}.
#' @inheritParams fit_model
#' @param check_preference_flip
#' Set to \code{TRUE} to check for flip in preferences after new \code{scale}.
#' @param ...
#' Ignored.
#'
#' @return
#' The transformed \code{RprobitB_fit} object.
#'
#' @export
#'
#' @rdname transform

transform.RprobitB_fit <- function(`_data`, B = NULL, Q = NULL, scale = NULL,
                                   check_preference_flip = TRUE, ...) {
  ### check inputs
  x <- `_data`
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit'.",
         call. = FALSE
    )
  }
  if (is.null(B)) {
    B <- x[["B"]]
  } else {
    x[["B"]] <- B
  }
  if (is.null(Q)) {
    Q <- x[["Q"]]
  } else {
    x[["Q"]] <- Q
  }
  R <- x[["R"]]
  P_f <- x[["data"]][["P_f"]]
  J <- x[["data"]][["J"]]
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.",
         call. = FALSE
    )
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.",
         call. = FALSE
    )
  }
  if (is.null(scale)) {
    normalization <- x[["normalization"]]
  } else {
    ### check if new scale flips preferences
    if (check_preference_flip) {
      preference_flip(
        model_old = x,
        model_new = transform.RprobitB_fit(
          x,
          scale = scale, check_preference_flip = FALSE
        )
      )
    }
    normalization <- RprobitB_normalization(
      level = x$level,
      scale = scale,
      form = x$data$form,
      re = x$data$re,
      alternatives = x$data$alternatives,
      base = x$data$base
    )
    x[["normalization"]] <- normalization
  }

  ### scale, burn and thin Gibbs samples
  x[["gibbs_samples"]] <- transform_gibbs_samples(
    x[["gibbs_samples"]][["gibbs_samples_raw"]],
    R = R,
    B = B,
    Q = Q,
    normalization = normalization
  )

  ### scale true model parameters
  if (x[["data"]][["simulated"]]) {
    x[["data"]][["true_parameter"]] <- transform_parameter(
      parameter = x[["data"]][["true_parameter"]],
      normalization = normalization
    )
  }

  ### re-calculate likelihood
  x[["ll"]] <- logLik(x, recompute = TRUE)

  ### return 'RprobitB_fit'
  return(x)
}

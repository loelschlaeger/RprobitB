#' Compare fitted models.
#'
#' @description
#' This function guides through the process of model selection by providing
#' criteria for model comparison.
#'
#' @details
#' See the vignette on model selection for more details about the criteria.
#'
#' @param ...
#' One or more objects of class \code{RprobitB_fit}.
#' @param criteria
#' A vector of one or more of the following characters:
#' \itemize{
#'   \item \code{"AIC"} for the AIC value,
#'   \item \code{"BIC"} for the BIC value,
#'   \item \code{"WAIC"} for the WAIC value,
#'   \item \code{"npar"} for the number of model parameters.
#'   \item \code{"LL"} for the log-likelihood value,
#'   \item \code{"BF"} for the Bayes factor,
#'   \item \code{"PA"} for the prediction accuracy (explaining the observed choices),
#' }
#' @param sort_by
#' Either \code{NULL} or one element of \code{criteria}, after which the output is sorted.
#' @param S,print_progress,ncores
#' Passed on to \code{\link{waic}} and \code{\link{mml}}.
#'
#' @return
#' A data frame, models in rows, criteria in columns.
#'
#' @export

model_selection <- function(..., criteria = c("AIC", "BIC", "WAIC", "npar", "LL", "BF", "PA"),
                            sort_by = NULL, S = 100, print_progress = TRUE, ncores = 1) {

  ### check inputs
  if (!(length(print_progress) == 1 && class(print_progress) == "logical")) {
    stop("'print_progress' must be a boolean.")
  }
  if (!(is.numeric(ncores) && length(ncores) == 1 && ncores > 0 && ncores %% 1 == 0)) {
    stop("'ncores' must be a positive integer.")
  }
  if (!is.character(criteria)) {
    stop("'criteria' must be a character vector.")
  }
  if (!is.null(sort_by)) {
    if (!(is.character(sort_by) && length(sort_by) == 1 && sort_by %in% criteria)) {
      stop("'sort_by' must be one element of 'criteria'.")
    }
  }

  ### read models
  models <- as.list(list(...))
  model_names <- unlist(lapply(sys.call()[-1], as.character))[1:length(models)]

  ### check if models are of class "RprobitB_fit"
  for (i in seq_len(length(models))) {
    if (!inherits(models[[i]], "RprobitB_fit")) {
      stop(paste0("Input '", model_names[i], "' is not of class 'RprobitB_fit'."))
    }
  }

  ### create output matrix
  output <- matrix(NA, nrow = length(models), ncol = 0)
  rownames(output) <- model_names

  ### pre-compute 'npar', number of observations, and 'LL' for each model
  npar <- sapply(models, function(mod) {
    mod$data$P_f + (mod$data$P_r + mod$data$P_r^2) * mod$latent_classes$C +
      mod$data$J * (mod$data$J - 1) / 2 - 1
  })
  nobs <- sapply(models, function(mod) sum(mod$data$T))
  ll <- sapply(models, function(mod) log_likelihood(mod))

  ### fill output
  for (crit in unique(criteria)) {
    if (crit == "AIC") {
      output <- cbind(output, "AIC" = mapply(function(ll, npar) -2 * ll + 2 * npar, ll, npar))
    }
    if (crit == "BIC") {
      output <- cbind(output, "BIC" = mapply(function(ll, npar, nobs) -2 * ll + npar * log(nobs), ll, npar, nobs))
    }
    if (crit == "WAIC") {
      waic_out <- lapply(models, waic, S = S, print_progress = print_progress, check_conv = FALSE, ncores = ncores)
      output <- cbind(output, "WAIC" = sapply(waic_out, function(x) x[["waic"]]))
      output <- cbind(output, "se(WAIC)" = sapply(waic_out, function(x) x[["se_waic"]]))
      output <- cbind(output, "pWAIC" = sapply(waic_out, function(x) x[["p_waic"]]))
    }
    if (crit == "npar") {
      output <- cbind(output, "npar" = npar)
    }
    if (crit == "LL") {
      output <- cbind(output, "LL" = ll)
    }
    if (crit == "BF") {
      mml_out <- sapply(models, mml, S = S, method = "pame", print_progress = print_progress, check_conv = FALSE, ncores = ncores, seq = FALSE)
      output <- cbind(output, "MML" = mml_out)
      for (nmod in seq_len(length(models))) {
        colnames_old <- colnames(output)
        output <- cbind(output, exp(log(mml_out) - log(mml_out[nmod])))
        colnames(output) <- c(colnames_old, paste0("BF(", model_names[nmod], ")"))
      }
    }
    if (crit == "PA") {
      pa <- function(x) sum(diag(x)) / sum(x)
      output <- cbind(output, "PA" = sapply(models, function(mod) pa(predict.RprobitB_fit(mod, data = NULL, overview = TRUE))))
    }
  }

  ### transform output to data frame
  output <- as.data.frame(output)

  ### sort output rows based on 'sort_by'
  if (!is.null(sort_by)) {
    mod_order <- order(output[, sort_by], decreasing = ifelse(sort_by %in% c("AIC", "BIC", "WAIC"), FALSE, TRUE))
    output <- output[mod_order, ]
  }

  return(output)
}

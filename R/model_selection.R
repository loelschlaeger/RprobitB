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
#'   \item \code{"WAIC"} for the WAIC value,
#'   \item \code{"npar"} for the number of model parameters.
#'   \item \code{"LL"} for the log-likelihood value,
#'   \item \code{"AIC"} for the AIC value,
#'   \item \code{"BIC"} for the BIC value,
#'   \item \code{"BF"} for the Bayes factor,
#'   \item \code{"PA"} for the prediction accuracy (explaining the observed choices),
#' }
#' @param S,print_progress,ncores
#' Passed on to \code{\link{waic}} and \code{\link{mml}}.
#'
#' @return
#' A data frame, models in rows, criteria in columns.
#'
#' @export

model_selection <- function(..., criteria = c("WAIC", "npar", "LL", "AIC", "BIC", "BF", "PA"),
                            S = 100, print_progress = TRUE, ncores = 1) {

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
  output <- cbind(output, "form" = sapply(models, function(x) deparse1(x$data$form)))

  ### pre-compute 'npar', number of observations, and 'LL' for each model
  npar <- sapply(models, function(mod) {
    mod$data$P_f + (mod$data$P_r + mod$data$P_r^2) * mod$latent_classes$C +
      mod$data$J * (mod$data$J - 1) / 2 - 1
  })
  nobs <- sapply(models, function(mod) sum(mod$data$T))
  ll <- sapply(models, function(mod) logLik(mod))

  ### fill output
  for (crit in unique(criteria)) {
    if (crit == "WAIC") {
      waic_out <- lapply(models, waic,
        S = S, print_progress = print_progress,
        check_conv = FALSE, ncores = ncores
      )
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
    if (crit == "AIC") {
      output <- cbind(output, "AIC" = mapply(function(ll, npar) -2 * ll + 2 * npar, ll, npar))
    }
    if (crit == "BIC") {
      output <- cbind(output, "BIC" = mapply(function(ll, npar, nobs) -2 * ll + npar * log(nobs), ll, npar, nobs))
    }
    if (crit == "BF") {
      mml_out <- sapply(models, mml, S = S, method = "pame", print_progress = print_progress, check_conv = FALSE, ncores = ncores, seq = FALSE)
      output <- cbind(output, "MML" = mml_out)
      for (nmod in seq_len(length(models))) {
        colnames_old <- colnames(output)
        output <- cbind(output, exp(log(mml_out) - log(mml_out[nmod])))
        colnames(output) <- c(colnames_old, paste0("BF:", model_names[nmod]))
      }
    }
    if (crit == "PA") {
      pa <- function(x) sum(diag(x)) / sum(x)
      output <- cbind(output, "PA" = sapply(models, function(mod) pa(predict.RprobitB_fit(mod, data = NULL, overview = TRUE))))
    }
  }

  ### transform output to data frame
  output <- as.data.frame(output)

  class(output) <- c("RprobitB_model_selection", "data.frame")
  return(output)
}

#' @noRd
#' @export

print.RprobitB_model_selection <- function(x, digits = 2, ...) {
  for (col in colnames(x)) {
    if (col == "form") {
      x[, "form"] <- sprintf(paste0("%-", max(nchar(x[, "form"])), "s"), x[, "form"])
    }
    if (col == "WAIC") {
      x[, "WAIC"] <- sprintf(paste0("%.", digits, "f"), as.numeric(x[, "WAIC"]))
    }
    if (col == "se(WAIC)") {
      x[, "se(WAIC)"] <- sprintf(paste0("%.", digits, "f"), as.numeric(x[, "se(WAIC)"]))
    }
    if (col == "pWAIC") {
      x[, "pWAIC"] <- sprintf(paste0("%.", digits, "f"), as.numeric(x[, "pWAIC"]))
    }
    if (col == "LL") {
      x[, "LL"] <- sprintf(paste0("%.", digits, "f"), as.numeric(x[, "LL"]))
    }
    if (col == "AIC") {
      x[, "AIC"] <- sprintf(paste0("%.", digits, "f"), as.numeric(x[, "AIC"]))
    }
    if (col == "BIC") {
      x[, "BIC"] <- sprintf(paste0("%.", digits, "f"), as.numeric(x[, "BIC"]))
    }
    if (col == "MML") {
      x[, "MML"] <- sprintf(paste0("%.", digits, "e"), as.numeric(x[, "MML"]))
    }
    if (startsWith(col, "BF:")) {
      x[, col] <- as.numeric(sprintf(paste0("%.", digits, "f"), as.numeric(x[, col])))
      for (row in 1:nrow(x)) {
        if (as.numeric(x[row, col]) < 1 / 100) {
          x[row, col] <- "< 0.01"
        } else if (as.numeric(x[row, col]) > 100) {
          x[row, col] <- "> 100"
        }
      }
    }
    if (col == "PA") {
      x[, "PA"] <- sprintf(paste0("%.", digits, "f%%"), as.numeric(x[, "PA"]) * 100)
    }
  }
  colnames(x)[which(colnames(x) == "form")] <- ""
  class(x) <- "data.frame"
  print(x)
}

#' Akaike's Information Criterion
#'
#' @description
#' This function calculates Akaike's Information Criterion (AIC) for an
#' \code{RprobitB_fit} object.
#'
#' @details
#' The AIC is computed as
#' \deqn{-2 \cdot \text{LL} + k \cdot \text{npar},}
#' where \eqn{\text{LL}} is the model's log-likelihood value at the estimated
#' parameters, \eqn{k} is the penalty per parameter (\eqn{k = 2} for the
#' classical AIC), and \eqn{npar} is the number of parameters in the fitted model.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Optionally more objects of class \code{RprobitB_fit}.
#'
#' @param k
#' A numeric, the penalty per parameter. The default is \code{k = 2} for the
#' classical AIC.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @examples
#' data("model_train", package = "RprobitB")
#' AIC(model_train)
#'
#' @export

AIC <- function(object, ..., k) {
  UseMethod("AIC")
}

#' @export

AIC.RprobitB_fit <- function(object, ..., k = 2) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik)
  npar <- sapply(models, npar)
  aic <- mapply(function(ll, npar) -2 * ll + 2 * npar, ll, npar)
  return(aic)
}

#' Bayesian Information Criterion
#'
#' @description
#' This function calculates the Bayesian Information Criterion (BIC) or
#' Schwarz Information Criterion for an \code{RprobitB_fit} object.
#'
#' @details
#' The BIC is computed as
#' \deqn{-2 \cdot \text{LL} + \text{npar} \cdot \ln{\text{nobs}},}
#' where \eqn{\text{LL}} is the model's log-likelihood value at the estimated
#' parameters, \eqn{npar} is the number of parameters in the fitted model,
#' and \eqn{\text{nobs}} is the number of data points.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Optionally more objects of class \code{RprobitB_fit}.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @examples
#' data("model_train", package = "RprobitB")
#' BIC(model_train)
#'
#' @export

BIC <- function(object, ...) {
  UseMethod("BIC")
}

#' @export

BIC.RprobitB_fit <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik)
  npar <- sapply(models, npar)
  nobs <- sapply(models, nobs)
  bic <- mapply(function(ll, npar, nobs) -2 * ll + npar * log(nobs), ll, npar, nobs)
  return(bic)
}

#' Number of observations
#'
#' @description
#' This function extracts the number of observations from an \code{RprobitB_fit}
#' object.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Ignored.
#'
#' @return
#' An integer.
#'
#' @examples
#' data("model_train", package = "RprobitB")
#' nobs(model_train)
#'
#' @export

nobs <- function(object, ...) {
  UseMethod("nobs")
}

#' @export

nobs.RprobitB_fit <- function(object, ...) {
  return(sum(object$data$T))
}

#' Log-likelihood value
#'
#' @description
#' This function computes the log-likelihood value of an \code{RprobitB_fit}
#' object.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Ignored.
#'
#' @return
#' A numeric.
#'
#' @examples
#' data("model_train", package = "RprobitB")
#' logLik(model_train)
#'
#' @export

logLik <- function(object, ...) {
  UseMethod("logLik")
}

#' @export

logLik.RprobitB_fit <- function(object, ...) {
  if(!is.null(object[["ll"]])){
    ll <- object[["ll"]]
  } else {
    probs <- choice_probabilities(x = object, par_set = mean)
    choices <- as.character(unlist(sapply(object$data$data, `[[`, "y")))
    ll <- 0
    for (row in 1:nrow(probs)){
      ll <- ll + log(probs[row, choices[row]])
    }
  }
  return(as.numeric(ll))
}

#' Number of model parameters
#'
#' @description
#' This function extracts the number of model parameters of an \code{RprobitB_fit}
#' object.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Optionally more objects of class \code{RprobitB_fit}.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @examples
#' data("model_train", package = "RprobitB")
#' npar(model_train)
#'
#' @export

npar <- function(object, ...) {
  UseMethod("npar")
}

#' @export

npar.RprobitB_fit <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  npar <- sapply(models, function(mod) {
    mod$data$P_f + (mod$data$P_r + mod$data$P_r^2) * mod$latent_classes$C +
      mod$data$J * (mod$data$J - 1) / 2 - 1
  })
  return(npar)
}

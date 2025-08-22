#' Compare fitted models
#'
#' @description
#' This function returns a table with several criteria for model comparison.
#'
#' @details
#' See the vignette on model selection for more details.
#'
#' @param ...
#' One or more objects of class \code{RprobitB_fit}.
#'
#' @param criteria \[`character()`\]\cr
#' One or more of the following:
#' \itemize{
#'   \item \code{"npar"} for the number of model parameters (see \code{\link{npar}}),
#'   \item \code{"LL"} for the log-likelihood value (see \code{\link{logLik}}),
#'   \item \code{"AIC"} for the AIC value (see \code{\link{AIC}}),
#'   \item \code{"BIC"} for the BIC value (see \code{\link{BIC}}),
#'   \item \code{"WAIC"} for the WAIC value (also shows its standard error
#'         `sd(WAIC)` and the number `pWAIC` of effective model parameters,
#'         see \code{\link{WAIC}}),
#'   \item \code{"MMLL"} for the marginal model log-likelihood,
#'   \item \code{"BF"} for the Bayes factor,
#'   \item \code{"pred_acc"} for the prediction accuracy (see \code{\link{pred_acc}}).
#' }
#'
#' @param add_form \[`logical(1)`\]\cr
#' Add the model formulas?
#'
#' @return
#' A `data.frame`, criteria in columns, models in rows.
#'
#' @export

model_selection <- function(
    ..., criteria = c("npar", "LL", "AIC", "BIC"), add_form = FALSE
  ) {

  ### check inputs
  models <- as.list(list(...))
  model_names <- unlist(lapply(sys.call()[-1], as.character))[1:length(models)]
  for (i in seq_len(length(models))) {
    if (!inherits(models[[i]], "RprobitB_fit")) {
      stop(
        paste0(
          "Input '", model_names[i],
          "' is not of class 'RprobitB_fit'."
        ),
        call. = FALSE
      )
    }
  }
  if (!is.character(criteria)) {
    stop("'criteria' must be a character vector.",
         call. = FALSE
    )
  }

  ### create output matrix
  output <- matrix(NA_real_, nrow = 0, ncol = length(models))
  colnames(output) <- model_names
  if (add_form) {
    output <- rbind(output,
                    "form" = sapply(models, function(x) deparse1(x$data$form))
    )
  }

  ### fill output
  for (crit in unique(criteria)) {
    if (crit == "npar") {
      output <- rbind(output, "npar" = sapply(models, npar))
    }
    if (crit == "LL") {
      output <- rbind(output, "LL" = sapply(models, logLik))
    }
    if (crit == "AIC") {
      output <- rbind(output, "AIC" = sapply(models, AIC))
    }
    if (crit == "BIC") {
      output <- rbind(output, "BIC" = sapply(models, BIC))
    }
    if (crit == "WAIC") {
      waic_out <- lapply(models, WAIC)
      output <- rbind(output, "WAIC" = sapply(waic_out, function(x) x))
      output <- rbind(output, "se(WAIC)" = sapply(waic_out, function(x) attr(x, "se_waic")))
      output <- rbind(output, "pWAIC" = sapply(waic_out, function(x) attr(x, "p_waic")))
    }
    if (crit == "MMLL") {
      models <- lapply(models, mml)
      output <- rbind(output, "MMLL" = sapply(models, function(x) attr(x[["mml"]], "mmll")))
    }
    if (crit == "BF" && length(models) >= 2) {
      models <- lapply(models, mml)
      mmll_out <- sapply(models, function(x) attr(x[["mml"]], "mmll"))
      for (nmod in seq_len(length(models))) {
        rownames_old <- rownames(output)
        output <- rbind(output, exp(mmll_out - mmll_out[nmod]))
        rownames(output) <- c(rownames_old, paste0("BF(*,", model_names[nmod], ")"))
      }
    }
    if (crit == "pred_acc") {
      output <- rbind(output, "pred_acc" = sapply(models, pred_acc))
    }
  }

  ### transform output to data frame
  output <- as.data.frame(output)

  class(output) <- c("RprobitB_model_selection", "data.frame")
  return(output)
}

#' @rdname model_selection
#' @param x
#' An object of class `RprobitB_model_selection`.
#' @param digits \[`integer(1)`\]\cr
#' The number of digits.
#' @export

print.RprobitB_model_selection <- function(x, digits = 2, ...) {
  for (row in rownames(x)) {
    if (row == "form") {
      x["form", ] <- sprintf("%s", x["form", ])
    }
    if (row == "LL") {
      x["LL", ] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["LL", ]))
    }
    if (row == "AIC") {
      x["AIC", ] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["AIC", ]))
    }
    if (row == "BIC") {
      x["BIC", ] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["BIC", ]))
    }
    if (row == "WAIC") {
      x["WAIC", ] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["WAIC", ]))
    }
    if (row == "se(WAIC)") {
      x["se(WAIC)", ] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["se(WAIC)", ]))
    }
    if (row == "pWAIC") {
      x["pWAIC", ] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["pWAIC", ]))
    }
    if (row == "MMLL") {
      x["MMLL", ] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["MMLL", ]))
    }
    if (startsWith(row, "BF(")) {
      x[row, ] <- as.numeric(sprintf(paste0("%.", digits, "f"), as.numeric(x[row, ])))
      for (col in 1:ncol(x)) {
        if (is.na(x[row, col])) {
          x[row, col] <- "NA"
        } else if (as.numeric(x[row, col]) < 1 / 100) {
          x[row, col] <- "< 0.01"
        } else if (as.numeric(x[row, col]) > 100) {
          x[row, col] <- "> 100"
        }
      }
    }
    if (row == "pred_acc") {
      x["pred_acc", ] <- sprintf(paste0("%.", digits, "f%%"), as.numeric(x["pred_acc", ]) * 100)
    }
  }
  class(x) <- "data.frame"
  print(x)
}

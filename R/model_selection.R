#' Compare fitted models.
#' @description
#' This function computes model comparison criteria.
#' @param ...
#' One or more objects of class \code{RprobitB_model}.
#' @return
#' A matrix with each model's
#' \itemize{
#'   \item number of parameters,
#'   \item log-likelihood,
#'   \item AIC, and
#'   \item BIC value.
#' }
#' @export

model_selection <- function(...) {

  ### read models
  models <- as.list(list(...))

  ### get model names
  model_names <- unlist(lapply(sys.call()[-1], as.character))

  ### check if models are of class "RprobitB_model"
  for (i in seq_len(length(models))) {
    if (!inherits(models[[i]], "RprobitB_model")) {
      stop(paste0(
        "Input '", model_names[i],
        "' is not of class 'RprobitB_model'."
      ))
    }
  }

  ### check if data is the same for each model
  for (i in seq_len(length(models))) {
    data_i <- as.numeric(unlist(models[[i]]$RprobitB_data$data))
    for (j in 1:i) {
      data_j <- as.numeric(unlist(models[[j]]$RprobitB_data$data))
      if (!identical(data_i, data_j)) {
        warning(paste0(
          "Models '", model_names[i], "' and '", model_names[j],
          "' are not estimated on the same data."
        ))
      }
    }
  }

  ### create output matrix
  criteria <- c("parameters", "log-likelihood", "AIC", "BIC")
  output <- matrix(NA, nrow = length(models), ncol = length(criteria))
  rownames(output) <- model_names
  colnames(output) <- criteria

  ### fill output
  for (i in seq_len(length(models))) {
    par_i <- models[[i]]$data$P_f +
      models[[i]]$data$P_r +
      models[[i]]$data$P_r^2 +
      models[[i]]$data$J * (models[[i]]$data$J - 1) / 2 - 1
    n_i <- sum(models[[i]]$data$T)
    ll_i <- compute_log_likelihood(models[[i]])
    aic_i <- -2 * ll_i + 2 * par_i
    bic_i <- -2 * ll_i + par_i * log(n_i)
    output[i, "parameters"] <- par_i
    output[i, "log-likelihood"] <- ll_i
    output[i, "AIC"] <- aic_i
    output[i, "BIC"] <- bic_i
  }

  return(output)
}

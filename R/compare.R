#' Compare fitted models.
#' @description
#' This function computes model selection criteria.
#' @details
#' For more details see the vignette "Model comparison":
#' \code{vignette("model_comparison", package = "RprobitB")}.
#' @param ...
#' One or more objects of class \code{RprobitB_model}.
#' @return
#' A matrix with the following model selection criteria:
#' \itemize{
#'   \item log-likelihood
#'   \item AIC
#'   \item BIC
#'   \item WAIC
#'   \item effective model parameters
#' }
#' @examples
#' @export

compare = function(...){

  ### read models
  models = as.list(list(...))

  ### get model names
  model_names = unlist(lapply(sys.call()[-1], as.character))

  ### check if models are of class "RprobitB_model"
  for(i in seq_len(length(models)))
    if(!inherits(models[[i]],"RprobitB_model"))
      stop(paste0("Model '",model_names[i],
                  "' is not of class 'RprobitB_model'."))

  ### check if data is the same for each model
  for(i in seq_len(length(models))){
    data_i = as.numeric(unlist(models[[i]]$RprobitB_data$data))
    for(j in 1:i){
      data_j = as.numeric(unlist(models[[j]]$RprobitB_data$data))
      if(!identical(data_i,data_j))
        warning(paste0("Models '",model_names[i],"' and '",model_names[j],
                       "' are not estimated on the same data."))
    }
  }

  ### create output matrix
  criteria = c("parameters","log-likelihood","AIC","BIC","WAIC")
  output = matrix(NA, nrow = length(models), ncol = length(criteria))
  rownames(output) = model_names
  colnames(output) = criteria

  ### fill output
  for(i in seq_len(length(models))){
    par_i = compute_number_parameters(models[[i]])
    n_i = sum(models[[i]]$RprobitB_data$T)
    ll_i = compute_loglikelihood(models[[i]])
    aic_i = -2*ll_i + 2*par_i
    bic_i = -2*ll_i + p*log(n_i)
    waic_i = compute_waic(models[[i]])
    output[i,"parameters"] = par_i
    output[i,"log-likelihood"] = ll_i
    output[i,"AIC"] = aic_i
    output[i,"BIC"] = bic_i
    output[i,"WAIC"] = waic_i
  }

  return(output)
}

#' Predict choices.
#' @description
#' This function predicts the choices of decision makers based on a fitted
#' \code{RprobitB_model} and choice characteristics.
#' @details
#' For more details see the vignette "Prediction":
#' \code{vignette("prediction", package = "RprobitB")}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param aggregate
#' If \code{aggregate = TRUE}, the function returns the actual and predicted
#' choices in a frequency table.
#' @return
#' value

predict = function(x, aggregate = TRUE) {
  choice_probs = as.data.frame(choice_probabilities(x))
  predictions = cbind(
    choice_probs, true = x$data$choice_data$choice,
    predicted = apply(choice_probs[x$data$alternatives], 1, which.max))
  if(aggregate){
    table(predictions$true, predictions$predicted, dnn = c("true","predicted"))
  } else {
    return(predictions)
  }
}

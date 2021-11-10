#' Predict choices.
#' @description
#' This function predicts the choices of decision makers.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param overview
#' If \code{TRUE}, aggregate the actual and predicted choices in a table.
#' @return
#' Either a table if \code{overview = TRUE} or a data frame otherwise.
#' @export

predict = function(x, overview = TRUE) {
  choice_probs = as.data.frame(choice_probs(x))
  true_choices = x$data$choice_data$choice
  true_choices = factor(true_choices, labels = x$data$alternatives)
  prediction = apply(choice_probs[x$data$alternatives], 1, which.max)
  prediction = factor(prediction, labels = x$data$alternatives)
  if(overview){
    table(true_choices, prediction, dnn = c("true","predicted"))
  } else {
    cbind(choice_probs, "true" = true_choices, "predicted" = prediction,
          "correct" = (true_choices == prediction))
  }
}

#' Predict choices.
#' @description
#' This function predicts the choices of decision makers.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param data
#' Either \code{NULL} or an object of class \code{RprobitB_data}.
#' @param overview
#' If \code{TRUE}, aggregate the prediction in a table.
#' @return
#' Either a table if \code{overview = TRUE} or a data frame otherwise.
#' @export

predict <- function(x, data = NULL, overview = TRUE) {

  ### choose data
  if (is.null(data)) {
    data <- x$data
  }
  if (class(data) != "RprobitB_data") {
    stop("'data' is not of class 'RprobitB_data'.")
  }

  ### compute choice probabilites
  choice_probs <- as.data.frame(choice_probs(x, data = data))

  ### check if true choices are available
  if (data$choice_available) {
    true_choices <- data$choice_data[["choice"]]
    true_choices <- factor(true_choices, labels = data$alternatives)
  }

  ### predict
  prediction <- factor(apply(choice_probs[data$alternatives], 1, which.max),
    labels = data$alternatives
  )

  ### create and return output
  if (overview) {
    if (data$choice_available) {
      out <- table(true_choices, prediction, dnn = c("true", "predicted"))
    } else {
      out <- table(prediction, dnn = c("prediction"))
    }
  } else {
    if (data$choice_available) {
      out <- cbind(choice_probs,
        "true" = true_choices, "predicted" = prediction,
        "correct" = (true_choices == prediction)
      )
    } else {
      out <- cbind(choice_probs, "prediction" = prediction)
    }
  }
  return(out)
}

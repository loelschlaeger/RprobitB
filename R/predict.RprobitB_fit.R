#' Choice prediction
#'
#' @description
#' This function predicts the choices of decision makers.
#'
#' @details
#' Predictions are made based on the maximum predicted probability for each
#' choice alternative.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#' @param data
#' Either \code{NULL} or an object of class \code{RprobitB_data}.
#' @param overview
#' If \code{TRUE}, aggregate the prediction in a table.
#' @param ...
#' Ignored.
#'
#' @return
#' Either a table if \code{overview = TRUE} or a data frame otherwise.
#'
#' @examples
#' data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#' object <- mcmc(data)
#' predict(object, overview = TRUE)
#' predict(object, overview = FALSE)
#'
#' @export

predict.RprobitB_fit <- function(object, data = NULL, overview = TRUE, ...) {

  ### choose data
  if (is.null(data)) {
    data <- object$data
  }
  if (class(data) != "RprobitB_data") {
    stop("'data' is not of class 'RprobitB_data'.")
  }

  ### compute choice probabilites
  choice_probs <- as.data.frame(choice_probabilities(object, data = data))

  ### check if true choices are available
  if (data$choice_available) {
    true_choices <- data$choice_data[[data$res_var_names$choice]]
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

#' @exportS3Method

logLik.RprobitB_fit <- function(object, par_set = mean, recompute = FALSE, ...) {
  if (!is.null(object[["ll"]]) && !recompute) {
    ll <- object[["ll"]]
  } else {
    probs <- choice_probabilities(x = object, par_set = par_set)
    choices <- as.character(unlist(sapply(object$data$data, `[[`, "y")))
    ll <- 0
    for (row in 1:nrow(probs)) {
      if (object$data$ranked) {
        y_seq <- strsplit(choices[row], ",")[[1]][1]
        ll <- ll + log(probs[row, y_seq])
      } else {
        ll <- ll + log(probs[row, choices[row]])
      }
    }
  }
  structure(
    as.numeric(ll),
    class = "logLik",
    df = npar(object),
    nobs = nobs(object)
  )
}

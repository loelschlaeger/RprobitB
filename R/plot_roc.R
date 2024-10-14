#' Plot ROC curve
#'
#' @description
#' This function draws receiver operating characteristic (ROC) curves.
#'
#' @param ...
#' One or more \code{RprobitB_fit} objects or \code{data.frame}s of choice
#' probability.
#' @param reference
#' The reference alternative.
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export

plot_roc <- function(..., reference = NULL) {
  D <- name <- NULL
  models <- as.list(list(...))
  model_names <- unlist(lapply(sys.call()[-1], as.character))[1:length(models)]
  pred_merge <- NULL
  for (m in 1:length(models)) {
    if (inherits(models[[m]], "RprobitB_fit")) {
      if (is.null(reference)) {
        reference <- models[[m]]$data$alternatives[1]
      }
      pred <- predict.RprobitB_fit(models[[m]], overview = FALSE, digits = 8)
      true <- ifelse(pred$true == reference, 1, 0)
      if (is.null(pred_merge)) {
        pred_merge <- data.frame(true)
        colnames(pred_merge) <- paste0("d_", model_names[m])
      } else {
        pred_merge[, paste0("d_", model_names[m])] <- true
      }
      pred_merge[, model_names[m]] <- pred[reference]
    } else {
      if (is.null(reference)) {
        reference <- colnames(models[[m]])[1]
      }
      if (is.null(pred_merge)) {
        stop("Not implemented yet.", call. = FALSE)
      } else {
        pred_merge[, model_names[m]] <- models[[m]][, reference]
      }
    }
  }
  if (length(models) > 1) {
    pred_merge <- plotROC::melt_roc(
      data = pred_merge, d = paste0("d_", model_names[1]), m = model_names
    )
  } else {
    colnames(pred_merge) <- c("D", "M")
  }
  if (length(models) > 1) {
    plot <- ggplot2::ggplot(
      data = pred_merge,
      ggplot2::aes(
        m = rlang::.data$M, d = rlang::.data$D, color = rlang::.data$name
      )
    )
  } else {
    plot <- ggplot2::ggplot(
      data = pred_merge,
      ggplot2::aes(m = rlang::.data$M, d = rlang::.data$D)
    )
  }
  plot <- plot + plotROC::geom_roc(n.cuts = 20, labels = FALSE) +
    plotROC::style_roc(theme = ggplot2::theme_grey) +
    theme(legend.position = "top") +
    theme(legend.title = ggplot2::element_blank())
  print(plot)
  return(plot)
}

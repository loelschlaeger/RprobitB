#' @noRd
#' @export

coef <- function (object, ...) {
  UseMethod("coef", object)
}

#' @noRd
#' @export
#' @importFrom stats sd

coef.RprobitB_fit <- function(object, ...) {
  C <- ifelse(object$data$simulated,
              max(object$latent_classes$C, object$data$true_parameter$C),
              object$latent_classes$C)
  statistics <- RprobitB_gibbs_samples_statistics(
    gibbs_samples = filter_gibbs_samples(
      x = object$gibbs_samples,
      P_f = object$data$P_f,
      P_r = object$data$P_r,
      J = object$data$J,
      C = C,
      cov_sym = FALSE,
    ),
    FUN = c("mean" = mean, "sd" = stats::sd)
  )
  coef <- rbind(statistics$alpha, statistics$b)
  coef_name_alpha <- c()
  coef_name_b <- c()
  coef_class <- c()
  for(row in 1:nrow(object$data$linear_coefs)){
    name <- object$data$linear_coefs[row,"name"]
    if(object$data$linear_coefs[row,"re"]) {
      coef_name_b <- c(coef_name_b, rep(name, C))
      coef_class <- c(coef_class, 1:C)
    } else {
      coef_name_alpha <- c(coef_name_alpha, name)
      coef_class <- c(coef_class, NA)
    }
  }
  rownames(coef) <- c(coef_name_alpha, coef_name_b)
  attr(coef, "coef_class") <- coef_class
  class(coef) <- "RprobitB_coef"
  return(coef)
}

#' @noRd
#' @export

print.RprobitB_coef <- function(x, ...) {
  classes <- sapply(attr(x, "coef_class"),
                    function(cl) ifelse(is.na(cl), "", paste0("[",cl,"]")))
  out <- data.frame("covariate" = sprintf("%s %s", rownames(x), classes),
                    "mean" = sprintf("%.2f", x[,"mean"]),
                    "sd" = sprintf("(%.2f)", x[,"sd"]))
  print(out, row.names = FALSE)
}


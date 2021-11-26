#' Overview of effects.
#' @description
#' This function gives an overview of the linear coefficients and whether they
#' are connected to random effects.
#' @inheritParams RprobitB_data
#' @return
#' A data frame with the coefficient names and booleans indicating whether
#' they are connected to random effects.
#' @export

overview_effects <- function(form, re = NULL, alternatives) {

  ### sort and count 'alternatives'
  alternatives <- sort(alternatives)
  J <- length(alternatives)

  ### check 'form'
  check_form_out <- check_form(form = form, re = re)
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### determine names of linear coefficients
  overview <- data.frame()
  for (var in vars[[1]]) {
    overview <- rbind(overview, c(var, var %in% re))
  }
  for (var in c(vars[[2]], if (ASC) "ASC")) {
    for (j in 1:(J - 1)) {
      overview <- rbind(overview, c(paste0(var, "_", alternatives[j]), var %in% re))
    }
  }
  for (var in vars[[3]]) {
    for (j in 1:J) {
      overview <- rbind(overview, c(paste0(var, "_", alternatives[j]), var %in% re))
    }
  }
  colnames(overview) <- c("name", "re")

  ### sort 'overview', first by 'random' and second by appearance in formula
  overview <- overview[order(overview$re, rownames(overview)), ]
  rownames(overview) <- NULL

  ### return 'overview'
  return(overview)
}

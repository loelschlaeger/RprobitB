#' Compute parameter statistics.
#' @description
#' This function computes parameter statistics from Gibbs samples.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param FUN
#' A (preferably named) list of functions that compute parameter statistics
#' from the Gibbs samples, i.e.
#' \itemize{
#'   \item \code{mean} for the mean,
#'   \item \code{sd} for the standard deviation,
#'   \item \code{min} for the minimum,
#'   \item \code{max} for the maximum,
#'   \item \code{median} for the median,
#'   \item \code{function(x) quantile(x, p)} for the \code{p}th quantile,
#'   \item \code{R_hat} for the Gelman-Rubin statistic.
#' }
#' @return
#' An object of class \code{RprobitB_gibbs_samples_statistics}, which is a list
#' of statistics from \code{gibbs_samples} obtained by applying the elements of
#' \code{FUN}.
#' @keywords
#' s3

RprobitB_gibbs_samples_statistics = function(gibbs_samples, FUN) {

  ### check inputs
  if(class(gibbs_samples) != "RprobitB_gibbs_samples")
    stop("'gibbs_samples' must be of class 'RprobitB_gibbs_samples'.")
  for(i in seq_len(length(FUN))){
    if(class(FUN[[i]]) != "function")
      stop("Element ",i," in 'FUN' is not of class 'function'.")
    if(is.null(names(FUN)[i]) || names(FUN)[i] == "")
      names(FUN)[i] = paste0("FUN",i)
  }

  if(any(sapply(FUN, class) != "function"))
    stop("Not all elements of 'FUN' are functions.")

  ### build 'RprobitB_gibbs_sample_statistics'
  statistics = list()
  for(par in names(gibbs_samples$gibbs_samples)){
    statistics[[par]] = matrix(
      NA, nrow = ncol(gibbs_samples$gibbs_samples_nbt[[par]]), ncol = 0,
      dimnames = list(colnames(gibbs_samples$gibbs_samples_nbt[[par]])))
    for(i in seq_len(length(FUN))){
      fun = FUN[[i]]
      values = apply(gibbs_samples$gibbs_samples_nbt[[par]], 2, fun,
                     simplify = FALSE)
      nvalue = length(values[[1]])
      labels = colnames(gibbs_samples$gibbs_samples_nbt[[par]])
      fun_name = if(nvalue == 1) names(FUN[i]) else
        paste(names(FUN[i]), seq_len(nvalue), sep = "_", recycle0 = TRUE)
      append = matrix(NA, nrow = length(values), ncol = nvalue,
                      dimnames = list(labels, fun_name))
      for(j in seq_len(length(values)))
        append[j,] = values[[j]]
      statistics[[par]] = cbind(statistics[[par]], append)
    }
  }

  ### return
  class(statistics) = "RprobitB_gibbs_samples_statistics"
  return(statistics)
}

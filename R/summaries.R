#' Summary method for \code{RprobitB_data}
#' @param object
#' An object of class \code{RprobitB_data}
#' @param ...
#' ignored
#' @export

summary.RprobitB_data = function(object, ...){

  if(!inherits(object, "RprobitB_data"))
    stop("Not of class 'RprobitB_data'.")

  ### summary of covariates (exclude ASC)
  cov_colnames = c("type", "z", "re", "min", "mean", "median", "max", "sd")
  cov = data.frame(matrix(NA, nrow = 0, ncol = length(cov_colnames)))
  colnames(cov) = cov_colnames
  for(type in 1:3){
    for(var in object$vars[[type]]){
      var_alts = if(type %in% c(1,3)){
        paste(var,object$alternatives,sep="_")
      } else {
        var
      }
      #var_alts = var_alts[-which(var_alts == "ASC")]
      for(var_alt in var_alts){
        var_alt_dat = object$choice_data[,var_alt]
        cov[nrow(cov)+1,] =
          c(type,
            var_alt %in% object$standardize,
            var %in% object$re,
            min(var_alt_dat),
            mean(var_alt_dat),
            median(var_alt_dat),
            max(var_alt_dat),
            sd(var_alt_dat))
        rownames(cov)[nrow(cov)] = var_alt
      }
    }
  }
  cov[,c("re")] = as.logical(cov[,c("re")])
  cov[,c("z")] = as.logical(cov[,c("z")])
  cov[,c("min", "mean", "median", "max", "sd")] =
    round(cov[,c("min", "mean", "median", "max", "sd")], 2)

  ### summary of ASC
  if(object$ASC){
    asc_alt = object$alternatives[-object$J]
  } else {
    asc_alt = NULL
  }

  ### summary of alternatives
  alt_colnames = c("frequency")
  alt = data.frame(matrix(NA, nrow = 0, ncol = length(alt_colnames)))
  colnames(alt) = alt_colnames
  for(i in 1:object$J){
    alt[nrow(alt)+1,] =
      sum(unlist(lapply(object$data,function(object)object$y))==i)
    rownames(alt)[nrow(alt)] = object$alternatives[i]

  }

  ### build 'summary.RprobitB_data' object
  out = list("simulated" = object$simulated,
             "N" = object$N,
             "T" = object$T,
             "covariates" = cov,
             "asc_alt" = asc_alt,
             "alternatives" = alt)
  class(out) = "summary.RprobitB_data"

  ### return 'summary.RprobitB_data' object
  return(out)
}

#' Summary method for \code{RprobitB_model}
#' @param object
#' An object of class \code{RprobitB_model}.
#' @param ...
#' ignored
#' @export

summary.RprobitB_model = function(object, ... ) {

  ### check if 'object' is of class 'RprobitB_model'
  if(!inherits(object, "RprobitB_model"))
    stop("Not of class 'RprobitB_model'.")

  ### compute statistics from 'gibbs_samples'
  statistics = compute_parameter_statistics(
    gibbs_samples = gibbs_samples, P_f = data$P_f, P_r = data$P_r, J = data$J,
    C = latent_classes$C)

  ### build 'summary.RprobitB_model' object
  out = list("form" = object$data$form,
             "R" = object$R,
             "B" = object$B,
             "Q" = object$Q,
             "P_f" = object$data$P_f,
             "P_r" = object$data$P_r,
             "cov_fix" = object$data$cov_fix,
             "cov_random" = object$data$cov_random,
             "J" = object$data$J,
             "alternatives" = object$data$alternatives,
             "scale" = object$scale,
             "latent_classes" = object$latent_classes,
             "prior" = object$prior,
             "statistics" = object$statistics,
             "simulated" = object$data$simulated,
             "true_parameter" = object$data$true_parameter)
  class(out) = "summary.RprobitB_model"

  ### return 'summary.RprobitB_model' object
  return(out)
}

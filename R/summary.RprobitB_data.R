#' @export

summary.RprobitB_data = function(object, ...){

  if(!is.RprobitB_data(object))
    stop("Not of class 'RprobitB_data'.")

  ### summary of covariates
  cov_colnames = c("type", "re", "scaled", "min", "mean",
                   "median", "max", "sd")
  cov = data.frame(matrix(NA, nrow = 0, ncol = length(cov_colnames)))
  colnames(cov) = cov_colnames
  for(type in 1:3){
    for(var in object$vars[[type]]){
      var_alts = if(type %in% c(1,3)){
        paste(var,object$alternatives,sep="_")
      } else {
        var
      }
      for(var_alt in var_alts){
        var_alt_dat = object$choice_data[,var_alt]
        cov[nrow(cov)+1,] =
          c(type,
            var %in% object$cov_random,
            var %in% object$standardize,
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
  cov[,c("scaled")] = as.logical(cov[,c("scaled")])
  cov[,c("min", "mean", "median", "max", "sd")] =
    round(cov[,c("min", "mean", "median", "max", "sd")], 2)





      var_dat = object$choice_data[,grepl(paste0(var,".*$"),
                                          colnames(object$choice_data))]
      cov[nrow(cov)+1,] =
        c(type,
          var %in% unique(gsub("_.*$","",object$cov_random)),
          var %in% object$standardize,
          min(var_dat),
          mean(var_dat),
          median(var_dat),
          max(var_dat),
          sd(var_dat))
      rownames(cov)[nrow(cov)] = var
    }
  }
  cov[,c("re")] = as.logical(cov[,c("re")])
  cov[,c("standardized")] = as.logical(cov[,c("standardized")])

  ### summary of alternatives
  for(i in 1:object$J)
    cat(paste0(i,"."),object$alternatives[i],
        paste0("(",sum(unlist(lapply(object$data,function(object)object$y))==i),")"),"\n")

  ### build 'summary.RprobitB_data' object
  res = list("covariates" = cov)
  class(res) = "summary.RprobitB_data"
}

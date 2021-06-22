#' Check \code{model}
#' @description Function that checks the input \code{model} and sets default values.
#' @param model A list of model information.
#' @param data A list data information.
#' @return \code{model}

check_model = function(model,data){

  ### function that checks if input is an integer
  is_int = function(x) return(!is.na(x) & !is.nan(x) & x%%1==0 & x>=0)

  ### check model
  if(!is.null(model)) stopifnot(is.list(model))
  stopifnot(is_int(unlist(model)))

  ### add default values
  model = list("N"   = ifelse(is.null(model$N),   100, model$N),
               "T"   = if(is.null(model$T))       10   else model$T,
               "J"   = ifelse(is.null(model$J),   2  , model$J),
               "P_f" = ifelse(is.null(model$P_f), 1  , model$P_f),
               "P_r" = ifelse(is.null(model$P_r), 0  , model$P_r),
               "C"   = ifelse(is.null(model$C),   0  , model$C))

  ### add information from data to model
  if(!is.null(data)){
    model$N = length(data)
    model$T = unlist(lapply(data,function(x) length(x$y)))
    model$J = NROW(data[[1]]$X[[1]])
    if(is.null(model$C)) model$C=NA
  }

  ### check values of model
  stopifnot(c("N","T","J","P_f","P_r","C") %in% names(model))
  stopifnot(model$N>0,model$T>0)
  stopifnot(length(model$T) %in% c(1,model$N))
  stopifnot(model$J>1)
  if(model$P_r==0) model$C = 0
  if(model$P_r>0 & (model$C==0 | is.na(model$C))) model$C = 1

  ### return checked model
  return(model)
}

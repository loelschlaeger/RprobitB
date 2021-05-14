#' Check \code{data}
#' @description Function that checks the input \code{data}. 
#' @param data A list of data information.
#' @param model A list of model information.
#' @param parm A list of true parameter values. 
#' @param norm A list of normalization information. 
#' @return A list of \code{data} and \code{parm}.

check_data = function(data,model,parm,norm){
  
  ### simulate data if not supplied
  if(!is.null(data)){
    stopifnot(is.list(data)) 
  } else {
    simulate_data_out = simulate_data(model,parm)
    parm = c(parm,simulate_data_out$add_parm)
    data = simulate_data_out$data
  }
  
  ### return data and parm
  return(list("data" = data, "parm" = parm))
}
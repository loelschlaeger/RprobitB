#' Check \code{mcmc}
#' @description Function that checks the input \code{mcmc} and sets default values. 
#' @param mcmc A list of Markov chain Monte Carlo parameters.
#' @return \code{mcmc}

check_mcmc = function(mcmc){
  
  ### check if mcmc is a list
  if(!is.null(mcmc)){
    stopifnot(is.list(mcmc))
  } else {
    mcmc = list()
  }
  
  ### function that checks if input is an integer
  is_int = function(x) return(!is.na(x) & !is.nan(x) & x%%1==0 & x>=0)
  
  ### set missing values to default
  mcmc$R = ifelse(is.null(mcmc$R),10000,mcmc$R) 
  mcmc$B = ifelse(is.null(mcmc$B),mcmc$R/2,mcmc$B) 
  mcmc$Q = ifelse(is.null(mcmc$Q),100,mcmc$Q) 
  mcmc$nprint = ifelse(is.null(mcmc$nprint),floor(mcmc$R/10),mcmc$nprint)
  
  ### check mcmc
  stopifnot(is_int(unlist(mcmc)))
  stopifnot(mcmc$R>mcmc$B)
  stopifnot(mcmc$R>mcmc$Q)
  stopifnot(mcmc$nprint<=mcmc$R)

  ### return mcmc
  return(mcmc)
}
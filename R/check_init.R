#' Check \code{init}
#' @description Function that checks the input \code{init} and sets default values.
#' @details If \code{lcus$do_lcus = TRUE}, initialisation at true parameter 
#'   values is not possible.
#' @param init A list of initial values for the Gibbs sampler.
#' @param model A list of model information.
#' @param parm A list of true parameter values.
#' @param lcus A list of latent class updating scheme parameters.
#' @return \code{init}

check_init = function(init,model,parm,lcus){
  
  ### check if init is a list
  if(!is.null(init)){
    stopifnot(is.list(init))
  } else {
    init = list()
  }
  
  ### check initialization at true values
  if(!is.null(init$at_true)){
    stopifnot(is.logical(init$at_true))
  } else {
    init$at_true = FALSE
  }
  if(init$at_true & lcus$do_lcus){
    warning("If 'lcus$do_lcus = TRUE', initialisation at true parameter values is not possible.",call.=FALSE)
    init$at_true = FALSE
  }
  
  ### check supplied and set missing init values 
  if(init$at_true){ 
    init$m0     = parm$m
    init$alpha0 = parm$alpha
    init$b0     = parm$b
    init$Omega0 = parm$Omega
    init$U0     = parm$U
    init$beta0  = parm$beta
    init$Sigma0 = parm$Sigma
  } else {
    if(model$P_f==0){
      init$alpha0 = NA
    } else {
      if(!is.null(init$alpha0)){
        stopifnot(is.vector(init$alpha0),length(init$alpha0)==model$P_f)
      } else {
        init$alpha0 = numeric(model$P_f)
      }
    }
    if(model$P_r==0){
      init$m0 = NA
      init$b0 = NA
      init$Omega0 = NA
      init$beta0 = NA
    } else {
      C  = ifelse(lcus$do_lcus,lcus$C0,model$C)
      if(!is.null(init$m0)){
        stopifnot(is.vector(init$m0),length(init$m0)==C)
      } else {
        init$m0 = round(rep(model$N,C)*2^(1:C-1)/sum(2^(1:C-1)))
      }
      if(!is.null(init$b0)){
        stopifnot(is.matrix(init$b0),nrow(init$b0)==model$P_r,ncol(init$b0)==C)
      } else {
        init$b0 = matrix(0,nrow=model$P_r,ncol=C)
      }
      if(!is.null(init$Omega0)){
        stopifnot(is.matrix(init$Omega0),nrow(init$Omega0)==model$P_r*model$P_r,ncol(init$Omega0)==C)
        for(c in 1:C) stopifnot(eigen(matrix(parm$Omega0[,c],nrow=model$P_r,ncol=model$P_r))$values >= 0)
      } else {
        init$Omega0 = matrix(0,nrow=model$P_r*model$P_r,ncol=C)
        for(c in 1:C) init$Omega0[,c] = as.vector(diag(model$P_r))
      }
      if(!is.null(init$beta0)){
        stopifnot(is.matrix(init$beta0),nrow(init$beta0)==model$P_r,ncol(init$beta0)==model$N)
      } else {
        init$beta0 = matrix(0,nrow=model$P_r,ncol=model$N)
      }
    }
    if(!is.null(init$U0)){
      stopifnot(is.matrix(init$U0),nrow(init$U0)==model$J-1,ncol(init$U0)==model$N*max(model$T))
    } else {
      init$U0 = matrix(0,nrow=model$J-1,ncol=model$N*max(model$T))
    }
    if(!is.null(init$Sigma0)){
      stopifnot(is.matrix(init$Sigma0),nrow(init$Sigma0)==model$J-1,ncol(init$Sigma0)==model$J-1)
      stopifnot(eigen(parm$Sigma0)$values >= 0)
    } else {
      init$Sigma0 = diag(model$J-1)
    }
  }
  
  ### return init
  return(init)
}

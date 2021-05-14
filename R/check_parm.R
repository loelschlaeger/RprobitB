#' Check \code{parm}
#' @description Function that checks the input \code{parm} and draws missing parameter values. 
#' @param parm A list of true parameter values. 
#' @param model A list of model information.
#' @param data A list of data information.
#' @param norm A list of normalization information.
#' @return \code{parm}

check_parm = function(parm,model,data,norm){

  ### check if parm is a list
  if(!is.null(parm)){
    stopifnot(is.list(parm))
  } else {
    parm = list()
  }
    
  ### fill parm only if data is not supplied
  if(is.null(data)){
    
    ### check supplied and draw missing parm values
    if(model$P_f==0){
      parm$alpha = NA
    } else {
      if(!is.null(parm$alpha)){
        stopifnot(length(parm$alpha)==model$P_f)
      } else {
        parm$alpha = round(runif(model$P_f,-3,3),1)
      }
    }
    if(model$P_r==0){
      parm$s = NA
      parm$b = NA
      parm$Omega = NA
    } else {
      if(!is.null(parm$s)){
        stopifnot(length(parm$s)==model$C)
      } else {
        parm$s = sort(as.vector(rdirichlet(rep(1,model$C))))
      }
      if(!is.null(parm$b)){
        parm$b = as.matrix(parm$b)
        stopifnot(nrow(parm$b)==model$P_r,ncol(parm$b)==model$C)
      } else {
        parm$b = matrix(0,nrow=model$P_r,ncol=model$C)
        for(c in 1:model$C) parm$b[,c] = round(runif(model$P_r,-3,3),1) 
      }
      if(!is.null(parm$Omega)){
        parm$Omega = as.matrix(parm$Omega)
        stopifnot(nrow(parm$Omega)==model$P_r*model$P_r,ncol(parm$Omega)==model$C)
        for(c in 1:model$C) stopifnot(eigen(matrix(parm$Omega[,c],nrow=model$P_r,ncol=model$P_r))$values >= 0)
      } else {
        parm$Omega = matrix(0,nrow=model$P_r*model$P_r,ncol=model$C)
        for(c in 1:model$C) parm$Omega[,c] = as.vector(rwishart(model$P_r,diag(model$P_r))$W)
      }
    } 
    if(!is.null(parm$Sigma)){
      parm$Sigma = as.matrix(parm$Sigma)
      stopifnot(nrow(parm$Sigma)==model$J-1,ncol(parm$Sigma)==model$J-1)
      stopifnot(eigen(parm$Sigma)$values >= 0)
    } else {
      parm$Sigma = rwishart(model$J-1,diag(model$J-1))$W
    }
      
    ### normalize
    if(norm$parameter=="a") parm$alpha[norm$index] = norm$value
    if(norm$parameter=="s") parm$Sigma[norm$index,norm$index] = norm$value
    
    ### check if Sigma is still positive-semidefinite (Gershgorin circle theorem)
    if(!all(eigen(parm$Sigma)$values >= 0)){
      for(j in 1:(model$J-1)) parm$Sigma[j,j] = max(parm$Sigma[j,j], sum(abs(parm$Sigma[j,-j])))
    }

    ### check if parm contains all required parameters
    stopifnot(c("alpha","s","b","Omega","Sigma") %in% names(parm))
  }
  
  ### return parm
  return(parm)
}  

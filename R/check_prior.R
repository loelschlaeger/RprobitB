#' Check \code{prior}
#' @description Function that checks the input \code{prior} and sets default values. 
#' @param prior A list of prior parameters.
#' @param model A list of model information.
#' @return \code{prior}

check_prior = function(prior,model){
  
  ### check if prior is a list
  if(!is.null(prior)){
    stopifnot(is.list(prior))
  } else {
    prior = list()
  }
  
  ### check supplied and set missing prior values
  if(model$P_f>0){
    ### alpha ~ MVN(eta,Psi)
    if(!is.null(prior$eta)){
      stopifnot(is.vector(prior$eta),length(prior$eta)==model$P_f)
    } else {
      prior$eta = numeric(model$P_f)
    }
    if(!is.null(prior$Psi)){
      stopifnot(is.matrix(prior$Psi),nrow(prior$Psi)==model$P_f,ncol(prior$Psi)==model$P_f)
    } else {
      prior$Psi = matrix(1,model$P_f,model$P_f); diag(prior$Psi) = 5
    }
  } else {
    prior$eta = NA
    prior$Psi = NA
  }
  if(model$P_r>0){
    ### s ~ D(delta)
    if(!is.null(prior$delta)){
      stopifnot(is.numeric(prior$delta),length(prior$delta)==1)
    } else {
      prior$delta = 1
    }
    ### b_c ~ MVN(xi,D)
    if(!is.null(prior$xi)){
      stopifnot(is.vector(prior$xi),length(prior$xi)==model$P_r)
    } else {
      prior$xi = numeric(model$P_r)
    }
    if(!is.null(prior$D)){
      stopifnot(is.matrix(prior$D),nrow(prior$D)==model$P_r,ncol(prior$D)==model$P_r)
    } else {
      prior$D  = matrix(1,model$P_r,model$P_r); diag(prior$D) = 5
    }
    ### Omega_c ~ IW(nu,Theta)
    if(!is.null(prior$nu)){
      stopifnot(is.numeric(prior$nu),length(prior$nu)==1)
    } else {
      ### nu must exceed P_r; more diffuse with lower nu; if nu = P_r+2, Theta represents the mean 
      prior$nu = model$P_r+2
    }
    if(!is.null(prior$Theta)){
      stopifnot(is.matrix(prior$Theta),nrow(prior$Theta)==model$P_r,ncol(prior$Theta)==model$P_r)
    } else {
      prior$Theta = matrix(1,model$P_r,model$P_r); diag(prior$Theta) = 5
    }
  } else {
    prior$delta = NA
    prior$xi = NA
    prior$D = NA
    prior$nu = NA
    prior$Theta = NA
  }
  ### Sigma ~ IW(kappa,E)
  if(!is.null(prior$kappa)){
    stopifnot(is.vector(prior$kappa),length(prior$kappa)==1)
  } else {
    ### kappa must exceed J-1; more diffuse with lower kappa; if kappa = J-1+2, E represents the mean 
    prior$kappa = model$J-1+2
  }
  if(!is.null(prior$E)){
    stopifnot(is.matrix(prior$E),nrow(prior$E)==model$J-1,ncol(prior$E)==model$J-1)
  } else {
    prior$E = matrix(1,model$J-1,model$J-1); diag(prior$E) = 5
  }
  
  ### return prior
  return(prior)
}
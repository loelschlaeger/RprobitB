#' Check \code{parm}
#' @description
#' Function that checks \code{parm} and draws missing parameter values.
#' @param parm
#' A list of true parameter values.
#' \itemize{
#'   \item \code{alpha}:
#'   the fixed coefficient vector of length \code{P_f}
#'   \item \code{s}:
#'   the vector of class weights of length \code{C}
#'   \item \code{b}:
#'   the matrix of class means as columns of dimension \code{P_r} x \code{C}
#'   \item \code{Omega}:
#'   the matrix of class covariance matrices as columns of dimension
#'   \code{P_r*P_r} x \code{C}
#'   \item \code{Sigma}:
#'   the error term covariance matrix of dimension \code{J} x \code{J}
#' }
#' @inheritParams RprobitB_data
#' @return
#' \code{parm}

check_parm = function(parm, P_f, P_r, J, C){

  ### check if parm is a list
  if(!is.null(parm)){
    if(!is.list(parm))
      stop("'parm' must be a list.")
  } else {
    parm = list()
  }

  ### alpha
  if(P_f==0){
    parm$alpha = NA
  } else {
    if(!is.null(parm$alpha)){
      if(length(parm$alpha)!=P_f || !is.numeric(parm$alpha))
        stop("'alpha' must be a numeric vector of length 'P_f'.")
    } else {
      parm$alpha = round(runif(P_f,-3,3),1)
    }
  }

  ### s, b, Omega
  if(P_r==0){
    parm$s = NA
    parm$b = NA
    parm$Omega = NA
  } else {

    ### s
    if(is.null(parm$s))
      parm$s = round(sort(as.vector(rdirichlet(rep(1,C)))),2)
    if(length(parm$s)!=C || !is.numeric(parm$s) || sum(parm$s)!=1)
      stop("'s' must be a numeric vector of length 'C' which sums up to 1.")

    ### b
    if(is.null(parm$b)){
      parm$b = matrix(0,nrow=P_r,ncol=C)
      for(c in 1:C) parm$b[,c] = round(runif(P_r,-3,3),1)
    }
    parm$b = as.matrix(parm$b)
    if(!is.numeric(parm$b) || nrow(parm$b)!=P_r || ncol(parm$b)!=C)
      stop("'b' must be a numeric matrix of dimension P_r x C.")

    ### Omega
    if(is.null(parm$Omega)){
      parm$Omega = matrix(0,nrow=P_r*P_r,ncol=C)
      for(c in 1:C)
        parm$Omega[,c] = as.vector(rwishart(P_r,diag(P_r))$W)
    }
    parm$Omega = as.matrix(parm$Omega)
    if(!is.numeric(parm$Omega) || nrow(parm$Omega)!=P_r*P_r ||
       ncol(parm$Omega)!=C)
      stop("'Omega' must be a numeric matrix of dimension P_r*P_r x C.")
    for(c in 1:C)
      if(!is.covariance.matrix(matrix(parm$Omega[,c],nrow=P_r,ncol=P_r)))
        stop(paste("Column",c,"in 'Omega' builds no covariance matrix."))
  }

  ### Sigma
  if(is.null(parm$Sigma))
    parm$Sigma = rwishart(J,diag(J))$W
  parm$Sigma = as.matrix(parm$Sigma)
  if(!is.numeric(parm$Sigma) || nrow(parm$Sigma)!=J || ncol(parm$Sigma)!=J)
    stop("'Sigma' must be a numeric matrix of dimension J x J.")
  if(!is.covariance.matrix(parm$Sigma))
    stop("'Sigma' builds is no covariance matrix.")

  ### check if 'parm' contains all required parameters
  stopifnot(c("alpha","s","b","Omega","Sigma") %in% names(parm))

  ### return 'parm'
  return(parm)
}

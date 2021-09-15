#' Construct object of class \code{RprobitB_parameter}.
#' @description
#' This function constructs an object of class \code{RprobitB_parameter}.
#' Missing parameters are sampled. All parameters are checked against the values
#' of \code{P_f}, \code{P_r}, \code{J}, and \code{N}.
#' @inheritParams RprobitB_data
#' @param alpha
#' The fixed coefficient vector of length \code{P_f}.
#' Set to \code{NA} if \code{P_f = 0}.
#' @param C
#' The number (greater or equal 1) of latent classes of decision makers.
#' Set to \code{NA} if \code{P_r = 0}. Otherwise, \code{C = 1} per default.
#' @param s
#' The vector of class weights of length \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param b
#' The matrix of class means as columns of dimension \code{P_r} x \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param Omega
#' The matrix of class covariance matrices as columns of dimension
#' \code{P_r*P_r} x \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param Sigma
#' The error term covariance matrix of dimension \code{J} x \code{J}.
#' Internally, \code{Sigma} gets differenced with respect to alternative
#' \code{J}, so it becomes an identified covariance matrix of dimension
#' \code{J-1} x \code{J-1}.
#' @param beta
#' The matrix of the decision-maker specific coefficient vectors of dimension
#' \code{P_r} x \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param z
#' The vector of the allocation variables of length \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param U
#' The matrix of the latent utilities of dimension \code{J} x 'number of total
#' choice occasions'.
#' @return
#' An object of class \code{RprobitB_parameter}, i.e. a named list with the
#' model parameters \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{beta}, \code{z}, and \code{U}.

RprobitB_parameter = function(P_f, P_r, J, N, alpha = NULL, C = NULL, s = NULL,
                              b = NULL, Omega = NULL, Sigma = NULL, beta = NULL,
                              z = NULL, U = NULL) {

  ### function that checks if input is a proper covariance matrix
  is.covariance.matrix = function(x){
    is.matrix(x) && ncol(x)==nrow(x) && isSymmetric(x) && all(eigen(x)$value>=0)
  }

  ### alpha
  if(P_f==0){
    alpha = NA
  } else {
    if(!is.null(alpha)){
      if(length(alpha)!=P_f || !is.numeric(alpha))
        stop("'alpha' must be a numeric vector of length ",P_f,".")
    } else {
      alpha = round(runif(P_f,-3,3),1)
    }
  }

  ### C, s, b, Omega, z, beta
  if(P_r==0){
    C = NA
    s = NA
    b = NA
    Omega = NA
    z = NA
    beta = NA
  } else {

    ### C
    if(!is.null(C)){
      if(!is.numeric(C) || !C%%1 == 0 || !C>0)
        stop("'C' must be a number greater or equal 1.")
    } else {
      C = 1
    }

    ### s
    if(is.null(s))
      s = round(sort(as.vector(rdirichlet(rep(1,C)))),2)
    if(length(s)!=C || !is.numeric(s) || sum(s)!=1)
      stop("'s' must be a numeric vector of length ", C, " which sums up to 1.")

    ### b
    if(is.null(b)){
      b = matrix(0,nrow=P_r,ncol=C)
      for(c in 1:C) b[,c] = round(runif(P_r,-3,3),1)
    }
    b = as.matrix(b)
    if(!is.numeric(b) || nrow(b)!=P_r || ncol(b)!=C)
      stop("'b' must be a numeric matrix of dimension ", P_r, " x ", C, ".")

    ### Omega
    if(is.null(Omega)){
      Omega = matrix(0,nrow=P_r*P_r,ncol=C)
      for(c in 1:C)
        Omega[,c] = as.vector(rwishart(P_r,diag(P_r))$W)
    }
    Omega = as.matrix(Omega)
    if(!is.numeric(Omega) || nrow(Omega)!=P_r*P_r ||
       ncol(Omega)!=C)
      stop("'Omega' must be a numeric matrix of dimension ", P_r*P_r, " x ",
           C, ".")
    for(c in 1:C)
      if(!is.covariance.matrix(matrix(Omega[,c],nrow=P_r,ncol=P_r)))
        stop(paste("Column",c,"in 'Omega' builds no covariance matrix."))

    ### z
    if(is.null(z))
      z = sample(1:C, N, prob=s, replace=TRUE)
    if(length(z)!=N || !is.numeric(z) || !all(z %in% 1:C))
      stop("'z' must be a numeric vector of length ", N,
           " with elements of value ", paste(seq_len(C), collapse = ", "), ".")

    ### beta
    if(is.null(beta)){
      beta = matrix(0, nrow=P_r, ncol=N)
      for(n in seq_len(N))
        beta[,n] = b[,z[n]] +
          t(chol(matrix(Omega[,z[n]], nrow=P_r, ncol=P_r))) %*% rnorm(P_r)
    }
    if(!is.numeric(beta) || nrow(beta)!=P_r ||
       ncol(beta)!=N)
      stop("'beta' must be a numeric matrix of dimension ", P_r, " x ", N, ".")

  }

  ### Sigma
  if(is.null(Sigma))
    Sigma = rwishart(J,diag(J))$W
  Sigma = as.matrix(Sigma)
  if(!is.numeric(Sigma) || nrow(Sigma)!=J || ncol(Sigma)!=J)
    stop("'Sigma' must be a numeric matrix of dimension ", J, " x ", J, ".")
  if(!is.covariance.matrix(Sigma))
    stop("'Sigma' is not a proper covariance matrix.")

  ### build and return 'RprobitB_parameter'-object
  out = list("alpha" = alpha,
             "C" = C,
             "s" = s,
             "b" = b,
             "Omega" = Omega,
             "Sigma" = Sigma,
             "beta" = beta,
             "z" = z,
             "U" = NA)
  class(out) = "RprobitB_parmameter"
  return(out)

}

#' Compute probit choice probabilities
#' @description
#' Function that computes the probit choice probabilities.
#' @details
#' If \code{P_f = 0}, \code{alpha} is ignored.
#' If \code{P_r = 0}, \code{C}, \code{s}, \code{b} and \code{Omega} are ignored.
#' @param X
#' A matrix of covariates with one row per alternative.
#' The first \code{P_f} columns are connected to fixed coefficients, the last
#' \code{P_r} columns are connected to random coefficients.
#' @param J
#' The number of choice alternatives.
#' @param P_f
#' The number of covariates connected to fixed coefficients.
#' @param P_r
#' The number of covariates connected to random coefficients.
#' @param C
#' The number of latent classes.
#' @param alpha
#' The fixed coefficient vector of length \code{P_f}.
#' @param s
#' The vector of class weights of length \code{C}.
#' @param b
#' The matrix of class means as columns of dimension \code{P_r} x \code{C}.
#' @param Omega
#' The matrix of class covariance matrices as columns of dimension \code{P_r}^2 x \code{C}.
#' @param Sigma
#' The error term covariance matrix of dimension \code{J-1} x \code{J-1}.
#' @return
#' The probability.
#' @examples
#' ### covariates
#' X = matrix(c(3,2,1,3,2,1,3,2,1),3,3)
#' ### model setting
#' J = 3
#' P_f = 1
#' P_r = 2
#' C = 2
#' ### model parameter
#' alpha = 1
#' s = c(0.3,0.7)
#' b = matrix(c(1,1,1,1),2,2)
#' Omega = matrix(c(as.numeric(diag(2)),as.numeric(diag(2))),4,2)
#' Sigma = diag(J-1)
#' ### compute choice probability
#' prob = compute_choice_probabilities(X,J,P_f,P_r,C,alpha,s,b,Omega,Sigma)
#' @export

compute_choice_probabilities = function(X,J,P_f,P_r,C,alpha,s,b,Omega,Sigma) {

  ### function that checks if input is an integer
  is.int = function(x) return(!is.na(x) & !is.nan(x) & x%%1==0 & x>=0)

  ### check inputs
  if(!is.matrix(X))
    stop("'X' must be a matrix.")
  if(!is.int(J))
    stop("'J' must be an integer.")
  if(!is.int(P_f))
    stop("'P_f' must be an integer.")
  if(!is.int(P_r))
    stop("'P_r' must be an integer.")
  if(ncol(X)!=(P_f+P_r))
    stop("'X' must have 'P_f'+'P_r' columns.")
  if(nrow(X)!=J)
    stop("'X' must have 'J' columns.")
  if(P_f==0){
    alpha = NULL
  } else {
    if(length(alpha)!=P_f || !is.numeric(alpha))
      stop("'alpha' must be a numeric vector of length 'P_f'.")
  }
  if(P_r==0){
    C = 1
    s = 1
    b = NULL
    Omega = NULL
  } else {
    if(!is.int(C))
      stop("'C' must be an integer.")
    if(length(s)!=C || !is.numeric(s))
      stop("'s' must be a numeric vector of length 'C'.")
    if(!is.matrix(b) || any(dim(b)!=c(P_r,C)) || !is.numeric(b))
      stop("'b' must be a numeric matrix of dimension 'P_r' x 'C'.")
    if(!is.matrix(Omega) || any(dim(Omega)!=c(P_r^2,C)) || !is.numeric(Omega))
      stop("'Omega' must be a numeric matrix of dimension 'P_r'^2 x 'C'.")
  }
  if(!is.matrix(Sigma) || any(dim(Sigma)!=c(J-1,J-1)))
    stop("'Sigma' must be a numeric matrix of dimension 'J-1' x 'J-1'.")

  ### define difference operator (computes differences wrt alternative i)
  Delta = function(J,i){
    Delta = diag(J)[-J,,drop=FALSE]; Delta[,i] = -1
    return(Delta)
  }

  ### compute differences with respect to reference alternative J
  X = Delta(J,J) %*% X

  ### append 0's for reference alternative J
  X = rbind(X,0)
  Sigma = cbind(rbind(Sigma,0),0)

  ### allocate space for choice probabilities
  probabilities = numeric(J)

  ### compute choice probabilities
  for(j in 1:J){

    probability_j = 0

    for(c in 1:C){

      ### define parameters of multivariate normal distribution
      if(P_f>0){
        if(P_r>0){
          upper = as.vector(-Delta(J,j) %*% X %*% c(alpha,b[,c]))
          cov_matrix = Delta(J,j) %*% ( X[,-(1:P_f)] %*% matrix(Omega[,c],P_r,P_r) %*% t(X[,-(1:P_f)]) + Sigma ) %*% t(Delta(J,j))
        }
        if(P_r==0){
          upper = as.vector(-Delta(J,j) %*% X %*% alpha)
          cov_matrix = Delta(J,j) %*% Sigma %*% t(Delta(J,j))
        }
      }
      if(P_f==0){
        if(P_r>0){
          upper = as.vector(-Delta(J,j) %*% X %*% b[,c])
          cov_matrix = Delta(J,j) %*% ( X %*% matrix(Omega[,c],P_r,P_r) %*% t(X) + Sigma ) %*% t(Delta(J,j))
        }
        if(P_r==0){
          upper = rep(0,J-1)
          cov_matrix = Delta(J,j) %*% Sigma %*% t(Delta(J,j))
        }
      }

      probability_j = probability_j + s[c] * mvtnorm::pmvnorm(lower = rep(-Inf,J-1),
                                                              upper = upper,
                                                              mean  = rep(0,J-1),
                                                              sigma = cov_matrix)
    }

    probabilities[j] = probability_j

  }

  return(probabilities)
}

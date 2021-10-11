#' Compute probit choice probabilities.
#' @description
#' This function computes probit choice probabilities.
#' @param X
#' A matrix of covariates with one row per alternative.
#' The first \code{P_f} columns are connected to fixed coefficients, the last
#' \code{P_r} columns are connected to random coefficients.
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#' @return
#' A probability vector of length \code{C}.
#' @examples
#' X = matrix(c(3,2,1,3,2,1,3,2,1),3,3)
#' parameter = RprobitB_true_parameter(P_f = 1, P_r = 2, J = 3, N = 100, C = 2)
#' prob = compute_choice_probabilities(X = X, parameter = parameter)
#' @export

compute_choice_probabilities = function(X, parameter) {

  ### check inputs
  if(P_f>0 || P_r>0){
    if(!is.matrix(X))
      stop("'X' must be a matrix.")
    if(ncol(X)!=(P_f+P_r))
      stop("'X' must have 'P_f'+'P_r' columns.")
    if(nrow(X)!=J)
      stop("'X' must have 'J' columns.")
  }
  if(class(parameter) != "RprobitB_true_parameter")
    stop("'parameter' is not of class 'RprobitB_true_parameter.")

  ### unpack parameters
  alpha = parameter$alpha
  C = parameter$C
  s = parameter$s
  b = parameter$b
  Omega = parameter$Omega
  Sigma = parameter$Sigma

  ### compute differences with respect to reference alternative J
  if(P_f>0 || P_r>0)
    X = delta(J,J) %*% X

  ### allocate space for choice probabilities
  probabilities = numeric(J)

  ### compute choice probability of alternative J
  probability_J = 0
  for(c in 1:C){

    ### define parameters of multivariate normal distribution
    if(P_f>0){
      if(P_r>0){
        upper = as.vector(- X %*% c(alpha,b[,c]))
        cov_matrix = X[,-(1:P_f)] %*% matrix(Omega[,c],P_r,P_r) %*% t(X[,-(1:P_f)]) + Sigma
      }
      if(P_r==0){
        upper = as.vector(- X %*% alpha)
        cov_matrix = Sigma
      }
    }
    if(P_f==0){
      if(P_r>0){
        upper = as.vector(- X %*% b[,c])
        cov_matrix = X %*% matrix(Omega[,c],P_r,P_r) %*% t(X) + Sigma
      }
      if(P_r==0){
        upper = rep(0,J-1)
        cov_matrix = Sigma
      }
    }

    probability_J = probability_J + s[c] * mvtnorm::pmvnorm(lower = rep(-Inf,J-1),
                                                            upper = upper,
                                                            mean  = rep(0,J-1),
                                                            sigma = cov_matrix)[1]
  }
  probabilities[J] = probability_J

  ### append 0's for reference alternative J
  if(P_f>0 || P_r>0){
    X = rbind(X,0)
  }
  Sigma = cbind(rbind(Sigma,0),0)

  ### compute choice probabilities of alternatives 1 to J-1
  for(j in 1:(J-1)){

    probability_j = 0

    for(c in 1:C){

      ### define parameters of multivariate normal distribution
      if(P_f>0){
        if(P_r>0){
          upper = as.vector(-delta(J,j) %*% X %*% c(alpha,b[,c]))
          cov_matrix = delta(J,j) %*% ( X[,-(1:P_f)] %*% matrix(Omega[,c],P_r,P_r) %*% t(X[,-(1:P_f)]) + Sigma ) %*% t(delta(J,j))
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
                                                              sigma = cov_matrix)[1]
    }

    probabilities[j] = probability_j

  }

  return(probabilities)
}

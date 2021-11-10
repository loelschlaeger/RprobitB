#' Compute probit choice probabilities for a single choice situation.
#' @description
#' This function computes the probit choice probabilities for a single choice
#' situation with \code{J} alternatives.
#' @param X
#' A matrix of covariates with \code{J} rows and \code{P_f + P_r} columns, where
#' the first \code{P_f} columns are connected to fixed coefficients and the last
#' \code{P_r} columns are connected to random coefficients.
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#' @return
#' A probability vector of length \code{J}.
#' @keywords
#' internal

compute_choice_probabilities = function(X, parameter) {

  ### unpack and check inputs
  if(class(parameter) != "RprobitB_parameter")
    stop("'parameter' is not of class 'RprobitB_parameter.")
  alpha = parameter$alpha
  C = parameter$C
  s = ifelse(is.na(parameter$s),1,parameter$s)
  b = parameter$b
  Omega = parameter$Omega
  Sigma_full = parameter$Sigma_full
  P_f = ifelse(any(is.na(alpha)), 0, length(alpha))
  P_r = ifelse(any(is.na(parameter$s)),0,length(parameter$s))
  J = nrow(Sigma_full)
  C = ifelse(is.na(parameter$C),1,parameter$C)
  if(P_f>0 || P_r>0){
    if(!is.matrix(X))
      stop("'X' must be a matrix.")
    if(ncol(X)!=(P_f+P_r))
      stop("'X' must have 'P_f'+'P_r' columns.")
    if(nrow(X)!=J)
      stop("'X' must have 'J' columns.")
  }

  ### compute choice probabilities
  probabilities = numeric(J)
  for(j in 1:J){
    probability_j = 0
    for(c in 1:C){
      ### define parameters of multivariate normal distribution
      if(P_f>0){
        if(P_r>0){
          upper = as.vector(-delta(J,j) %*% X %*% c(alpha,b[,c]))
          cov_matrix = delta(J,j) %*%
            ( X[,-(1:P_f)] %*% matrix(Omega[,c],P_r,P_r) %*% t(X[,-(1:P_f)]) +
                Sigma_full ) %*% t(delta(J,j))
        }
        if(P_r==0){
          upper = as.vector(-delta(J,j) %*% X %*% alpha)
          cov_matrix = delta(J,j) %*% Sigma_full %*% t(delta(J,j))
        }
      }
      if(P_f==0){
        if(P_r>0){
          upper = as.vector(-delta(J,j) %*% X %*% b[,c])
          cov_matrix = delta(J,j) %*%
            ( X %*% matrix(Omega[,c],P_r,P_r) %*% t(X) + Sigma_full ) %*%
            t(delta(J,j))
        }
        if(P_r==0){
          upper = rep(0,J-1)
          cov_matrix = delta(J,j) %*% Sigma_full %*% t(delta(J,j))
        }
      }
      probability_j = probability_j +
        s[c] * mvtnorm::pmvnorm(lower = rep(-Inf,J-1), upper = upper,
                                mean  = rep(0, J-1), sigma = cov_matrix)[1]
    }
    probabilities[j] = probability_j
  }

  ### check if probabilities sum to 1
  if(abs(sum(probabilities)-1)>sqrt(.Machine$double.eps))
    warning("probabilities do not sum to 1.")

  ### return probabilities
  return(probabilities)
}

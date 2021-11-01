#' Check \code{prior}.
#' @description
#' This function checks the input \code{prior} and sets missing values to
#' default values.
#' @param prior
#' A named list of parameters for the prior distributions of the normalized
#' parameters:
#' \itemize{
#'   \item \code{eta}:
#'   The mean vector of length \code{P_f} of the normal prior for
#'   \code{alpha}.
#'   \item \code{Psi}:
#'   The covariance matrix of dimension \code{P_f} x \code{P_f} of the
#'   normal prior for \code{alpha}.
#'   \item \code{delta}:
#'   The concentration parameter of length 1 of the Dirichlet prior for
#'   \code{s}.
#'   \item \code{xi}:
#'   The mean vector of length \code{P_r} of the normal prior for each
#'   \code{b_c}.
#'   \item \code{D}:
#'   The covariance matrix of dimension \code{P_r} x \code{P_r} of the
#'   normal prior for each \code{b_c}.
#'   \item \code{nu}:
#'   The degrees of freedom (a natural number greater than \code{P_r}) of
#'   the Inverse Wishart prior for each \code{Omega_c}.
#'   \item \code{Theta}:
#'   The scale matrix of dimension \code{P_r} x \code{P_r} of the
#'   Inverse Wishart prior for each \code{Omega_c}.
#'   \item \code{kappa}:
#'   The degrees of freedom (a natural number greater than \code{J-1}) of
#'   the Inverse Wishart prior for \code{Sigma}.
#'   \item \code{E}:
#'   The scale matrix of dimension \code{J-1} x \code{J-1} of the
#'   Inverse Wishart prior for \code{Sigma}.
#' }
#' @inheritParams RprobitB_data
#' @return
#' The checked input \code{prior}
#' @keywords
#' internal

check_prior = function(prior, P_f, P_r, J){

  ### check if prior is a list
  if(!is.null(prior)){
    if(!is.list(prior))
      stop("'prior' must be either 'NULL' or a list.")
  } else {
    prior = list()
  }

  ### check supplied and set missing prior parameters
  if(P_f>0){

    ### alpha ~ MVN(eta,Psi)
    if(is.null(prior$eta))
      prior$eta = numeric(P_f)
    if(!is.numeric(prior$eta) || length(prior$eta)!=P_f)
      stop("'prior$eta' must be a numeric vector of length 'P_f'.")
    if(is.null(prior$Psi))
      prior$Psi = diag(P_f)
    if(!is.numeric(prior$Psi) || !is.matrix(prior$Psi) ||
       any(dim(prior$Psi)!=c(P_f,P_f)))
      stop("'prior$Psi' must be a numeric matrix of dimension 'P_f' x 'P_f'.")
  } else {
    prior$eta = NA
    prior$Psi = NA
  }
  if(P_r>0){

    ### s ~ D(delta)
    if(is.null(prior$delta))
      prior$delta = 1
    if(!is.numeric(prior$delta) || length(prior$delta)!=1)
      stop("'prior$delta' must be a single numeric value.")

    ### b_c ~ MVN(xi,D)
    if(is.null(prior$xi))
      prior$xi = numeric(P_r)
    if(!is.numeric(prior$xi) || length(prior$xi)!=P_r)
      stop("'prior$xi' must be a numeric vector of length 'P_r'.")
    if(is.null(prior$D))
      prior$D = diag(P_r)
    if(!is.numeric(prior$D) || !is.matrix(prior$D) ||
       any(dim(prior$D)!=c(P_r,P_r)))
      stop("'prior$D' must be a numeric matrix of dimension 'P_r' x 'P_r'.")

    ### Omega_c ~ IW(nu,Theta)
    if(is.null(prior$nu))
      ### nu must exceed P_r; more diffuse with lower nu;
      ### if nu = P_r+2, Theta represents the mean
      prior$nu = P_r+2
    if(!is.numeric(prior$nu) || length(prior$nu)!=1 || prior$nu<=P_r)
      stop("'prior$nu' must be a single numeric value greater 'P_r'.")
    if(is.null(prior$Theta))
      prior$Theta = diag(P_r)
    if(!is.numeric(prior$Theta) || !is.matrix(prior$Theta) ||
       any(dim(prior$Theta)!=c(P_r,P_r)))
      stop("'prior$Theta' must be a numeric matrix of dimension 'P_r' x 'P_r'.")
  } else {
    prior$delta = NA
    prior$xi = NA
    prior$D = NA
    prior$nu = NA
    prior$Theta = NA
  }

  ### Sigma ~ IW(kappa,E)
  if(is.null(prior$kappa))
    ### kappa must exceed J-1; more diffuse with lower kappa;
    ### if kappa = J-1+2, E represents the mean
    prior$kappa = J-1+2
  if(!is.numeric(prior$kappa) || length(prior$kappa)!=1 || prior$kappa<=J-1)
    stop("'prior$kappa' must be a single numeric value greater 'J-1'.")
  if(is.null(prior$E))
    prior$E = diag(J-1)
  if(!is.numeric(prior$E) || !is.matrix(prior$E) ||
     any(dim(prior$E)!=c(J-1,J-1)))
    stop("'prior$E' must be a numeric matrix of dimension 'J-1' x 'J-1'.")

  ### add class to 'prior'
  class(prior) = "RprobitB_prior"

  ### return prior
  return(prior)
}

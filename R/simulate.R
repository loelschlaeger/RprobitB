#' Simulate choice data
#' @description
#' Function that simulates choice data for the RprobitB package.
#' @details
#' For more details see the vignette "How to simulate choice data?":
#' \code{vignette("How to simulate choice data?", package = "RprobitB")}
#' @inheritParams RprobitB_data
#' @inheritParams check_parm
#' @param distr
#' A list of number generation functions from which the covariates
#' are drawn. Each element of \code{distr} must be of the form
#' \code{"name" = list(pars)}, where \code{"name"} is the name of the number
#' generation function and \code{pars} are required parameters.
#' Possible number generation functions are
#' \itemize{
#'   \item functions of the type \code{r*} from base R (e.g. \code{rnorm}) where
#'         all required parameters (except for \code{n}) must be specified,
#'   \item the function \code{sample}, where all required parameters
#'         (except for \code{size}) must be specified.
#' }
#' The order is the same as in \code{form}, i.e. the first element of
#' \code{distr} draws the first covariate of \code{form}.
#' If \code{distr} is not specified, all covariates are drawn from a
#' standard normal distribution.
#' If \code{distr} is specified, the length of \code{distr} must equal the
#' number of covariates.
#' @return
#' An object of class \code{RprobitB_data}, ready to be submitted to
#' \link[RprobitB]{fit}.
#' @examples
#' form = choice ~ var1 | var2
#' N = 10
#' T = 1:10
#' J = 3
#' C = 2
#' re = c("ASC")
#' alternatives = c("A","B")
#' distr = list("rgamma" = list(mean = 0, sd = 1),
#'              "sample" = list(x = 1:10, replace = TRUE))
#' data = simulate(form = form, N = N, T = T, J = J, C = C, re = re,
#'                 alternatives = alternatives, distr = distr,
#'                 standardize = standardize)
#' @export

simulate = function(form, N, T, J, C = 1, re = NULL, alternatives = NULL,
                    parm = NULL, distr = NULL, standardize = NULL) {

  ### check input
  if(!inherits(form,"formula"))
    stop("'form' must be of class 'formula'.")
  if(!is.natural.number(N))
    stop("'N' must be a non-negative number.")
  if(!is.natural.number(T))
    stop("'T' must be a non-negative number.")
  if(!is.natural.number(J))
    stop("'J' must be a non-negative number.")
  if(!is.natural.number(C))
    stop("'C' must be a non-negative number.")
  if(!is.null(re))
    if(!is.character(re))
      stop("'re' must be a character (vector).")
  if(!is.null(alternatives))
    if(length(alternatives) != J || !is.character(alternatives))
      stop("'alternatives' must be a character (vector) of length 'J'.")
  parm = check_parm(parm)
  if(!is.null(distr))
    if(!is.list(distr))
      stop("")
  if(!is.null(standardize))
    if(!is.character(standardize))
      stop("'standardize' must be a character (vector).")

  ### read parm
  alpha = parm$alpha
  s = parm$s
  b = parm$b
  Omega = parm$Omega
  Sigma = parm$Sigma

  if(P_r>0){
    ### draw allocation variable (N x 1)
    z = sample(1:C,N,prob=s,replace=TRUE)

    ### compute class sizes
    m = as.numeric(table(z))

    ### draw beta
    beta = matrix(0,nrow=P_r,ncol=N)
    for(n in seq_len(N)) beta[,n] = b[,z[n]] + t(chol(matrix(Omega[,z[n]],nrow=P_r,ncol=P_r))) %*% rnorm(P_r)

  } else{
    beta = NA
    z = NA
    m = NA
  }

  ### compute lower-triangular Cholesky root of Sigma
  L = t(chol(Sigma))

  ### allocate space for output
  data = list()
  U = matrix(0,nrow=J-1,ncol=sum(T))

  ### difference operator
  Delta_J = diag(J)[-J,,drop=FALSE]; Delta_J[,J] = -1

  for(n in seq_len(N)){
    data[[n]] = list()
    data[[n]][["X"]] = list()
    y_n = numeric(T[n])

    for(t in seq_len(T[n])){
      ### draw covariates
      if(P_f==0 & P_r==0){
        W_nt_X_nt = NA
      } else {
        W_nt_X_nt = matrix(rnorm(J*(P_f+P_r),sd=sd),nrow=J,ncol=(P_f+P_r))
      }
      data[[n]][["X"]][[t]] = W_nt_X_nt

      ### build coefficient vector
      if(P_f>0 & P_r>0) coeff = c(alpha,beta[,n])
      if(P_f>0 & P_r==0) coeff = alpha
      if(P_f==0 & P_r>0) coeff = beta[,n]
      if(P_f==0 & P_r==0) coeff = NA

      ### compute utility and choice decision
      eps  = L %*% rnorm(J-1)
      if(P_f==0 & P_r==0){
        U_nt = eps
      } else {
        V_nt = Delta_J %*% W_nt_X_nt %*% coeff
        U_nt = V_nt + eps
      }
      U[,sum(T[seq_len(n-1)])+t] = U_nt
      y_n[t] = ifelse(max(U_nt) < 0, J, which.max(U_nt))
    }

    data[[n]][["y"]] = y_n
  }

  add_parm = list("U"    = U,
                  "beta" = beta,
                  "z"    = z,
                  "m"    = m)

  return(list("data"     = data,
              "add_parm" = add_parm))
}

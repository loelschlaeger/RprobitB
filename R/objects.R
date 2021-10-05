#' Create object of class \code{RprobitB_data}.
#' @description
#' This function creates an object of class \code{RprobitB_data}.
#' @param data
#' A list with the choice data.
#' The list has \code{N} elements.
#' Each element is a list with two elements, \code{X} and \code{y}, which are
#' the covariates and decisions for a decision maker. More precisely,
#' \code{X} is a list of \code{T} elements, where each element is a matrix of
#' dimension \code{J}x(\code{P_f}+\code{P_r}) and contains the characteristics
#' for one choice occasion.
#' \code{y} is a vector of length \code{T} and contains the labels for the
#' chosen alternatives.
#' @param N
#' The number (greater or equal 1) of decision makers.
#' @param T
#' The number (greater or equal 1) of choice occasions or a vector of choice
#' occasions of length \code{N} (i.e. a decision maker specific number).
#' @param J
#' The number (greater or equal 2) of choice alternatives.
#' @param P_f
#' The number of covariates connected to a fixed coefficient (can be 0).
#' @param P_r
#' The number of covariates connected to a random coefficient (can be 0).
#' @param alternatives
#' A character vector with the names of the choice alternatives.
#' @param vars
#' The element \code{vars} in the output of \link{check_form}.
#' @param ASC
#' A boolean, determining whether the model has ASCs.
#' @param standardize
#' A character vector of names of covariates that get standardized.
#' Covariates of type 1 or 3 have to be addressed by
#' \code{covariate_alternative}.
#' If \code{standardize = "all"}, all covariates get standardized.
#' @param simulated
#' A boolean, if \code{TRUE} then \code{data} is simulated, otherwise
#' \code{data} is empirical.
#' @inheritParams check_form
#' @inheritParams prepare
#' @inheritParams simulate
#' @params true_parameter
#' An object of class \code{RprobitB_true_parameters}.
#' @return
#' An object of class \code{RprobitB_data}, which is a list of
#' \itemize{
#'   \item the arguments of this functions as elements, and
#'   \item the element \code{covs}, which is a data frame of the two
#'   columns \code{name} (the covariate names) and \code{random} (a boolean
#'   determining whether the covariates are random effects).
#' }

RprobitB_data = function(data, choice_data, N, T, J, P_f, P_r, alternatives,
                         form, re, vars, ASC, standardize, simulated, distr,
                         true_parameter){

  ### check inputs
  stopifnot(is.list(data))
  stopifnot(is.list(vars), length(vars)==3)
  stopifnot(is.numeric(N), N%%1 == 0)
  stopifnot(is.numeric(T), T%%1 == 0)
  stopifnot(is.numeric(J), J%%1 == 0)
  stopifnot(is.numeric(P_f), P_f%%1 == 0)
  stopifnot(is.numeric(P_r), P_r%%1 == 0)
  stopifnot(is.character(alternatives) || J != length(alternatives))
  stopifnot(inherits(form,"formula"))
  stopifnot(is.null(re) || is.character(re))
  stopifnot(is.logical(simulated))
  stopifnot(is.logical(ASC))
  stopifnot(is.null(true_parameter) || inherits(true_parameter,"RprobitB_true_parameter"))

  ### create data frame of covariate names
  cov_names = colnames(data[[1]][["X"]][[1]])
  covs = data.frame("names" = colnames(data[[1]][["X"]][[1]]),
                    "random" = FALSE)
  covs[which(gsub("_.*$","",cov_names) %in% re),"random"] = TRUE

  ### create and return object of class "RprobitB_data"
  out = list("data"           = data,
             "choice_data"    = choice_data,
             "N"              = N,
             "T"              = T,
             "J"              = J,
             "P_f"            = P_f,
             "P_r"            = P_r,
             "alternatives"   = alternatives,
             "covs"           = covs,
             "form"           = form,
             "re"             = re,
             "vars"           = vars,
             "ASC"            = ASC,
             "standardize"    = standardize,
             "simulated"      = simulated,
             "distr"          = distr,
             "true_parameter" = true_parameter)
  class(out) = "RprobitB_data"
  return(out)
}

#' Create object of class \code{RprobitB_model}.
#' @description
#' This function creates an object of class \code{RprobitB_model}.
#' @inheritParams fit
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @return
#' An object of class \code{RprobitB_model}, i.e. a list with the arguments of
#' this function as elements.

RprobitB_model = function(data, normalization, R, B, Q, latent_classes, prior,
                          gibbs_samples) {

  ### check inputs
  stopifnot(inherits(data,"RprobitB_data"))
  stopifnot(inherits(normalization,"RprobitB_normalization"))
  stopifnot(is.numeric(R), R%%1 == 0, R>0)
  stopifnot(is.numeric(B), B%%1 == 0, B>0)
  stopifnot(is.numeric(Q), Q%%1 == 0, Q>0)
  stopifnot(inherits(latent_classes,"RprobitB_latent_classes"))
  stopifnot(inherits(prior,"RprobitB_prior"))
  stopifnot(inherits(gibbs_samples,"RprobitB_gibbs_samples"))

  ### create and return object of class "RprobitB_model"
  out = list("data"           = data,
             "normalization"  = normalization,
             "R"              = R,
             "B"              = B,
             "Q"              = Q,
             "latent_classes" = latent_classes,
             "prior"          = prior,
             "gibbs_samples"  = gibbs_samples)
  class(out) = "RprobitB_model"
  return(out)
}

#' Create object of class \code{RprobitB_normalization}.
#' @description
#' This function creates an object of class \code{RprobitB_normalization}.
#' @details
#' Any choice model has to be normalized with respect to level and scale.
#' \itemize{
#'   \item Level normalization: The package \code{RprobitB} takes utility
#'         differences with respect to the last alternative \code{J}.
#'   \item Scale normalization: Some model parameter has to be fixed. Per
#'         default, the first error-term variance is fixed to \code{1}, i.e.
#'         \code{scale = list("parameter" = "s", "index" = 1, "value" = 1)}.
#'         Alternatively, any error-term variance or any linear coefficient can
#'         be fixed.
#' }
#' @inheritParams RprobitB_data
#' @param level
#' The number of the alternative with respect which utility differences are
#' computed. Currently, only \code{level = J} (i.e. utility differences with
#' respect to the last alternative) is implemented.
#' @param scale
#' A named list of three elements, determining the parameter normalization with
#' respect to the utility scale:
#' \itemize{
#'   \item \code{parameter}:
#'   Either \code{"a"} (for a linear coefficient of \code{"alpha"}) or
#'   \code{"s"} (for a variance of the error-term covariance matrix
#'   \code{"Sigma"}).
#'   \item \code{index}:
#'   The index of the parameter that gets fixed.
#'   \item \code{value}:
#'   The value for the fixed parameter.
#' }
#' @examples
#' RprobitB_normalization(scale = NULL, P_f = 2, J = 3)
#' @return
#' An object of class \code{RprobitB_normalization}, which is a list of the
#' elements \code{level} and \code{scale}.

RprobitB_normalization = function(J, P_f, level = J,
                                  scale = list("parameter" = "s", "index" = 1,
                                               "value" = 1)) {

  ### check 'level' and 'scale' based on 'J' and 'P_f' and set default values
  if(level != J)
    stop("'level' must be equal to 'J'.")
  if(is.null(scale))
    scale = list()
  if(!is.list(scale))
    stop("'scale' must be a list")
  if(is.null(scale[["parameter"]]))
    scale[["parameter"]] = "s"
  if(!scale[["parameter"]] %in% c("s","a"))
    stop("'scale$parameter' must be one of 'a' or 's'.")
  if(is.null(scale[["index"]]))
    scale[["index"]] = 1
  if(scale[["parameter"]] == "a"){
    if(P_f == 0)
      stop("Cannot use 'alpha' for normalization because the model has no fixed coefficients.")
    if(!scale[["index"]] %in% seq_len(P_f))
      stop("'scale$index' is out of bound.")
  }
  if(scale[["parameter"]] == "s" &&
     !scale[["index"]] %in% seq_len(J-1))
    stop("'scale$index' is out of bound.")
  if(is.null(scale[["value"]]))
    scale[["value"]] = 1
  if(!is.numeric(scale[["value"]]) || length(scale[["value"]])!=1 ||
     scale[["value"]] == 0)
    stop("'scale$value' must be a single numeric value not equal to zero.")
  if(scale[["parameter"]] == "s" && scale[["value"]] < 0)
    stop("'scale$value' must be non-negative.")

  ### create and return object of class 'RprobitB_normalization'
  out = list(level = level,
             scale = scale)
  class(out) = "RprobitB_normalization"
  return(out)

}

#' Create object of class \code{RprobitB_true_parameter}.
#' @description
#' This function creates an object of class \code{RprobitB_true_parameter}.
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
#' @param Sigma_diff
#' The differenced error term covariance matrix of dimension
#' \code{J-1} x \code{J-1} with respect to alternative \code{J}. If \code{Sigma}
#' is specified, \code{Sigma_diff} is ignored.
#' @param beta
#' The matrix of the decision-maker specific coefficient vectors of dimension
#' \code{P_r} x \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param z
#' The vector of the allocation variables of length \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param seed
#' Set a seed for sampling missing parameters.
#' @return
#' An object of class \code{RprobitB_true_parameter}, i.e. a named list with the
#' model parameters \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{Sigma_diff}, \code{beta}, and \code{z}.

RprobitB_true_parameter = function(P_f, P_r, J, N, alpha = NULL, C = NULL,
                                   s = NULL, b = NULL, Omega = NULL,
                                   Sigma = NULL, Sigma_diff = NULL, beta = NULL,
                                   z = NULL, seed = NULL) {

  ### seed for sampling missing parameters
  if(!is.null(seed))
    set.seed(seed)

  ### function that checks if input is a proper covariance matrix
  is_covariance_matrix  = function(x){
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
      s = round(sort(as.vector(rdirichlet(rep(1,C))), decreasing = TRUE),2)
    if(length(s)!=C || !is.numeric(s) || sum(s)!=1 || is.unsorted(rev(s)))
      stop("'s' must be a non-ascending numeric vector of length ", C, " which sums up to 1.")

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
      if(!is_covariance_matrix (matrix(Omega[,c],nrow=P_r,ncol=P_r)))
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
  if(!is_covariance_matrix (Sigma))
    stop("'Sigma' is not a proper covariance matrix.")

  ### build and return 'RprobitB_true_parameter'-object
  out = list("alpha" = alpha,
             "C" = C,
             "s" = s,
             "b" = b,
             "Omega" = Omega,
             "Sigma" = Sigma,
             "Sigma_diff" = Sigma,
             "beta" = beta,
             "z" = z)
  class(out) = "RprobitB_true_parameter"
  return(out)
}

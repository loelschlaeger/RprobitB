#' Check \code{distr}
#' @description
#' Function that checks the input \code{distr}.
#' @param distr
#' A named list of number generation functions from which the covariates are
#' drawn. Each element of \code{distr} must be of the form
#' \code{"cov" = list("name" = "<name of the number generation function>", ...)},
#' where \code{cov} is the name of the covariate and \code{...} are required
#' parameters for the number generation function.
#' Covariates for which no distribution is specified are drawn from a standard
#' normal distribution.
#' Possible number generation functions are
#' \itemize{
#'   \item functions of the type \code{r*} from base R (e.g. \code{rnorm}) where
#'         all required parameters (except for \code{n}) must be specified,
#'   \item the function \code{sample}, where all required parameters
#'         (except for \code{size}) must be specified.
#' }
#' @examples
#' check_distr(distr = list("price_bus" = list("name"= "rgamma", shape = 1),
#'                          "cars" = list("name" = "sample", x = 0:2,
#'                                        replace = TRUE)))
#' @return
#' The checked input \code{distr}

check_distr = function(distr) {

  ### check if 'distr' is a list
  if(!is.list(distr))
    stop("'distr' must be a list.")

  ### check if 'distr' has proper elements
  for(i in seq_len(length(distr))){

    ### set 'size' and 'n' arguments to 1
    if(distr[[i]][["name"]] == "sample"){
      distr[[i]][["size"]] = 1
    } else if(grepl("^r",distr[[i]][["name"]])) {
      distr[[i]][["n"]] = 1
    } else {
      stop(
        paste0("The 'name' of element '", names(distr)[i],
               "' in 'distr' is not a valid number generation function name."))
    }

    ### check if element in 'distr' gives single numeric draw
    out = try(do.call(what = distr[[i]][["name"]],
                      args = distr[[i]][names(distr[[i]]) != "name"]),
              silent = TRUE)
    if(inherits(out,"try-error") || length(out) != 1 || !is.numeric(out))
      stop(paste0("Could not interpret element '", names(distr)[i],
                  "' in 'distr'."))
  }

  ### add class to 'distr'
  class(distr) = "RprobitB_distr"

  ### return checked 'distr'
  return(distr)

}

#' Check \code{form}.
#' @description
#' This function checks the input \code{form}.
#' @param form
#' A formula object that is used to specify the probit model.
#' The structure is \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{A} are names of alternative and choice situation specific
#'   covariates with a generic coefficient,
#'   \item \code{B} are names of choice situation specific covariates with
#'   alternative specific coefficients,
#'   \item and \code{C} are names of alternative and choice situation specific
#'   covariates with alternative specific coefficients.
#' }
#' Separate multiple covariates of one type by a \code{+} sign.
#' By default, alternative specific constants (ASCs) are added to the model
#' (for all except for the last alternative).
#' They can be removed by adding \code{+0} in the second spot.
#' See the vignette \code{vignette("data_management", package = "RprobitB")}
#' for more details.
#' @param re
#' A character (vector) of covariates of \code{form} with random effects.
#' If \code{re = NULL} (the default), there are no random effects.
#' To have random effects for the alternative specific constants, include
#' \code{"ASC"} in \code{re}.
#' @return
#' \itemize{
#'   \item \code{choice}:
#'   The dependent variable in \code{form}.
#'   \item \code{re}:
#'   The input \code{re}, where covariates that are not part of \code{form}
#'   are removed.
#'   \item \code{vars}:
#'   A list of three character vectors of covariate names of the three
#'   covariate types.
#'   \item \code{ASC}:
#'   A boolean, determining whether the model has ASCs.
#' }

check_form = function(form, re = NULL) {

  ### check inputs
  if(!inherits(form,"formula"))
    stop("'form' must be of class 'formula'.")
  if(!is.null(re))
    if(!is.character(re))
      stop("'re' must be a character (vector).")

  ### extract name of depentend variable
  choice = all.vars(form)[1]

  ### build 'vars'
  vars = trimws(strsplit(as.character(form)[3], split="|", fixed = TRUE)[[1]])
  while(length(vars)<3)
    vars = c(vars,NA)
  vars = lapply(strsplit(vars, split="+", fixed=TRUE), trimws)

  ### build 'ASC'
  ASC = ifelse(any(vars[[2]] %in% 0), FALSE, TRUE)
  for(i in 1:3)
    vars[[i]] = vars[[i]][!vars[[i]] %in% c(0,1,NA)]

  ### match 're' with 'form'
  if(!is.null(re))
    for(re_element in re)
      if(!re_element %in% c("ASC",unlist(vars))){
        re = setdiff(re, re_element)
        warning("The covariate '",re_element,
                "' in 're' is not part of 'form' and hence ignored.")
      }

  ### return
  out = list("choice" = choice,
             "re" = re,
             "vars" = vars,
             "ASC" = ASC)
  return(out)

}

#' Check \code{latent_classes}
#' @description
#' Function that checks the input \code{latent_classes} and sets missing values
#' to default values.
#' @param latent_classes
#' A list of parameters specifying the number and the updating scheme of latent
#' classes:
#' \itemize{
#'   \item \code{C}:
#'   The number (greater or equal 1) of latent classes. Set to 1 per default
#'   and is ignored if \code{P_r = 0}.
#'   \item \code{update}:
#'   A boolean, determining whether to update \code{C}. Ignored if
#'   \code{P_r = 0}. If \code{update = FALSE}, all of the following elements are
#'   ignored.
#'   \item \code{Cmax}:
#'   The maximum number of latent classes.
#'   \item \code{buffer}:
#'   The updating buffer (number of iterations to wait before the next update).
#'   \item \code{epsmin}:
#'   The threshold weight for removing latent classes (between 0 and 1).
#'   \item \code{epsmax}:
#'   The threshold weight for splitting latent classes (between 0 and 1).
#'   \item \code{distmin}:
#'   The threshold difference in means for joining latent classes
#'   (non-negative).
#' }
#' @examples
#' check_latent_classes(latent_classes = NULL)
#' check_latent_classes(latent_classes = list("C" = 2, "update" = TRUE,
#'                                            "Cmax" = 10, "buffer" = 100,
#'                                            "epsmin" = 0.01, "epsmax" = 0.99,
#'                                            "distmin" = 0.1))
#' @return
#' The checked input \code{latent_classes}

check_latent_classes = function(latent_classes){

  ### check if 'latent_classes' is a list
  if(!is.null(latent_classes)){
    if(!is.list(latent_classes))
      stop("'latent_classes' must be either 'NULL' or a list.")
  } else {
    latent_classes = list()
  }

  ### set default number of latent classes
  if(is.null(latent_classes[["C"]])) latent_classes[["C"]] = 1

  ### determine whether latent classes should be updated
  latent_classes$update =
    ifelse(is.na(latent_classes$update) || !is.logical(latent_classes$update),
           FALSE,latent_classes$update)

  if(!latent_classes$update){
    ### remove other parameters if 'latent_classes$update = FALSE'
    latent_classes = list("C" = latent_classes[["C"]], "update" = FALSE)

  } else {
    ### set missing parameters to default values
    if(is.null(latent_classes[["Cmax"]])) latent_classes[["Cmax"]] = 10
    if(is.null(latent_classes[["buffer"]])) latent_classes[["buffer"]] = 100
    if(is.null(latent_classes[["epsmin"]])) latent_classes[["epsmin"]] = 0.01
    if(is.null(latent_classes[["epsmax"]])) latent_classes[["epsmax"]] = 0.09
    if(is.null(latent_classes[["distmin"]])) latent_classes[["distmin"]] = 0.1

    ### remove redundant parameters
    names = c("C","update","Cmax","buffer","epsmin","epsmax","distmin")
    latent_classes[!names(latent_classes) %in% names] = NULL
  }

  ### check 'latent_classes'
  if(latent_classes$update){
    if(!is.numeric(latent_classes$C) || !latent_classes$C%%1 == 0 ||
       !latent_classes$C>0)
      stop("'latent_classes$C' must be a positive integer.")
    if(!is.numeric(latent_classes$Cmax) || !latent_classes$Cmax%%1 == 0 ||
       !latent_classes$Cmax>0 || !latent_classes$C <= latent_classes$Cmax)
      stop("'latent_classes$Cmax' must be a positive integer greater than
           'latent_classes$0'.")
    if(!is.numeric(latent_classes$buffer) || !latent_classes$buffer%%1 == 0 ||
       !latent_classes$buffer>0)
      stop("'latent_classes$buffer' must be a positive integer.")
    if(!is.numeric(latent_classes$epsmin) || !latent_classes$epsmin <= 1 ||
       !latent_classes$epsmin >= 0)
      stop("'latent_classes$epsmin' must be a numeric between 0 and 1.")
    if(!is.numeric(latent_classes$epsmax) || !latent_classes$epsmax <= 1 ||
       !latent_classes$epsmax >= 0 ||
       !latent_classes$epsmin < latent_classes$epsmax)
      stop("'latent_classes$epsmax' must be a numeric between 0 and 1 and
           greater than 'latent_classes$epsmin'.")
    if(!is.numeric(latent_classes$distmin) || !0<=latent_classes$distmin)
      stop("'latent_classes$distmin' must be a non-negative numeric value.")
  }

  ### add class to 'latent_classes'
  class(latent_classes) = "RprobitB_latent_classes"

  ### return latent_classes
  return(latent_classes)
}

#' Check \code{parm}.
#' @description
#' Function that checks \code{parm} and draws missing parameter values.
#' @param parm
#' A named list of true parameter values:
#' \itemize{
#'   \item \code{alpha}:
#'   The fixed coefficient vector of length \code{P_f}.
#'   \item \code{C:}
#'   The number (greater or equal 1) of latent classes of decision makers.
#'   If \code{P_r = 0}, then \code{C} is ignored.
#'   \item \code{s}:
#'   The vector of class weights of length \code{C}.
#'   \item \code{b}:
#'   The matrix of class means as columns of dimension \code{P_r} x \code{C}.
#'   \item \code{Omega}:
#'   The matrix of class covariance matrices as columns of dimension
#'   \code{P_r*P_r} x \code{C}.
#'   \item \code{Sigma}:
#'   The error term covariance matrix of dimension \code{J} x \code{J}.
#'   Internally, \code{Sigma} gets differenced with respect to alternative
#'   \code{J}, so it becomes a covariance matrix of dimension
#'   \code{J-1} x \code{J-1}.
#' }
#' @inheritParams RprobitB_data
#' @examples
#' check_parm(parm = list("alpha" = 1:3), P_f = 3, P_r = 1, J = 2)
#' @return
#' The checked input \code{parm}

check_parm = function(parm, P_f, P_r, J){

  ### define function that checks if input is a proper covariance matrix
  is.covariance.matrix = function(x){
    is.matrix(x) && ncol(x)==nrow(x) && isSymmetric(x) && all(eigen(x)$value>=0)
  }

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

  ### C
  if(P_r==0){
    parm$C = NA
  } else {
    if(!is.null(parm$C)){
      if(!is.numeric(parm$C) || !parm$C%%1 == 0 || !parm$C>0)
        stop("'C' must be a number greater or equal 1.")
    } else {
      parm$C = 1
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
      parm$s = round(sort(as.vector(rdirichlet(rep(1,parm$C)))),2)
    if(length(parm$s)!=parm$C || !is.numeric(parm$s) || sum(parm$s)!=1)
      stop("'s' must be a numeric vector of length 'C' which sums up to 1.")

    ### b
    if(is.null(parm$b)){
      parm$b = matrix(0,nrow=P_r,ncol=parm$C)
      for(c in 1:parm$C) parm$b[,c] = round(runif(P_r,-3,3),1)
    }
    parm$b = as.matrix(parm$b)
    if(!is.numeric(parm$b) || nrow(parm$b)!=P_r || ncol(parm$b)!=parm$C)
      stop("'b' must be a numeric matrix of dimension P_r x C.")

    ### Omega
    if(is.null(parm$Omega)){
      parm$Omega = matrix(0,nrow=P_r*P_r,ncol=parm$C)
      for(c in 1:parm$C)
        parm$Omega[,c] = as.vector(rwishart(P_r,diag(P_r))$W)
    }
    parm$Omega = as.matrix(parm$Omega)
    if(!is.numeric(parm$Omega) || nrow(parm$Omega)!=P_r*P_r ||
       ncol(parm$Omega)!=parm$C)
      stop("'Omega' must be a numeric matrix of dimension P_r*P_r x C.")
    for(c in 1:parm$C)
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
  stopifnot(c("alpha","C","s","b","Omega","Sigma") %in% names(parm))

  ### add class to 'parm'
  class(parm) = "RprobitB_parm"

  ### return 'parm'
  return(parm)
}

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
#' @examples
#' check_prior(prior = NULL, P_f = 1, P_r = 1, J = 2)
#' @return
#' The checked input \code{prior}

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

#' Check \code{scale}
#' @description
#' Function that checks the input \code{scale}.
#' @details
#' Per default, the first error-term variance is fixed to \code{1}, i.e.
#' \code{scale = list("parameter" = "s", "index" = 1, "value" = 1)}. Note that
#' you can set \code{"parameter" = "a"} only if the model has parameters with a
#' fixed coefficient (i.e. \code{P_f>0}).
#' @inheritParams RprobitB_data
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
#' check_scale(scale = NULL, P_f = 2, J = 3)
#' @return
#' The checked input \code{scale}

check_scale = function(scale, P_f, J) {

  ### check 'scale' based on 'P_f' and 'J' and set default values
  if(!is.null(scale))
    if(!is.list(scale))
      stop("'scale' must be a list")
  if(is.null(scale[["parameter"]]))
    scale[["parameter"]] = "s"
  if(!scale[["parameter"]] %in% c("s","a"))
    stop("'scale$parameter' must be one of 'a' or 's'.")
  if(is.null(scale[["index"]]))
    scale[["index"]] = 1
  if(scale[["parameter"]] == "a" &&
     !scale[["index"]] %in% seq_len(P_f))
    stop("'scale$index' is out of bound.")
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

  ### add class to 'scale'
  class(scale) = "RprobitB_scale"

  ### return checked 'scale'
  return(scale)

}

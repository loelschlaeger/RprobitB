#' Prepare empirical choice data
#' @description
#' Function that prepares empirical choice data for estimation.
#' @details
#' Please see the vignette "How to work with empirical choice data?" for more
#' details on how the specify \code{data_raw} and \code{form}.
#' @param data_raw
#' A data frame of choice data in "wide" format.
#' Must contain columns named "id" (a unique identifier for each decision maker)
#' and "choice" (the chosen alternatives).
#' @param form
#' A formula object that is used to specify the probit model.
#' The structure is \code{choice ~ A | B | C}, where
#' \code{A} are alternative (and choice situation) specific covariate with generic coefficient,
#' \code{B} are choice situation specific covariate with alternative specific coefficient,
#' and \code{C} are alternative and choice situation specific covariate with alternative specific coefficient.
#' By default, alternative specific constants are added to the model. They can be removed by adding \code{+1} in the second part.
#' @param re
#' A character vector of variable names of \code{form} which are considered to have random effects.
#' @param scale
#' If \code{scale = TRUE}, covariates get z-standardized.
#' @return
#' A list of transformed data, ready to be submitted to \link[RprobitB]{fit_mnp}.
#' @examples
#' data_raw = data.frame(id = rep(1:10,each=10),
#'                       choice = sample(c("A","B","C"),size=100,replace=TRUE),
#'                       cost_A = runif(100),
#'                       cost_B = runif(100),
#'                       cost_C = runif(100),
#'                       income = runif(100),
#'                       travel_time_A = runif(100),
#'                       travel_time_B = runif(100),
#'                       travel_time_C = runif(100))
#' form = choice ~ cost | income + 1 | travel_time
#' re = c("cost","ASC")
#' data = prepare_data(data_raw = data_raw, form = form, re = re)
#' @export

prepare_data = function(data_raw, form, re, scale = FALSE) {

  ### check input
  if(!is.data.frame(data_raw))
    stop("'data_raw' must be a data.frame.")
  if(!inherits(form,"formula"))
    stop("'form' must be a formula.")
  if(!is.character(re))
    stop("'re' must be a character (vector).")
  if(!is.logical(scale))
    stop("'scale' must be a boolean.")

  ### read formula
  vars = trimws(strsplit(as.character(form)[3], split="|", fixed = TRUE)[[1]])
  while(length(vars)<3) vars = c(vars,0)
  vars = lapply(strsplit(vars, split="+", fixed=TRUE), trimws)
  ASC = ifelse(any(allvars[[2]] %in% 0), FALSE, TRUE)
  for(i in 1:3) vars[[i]] = vars[[i]][!vars[[i]] %in% c(0,1)]
  message("Variables: ",paste(unlist(vars),collapse=", "))
  message("ASC: ", ASC)

  ### check formula
  if(!all.vars(form)[1]=="choice")
    stop("The dependent variable in 'form' must be named 'choice'.")
  if(!all(re %in% c("ASC",unlist(vars))))
    stop("The following elements in 're' are no columns in 'data_raw': ", paste(re[!(re %in% c("ASC",unlist(allvars)))],collapse = ", "))

  ### check data_raw
  if(is.null(data_raw[["id"]]))
    stop("Column 'id' not found.")
  if(is.null(data_raw[["choice"]]))
    stop("Column 'choice' not found.")
  alternatives = unique(data_raw$choice)
  for(var in vars[[2]])
    if(!var %in% names(data_raw))
      stop(paste0("Column '",var,"' not found in data_raw."))
  for(var in c(vars[[1]],vars[[3]]))
    for(alternative in alternatives)
      if(!paste0(var,"_",alternative) %in% names(data_raw))
        stop(paste0("Column '",paste0(var,"_",alternative),"' not found in 'data_raw'."))

  ### make choice variable numeric
  message("Alternatives: ",paste(levels(as.factor(data_raw$choice)),collapse=", "))
  data_raw$choice = as.numeric(as.factor(data_raw$choice))
  message("Internally transformed to: ",paste(as.numeric(levels(as.factor(data_raw$choice))),collapse=", "))

  ### standardize covariates
  if(scale){
    for(var in vars[[2]])
      data_raw[,var] = scale(data_raw[,var])
    for(var in c(vars[[1]],vars[[3]]))
      for(alternative in alternatives)
        data_raw[,paste0(var,"_",alternative)] = (data_raw[,paste0(var,"_",alternative)] - mean(unlist(data_raw[,paste0(var,"_",alternative)]),na.rm = TRUE)) / sd(unlist(data_raw[,paste0(var,"_",alternative)]),na.rm=TRUE)
    message("Standardized covariates.")
  }

  ### compute number of decision makers and choice occasions
  N = length(unique(data_raw$id))
  T = as.numeric(table(data_raw$id))
  message("Number of decision makers: ",N)
  if(length(unique(T))==1) message("Number of choice occasions per decision makers: ",T[1])
  if(length(unique(T))>1) message("Number of choice occasions per decision makers: ",min(T)," to ",max(T))

  ### transform data
  data = list()
  for(n in 1:N){
    data[[n]] = list()
    data_n = data_raw[data_raw$id == n,]
    X_n = list()

    for(t in 1:nrow(data_n)){
      data_nt = data_n[t,]
      X_nt = matrix(0,nrow=J,ncol=P)
      for(j in 1:J) for(p in 1:P) X_nt[j,p] = data_nt[,paste0(co_vars[p],"_",alternatives[j])]
      colnames(X_nt) = co_vars

      ### order covariates
      X_nt = X_nt[,cov_ord]

      ### save in list
      X_n[[t]] = X_nt
    }

    data[[n]][["X"]] = X_n
    data[[n]][["y"]] = data_n$choice
  }

  ### return transformed data
  return(data)
}

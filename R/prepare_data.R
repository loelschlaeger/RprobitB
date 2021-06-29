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
#' \code{A} are alternative (and choice situation) specific covariates with a generic coefficient,
#' \code{B} are choice situation specific covariates with alternative specific coefficients,
#' and \code{C} are alternative and choice situation specific covariates with alternative specific coefficients.
#' By default, alternative specific constants are added to the model. They can be removed by adding \code{+0} in the second spot.
#' @param re
#' A character vector of variable names of \code{form} which are considered to have random effects.
#' @param standardize
#' A character vector of variable names of \code{form} that get standardized.
#' @return
#' A list of transformed data, ready to be submitted to \link[RprobitB]{fit_mnp}.
#' @examples
#' data_raw = data.frame(id = rep(1:10,each=10),
#'                       choice = sample(c("car","bike","train"),size=100,replace=TRUE),
#'                       cost_car = runif(100),
#'                       cost_bike = runif(100),
#'                       cost_train = runif(100),
#'                       income = runif(100),
#'                       travel_time_car = runif(100),
#'                       travel_time_bike = runif(100),
#'                       travel_time_train = runif(100))
#' form = choice ~ cost | income | travel_time
#' re = c("cost","ASC")
#' standardize = c("cost","income","travel_time")
#' data = prepare_data(data_raw = data_raw, form = form, re = re, standardize = standardize)
#' @export

prepare_data = function(data_raw, form, re, standardize = NULL) {

  ### initialization message
  seperator = paste0(rep("-",42),collapse="")
  cat(seperator,"\n")
  cat("Prepare data for RprobitB::fit_mnp().\n")
  cat(seperator,"\n")

  ### check input
  if(!is.data.frame(data_raw))
    stop("'data_raw' must be a data.frame.")
  if(is.null(data_raw[["id"]]))
    stop("Column 'id' not found in 'data_raw'.")
  if(is.null(data_raw[["choice"]]))
    stop("Column 'choice' not found in 'data_raw'.")
  if(!inherits(form,"formula"))
    stop("'form' must be a formula.")
  if(!is.character(re))
    stop("'re' must be a character (vector).")
  if(!is.character(standardize))
    stop("'standardize' must be a character (vector).")

  ### check if any data point is NA or infinite
  for(col in 1:ncol(data_raw))
    if(any(is.na(data_raw[,col])|is.infinite(data_raw[,col])))
      stop(paste0("Please remove NAs or infinite values in column '",colnames(data_raw)[col],"'."))

  ### compute number of decision makers and choice occasions
  N = length(unique(data_raw$id))
  T = as.numeric(table(data_raw$id))
  cat("observations:","\n-",N,ifelse(N==1,"decision maker","decision makers"),"\n")
  if(length(unique(T))==1) cat("-",T[1],ifelse(unique(T)==1,"choice occasion","choice occasions"),ifelse(N==1,"","each"),"\n")
  if(length(unique(T))>1) cat("-",min(T),"to",max(T),"choice occasions",ifelse(N==1,"","each"),"\n")
  cat(seperator,"\n")

  ### read formula
  vars = trimws(strsplit(as.character(form)[3], split="|", fixed = TRUE)[[1]])
  while(length(vars)<3) vars = c(vars,NA)
  vars = lapply(strsplit(vars, split="+", fixed=TRUE), trimws)
  ASC = ifelse(any(vars[[2]] %in% 0), FALSE, TRUE)
  for(i in 1:3) vars[[i]] = vars[[i]][!vars[[i]] %in% c(0,1,NA)]
  cat("covariates:\n")
  for(type in 1:3){
    for(var in vars[[type]]){
      cat("-",var)
      cat(" (")
      cat(paste(c(paste0("t",type),if(var %in% re){"re"},if(var %in% standardize){"z"}),collapse=", "))
      cat(")\n")
    }
  }
  if(ASC){
    cat("- ASC",if("ASC" %in% re){"(re)\n"})
  }
  cat(seperator,"\n")

  ### check formula
  if(!all.vars(form)[1]=="choice")
    stop("The dependent variable in 'form' must be named 'choice'.")
  if(!all(re %in% c("ASC",unlist(vars))))
    stop("The following elements in 're' are no columns in 'data_raw': ", paste(re[!(re %in% c("ASC",unlist(vars)))],collapse = ", "))

  ### identify, sort and transform alternatives
  alternatives = unique(data_raw[["choice"]])
  alternatives = sort(alternatives)
  cat("alternatives:","\n")
  for(i in 1:length(alternatives)){
    cat("-",alternatives[i],paste(c("(",i,")"),collapse=""),"\n")
    data_raw[["choice"]][data_raw[["choice"]]==alternatives[i]] = i
  }
  data_raw[["choice"]] = as.numeric(data_raw[["choice"]])
  cat(seperator,"\n")

  ### check data_raw
  for(var in vars[[2]])
    if(!var %in% names(data_raw))
      stop(paste0("Column '",var,"' not found in data_raw."))
  for(var in c(vars[[1]],vars[[3]]))
    for(alternative in alternatives)
      if(!paste0(var,"_",alternative) %in% names(data_raw))
        stop(paste0("Column '",paste0(var,"_",alternative),"' not found in 'data_raw'."))

  ### standardize covariates
  for(var in vars[[2]])
    if(var %in% standardize)
      data_raw[,var] = (data_raw[,var] - mean(data_raw[,var])) / sd(data_raw[,var])
  for(var in c(vars[[1]],vars[[3]]))
    if(var %in% standardize)
      for(alternative in alternatives)
        data_raw[,paste0(var,"_",alternative)] =
          (data_raw[,paste0(var,"_",alternative)] - mean(unlist(data_raw[,paste0(var,"_",alternatives)]))) / sd(unlist(data_raw[,paste0(var,"_",alternatives)]))

  ### transform data
  data = list()
  for(n in 1:N){
    data[[n]] = list()
    data_n = data_raw[data_raw$id == n,]
    X_n = list()

    for(t in 1:nrow(data_n)){
      data_nt = data_n[t,]
      X_nt = matrix(NA, nrow = length(alternatives), ncol = 0)

      ### type-1 covariates
      for(var in vars[[1]]){
        old_names = colnames(X_nt)
        col = numeric(length(alternatives))
        for(alternative in 1:length(alternatives))
          col[alternative] = data_nt[,paste0(var,"_",alternatives[alternative])]
        ### put covariates with random effects at the end
        if(var %in% re){
          X_nt = cbind(X_nt,col)
          colnames(X_nt) = c(old_names,var)
        } else {
          X_nt = cbind(col,X_nt)
          colnames(X_nt) = c(var,old_names)
        }
      }

      ### type-2 covariates
      for(var in vars[[2]]){
        old_names = colnames(X_nt)
        mat = matrix(0,length(alternatives),length(alternatives))
        for(alternative in 1:length(alternatives))
          mat[alternative,alternative] = data_nt[,var]
        ### put covariates with random effects at the end
        if(var %in% re){
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0(var,"_",alternatives))
        } else {
          X_nt = cbind(mat,X_nt)
          colnames(X_nt) = c(paste0(var,"_",alternatives),old_names)
        }
      }

      ### type-3 covariates
      for(var in vars[[3]]){
        old_names = colnames(X_nt)
        mat = matrix(0,length(alternatives),length(alternatives))
        for(alternative in 1:length(alternatives))
          mat[alternative,alternative] = data_nt[,paste0(var,"_",alternatives[alternative])]
        ### put covariates with random effects at the end
        if(var %in% re){
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0(var,"_",alternatives))
        } else {
          X_nt = cbind(mat,X_nt)
          colnames(X_nt) = c(paste0(var,"_",alternatives),old_names)
        }
      }

      ### ASC (for all but the last alternative)
      if(ASC){
        old_names = colnames(X_nt)
        mat = diag(length(alternatives))[,-length(alternatives)]
        ### put covariates with random effects at the end
        if("ASC" %in% re){
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0("ASC_",alternatives[-length(alternatives)]))
        } else {
          X_nt = cbind(mat,X_nt)
          colnames(X_nt) = c(paste0("ASC_",alternatives[-length(alternatives)]),old_names)
        }
      }

      ### save in list
      X_n[[t]] = X_nt
    }

    data[[n]][["X"]] = X_n
    data[[n]][["y"]] = data_n[["choice"]]
  }

  ### add attributes to data
  attr(data,"RprobitB_data") = TRUE
  split =
    length(intersect(re,vars[[1]])) * 1 +
    length(intersect(re,vars[[2]])) * length(alternatives) +
    length(intersect(re,vars[[3]])) * length(alternatives) +
    ("ASC" %in% re) * (length(alternatives) - 1)
  cov_names = colnames(data[[1]][["X"]][[1]])
  attr(data,"cov_fixed") = cov_names[seq_len(length(cov_names)-split)]
  attr(data,"cov_random") = tail(cov_names,split)

  ### return transformed data
  return(data)
}

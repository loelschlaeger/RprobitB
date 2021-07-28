#' Prepare empirical choice data
#' @description
#' Function that prepares empirical choice data for the RprobitB package.
#' @details
#' For more details see the vignette "How to work with empirical choice data?":
#' \code{vignette("How to work with empirical choice data?",
#' package = "RprobitB")}
#' @param choice_data
#' A data frame of choice data in "wide" format (i.e. one row for each choice
#' situation).
#' It must contain columns named \code{id} (a unique identifier for each
#' decision maker) and \code{choice} (the chosen alternatives), where
#' \code{choice} is the name of the dependent variable in \code{form}.
#' For each alternative specific covariate *p* in \code{form} and each choice
#' alternative *j* (i.e. each unique element in \code{choice_data[,choice]}),
#' \code{choice_data} must contain a column named *p_j*.
#' For each covariate *q* that is constant across covariates, \code{choice_data}
#' must contain a column named *q*.
#' @param re
#' A character vector of variable names of \code{form} which are considered to
#' have random effects. To have random effects for the alternative specific
#' constants, include \code{"ASC"} in \code{re}.
#' @param id
#' A character, the name of the column in \code{choice_data} that contains
#' unique identifier for each decision maker. The default is \code{"id"}.
#' @inheritParams RprobitB_data
#' @return
#' An object of class \code{RprobitB_data}
#' @examples
#' form = choice ~ cost | income | travel_time
#' choice_data = data.frame("id" = paste("decider", rep(1:10,each=10), sep="_"),
#'                          "choice" = sample(c("car","bike","train"), size=100,
#'                                            replace=TRUE),
#'                          "cost_car" = runif(n=100, min=5, max=8),
#'                          "cost_bike" = runif(n=100, min=0, max=1),
#'                          "cost_train" = runif(n=100, min=2, max=10),
#'                          "income" = rep(sample(10)*1e4,each=10),
#'                          "travel_time_car" = runif(n=100, min=10, max=30),
#'                          "travel_time_bike" = runif(n=100, min=20, max=40),
#'                          "travel_time_train" = runif(n=100, min=5, max=20))
#' re = c("cost","ASC")
#' standardize = c("cost","income","travel_time")
#' data = prepare(form = form, choice_data = choice_data, re = re,
#'                standardize = standardize)
#' @export

prepare = function(form, choice_data, re = NULL, id = "id", standardize = NULL){

  ### check input
  if(!inherits(form,"formula"))
    stop("'form' must be of class 'formula'.")
  choice = all.vars(form)[1]
  if(!is.data.frame(choice_data))
    stop("'choice_data' must be a data frame.")
  if(!id %in% colnames(choice_data))
    stop(paste0("Identification column '",id,"' not found in 'choice_data'."))
  if(!choice %in% colnames(choice_data))
    stop(paste0("Choice column '",choice,"' not found in 'choice_data'."))
  if(!is.null(re))
    if(!is.character(re))
      stop("'re' must be a character (vector).")
  if(!is.null(standardize))
    if(!is.character(standardize))
      stop("'standardize' must be a character (vector).")

  ### check if any data point is NA or infinite
  for(col in 1:ncol(choice_data))
    if(any(is.na(choice_data[,col]) | is.infinite(choice_data[,col]) |
           is.nan(choice_data[,col])))
      stop(paste0("Please remove NAs, NaNs or infinite values in column '",
                  colnames(choice_data)[col],"'."))

  ### convert decision maker ids to numeric
  choice_data[,id] = as.numeric(factor(choice_data[,id],
                                       levels=unique(choice_data[,id])))

  ### compute number of decision makers and choice occasions
  N = length(unique(choice_data[,id]))
  T = as.numeric(table(choice_data[,id]))

  ### read formula
  vars = trimws(strsplit(as.character(form)[3], split="|", fixed = TRUE)[[1]])
  while(length(vars)<3) vars = c(vars,NA)
  vars = lapply(strsplit(vars, split="+", fixed=TRUE), trimws)
  ASC = ifelse(any(vars[[2]] %in% 0), FALSE, TRUE)
  for(i in 1:3) vars[[i]] = vars[[i]][!vars[[i]] %in% c(0,1,NA)]

  ### check elements of form, re and standardize
  if(!all(re %in% c("ASC",unlist(vars))))
    stop("The following elements in 're' are not part of 'form': ",
         paste(re[!(re %in% c("ASC",unlist(vars)))],collapse = ", "))
  if(!all(standardize %in% unlist(vars)))
    stop("The following elements in 'standardize' are not part of 'form': ",
         paste(standardize[!(standardize %in% unlist(vars))],collapse = ", "))

  ### identify, sort and transform alternatives
  alternatives = unique(choice_data[[choice]])
  alternatives = sort(alternatives)
  J = length(alternatives)

  ### decode alternatives to numerics (sorted alphabetically)
  for(i in 1:J)
    choice_data[[choice]][choice_data[[choice]]==alternatives[i]] = i
  choice_data[[choice]] = as.numeric(choice_data[[choice]])

  ### check choice_data
  for(var in vars[[2]])
    if(!var %in% names(choice_data))
      stop(paste0("Column '",var,"' not found in choice_data."))
  for(var in c(vars[[1]],vars[[3]]))
    for(alternative in alternatives)
      if(!paste0(var,"_",alternative) %in% names(choice_data))
        stop(paste0("Column '",paste0(var,"_",alternative),"' not found in
                    'choice_data'."))

  ### standardize covariates
  for(var in vars[[2]])
    if(var %in% standardize)
      choice_data[,var] = scale(choice_data[,var])
  for(var in c(vars[[1]],vars[[3]]))
    if(var %in% standardize)
      for(alternative in alternatives)
        choice_data[,paste0(var,"_",alternative)] =
          (choice_data[,paste0(var,"_",alternative)] -
             mean(unlist(choice_data[,paste0(var,"_",alternatives)]))) /
               sd(unlist(choice_data[,paste0(var,"_",alternatives)]))

  ### transform choice_data in list format
  data = list()
  for(n in 1:N){
    data[[n]] = list()
    data_n = choice_data[choice_data[,id] == n,]
    X_n = list()

    for(t in 1:T[n]){
      data_nt = data_n[t,]
      X_nt = matrix(NA, nrow = J, ncol = 0)

      ### type-1 covariates
      for(var in vars[[1]]){
        old_names = colnames(X_nt)
        col = numeric(J)
        for(alternative in 1:J)
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
        mat = matrix(0,J,J)[,-J]
        for(alternative in 1:(J-1))
          mat[alternative,alternative] = data_nt[,var]
        ### put covariates with random effects at the end
        if(var %in% re){
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0(var,"_",alternatives[1:(J-1)]))
        } else {
          X_nt = cbind(mat,X_nt)
          colnames(X_nt) = c(paste0(var,"_",alternatives[1:(J-1)]),old_names)
        }
      }

      ### type-3 covariates
      for(var in vars[[3]]){
        old_names = colnames(X_nt)
        mat = matrix(0,J,J)
        for(alternative in 1:J)
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
        mat = diag(J)[,-J]
        ### put covariates with random effects at the end
        if("ASC" %in% re){
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0("ASC_",alternatives[-J]))
        } else {
          X_nt = cbind(mat,X_nt)
          colnames(X_nt) = c(paste0("ASC_",alternatives[-J]),old_names)
        }
      }

      ### save in list
      X_n[[t]] = X_nt
    }

    data[[n]][["X"]] = X_n
    data[[n]][["y"]] = data_n[[choice]]
  }

  ### create cov_fix and cov_random
  cov_names = colnames(data[[1]][["X"]][[1]])
  cov_fix = cov_names[which(!gsub("_.*$","",cov_names) %in% re)]
  cov_random = cov_names[which(gsub("_.*$","",cov_names) %in% re)]

  ### create RprobitB_data object
  out = RprobitB_data(data         = data,
                      N            = N,
                      T            = T,
                      J            = J,
                      P_f          = length(cov_fix),
                      P_r          = length(cov_random),
                      C            = NA,
                      alternatives = alternatives,
                      cov_fix      = cov_fix,
                      cov_random   = cov_random,
                      form         = form,
                      vars         = vars,
                      ASC          = ASC,
                      standardize  = standardize,
                      simulated    = FALSE,
                      parm         = NULL,
                      distr        = NULL)

  ### return RprobitB_data object
  return(out)
}

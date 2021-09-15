#' Prepare empirical choice data.
#' @description
#' This function prepares empirical choice data for the RprobitB package.
#' @details
#' For more details see the vignette "Data management":
#' \code{vignette("data_management", package = "RprobitB")}.
#' @param choice_data
#' A data frame of choice data in "wide" format (i.e. each row represents
#' one choice occasion) with the following requirements:
#' \itemize{
#'   \item It must contain columns named \code{id} (a unique identifier for each
#'         decision maker) and \code{choice} (the chosen alternatives), where
#'         \code{choice} is the name of the dependent variable in \code{form}.
#'   \item For each alternative specific covariate *p* (covariate of type 1 or
#'         3) in \code{form} and each choice alternative *j* in
#'         \code{alternatives}, \code{choice_data} must contain a column named
#'         *p_j*.
#'   \item For each covariate *q* in \code{form} that is constant across
#'         covariates (covariate of type 2), \code{choice_data} must contain a
#'         column named *q*.
#' }
#' @param id
#' A character, the name of the column in \code{choice_data} that contains
#' unique identifier for each decision maker. The default is \code{"id"}.
#' @inheritParams RprobitB_data
#' @return
#' An object of class \code{RprobitB_data}.
#' @examples
#' data("Train", package = "mlogit")
#' data = prepare(form = choice ~ price | 0 | time + comfort + change,
#'                choice_data = Train,
#'                re = c("price","time"),
#'                standardize = "all")
#' @export

prepare = function(form, choice_data, alternatives = NULL, re = NULL, id = "id",
                   standardize = NULL){

  ### check 'form'
  check_form_out = check_form(form = form, re = re)
  choice = check_form_out$choice
  re = check_form_out$re
  vars = check_form_out$vars
  ASC = check_form_out$ASC

  ### check other inputs
  if(!is.data.frame(choice_data))
    stop("'choice_data' must be a data frame.")
  if(!(is.character(id) && length(id) == 1))
    stop("'id' must be a character.")
  if(!id %in% colnames(choice_data))
    stop(paste0("Identification column '",id,"' not found in 'choice_data'."))
  if(!choice %in% colnames(choice_data))
    stop(paste0("Choice column '",choice,"' not found in 'choice_data'."))
  if(!is.null(alternatives))
    if(!is.character(alternatives))
      stop("'alternatives' must be a character (vector).")
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

  ### identify / filter, sort and count alternatives
  if(is.null(alternatives)){
    alternatives = as.character(unique(choice_data[[choice]]))
  } else {
    choice_data = choice_data[choice_data[[choice]] %in% alternatives,]
    if(nrow(choice_data)==0)
      stop(paste("No choices for", paste(alternatives, collapse=", "),"found."))
  }
  alternatives = sort(alternatives)
  J = length(alternatives)
  if(J <= 1)
    stop("At least two alternatives are required.")

  ### compute number of linear coefficients
  P = compute_number_of_linear_coefficients(vars = vars, ASC = ASC, J = J,
                                            re = re)
  P_f = P$P_f
  P_r = P$P_r

  ### compute number of decision makers and choice occasions
  N = length(unique(choice_data[,id]))
  T = as.numeric(table(choice_data[,id]))

  ### decode choices to numeric (sorted alphabetically)
  choice_data[[choice]] = as.character(choice_data[[choice]])
  for(i in 1:J)
    choice_data[[choice]][choice_data[[choice]]==alternatives[i]] = i
  choice_data[[choice]] = as.numeric(choice_data[[choice]])

  ### add ASCs (for all but the last alternative)
  if(ASC)
    choice_data$ASC = 1

  ### if 'standardize = all', add all covariates
  if(length(standardize) == 1)
    if(standardize == "all")
      standardize =
    c(apply(expand.grid(vars[[1]], alternatives), 1, paste, collapse="_"),
      vars[[2]],
      apply(expand.grid(vars[[3]], alternatives), 1, paste, collapse="_"))

  ### ASCs do not get standardized
  if("ASC" %in% standardize)
    standardize = standardize[-which(standardize == "ASC")]

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
  for(var in c(vars[[1]],vars[[3]])){
    for(alternative in alternatives){
      var_alt = paste0(var,"_",alternative)
      if(var_alt %in% standardize){
        choice_data[,var_alt] = scale(choice_data[,var_alt])
      }
    }
  }

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
      for(var in c(vars[[2]],if(ASC)"ASC")){
        old_names = colnames(X_nt)
        mat = matrix(0,J,J)[,-J,drop=FALSE]
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
          mat[alternative,alternative] =
          data_nt[,paste0(var,"_",alternatives[alternative])]
        ### put covariates with random effects at the end
        if(var %in% re){
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0(var,"_",alternatives))
        } else {
          X_nt = cbind(mat,X_nt)
          colnames(X_nt) = c(paste0(var,"_",alternatives),old_names)
        }
      }

      ### save in list
      X_n[[t]] = X_nt
    }

    data[[n]][["X"]] = X_n
    data[[n]][["y"]] = data_n[[choice]]
  }

  ### create RprobitB_data object
  out = RprobitB_data(data         = data,
                      choice_data  = choice_data,
                      N            = N,
                      T            = T,
                      J            = J,
                      P_f          = P_f,
                      P_r          = P_r,
                      alternatives = alternatives,
                      form         = form,
                      re           = re,
                      vars         = vars,
                      ASC          = ASC,
                      standardize  = standardize,
                      simulated    = FALSE,
                      parm         = NULL,
                      distr        = NULL)

  ### return RprobitB_data object
  return(out)
}

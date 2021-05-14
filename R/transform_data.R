#' Data transformation
#' @description Function that transforms empricial data for estimation.
#' @details \code{data_raw} must contain columns named "id" (unique identifier for each decision maker)
#'  and "choice" (the chosen alternatives).
#' @param data_raw A data frame of choice data in "wide" format.
#' @param cov_col A numeric vector, columns of data_raw with covariates.
#' @param cov_ord A character vector, order of covariates, where fixed-coefficient covariates come first.
#' @param cov_zst A boolean, if \code{TRUE} covariates get z-standardized.
#' @return A list of transformed data.

transform_data = function(data_raw,cov_col,cov_ord,cov_zst) {
  
  ### check input
  stopifnot(is.data.frame(data_raw))
  stopifnot(is.vector(cov_col))
  stopifnot(is.vector(cov_ord))
  stopifnot(is.logical(cov_zst))
  
  ### check columns
  if(is.null(data_raw$id)) stop("Column 'id' not found.")
  if(is.null(data_raw$choice)) stop("Column 'choice' not found.")
  choice_vars = unique(data_raw$choice)
  J = length(choice_vars)
  co_vars = unique(sub("_.+","",names(data_raw[cov_col])))
  stopifnot(co_vars %in% cov_ord, cov_ord %in% co_vars)
  P = length(co_vars)
  for(co_var in co_vars) for(choice_var in choice_vars) if(!paste0(co_var,"_",choice_var) %in% names(data_raw[cov_col])) stop(paste("Column '",paste0(co_var,"_",choice_var),"' not found."))
  
  ### standardise covariates
  if(cov_zst) for(co_var in co_vars) for(choice_var in choice_vars) data_raw[,paste0(co_var,"_",choice_var)] = (data_raw[,paste0(co_var,"_",choice_var)] - mean(unlist(data_raw[,paste0(co_var,"_",choice_vars)]),na.rm = TRUE)) / sd(unlist(data_raw[,paste0(co_var,"_",choice_vars)]),na.rm=TRUE)
  
  ### make choice numeric
  data_raw$choice = as.numeric(data_raw$choice)
  
  ### compute number of decision makers and choice occasions
  N = length(unique(data_raw$id))
  T = as.numeric(table(data_raw$id))
  
  ### transform data
  data = list()
  for(n in 1:N){
    data[[n]] = list()
    data_n = data_raw[data_raw$id == n,]
    X_n = list()
    
    for(t in 1:nrow(data_n)){
      data_nt = data_n[t,]
      X_nt = matrix(0,nrow=J,ncol=P)
      for(j in 1:J) for(p in 1:P) X_nt[j,p] = data_nt[,paste0(co_vars[p],"_",choice_vars[j])]
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
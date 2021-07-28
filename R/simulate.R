#' Simulate choice data
#' @description
#' Function that simulates choice data for the RprobitB package.
#' @details
#' For more details see the vignette "How to simulate choice data?":
#' \code{vignette("How to simulate choice data?", package = "RprobitB")}
#' @inheritParams RprobitB_data
#' @inheritParams check_parm
#' @inheritParams check_distr
#' @return
#' An object of class \code{RprobitB_data}
#' @examples
#' form = choice ~ var1 | var2
#' N = 10
#' T = 1:10
#' J = 3
#' C = 2
#' re = c("ASC")
#' alternatives = c("A","B","C")
#' parm = list("s" = c(0.5,0.5))
#' distr = list("rgamma" = list(shape = 1),
#'              "sample" = list(x = 1:10, replace = TRUE))
#' standardize = "var1"
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
  if(is.null(alternatives))
    alternatives = LETTERS[1:J]
  if(length(alternatives) != J || !is.character(alternatives))
    stop("'alternatives' must be a character (vector) of length 'J'.")
  if(!is.null(parm))
    if(!is.list(parm))
      stop("'parm' must be a list.")
  if(!is.null(distr))
    if(!is.list(distr))
      stop("'distr' must be a list.")
  if(!is.null(standardize))
    if(!is.character(standardize))
      stop("'standardize' must be a character (vector).")

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

  ### check distr
  distr = check_distr(distr = distr, no_cov = length(unlist(vars)))

  ### determine numbers P_f and P_r
  P_f_plus_P_r = length(vars[[1]]) + (length(vars[[2]]) + ASC) * (J-1) + length(vars[[3]]) * J
  P_r = sum(re %in% vars[[1]]) +
    (sum(re %in% vars[[2]]) + "ASC" %in% re) * (J-1) +
    sum(re %in% vars[[3]]) * J
  P_f = P_f_plus_P_r - P_r

  ### draw covariates
  choice_data = data.frame("id" = rep(1:N,times=T))
  distr_i = 0
  for(var in vars[[1]]){
    distr_i = distr_i + 1
    for(alternative in alternatives)
      choice_data[,paste(var,alternative,sep="_")] =
    replicate(do.call(names(distr)[distr_i], distr[[distr_i]]), n = sum(T))
  }
  for(var in vars[[2]]){
    distr_i = distr_i + 1
    choice_data[,var] =
      replicate(do.call(names(distr)[distr_i], distr[[distr_i]]), n = sum(T))
  }
  for(var in vars[[3]]){
    distr_i = distr_i + 1
    for(alternative in alternatives)
      choice_data[,paste(var,alternative,sep="_")] =
        replicate(do.call(names(distr)[distr_i], distr[[distr_i]]), n = sum(T))
  }

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

  ### check and read parm
  parm = check_parm(parm = parm, P_f = P_f, P_r = P_r, J = J, C = C)

  ### draw additional model parameters and save them in 'parm'
  if(P_r>0){

    ### draw allocation variable
    z = sample(1:C,N,prob=parm$s,replace=TRUE)

    ### compute class sizes
    m = as.numeric(table(z))

    ### draw beta
    beta = matrix(0,nrow=P_r,ncol=N)
    for(n in seq_len(N))
      beta[,n] = parm$b[,z[n]] +
      t(chol(matrix(parm$Omega[,z[n]],nrow=P_r,ncol=P_r))) %*% rnorm(P_r)

  } else{
    beta = NA
    z = NA
    m = NA
  }
  parm$beta = beta
  parm$z = z
  parm$m = m

  ### compute lower-triangular Choleski root of Sigma
  L = t(chol(parm$Sigma))

  ### allocate space for data and utilities
  data = list()
  U = matrix(0,nrow=J,ncol=sum(T))

  ### simulate choices
  for(n in seq_len(N)){

    ### extract data for each decision maker
    data[[n]] = list()
    data[[n]][["X"]] = list()
    data_n = choice_data[choice_data[,"id"] == n,]
    y_n = numeric(T[n])

    for(t in seq_len(T[n])){

      ### extract data for each choice occasion
      data_nt = data_n[t,]

      ### sort covariates
      {
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
        data[[n]][["X"]][[t]] = X_nt
      }

      ### build coefficient vector
      if(P_f>0 & P_r>0) coeff = c(parm$alpha,parm$beta[,n])
      if(P_f>0 & P_r==0) coeff = parm$alpha
      if(P_f==0 & P_r>0) coeff = parm$beta[,n]
      if(P_f==0 & P_r==0) coeff = NA

      ### compute utility and choice decision
      eps = L %*% rnorm(J)
      if(P_f==0 & P_r==0){
        U_nt = eps
      } else {
        V_nt = X_nt %*% coeff
        U_nt = V_nt + eps
      }
      U[,sum(T[seq_len(n-1)])+t] = U_nt
      y_n[t] = which.max(U_nt)
    }

    data[[n]][["y"]] = y_n
  }

  ### add 'U' and 'beta' to 'parm'
  parm$U = U
  parm$beta = beta

  ### create cov_fix and cov_random
  cov_names = colnames(data[[1]][["X"]][[1]])
  cov_fix = cov_names[which(!gsub("_.*$","",cov_names) %in% re)]
  cov_random = cov_names[which(gsub("_.*$","",cov_names) %in% re)]

  ### create RprobitB_data object
  out = RprobitB_data(data         = data,
                      N            = N,
                      T            = T,
                      J            = J,
                      P_f          = P_f,
                      P_r          = P_r,
                      C            = C,
                      alternatives = alternatives,
                      cov_fix      = cov_fix,
                      cov_random   = cov_random,
                      form         = form,
                      vars         = vars,
                      ASC          = ASC,
                      standardize  = standardize,
                      simulated    = TRUE,
                      parm         = parm,
                      distr        = distr)

  ### return RprobitB_data object
  return(out)
}

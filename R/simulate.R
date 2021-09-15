#' Simulate choice data.
#' @description
#' This function simulates choice data for the RprobitB package.
#' @details
#' For more details see the vignette "Data management":
#' \code{vignette("data_management", package = "RprobitB")}.
#' @inheritParams RprobitB_data
#' @inheritParams check_distr
#' @param seed
#' Set a seed for the simulation.
#' @param ...
#' Optionally specify \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{beta}, or \code{z} for the simulation.
#' @return
#' An object of class \code{RprobitB_data}.
#' @examples
#' form = choice ~ cost | income | travel_time
#' re = "cost"
#' N = 100
#' T = 10
#' J = 3
#' alternatives = c("car", "bus", "train")
#' distr = list("cost" = list("name" = "rnorm", sd = 3),
#'              "income" = list("name" = "sample", x = (1:10)*1e3, replace = T),
#'              "travel_time_car" = list("name" = "rlnorm", meanlog = 1),
#'              "travel_time_bus" = list("name" = "rlnorm", meanlog = 2))
#' standardize = c("income", "travel_time_car", "travel_time_bus",
#'                 "travel_time_train")
#' data = simulate(form = form, N = N, T = T, J = J, re = re,
#'                 alternatives = alternatives, distr = distr,
#'                 standardize = standardize, C = 2, s = c(0.5,0.5))
#' @export

simulate = function(form, N, T, J, re = NULL, alternatives = NULL,
                    distr = NULL, standardize = NULL, seed = NULL, ...) {

  ### check 'form'
  check_form_out = check_form(form = form, re = re)
  choice = check_form_out$choice
  re = check_form_out$re
  vars = check_form_out$vars
  ASC = check_form_out$ASC

  ### check other inputs
  if(!is.numeric(N) || N%%1!=0)
    stop("'N' must be a non-negative number.")
  if(length(T)==1)
    T = rep(T,N)
  if(any(!is.numeric(T)) || any(T%%1!=0))
    stop("'T' must be a non-negative number or a vector of non-negative numbers.")
  if(!is.numeric(J) || J%%1!=0 || !J>=2)
    stop("'J' must be a number greater or equal 2.")
  if(is.null(alternatives))
    alternatives = LETTERS[1:J]
  if(length(alternatives) != J || !is.character(alternatives))
    stop("'alternatives' must be a character (vector) of length 'J'.")
  if(!is.null(distr))
    distr = check_distr(distr = distr)
  if(!is.null(standardize))
    if(!is.character(standardize))
      stop("'standardize' must be a character (vector).")

  ### draw covariates
  if(!is.null(seed))
    set.seed(seed)
  choice_data = data.frame("id" = rep(1:N,times=T))
  for(var in c(vars[[1]],vars[[3]])){
    if(var %in% names(distr)){
      distr_i = distr[[which(names(distr) == var)]]
      cov = matrix(replicate(do.call(what = distr_i[["name"]],
                                     args = distr_i[names(distr_i) != "name"]),
                             n = sum(T)*J),
                   nrow = sum(T), ncol = J)
    } else {
      cov = matrix(rnorm(n = sum(T)*J), nrow = sum(T), ncol = J)
    }
    colnames(cov) = paste(var,alternatives,sep="_")
    for(alternative in alternatives){
      if(paste(var,alternative,sep="_") %in% names(distr)){
        distr_i = distr[[which(names(distr) == paste(var,alternative,sep="_"))]]
        cov[,paste(var,alternative,sep="_")] =
          replicate(do.call(what = distr_i[["name"]],
                            args = distr_i[names(distr_i) != "name"]),
                    n = sum(T))
      }
    }
    choice_data = cbind(choice_data, cov)
  }
  for(var in vars[[2]]){
    if(var %in% names(distr)){
      distr_i = distr[[which(names(distr) == var)]]
      cov = matrix(replicate(do.call(what = distr_i[["name"]],
                                     args = distr_i[names(distr_i) != "name"]),
                             n = sum(T)),
                   nrow = sum(T), ncol = 1)
    } else {
      cov = matrix(rnorm(n = sum(T)), nrow = sum(T), ncol = 1)
    }
    colnames(cov) = var
    choice_data = cbind(choice_data, cov)
  }

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

  ### compute number of linear coefficients
  P = compute_number_of_linear_coefficients(vars = vars, ASC = ASC, J = J,
                                            re = re)
  P_f = P$P_f
  P_r = P$P_r

  ### check supplied and draw missing model parameters
  true_parameter = do.call(what = RprobitB_parameter,
                           args = c(list("P_f" = P_f, "P_r" = P_r,
                                         "J" = J, "N" = N, "seed" = seed),
                                    list(...)))

  ### compute lower-triangular Choleski root of Sigma
  L = t(chol(true_parameter$Sigma))

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
            col[alternative] =
            data_nt[,paste0(var,"_",alternatives[alternative])]
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
        data[[n]][["X"]][[t]] = X_nt
      }

      ### build coefficient vector
      if(P_f>0 & P_r>0)
        coeff = c(true_parameter$alpha, true_parameter$beta[,n])
      if(P_f>0 & P_r==0)
        coeff = true_parameter$alpha
      if(P_f==0 & P_r>0)
        coeff = true_parameter$beta[,n]
      if(P_f==0 & P_r==0)
        coeff = NA

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

  ### define difference operator (computes differences wrt alternative i)
  Delta = function(i){
    Delta = diag(J)[-J,,drop=FALSE]; Delta[,i] = -1
    return(Delta)
  }

  ### difference 'Sigma' in 'true_parameter'
  true_parameter$Sigma = Delta(J) %*% true_parameter$Sigma %*% t(Delta(J))

  ### create RprobitB_data object
  out = RprobitB_data(data           = data,
                      choice_data    = choice_data,
                      N              = N,
                      T              = T,
                      J              = J,
                      P_f            = P_f,
                      P_r            = P_r,
                      alternatives   = alternatives,
                      form           = form,
                      re             = re,
                      vars           = vars,
                      ASC            = ASC,
                      standardize    = standardize,
                      simulated      = TRUE,
                      distr          = distr,
                      true_parameter = true_parameter)

  ### return 'RprobitB_data'-object
  return(out)
}

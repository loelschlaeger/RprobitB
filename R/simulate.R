#' Simulate choice data.
#' @description
#' This function simulates choice data for the RprobitB package.
#' @details
#' See the vignette "Data management" for more details:
#' \code{vignette("data_management", package = "RprobitB")}.
#' @inheritParams RprobitB_data
#' @inheritParams check_distr
#' @inheritParams check_form
#' @param seed
#' Set a seed for the simulation.
#' @param ...
#' Optionally specify \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{Sigma_full}, \code{beta}, or \code{z} for the simulation.
#' @inheritParams prepare
#' @return
#' An object of class \code{RprobitB_data}.
#' If \code{test_prop} is specified, a list of two \code{RprobitB_data} objects
#' labelled \code{"train"} and \code{"test"}.
#' @examples
#' data = simulate(form = choice ~ cost | income + 0 | time,
#'                 N = 100, T = 10, J = 3, re = "cost",
#'                 alternatives = c("car", "bus", "scooter"))
#' @export

simulate = function(form, N, T, J, re = NULL, alternatives = NULL, distr = NULL,
                    standardize = NULL, seed = NULL, test_prop = NULL, ...) {

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
    stop("'T' must be non-negative or a vector of non-negative numbers.")
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

  ### sort alternatives
  alternatives = sort(alternatives)

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
    for(j in alternatives){
      if(paste(var,j,sep="_") %in% names(distr)){
        distr_i = distr[[which(names(distr) == paste(var,j,sep="_"))]]
        cov[,paste(var,j,sep="_")] =
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
      for(j in alternatives){
        var_alt = paste0(var,"_",j)
        if(var_alt %in% standardize){
          choice_data[,var_alt] = scale(choice_data[,var_alt])
        }
      }
  }

  ### determine number and names of linear coefficients
  linear_coeffs = overview_effects(form, re, alternatives)
  P_f = sum(linear_coeffs$re == FALSE)
  P_r = sum(linear_coeffs$re == TRUE)
  linear_coeffs_names = linear_coeffs$name

  ### check supplied and draw missing model parameters
  true_parameter = do.call(what = RprobitB_parameter,
                           args = c(list("P_f" = P_f, "P_r" = P_r,
                                         "J" = J, "N" = N, "seed" = seed),
                                    list(...)))

  ### compute lower-triangular Choleski root of 'Sigma_full'
  L = suppressWarnings(t(chol(true_parameter$Sigma_full, pivot = TRUE)))

  ### check if 'choice_data' is to be splitted in train and test set
  out = list()
  if(is.null(test_prop)){
    split = FALSE
    choice_data = list(choice_data)
  } else {
    if(!(is.numeric(test_prop) && length(test_prop) == 1 && test_prop <= 1 &&
         test_prop >= 0))
      stop("'test_prop' must be a numeric between 0 and 1.")
    split = TRUE
    cutoff = round(length(unique(choice_data[,"id"])) * (1-test_prop))
    choice_data = split(choice_data, choice_data$id > cutoff)
  }

  ### transform 'choice_data' in list format 'data'
  for(b in seq_len(ifelse(split,2,1))){

    data = list()
    ids = unique(choice_data[[b]][,"id"])
    N = length(ids)
    T = as.numeric(table(choice_data[[b]][,"id"]))

    ### simulate choices
    for(n in seq_len(N)){

      ### extract data for each decision maker
      data[[n]] = list()
      data[[n]][["X"]] = list()
      data_n = choice_data[[b]][choice_data[[b]][,"id"] == ids[n],]
      y_n = numeric(T[n])

      for(t in seq_len(T[n])){

        ### extract data for each choice occasion
        data_nt = data_n[t,]
        X_nt = matrix(NA, nrow = J, ncol = 0)

        ### type-1 covariates
        for(var in vars[[1]]){
          old_names = colnames(X_nt)
          col = numeric(J)
          for(j in 1:J)
            col[j] = data_nt[,paste0(var,"_",alternatives[j])]
          X_nt = cbind(X_nt,col)
          colnames(X_nt) = c(old_names,var)
        }

        ### type-2 covariates
        for(var in c(vars[[2]],if(ASC)"ASC")){
          old_names = colnames(X_nt)
          mat = matrix(0,J,J)[,-J,drop=FALSE]
          for(j in 1:(J-1))
            mat[j,j] = data_nt[,var]
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0(var,"_",alternatives[1:(J-1)]))
        }

        ### type-3 covariates
        for(var in vars[[3]]){
          old_names = colnames(X_nt)
          mat = matrix(0,J,J)
          for(j in 1:J)
            mat[j,j] = data_nt[,paste0(var,"_",alternatives[j])]
          X_nt = cbind(X_nt,mat)
          colnames(X_nt) = c(old_names,paste0(var,"_",alternatives))
        }

        ### sort covariates
        X_nt = X_nt[,linear_coeffs_names, drop = FALSE]

        ### save in list
        data[[n]][["X"]][[t]] = X_nt

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
        y_n[t] = which.max(U_nt)
      }

      data[[n]][["y"]] = y_n
    }

    ### save choices in 'choice_data'
    choice_data[[b]]["choice"] = unlist(lapply(data, function(x) x[["y"]]))

    ### create output
    out[[b]] = RprobitB_data(data             = data,
                             choice_data      = choice_data[[b]],
                             N                = N,
                             T                = T,
                             J                = J,
                             P_f              = P_f,
                             P_r              = P_r,
                             alternatives     = alternatives,
                             form             = form,
                             re               = re,
                             ASC              = ASC,
                             linear_coeffs    = linear_coeffs,
                             standardize      = standardize,
                             simulated        = TRUE,
                             choice_available = TRUE,
                             true_parameter   = true_parameter)
  }

  ### return 'RprobitB_data' object
  if(split){
    names(out) = c("train","test")
    return(out)
  } else {
    return(out[[1]])
  }
}

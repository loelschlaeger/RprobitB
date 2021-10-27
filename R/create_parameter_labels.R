#' Create parameters labels.
#' @description
#' This function creates labels for the model parameters.
#' @inheritParams RprobitB_data
#' @param C
#' The number of latent classes.
#' @param cov_sym
#' If \code{TRUE} considers symmetric covariance matrix elements.
#' @param keep_par
#' A vector of parameter names which are kept.
#' @param drop_par
#' A vector of parameter names which get dropped.
#' @return
#' A list of labels for the model parameters.
#' @keywords
#' internal

create_parameter_labels = function(P_f, P_r, J, C, cov_sym,
                                   keep_par = c("s","alpha","b","Omega","Sigma"),
                                   drop_par = NULL){

  ### check that 'C' is a number if 'P_r>0'
  if(P_r>0) if(!(is.numeric(C) && C%%1 == 0 && C>=1))
      stop("'C' must be a number greater or equal 1.")

  ### create and return list of labels
  labels = list("s" = create_labels_s(P_r, C),
                "alpha" = create_labels_alpha(P_f),
                "b" = create_labels_b(P_r, C),
                "Omega" = create_labels_Omega(P_r, C, cov_sym),
                "Sigma" = create_labels_Sigma(J, cov_sym))
  labels = labels[lengths(labels) != 0 &
                    names(labels) %in% keep_par &
                    !names(labels) %in% drop_par]
  return(labels)
}

#' Create labels for \code{alpha}.
#' @description
#' This function creates labels for the model parameter \code{alpha}.
#' @inheritParams create_parameter_labels
#' @return
#' A vector of labels for the model parameter \code{alpha}.
#' @keywords
#' internal

create_labels_alpha = function(P_f) {
  if(P_f == 0) NULL else as.character(seq_len(P_f))
}

#' Create labels for \code{s}.
#' @description
#' This function creates labels for the model parameter \code{s}.
#' @inheritParams create_parameter_labels
#' @return
#' A vector of labels for the model parameter \code{s}.
#' @keywords
#' internal

create_labels_s = function(P_r, C) {
  if(P_r == 0) NULL else as.character(seq_len(C))
}

#' Create labels for \code{b}.
#' @description
#' This function creates labels for the model parameter \code{b}.
#' @inheritParams create_parameter_labels
#' @return
#' A vector of labels for the model parameter \code{b}.
#' @keywords
#' internal

create_labels_b = function(P_r, C) {
  if(P_r == 0) NULL else paste0(as.character(rep(1:C,each=P_r)), rep(".",P_r*C),
                                as.character(rep(1:P_r,times=C)))
}

#' Create labels for \code{Omega}.
#' @description
#' This function creates labels for the model parameter \code{Omega}.
#' @inheritParams create_parameter_labels
#' @return
#' A vector of labels for the model parameter \code{Omega}.
#' @keywords
#' internal

create_labels_Omega = function(P_r, C, cov_sym) {
  if(P_r == 0){
    NULL
  } else {
    Omega_id = rep(TRUE,P_r*P_r)
    if(!cov_sym)
      Omega_id[-which(lower.tri(matrix(NA,P_r,P_r),diag=TRUE)==TRUE)] = FALSE
    Omega_id = rep(Omega_id, C)
    paste0(as.character(rep(1:C,each=P_r^2)),rep(".",P_r*C),
           as.character(rep(paste0(rep(1:P_r,each=P_r),",",
                                   rep(1:P_r,times=P_r)), times = C)))[Omega_id]
  }
}

#' Create labels for \code{Sigma}.
#' @description
#' This function creates labels for the model parameter \code{Sigma}.
#' @inheritParams create_parameter_labels
#' @return
#' A vector of labels for the model parameter \code{Sigma}.
#' @keywords
#' internal

create_labels_Sigma = function(J, cov_sym) {
  Sigma_id = rep(TRUE,(J-1)*(J-1))
  if(!cov_sym)
    Sigma_id[-which(lower.tri(matrix(NA,J-1,J-1),diag=TRUE)==TRUE)] = FALSE
  paste0(rep(1:(J-1),each=J-1),",",rep(1:(J-1),times=J-1))[Sigma_id]
}

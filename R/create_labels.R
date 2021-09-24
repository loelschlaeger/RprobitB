#' Create coefficient labels
#' @description
#' Function that creates labels for the model coefficients.
#' @inheritParams RprobitB_data
#' @param C
#' If \code{P_r>0}, the number (greater or equal 1) of latent classes.
#' Ignored otherwise.
#' @param symmetric
#' Set \code{TRUE} to create labels for symmetric matrix elements.
#' @examples
#' create_labels(P_f = 1, P_r = 2, J = 3, C = 4, symmetric = TRUE)
#' @return
#' A list of labels for the model coefficients \code{alpha}, \code{s}, \code{b},
#' \code{Omega}, and \code{Sigma}.

create_labels = function(P_f, P_r, J, C, symmetric){

  ### check that 'C' is a number if 'P_r>0'
  if(P_r>0)
    if(!(is.numeric(C) && C%%1 == 0 && C>=1))
      stop("'C' must be a number greater or equal 1.")

  ### labels for 'alpha'
  alpha_label = NA
  if(P_f>0)
    alpha_label = as.character(seq_len(P_f))

  ### labels for 's'
  s_label = NA
  if(P_r>0)
    s_label = as.character(seq_len(C))

  ### labels for 'b'
  b_label = NA
  if(P_r>0)
    b_label = paste0(as.character(rep(1:C,each=P_r)), rep(".",P_r*C),
                     as.character(rep(1:P_r,times=C)))

  ### labels for 'Omega'
  Omega_label = NA
  if(P_r>0){
    Omega_id = rep(TRUE,P_r*P_r)
    if(!symmetric)
      Omega_id[-which(lower.tri(matrix(NA,P_r,P_r),diag=TRUE)==TRUE)] = FALSE
    Omega_id = rep(Omega_id,C)
    Omega_label =
      paste0(as.character(rep(1:C,each=P_r^2)),rep(".",P_r*C),
             as.character(rep(paste0(rep(1:P_r,each=P_r),",",rep(1:P_r,times=P_r)),
                              times = C)))[Omega_id]
  }

  ### labels for 'Sigma'
  Sigma_id = rep(TRUE,(J-1)*(J-1))
  if(!symmetric)
    Sigma_id[-which(lower.tri(matrix(NA,J-1,J-1),diag=TRUE)==TRUE)] = FALSE
  Sigma_label =
    paste0(rep(1:(J-1),each=J-1),",",rep(1:(J-1),times=J-1))[Sigma_id]

  ### create 'label_list'
  labels = list("alpha" = alpha_label,
                "s"     = s_label,
                "b"     = b_label,
                "Omega" = Omega_label,
                "Sigma" = Sigma_label)

  ### return 'labels'
  return(labels)
}

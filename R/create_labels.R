#' Create coefficient labels
#' @description
#' Function that creates labels for the model coefficients.
#' @inheritParams RprobitB_data
#' @param C
#' The number (greater or equal 1) of latent classes.
#' @param C_est
#' The estimated number (greater or equal 1) of latent classes.
#' @param symmetric
#' A boolean, determining whether labels for symmetric matrix elements should be
#' created.
#' @examples
#' create_labels(P_f = 1, P_r = 2, J = 3, C = 4, C_est = 5, symmetric = TRUE)
#' @return
#' A list of labels for the model coefficients \code{alpha}, \code{s}, \code{b},
#' \code{Omega}, and \code{Sigma}.

create_labels = function(P_f, P_r, J, C, C_est, symmetric){

  ### labels for 'alpha'
  alpha_label = NA
  if(P_f>0)
    alpha_label = as.character(seq_len(P_f))

  ### labels for 's'
  s_label = NA
  if(P_r>0)
    s_label = as.character(seq_len(max(C,C_est,na.rm=TRUE)))

  ### labels for 'b'
  b_label = NA
  if(P_r>0)
    b_label = paste0(as.character(rep(1:max(C,C_est,na.rm=TRUE),each=P_r)),
                     rep(".",P_r*max(C,C_est,na.rm=TRUE)),
                     as.character(rep(1:P_r,times=max(C,C_est,na.rm=TRUE))))

  ### labels for 'Omega'
  Omega_label = NA
  if(P_r>0){
    Omega_id = rep(TRUE,P_r*P_r)
    if(!symmetric)
      Omega_id[-which(lower.tri(matrix(NA,P_r,P_r),diag=TRUE)==TRUE)] = FALSE
    Omega_id = rep(Omega_id,max(C,C_est,na.rm=TRUE))
    Omega_label =
      paste0(as.character(rep(1:max(C,C_est,na.rm=TRUE),each=P_r^2)),
             rep(".",P_r*max(C,C_est,na.rm=TRUE)),
             as.character(rep(paste0(rep(1:P_r,each=P_r),
                                     ",",rep(1:P_r,times=P_r)),
                              times=max(C,C_est,na.rm=TRUE))))[Omega_id]
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

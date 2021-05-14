#' Coefficient labels
#' @description Function that creates labels for the model coefficients. 
#' @param gibbs_samples A list of Gibbs samples.
#' @param model A list of model information.
#' @param symm A boolean, determining whether labels for symmetric matrix elements should be added.
#' @return A list of model coefficient labels.

make_labels = function(gibbs_samples,model,symm){
  alpha_label = NA
  if(model$P_f>0){
    alpha_label = seq_len(model$P_f)
  }
  s_label = NA
  b_label = NA
  Omega_label = NA
  if(model$P_r>0){
    C_range = max(model$C,gibbs_samples$C_est,na.rm=TRUE)
    s_label = seq_len(C_range)
    b_label = paste0(as.character(rep(1:C_range,each=model$P_r)),rep(".",model$P_r*C_range),as.character(rep(1:model$P_r,times=C_range)))
    Omega_id = rep(TRUE,model$P_r*model$P_r)
    if(!symm) Omega_id[-which(lower.tri(matrix(NA,model$P_r,model$P_r),diag=TRUE)==TRUE)] = FALSE
    Omega_id = rep(Omega_id,C_range)
    Omega_label = paste0(as.character(rep(1:C_range,each=model$P_r*model$P_r)),rep(".",model$P_r*C_range),as.character(rep(paste0(rep(1:model$P_r,each=model$P_r),",",rep(1:model$P_r,times=model$P_r)),times=C_range)))[Omega_id]
  }
  Sigma_id = rep(TRUE,(model$J-1)*(model$J-1))
  if(!symm) Sigma_id[-which(lower.tri(matrix(NA,model$J-1,model$J-1),diag=TRUE)==TRUE)] = FALSE
  Sigma_label = paste0(rep(1:(model$J-1),each=model$J-1),",",rep(1:(model$J-1),times=model$J-1))[Sigma_id]
  label_list = list("alpha_label" = alpha_label,
                    "s_label"     = s_label,
                    "b_label"     = b_label,
                    "Omega_label" = Omega_label,
                    "Sigma_label" = Sigma_label)
  return(label_list)
}
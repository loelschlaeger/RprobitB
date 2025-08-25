#' Check prior parameters
#'
#' @description
#' This function checks the compatibility of submitted parameters for the prior
#' distributions and sets missing values to default values.
#'
#' @inheritParams RprobitB_data
#'
#' @param mu_alpha_0 \[`numeric(P_f)`\]\cr
#' The mean vector of the normal prior for `alpha`.
#'
#' @param Sigma_alpha_0 \[`matrix(P_f, P_f)`\]\cr
#' The covariance matrix of the normal prior for `alpha`.
#'
#' @param delta \[`numeric(1)`\]\cr
#' The prior concentration for `s`.
#'
#' @param mu_b_0 \[`numeric(P_r)`\]\cr
#' The mean vector of the normal prior for each `b_c`.
#'
#' @param Sigma_b_0 \[`matrix(P_r, P_r)`\]\cr
#' The covariance matrix of the normal prior for each `b_c`.
#'
#' @param n_Omega_0 \[`integer(1)`\]\cr
#' The degrees of freedom of the Inverse Wishart prior for each `Omega_c`.
#'
#' @param V_Omega_0 \[`matrix(P_r, P_r)`\]\cr
#' The scale matrix of the Inverse Wishart prior for each `Omega_c`.
#'
#' @param n_Sigma_0 \[`integer(1)`\]\cr
#' The degrees of freedom of the Inverse Wishart prior for `Sigma`.
#'
#' @param V_Sigma_0 \[`matrix(J - 1, J - 1)`\]\cr
#' The scale matrix of the Inverse Wishart prior for `Sigma`.
#'
#' @param mu_d_0 \[`numeric(J - 2)`\]\cr
#' The mean vector of the normal prior for `d` .
#'
#' @param Sigma_d_0 \[`matrix(J - 2, J - 2)`\]\cr
#' The covariance matrix of the normal prior for `d`.
#'
#' @details
#' A priori-distributions:
#' \itemize{
#'   \item \eqn{\alpha \sim N(\mu_{\alpha_0}, \Sigma_{\alpha_0})}
#'   \item \eqn{s \sim Dir(\delta)}
#'   \item \eqn{b_c \sim N(\mu_{b_0}, \Sigma_{b_0})} for all \eqn{c}
#'   \item \eqn{\Omega_c \sim IW(n_{\Omega_0}, V_{\Omega_0})} for all \eqn{c}
#'   \item \eqn{\Sigma \sim IW(n_{\Sigma_0}, V_{\Sigma_0})}
#'   \item \eqn{d \sim N(\mu_{d_0}, \Sigma_{d_0})}
#' }
#'
#' @return
#' An object of class `RprobitB_prior`, which is a `list` containing all
#' prior parameters.
#'
#' @export
#'
#' @examples
#' check_prior(P_f = 1, P_r = 2, J = 3, ordered = TRUE)

check_prior <- function(
    P_f, P_r, J, ordered = FALSE,
    mu_alpha_0 = numeric(P_f), Sigma_alpha_0 = 10 * diag(P_f), delta = 1,
    mu_b_0 = numeric(P_r), Sigma_b_0 = 10 * diag(P_r),
    n_Omega_0 = P_r + 2, V_Omega_0 = diag(P_r),
    n_Sigma_0 = J + 1, V_Sigma_0 = diag(J - 1),
    mu_d_0 = numeric(J - 2), Sigma_d_0 = diag(J - 2)
  ) {

  ### initialize prior list
  prior <- list()

  ### check supplied values and set missing prior parameters to default values
  if (P_f > 0) {
    ### alpha ~ MVN(mu_alpha_0, Sigma_alpha_0)
    oeli::input_check_response(
      check = oeli::check_numeric_vector(mu_alpha_0, len = P_f),
      var_name = 'prior$mu_alpha_0'
    )
    oeli::input_check_response(
      check = oeli::check_covariance_matrix(Sigma_alpha_0, dim = P_f),
      var_name = "prior$Sigma_alpha_0"
    )
  } else {
    mu_alpha_0 <- NA
    Sigma_alpha_0 <- NA
  }
  if (P_r > 0) {
    ### s ~ D(delta)
    oeli::input_check_response(
      check = checkmate::check_number(delta, lower = 0, finite = TRUE),
      var_name = "prior$delta"
    )

    ### b_c ~ MVN(mu_b_0, Sigma_b_0)
    oeli::input_check_response(
      check = oeli::check_numeric_vector(mu_b_0, len = P_r),
      var_name = 'prior$mu_b_0'
    )
    oeli::input_check_response(
      check = oeli::check_covariance_matrix(Sigma_b_0, dim = P_r),
      var_name = "prior$Sigma_b_0"
    )

    ### Omega_c ~ IW(n_Omega_0, V_Omega_0)
    oeli::input_check_response(
      check = checkmate::check_int(n_Omega_0, lower = P_r),
      var_name = "prior$n_Omega_0"
    )
    oeli::input_check_response(
      check = oeli::check_covariance_matrix(V_Omega_0, dim = P_r),
      var_name = "prior$V_Omega_0"
    )
  } else {
    delta <- NA
    mu_b_0 <- NA
    Sigma_b_0 <- NA
    n_Omega_0 <- NA
    V_Omega_0 <- NA
  }

  ### Sigma ~ IW(n_Sigma_0, V_Sigma_0)
  if (ordered) {
    n_Sigma_0 <- NA
    V_Sigma_0 <- NA
  } else {
    oeli::input_check_response(
      check = checkmate::check_int(n_Sigma_0, lower = J - 1),
      var_name = "prior$n_Sigma_0"
    )
    oeli::input_check_response(
      check = oeli::check_covariance_matrix(V_Sigma_0, dim = J - 1),
      var_name = "prior$V_Sigma_0"
    )
  }

  ### d ~ N(mu_d_0, Sigma_d_0)
  if (ordered) {
    oeli::input_check_response(
      check = oeli::check_numeric_vector(mu_d_0, len = J - 2),
      var_name = 'prior$mu_d_0'
    )
    oeli::input_check_response(
      check = oeli::check_covariance_matrix(Sigma_d_0, dim = J - 2),
      var_name = "prior$Sigma_d_0"
    )
  } else {
    mu_d_0 <- NA
    Sigma_d_0 <- NA
  }

  ### build and return prior parameters
  structure(
    list(
      "mu_alpha_0" = mu_alpha_0, "Sigma_alpha_0" = Sigma_alpha_0,
      "delta" = delta, "mu_b_0" = mu_b_0, "Sigma_b_0" = Sigma_b_0,
      "n_Omega_0" = n_Omega_0, "V_Omega_0" = V_Omega_0,
      "n_Sigma_0" = n_Sigma_0, "V_Sigma_0" = V_Sigma_0,
      "mu_d_0" = mu_d_0, "Sigma_d_0" = Sigma_d_0
    ),
    class = c("RprobitB_prior", "list")
  )
}

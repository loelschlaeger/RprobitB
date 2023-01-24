# RprobitB_prior can be created and validated

    Code
      print(RprobitB_prior(formula = A ~ B, J = 4))
    Output
      Priors:
      alpha prior : Normal (conjugate)
      mean : 0 0 0 0
      Sigma : [ 10 0 0 0; 0 10 0 0; 0 0 10 0; 0 0 0 10 ]
      Sigma_diff prior : Inverse-Wishart (conjugate)
      degrees of freedom : 5
      scale : [ 1 0 0; 0 1 0; 0 0 1 ]

# RprobitB_prior_alpha can be created and validated

    Code
      RprobitB_prior_alpha(P_f = 3)
    Output
      alpha prior : Normal (conjugate)
      mean : 0 0 0
      Sigma : [ 10 0 0; 0 10 0; 0 0 10 ]

---

    Code
      RprobitB_prior_alpha(P_f = 2, alpha_prior_custom = function(x) {
        stats::dnorm(x[1]) * stats::dunif(x[2])
      })
    Output
      alpha prior : custom
      alpha ~ stats::dnorm(x[1]) * stats::dunif(x[2])

# RprobitB_prior_s can be created and validated

    Code
      RprobitB_prior_s(C = 2)
    Output
      s prior : Dirichlet (conjugate)
      concentration : 1 1

---

    Code
      RprobitB_prior_s(C = 2, s_prior_custom = function(x) {
        stats::dunif(x[1], min = 0.5, max = 1) * (x[2] == 1 - x[1])
      }, s_prior_custom_test_par = c(0.6, 0.4))
    Output
      s prior : custom
      s ~ stats::dunif(x[1], min = 0.5, max = 1) * (x[2] == 1 - x[1])

# RprobitB_prior_b can be created and validated

    Code
      RprobitB_prior_b(P_r = 3)
    Output
      b prior : Normal (conjugate)
      mean : 0 0 0
      Sigma : [ 10 0 0; 0 10 0; 0 0 10 ]

---

    Code
      RprobitB_prior_b(P_r = 2, b_prior_custom = function(x) {
        stats::dnorm(x[1]) * stats::dunif(x[2])
      })
    Output
      b prior : custom
      b ~ stats::dnorm(x[1]) * stats::dunif(x[2])

# RprobitB_prior_Omega can be created and validated

    Code
      RprobitB_prior_Omega(P_r = 2)
    Output
      Omega prior : Inverse-Wishart (conjugate)
      degrees of freedom : 4
      scale : [ 1 0; 0 1 ]

---

    Code
      RprobitB_prior_Omega(P_r = 2, Omega_prior_custom = function(x) {
        dwishart(x, df = 4, scale = diag(2), inv = TRUE) * (x[1, 1] == x[2, 2])
      })
    Output
      Omega prior : custom
      Omega ~ dwishart(x, df = 4, scale = diag(2), inv = TRUE) * (x[1, 1] == x[2, 2])

# RprobitB_prior_Sigma can be created and validated

    Code
      RprobitB_prior_Sigma(J = 4, ordered = TRUE)
    Output
      Sigma prior : Inverse-Wishart (conjugate)
      degrees of freedom : 3
      scale : 1

---

    Code
      RprobitB_prior_Sigma(ordered = TRUE, J = 4, Sigma_prior_custom = function(x)
        dunif(x))
    Output
      Sigma prior : custom
      Sigma ~ dunif(x)

# RprobitB_prior_Sigma_diff can be created and validated

    Code
      RprobitB_prior_Sigma_diff(J = 4, ordered = FALSE)
    Output
      Sigma_diff prior : Inverse-Wishart (conjugate)
      degrees of freedom : 5
      scale : [ 1 0 0; 0 1 0; 0 0 1 ]

---

    Code
      RprobitB_prior_Sigma_diff(J = 3, Sigma_diff_prior_custom = function(x) {
        dwishart(x, df = 4, scale = diag(2), inv = TRUE) * all(x[row(x) != col(x)] ==
          0)
      })
    Output
      Sigma_diff prior : custom
      Sigma_diff ~ dwishart(x, df = 4, scale = diag(2), inv = TRUE) * all(x[row(x) != col(x)...

# RprobitB_prior_d can be created and validated

    Code
      RprobitB_prior_d(J = 4, ordered = TRUE)
    Output
      d prior : Normal (conjugate)
      mean : 0 0
      Sigma : [ 10 0; 0 10 ]

---

    Code
      RprobitB_prior_d(ordered = TRUE, J = 4, d_prior_custom = function(x) {
        stats::dnorm(x[1]) * stats::dunif(x[2])
      })
    Output
      d prior : custom
      d ~ stats::dnorm(x[1]) * stats::dunif(x[2])


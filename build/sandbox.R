data <- simulate_choices(
  form = choice ~ cost,
  N = 100,
  T = 10,
  J = 3,
  re = c("cost", "ASC"),
  alternatives = c("train", "bus", "car"),
  seed = 1,
  C = 2
)
model <- mcmc(
  data, R = 2000, B = 1998, print_progress = T, seed = 1, prior = list("delta" = 1),
  latent_classes = list("C" = 8, "weight_update" = TRUE, "dp_update" = TRUE, "epsmin" = 0.1, "epsmax" = 0.9)
)

Cmax = model$Cmax
beta = model$beta
z = model$z
b = model$b
Omega = model$Omega
delta = model$delta
xi = model$xi
D = model$D
nu = model$nu
Theta = model$Theta
update_classes_dp(Cmax, beta, z, b, Omega, delta, xi, D, nu, Theta)

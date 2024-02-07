test_that("Gelman-Rubin statistic can be computed", {
  set.seed(1)
  no_chains <- 2
  length_chains <- 1e3
  samples <- matrix(NA_real_, length_chains, no_chains)
  samples[1, ] <- 1
  Gamma <- matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)
  for (c in 1:no_chains) {
    for (t in 2:length_chains) {
      samples[t, c] <- sample(1:2, 1, prob = Gamma[samples[t - 1, c], ])
    }
  }
  expect_snapshot(R_hat(samples))
})

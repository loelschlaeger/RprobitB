test_that("Sigma backtransformation works", {
  J = sample(2:10,1)
  i = sample(1:J,1)
  Sigma_full = rwishart(J,diag(J))$W
  Sigma = delta(J,i) %*% Sigma_full %*% t(delta(J,i))
  Sigma_back = undiff_Sigma(Sigma = Sigma, i = i)
  expect_equal(Sigma, delta(J,i) %*% Sigma_back %*% t(delta(J,i)))
})

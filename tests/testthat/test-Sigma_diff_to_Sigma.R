test_that("Sigma backtransformation works", {
  J = sample(2:10,1)
  Sigma = rwishart(J,diag(J))$W
  Delta = function(i){
    Delta = diag(J)[-J,,drop=FALSE]; Delta[,i] = -1
    return(Delta)
  }
  Sigma_diff = Delta(J) %*% Sigma %*% t(Delta(J))
  Sigma_back = Sigma_diff_to_Sigma(Sigma_diff = Sigma_diff)
  expect_equal(Sigma_diff, Delta(J) %*% Sigma_back %*% t(Delta(J)))
})

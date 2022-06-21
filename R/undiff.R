U <- matrix(1:4)

ranking <- 1:4

D <- RprobitB:::delta(4,2)

Dinv <- MASS::ginv(D)

Dinv %*% D %*% U

comp_M <- function(ranking) {
  J <- length(ranking)
  out <- matrix(0, nrow = J-1, ncol = J)
  for(i in 1:(J-1)) {
    out[i,ranking[i]] <- -1
    out[i,ranking[i+1]] <- 1
  }
  return(out)
}

M <- comp_M(c(3,2,4,1))

M_inv <- MASS::ginv(M)

D %*% M_inv %*% M %*% Dinv %*% D %*% U

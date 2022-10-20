library(ggplot2, dplyr, RprobitB)
set.seed(1)

# parameters
N <- 200
C <- 4
z <- sample.int(C, size = N, replace = TRUE)
b <- matrix(rnorm(2*C, mean = 0, sd = 3), ncol = C)
Omega <- matrix(rep(diag(2), C), ncol = C)
beta <- sapply(z, function(z) rmvnorm(b[,z], matrix(Omega[,z], 2, 2)))
ggplot(data = as.data.frame(t(beta)), aes(x = V1, y = V2)) +
  geom_point() +
  geom_point(data = as.data.frame(t(b)), color = "red")

# priors
xi <- numeric(2)
D <- diag(2)
nu <- 4
Theta <- diag(2)

# DP (different delta)
delta_list <- as.list(c(0.1,0.2,1/C,0.5,1,2))
R <- 200
data <- data.frame(matrix(ncol = 3, nrow = 0))
for(delta in delta_list) {
  C_seq <- numeric(R)
  z_update <- z
  b_update <- b
  Omega_update <- Omega
  for(r in 1:R) {
    update <- RprobitB:::update_classes_dp(
      Cmax = 10, beta = beta, z = z_update, b = b_update, Omega = Omega_update,
      delta = delta, xi = xi, D = D, nu = nu, Theta = Theta
    )
    z_update <- update$z
    b_update <- update$b
    Omega_update <- update$Omega
    C_seq[r] <- update$C
  }
  data <- rbind(data, data.frame(delta, r = 1:R, C = C_seq))
}

# class number
data %>% group_by(delta) %>% summarize(C_mean = median(C))

data %>% group_by(delta) %>% filter(row_number() == n())
data %>% ggplot(aes(x = r, y = C, color = as.factor(delta))) +
  geom_line(position = position_dodge(width = 0.1), alpha = 0.5) +
  scale_y_continuous() +
  geom_hline(yintercept = C, col = "green", alpha = 0.1, size = 3) +
  scale_color_brewer(palette = "YlOrRd")




set.seed(1)
beta <- datasauRus::datasaurus_dozen |>
  dplyr::filter(dataset == "bullseye") |>
  dplyr::select(x, y) |>
  as.matrix() |>
  {\(m) scale(m)}() |> t()
N <- ncol(beta)
C <- 10
epsmin <- 0.01
epsmax <- 0.7 # 0.7
deltamin <- 0.1
delta <- 1
buffer <- 5
mu_b_0 <- numeric(2)
Sigma_b_0_inv <- solve(10 * diag(2))
n_Omega_0 <- 4
V_Omega_0 <- diag(2)
z <- sample.int(C, size = N, replace = TRUE)
b <- matrix(0, nrow = 2, ncol = C)
Omega <- matrix(diag(2), nrow = 4, ncol = C)
R <- 1000
C_seq <- numeric(R)
update_type <- numeric(R)
s_samples <- list()
b_samples <- list()
Omega_samples <- list()

for (r in seq_len(R)) {
  if (r %% max(1, floor(R / 10)) == 0) cat(round(100 * r / R), "%\n")
  m <- update_m(C = C, z = z, non_zero = TRUE)
  s <- update_s(delta = delta, m = m)
  z <- update_z(s = s, beta = beta, b = b, Omega = Omega)
  b <- update_b(
    beta = beta, Omega = Omega, z = z, m = m,
    Sigma_b_0_inv = Sigma_b_0_inv, mu_b_0 = mu_b_0
  )
  Omega <- update_Omega(
    beta = beta, b = b, z = z, m = m,
    n_Omega_0 = n_Omega_0, V_Omega_0 = V_Omega_0
  )
  if (r %% buffer == 0) {
    class_update <- update_classes_wb(
      epsmin = epsmin, epsmax = epsmax, deltamin = deltamin,
      s = s, b = b, Omega = Omega, identify_classes = TRUE
    )
    list2env(class_update[c("s", "b", "Omega")], envir = environment())
    update_type[r] <- class_update$update_type
  }
  C_seq[r] <- C <- length(s)
  s_samples[[r]] <- s
  b_samples[[r]] <- b
  Omega_samples[[r]] <- Omega
}

table(C_seq)
plot(update_type, type = "l")


library(ggplot2)





dmix <- function(x, s, b, Sigma) {
  K <- ncol(b)
  sapply(1:nrow(x), function(i) {
    sum(s * sapply(1:K, function(k) {
      dmvnorm(x[i, ], mean = b[, k], sigma = matrix(Sigma[, k], nrow = 2))
    }))
  })
}

grid_x <- seq(-3, 3, length.out = 100)
grid_y <- seq(-3, 3, length.out = 100)
grid2 <- grid <- expand.grid(x = grid_x, y = grid_y)

which(update_type == 1)

# 1: 640 / 639
# 2: 784 / 785

it <- 40
C_seq[it]
C_seq[it-1]

grid2$z <- dmix(
  x = as.matrix(grid[, 1:2]), s = s_samples[[it]],
  b = b_samples[[it]], Sigma = Omega_samples[[it]]
)

C <- s_samples[[it]] |> length()

components <- lapply(1:C, function(k) {
  z <- dmvnorm(
    as.matrix(grid[, 1:2]),
    mean = b_samples[[it]][, k],
    sigma = matrix(Omega_samples[[it]][, k], nrow = 2)
  )
  cbind(grid, z = z, component = factor(k))
})

df_all <- do.call(rbind, components)

data_beta <- as.data.frame(t(beta))
colnames(data_beta) <- c("x", "y")

ggplot() +
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
  geom_point(data = data_beta, mapping = aes(x = x, y = y)) +
  geom_contour(
    data = df_all,
    aes(x = x, y = y, z = z, color = component), bins = 10, linewidth = 1.5,
    alpha = 0.7
  ) +
  geom_contour(
    data = grid2,
    aes(x = x, y = y, z = z), bins = 10, color = "black"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(x = expression(beta[1]), y = expression(beta[2]))



table(update_type)
table(C_seq)
#plot(C_seq, type = "l")









### data generating process U = Xbeta + eps, beta ~ sum(s * normal), C fix, eps ~ standard_normal
form = choice ~ cov | 0
N = 100
T = 10
J = 3
re = "cov"
data = simulate_choices(form = form, N = N, T = T, J = J, re = re, C = 3)

### estimate beta with LCMMNP approach
model = mcmc(data, latent_classes = list(C = 3))

### estimate beta with Dirichlet approach

## updated class assignments


## update mean and covariance in each class

## draw each beta_c based on class assignment, mean and covariance of class based on Bayes regression formula

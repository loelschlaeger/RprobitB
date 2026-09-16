# Update latent classes

Low-level sampler kernels for class weights, allocations, sizes, the
weight-based class update, and Dirichlet-process class updates.

## Usage

``` r
sample_allocation(prob)

update_s(delta, m)

update_z(s, beta, b, Omega)

update_m(C, z, non_zero = FALSE)

update_classes_wb(
  s,
  b,
  Omega,
  epsmin = 0.01,
  epsmax = 0.7,
  deltamin = 0.1,
  deltashift = 0.5,
  identify_classes = FALSE,
  Cmax = 10L
)

update_classes_dp(
  beta,
  z,
  b,
  Omega,
  delta,
  mu_b_0,
  Sigma_b_0,
  n_Omega_0,
  V_Omega_0,
  identify_classes = FALSE,
  Cmax = 10L
)
```

## Arguments

- prob:

  \[`numeric(C)`\]  
  Class probabilities.

- delta:

  \[`numeric(1)`\]  
  Dirichlet concentration parameter.

- m:

  \[`numeric(C)`\]  
  Class sizes.

- s:

  \[`numeric(C)`\]  
  Class weights.

- beta:

  \[`matrix(P, N)`\]  
  Individual coefficient draws in columns.

- b:

  \[`matrix(P, C)`\]  
  Class means in columns.

- Omega:

  \[`matrix(P * P, C)`\]  
  Vectorized class covariance matrices in columns.

- C:

  \[`integer(1)`\]  
  Number of classes.

- z:

  \[`numeric(N)`\]  
  Class allocations numbered from one to `C`.

- non_zero:

  \[`logical(1)`\]  
  Replace empty class sizes by one?

- epsmin:

  \[`numeric(1)`\]  
  Remove the smallest class when its weight is below this threshold.

- epsmax:

  \[`numeric(1)`\]  
  Split the largest class when its weight exceeds this threshold.

- deltamin:

  \[`numeric(1)`\]  
  Merge the two closest classes when the Euclidean distance between
  their means is below this threshold.

- deltashift:

  \[`numeric(1)`\]  
  Scale of the mean displacement along the leading covariance
  eigenvector after splitting a class.

- identify_classes:

  \[`logical(1)`\]  
  Order the current active classes by size?

- Cmax:

  \[`integer(1)`\]  
  Maximum number of classes.

- mu_b_0:

  \[`numeric(P)`\]  
  Prior mean for class means.

- Sigma_b_0:

  \[`matrix(P, P)`\]  
  Prior covariance for class means.

- n_Omega_0:

  \[`integer(1)`\]  
  Prior degrees of freedom for class covariances.

- V_Omega_0:

  \[`matrix(P, P)`\]  
  Prior scale matrix for class covariances.

## Value

The functions return one sampler update:

- `sample_allocation()`: an integer class label.

- `update_s()`: a `C` by 1 numeric matrix of class weights.

- `update_z()`: an `N` by 1 numeric matrix of allocations.

- `update_m()`: a `C` by 1 numeric matrix of class sizes.

- `update_classes_wb()`: a list with `s`, `b`, `Omega`, and
  `update_type`, where the latter is zero for no update, one for
  removal, two for splitting, and three for merging.

- `update_classes_dp()`: a list with `z`, `b`, `Omega`, and `C`.

## References

Neal RM (2000). “Markov Chain Sampling Methods for Dirichlet Process
Mixture Models.” *Journal of Computational and Graphical Statistics*,
**9**(2), 249–265.
[doi:10.1080/10618600.2000.10474879](https://doi.org/10.1080/10618600.2000.10474879)
.

Oelschläger L, Bauer D (2021). “Bayes Estimation of Latent Class Mixed
Multinomial Probit Models.” In *Proceedings of the 100th Annual Meeting
of the Transportation Research Board*.
<https://trid.trb.org/view/1759753>.

## Examples

``` r
### a latent class state of six deciders with one random coefficient
set.seed(1)
beta <- matrix(c(-1, -1.2, -0.8, 1, 1.3, 0.9), nrow = 1)
b <- matrix(c(-1, 1), nrow = 1)
Omega <- matrix(c(0.2, 0.2), nrow = 1)

### the weights, the allocations, and the class sizes are drawn in turn
s <- update_s(delta = 1, m = c(3, 3))
z <- update_z(s, beta, b, Omega)
m <- update_m(C = 2, z = z)
sample_allocation(c(0.5, 0.3, 0.2))
#> [1] 1

### the weight-based update splits a class that grew too large
update_classes_wb(s = c(0.9, 0.1), b = b, Omega = Omega)
#> $s
#> [1] 0.45 0.45 0.10
#> 
#> $b
#>            [,1]      [,2] [,3]
#> [1,] -0.7763932 -1.223607    1
#> 
#> $Omega
#>      [,1] [,2] [,3]
#> [1,]  0.2  0.2  0.2
#> 
#> $update_type
#> [1] 2
#> 

### the Dirichlet process update draws the class count from the data
update_classes_dp(
  beta = beta, z = z, b = b, Omega = Omega, delta = 1,
  mu_b_0 = 0, Sigma_b_0 = diag(1), n_Omega_0 = 4, V_Omega_0 = diag(1)
)
#> $z
#>      [,1]
#> [1,]    1
#> [2,]    1
#> [3,]    1
#> [4,]    2
#> [5,]    2
#> [6,]    2
#> 
#> $b
#>            [,1]      [,2]
#> [1,] -0.8195659 0.7048773
#> 
#> $Omega
#>            [,1]      [,2]
#> [1,] 0.09673685 0.6384678
#> 
#> $C
#> [1] 2
#> 
```

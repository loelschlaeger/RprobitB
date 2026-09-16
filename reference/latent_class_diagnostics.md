# Diagnose latent-class occupancy and membership

Computes label-invariant posterior summaries of a fitted mixture model:
the distribution of the occupied class count and the posterior
probability that every pair of deciders belongs to the same class. It
also returns the probability of every decider belonging to each
relabeled class.

## Usage

``` r
latent_class_diagnostics(object)
```

## Arguments

- object:

  \[`RprobitB_fit`\]  
  Fitted model with at least two finite classes, a sparse finite
  mixture, a Dirichlet-process mixture, or a weight-based heuristic fit.

## Value

A list with three elements:

- `occupancy`: a `data.frame` with the columns `n_classes` and
  `probability`. It gives the share of draws in which exactly
  `n_classes` classes contain at least one decider, so it answers how
  many classes the data support. For a fixed mixture, the number of
  classes is fixed, and the table shows how often one of them stays
  empty.

- `co_clustering`: a square matrix with one row and one column per
  decider. Entry `[i, j]` is the share of draws in which deciders `i`
  and `j` are in the same class, so it answers whether two deciders
  behave alike. It does not depend on how the classes are labeled.

- `membership`: a matrix with one row per decider and one column per
  class, `class_1` to `class_<maximum>`. Entry `[i, k]` is the share of
  draws in which decider `i` is in class `k` after relabeling, so it
  answers which class a decider most likely belongs to.

## References

Dahl DB (2006). “Model-Based Clustering for Expression Data via a
Dirichlet Process Mixture Model.” In Do K, Müller P, Vannucci M (eds.),
*Bayesian Inference for Gene Expression and Proteomics*, 201–218.
Cambridge University Press.
[doi:10.1017/CBO9780511584589.011](https://doi.org/10.1017/CBO9780511584589.011)
.

Papastamoulis P, Iliopoulos G (2010). “An Artificial Allocations Based
Solution to the Label Switching Problem in Bayesian Analysis of Mixtures
of Distributions.” *Journal of Computational and Graphical Statistics*,
**19**(2), 313–331.
[doi:10.1198/jcgs.2010.09008](https://doi.org/10.1198/jcgs.2010.09008) .

Stephens M (2000). “Dealing with Label Switching in Mixture Models.”
*Journal of the Royal Statistical Society: Series B (Statistical
Methodology)*, **62**(4), 795–809.
[doi:10.1111/1467-9868.00265](https://doi.org/10.1111/1467-9868.00265) .

## Examples

``` r
set.seed(1)
model <- fit(
  choice ~ x | 0, random_effects = "x", latent_class_effects = "x",
  classes = 2, class_update = "dirichlet_process",
  dgp_parameters = list(
    beta = list(c(x = -1), c(x = 2)),
    Omega = list(matrix(0.2), matrix(0.2)),
    weights = c(0.6, 0.4)
  ),
  n_occasions = 5,
  chains = 1
)
diagnostics <- latent_class_diagnostics(model)
diagnostics$occupancy
#>   n_classes probability
#> 1         2       0.148
#> 2         3       0.218
#> 3         4       0.246
#> 4         5       0.162
#> 5         6       0.120
#> 6         7       0.066
#> 7         8       0.028
#> 8         9       0.012
diagnostics$co_clustering[1:5, 1:5]
#>       1     2     3     4     5
#> 1 1.000 0.086 0.094 0.092 0.500
#> 2 0.086 1.000 0.838 0.818 0.178
#> 3 0.094 0.838 1.000 0.844 0.188
#> 4 0.092 0.818 0.844 1.000 0.184
#> 5 0.500 0.178 0.188 0.184 1.000
head(diagnostics$membership)
#>   class_1 class_2 class_3 class_4 class_5 class_6 class_7 class_8 class_9
#> 1   0.086   0.712   0.124   0.050   0.022   0.000   0.006   0.000       0
#> 2   0.872   0.002   0.044   0.050   0.010   0.020   0.002   0.000       0
#> 3   0.892   0.004   0.054   0.034   0.006   0.010   0.000   0.000       0
#> 4   0.858   0.004   0.056   0.044   0.012   0.022   0.004   0.000       0
#> 5   0.196   0.652   0.058   0.062   0.020   0.004   0.004   0.004       0
#> 6   0.846   0.002   0.044   0.066   0.020   0.022   0.000   0.000       0
#>   class_10
#> 1        0
#> 2        0
#> 3        0
#> 4        0
#> 5        0
#> 6        0
```

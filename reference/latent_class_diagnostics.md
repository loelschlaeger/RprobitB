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
  `probability`, with one row for every number of classes from one to
  the maximum number of classes of the model. It gives the share of
  draws in which exactly `n_classes` classes contain at least one
  decider, so it indicates how many classes the data support under the
  chosen prior. For a fixed mixture, the sampler keeps all classes
  occupied.

- `co_clustering`: a square matrix with one row and one column per
  decider. Entry `[i, j]` is the share of draws in which deciders `i`
  and `j` are in the same class, so it indicates whether two deciders
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
#> Warning: The retained draws reach the maximum of 10 latent classes.
#> ℹ Increase `max_classes` and refit to check the bound.
diagnostics <- latent_class_diagnostics(model)
diagnostics$occupancy
#>    n_classes probability
#> 1          1       0.000
#> 2          2       0.066
#> 3          3       0.182
#> 4          4       0.204
#> 5          5       0.164
#> 6          6       0.142
#> 7          7       0.110
#> 8          8       0.060
#> 9          9       0.044
#> 10        10       0.028
diagnostics$co_clustering[1:5, 1:5]
#>       1     2     3     4     5
#> 1 1.000 0.004 0.012 0.016 0.562
#> 2 0.004 1.000 0.774 0.764 0.154
#> 3 0.012 0.774 1.000 0.832 0.166
#> 4 0.016 0.764 0.832 1.000 0.164
#> 5 0.562 0.154 0.166 0.164 1.000
head(diagnostics$membership)
#>   class_1 class_2 class_3 class_4 class_5 class_6 class_7 class_8 class_9
#> 1   0.004   0.756   0.150   0.030   0.034   0.018   0.006   0.002   0.000
#> 2   0.832   0.002   0.074   0.040   0.028   0.010   0.002   0.004   0.008
#> 3   0.900   0.006   0.034   0.026   0.014   0.014   0.002   0.002   0.002
#> 4   0.878   0.010   0.032   0.056   0.010   0.006   0.004   0.004   0.000
#> 5   0.166   0.682   0.084   0.018   0.024   0.018   0.008   0.000   0.000
#> 6   0.736   0.002   0.126   0.048   0.044   0.022   0.010   0.010   0.002
#>   class_10
#> 1        0
#> 2        0
#> 3        0
#> 4        0
#> 5        0
#> 6        0
```

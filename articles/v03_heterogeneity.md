# Modeling preference heterogeneity

The probit model assigns alternative $`j`$ at occasion $`t`$ of decider
$`n`$ the latent utility
$`U_{ntj} = X_{ntj}^\top \beta_n + \epsilon_{ntj}`$ with the coefficient
vector $`\beta_n`$. A model with fixed coefficients sets
$`\beta_n = \beta`$ for all deciders and thereby assumes that all
deciders weigh price, time, and comfort in the same way. This is rarely
true: some travelers react mainly to the fare, others to travel time,
and some households would pay a premium for a local electricity supplier
while others would not. **RprobitB** lets coefficients differ between
deciders in two ways, which can be combined: random coefficients follow
a continuous distribution over the population, and latent classes divide
the deciders into groups. Both are requested through arguments of
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md).
Oelschläger ([2026](#ref-Oelschlaeger2026c)) treats the methodological
background. Each variant is first estimated on simulated data, where the
estimates can be compared with the parameters that generated them, and
then applied to data of the **mlogit** package ([Croissant
2020](#ref-Croissant2020)). Panel data are essential here: the
population distribution of a coefficient is estimated from deciders
observed repeatedly, so all simulations use several choices per decider.

``` r

library(RprobitB)
set.seed(1)
```

## Random coefficients

Each term named in `random_effects` receives a separate coefficient for
every decider, drawn from a population distribution whose parameters the
sampler estimates together with the other parameters. Such hierarchical
models capture continuous preference heterogeneity and allow inference
about the coefficients of individual deciders ([Allenby and Rossi
1998](#ref-Allenby1998)). The population distribution is normal on a
latent scale: the random coefficients of decider $`n`$ are
$`\beta_n \sim \mathrm{N}(\mu, \Omega)`$ with the mean vector $`\mu`$
and the covariance matrix $`\Omega`$, reported as `mu[<effect>]` and
`Omega[<effect>,<effect>]`. The mixing distribution of an effect decides
how its latent normal variable enters the utility. An unnamed vector
requests correlated normal effects. A named vector selects the mixing
distribution of each term as `"<covariate>" = "<distribution>"`, where
`"ASC"` names the alternative-specific constants:

| Value    | Distribution            | Coefficient |
|:---------|:------------------------|:------------|
| `"cn"`   | correlated normal       | any sign    |
| `"n"`    | uncorrelated normal     | any sign    |
| `"cln"`  | correlated log-normal   | positive    |
| `"ln"`   | uncorrelated log-normal | positive    |
| `"cln-"` | correlated log-normal   | negative    |
| `"ln-"`  | uncorrelated log-normal | negative    |

The six values combine two choices. The first is the shape of the
distribution. A normal coefficient can take any value, which suits an
attribute that some deciders like and others dislike. A log-normal
coefficient is the exponential of a normal variable and therefore always
positive, or, with the trailing minus, always negative. It suits an
attribute whose sign is fixed by theory: a higher price does not raise
the utility of any decider. Log-normal effects are estimated on the
latent normal scale, so `mu`, `Omega`, and the individual draws refer to
the normal variable whose exponential enters the utility. The second
choice is whether an effect is correlated with the other correlated
effects. Deciders who value travel time may also value comfort, and the
`c` prefix adds the covariance between such effects to the model. An
uncorrelated effect has its own variance but no covariance with any
other effect, which appears as zeros in `Omega`.

The following demonstration combines both choices. The price coefficient
is negative log-normal and therefore uncorrelated. Travel time and
comfort receive correlated normal effects with a positive covariance.

``` r

mixing <- fit(
  choice ~ price + time + comfort | 0,
  random_effects = c(price = "ln-", time = "cn", comfort = "cn"),
  n_deciders = 400,
  n_occasions = 12,
  dgp_parameters = list(
    beta = c(price = -1, time = -0.8, comfort = 0.5),
    Omega = rbind(c(0.25, 0, 0), c(0, 0.4, 0.2), c(0, 0.2, 0.3))
  ),
  iterations = 2000,
  warmup = 1000,
  chains = 2,
  save_individual_draws = TRUE
)
summary(mixing)
#> Bayesian probit choice model
#> Formula: choice ~ price + time + comfort | 0 | 0 
#> Samples: 1000 retained per chain, 2 chains
#> 
#>                variable   dgp   mean   mode     sd rhat ess_bulk
#>               mu[price] -1.00 -0.983 -0.976 0.0590 1.09    18.34
#>                mu[time] -0.80 -0.799 -0.798 0.0529 1.15     9.39
#>             mu[comfort]  0.50  0.522  0.522 0.0444 1.08    21.13
#>      Omega[price,price]  0.25  0.349  0.308 0.0883 1.46     4.10
#>        Omega[time,time]  0.40  0.472  0.446 0.0737 1.33     5.06
#>     Omega[time,comfort]  0.20  0.208  0.209 0.0365 1.03    74.59
#>  Omega[comfort,comfort]  0.30  0.367  0.348 0.0548 1.22     7.29
```

The `dgp` column lists the parameters that generated the data. The
posterior means of the three population means deviate from their true
values by at most 0.5 posterior standard deviations. For the price
coefficient, `mu[price]` is the mean of the latent normal variable, so
the coefficient that enters the utility is minus its exponential and
negative for every decider. `time` and `comfort` share the estimated
covariance `Omega[time,comfort]`, while `price` has no covariance entry
with either of them.

[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
converts the coefficients into trade-offs. For a random effect it uses
the coefficient of the median decider, which for the log-normal price is
minus the exponential of `mu[price]`:

``` r

mixing_trade <- interpret(mixing, reference = "price")
mixing_trade
#> 1 `time` compensates -2.14 `price` (95% interval -2.48 to -1.83)
#> 1 `comfort` compensates 1.4 `price` (95% interval 1.14 to 1.66)
```

The `dgp_parameters` set the latent mean of the price effect to `-1`, so
the median price coefficient is `-exp(-1)`, about `-0.37`. One unit of
time is therefore worth -2.17 units of price and one unit of comfort
1.36 units. Both values lie inside the 95% credible intervals.

The panel structure makes individual coefficients estimable.
`coef(level = "individual")` returns the posterior mean coefficient of
every decider, on the latent normal scale for log-normal effects. The
histogram shows the price coefficient of each of the 400 deciders, all
negative as the log-normal specification enforces, with the left tail
containing the deciders who react most strongly to a higher price.

``` r

individual <- coef(mixing, level = "individual")
head(individual)
#>        price       time    comfort
#> 1 -0.8163486 -0.3926287  1.1094096
#> 2 -1.0311631  0.4183956  0.3786188
#> 3 -1.1071621 -1.1925806  0.3301504
#> 4 -1.2367155 -1.5643469 -0.3534857
#> 5 -0.5936260 -0.3103094  0.7787451
#> 6 -1.0373844 -1.5510612  0.4398722
price <- -exp(individual[, "price"])
hist(
  price,
  breaks = 30, col = "grey85", border = "white",
  main = "", xlab = "price coefficient of a decider"
)
```

![](v03_heterogeneity_files/figure-html/individual-1.png)

### Willingness to pay for electricity supplier attributes

The `Electricity` data of the **mlogit** package come from a stated
choice experiment in which 361 US households chose 8 to 12 times among
four hypothetical suppliers. The suppliers differ in price (`pf`),
contract length (`cl`), whether the supplier is local (`loc`) or
well-known (`wk`), and whether it offers time-of-day (`tod`) or seasonal
(`seas`) rates ([Huber and Train 2001](#ref-Huber2001)). The attribute
columns end in the supplier number without a delimiter, `pf1` to `pf4`,
so an underscore is inserted first, and the choice occasions of a
household are numbered in the order of the rows.

Contract length and locality receive correlated normal random
coefficients, the other attributes one coefficient for all households.
Fixing the price coefficient to `-1` identifies the scale and expresses
every other coefficient in cents per kWh, that is, directly as a
willingness to pay. All six attributes must enter the model: suppliers
with time-of-day or seasonal rates have a price of zero in these data,
so without their dummies the price coefficient would absorb the effect
of their zero price.

``` r

data("Electricity", package = "mlogit")
names(Electricity) <- sub("([a-z]+)([1-4])$", "\\1_\\2", names(Electricity))
Electricity$occasion <- ave(Electricity$id, Electricity$id, FUN = seq_along)
electricity <- fit(
  choice ~ pf + cl + loc + wk + tod + seas | 0,
  data = Electricity,
  random_effects = c("cl", "loc"),
  column_decider = "id",
  column_occasion = "occasion",
  scale = c(pf = -1),
  iterations = 3000,
  warmup = 1500,
  thin = 2,
  chains = 2,
  save_individual_draws = TRUE,
  progress = FALSE
)
summary(electricity)
#> Bayesian probit choice model
#> Formula: choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 
#> Samples: 750 retained per chain, 2 chains
#> 
#>        variable    mean    mode     sd rhat ess_bulk
#>        beta[wk]  1.5224  1.5259 0.0754 1.01    295.9
#>       beta[tod] -8.7348 -8.7287 0.0765 1.01    200.5
#>      beta[seas] -9.2978 -9.2676 0.0840 1.02    216.6
#>          mu[cl] -0.2165 -0.2152 0.0274 1.01    723.4
#>         mu[loc]  2.0493  2.0366 0.1259 1.00    372.2
#>    Omega[cl,cl]  0.1886  0.1839 0.0223 1.00    359.5
#>   Omega[cl,loc]  0.0459  0.0402 0.0508 1.00    295.8
#>  Omega[loc,loc]  2.2652  2.1807 0.3320 1.00    326.6
#>      Sigma[2,2]  7.0883  7.0644 0.7139 1.03     52.8
#>      Sigma[2,3]  2.8021  2.6859 0.5159 1.04     43.3
#>      Sigma[3,3]  5.8244  5.7776 0.6674 1.00     99.8
#>      Sigma[2,4]  3.3193  3.1529 0.4972 1.12     18.0
#>      Sigma[3,4]  2.7979  2.6260 0.4605 1.03     59.3
#>      Sigma[4,4]  6.5651  6.3676 0.7563 1.11     16.1
```

Because the price coefficient is fixed,
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
reports the coefficients directly as willingness to pay in cents per
kWh, with credible intervals:

``` r

interpret(electricity, effects = c("cl", "loc", "wk"))
#> 1 `cl` compensates -0.217 `pf` (95% interval -0.271 to -0.164)
#> 1 `loc` compensates 2.05 `pf` (95% interval 1.81 to 2.3)
#> 1 `wk` compensates 1.52 `pf` (95% interval 1.38 to 1.67)
```

On average, households would accept a price about 2 cents per kWh higher
for a local supplier, and would need a price about 0.22 cents lower for
each additional year of contract length. The `Omega[...]` variables
describe how much these valuations vary between households: the standard
deviation of the locality premium across households is 1.5 cents, which
is below its mean.

## Latent classes

A single normal distribution is a strong assumption about the
distribution of preferences in a population. There may be commuters and
leisure travelers, or households that focus on price and households that
focus on service. `latent_class_effects` names the effects that differ
between `classes` latent classes. Every decider belongs to exactly one
class. The class weights $`w_1, \dots, w_K`$ are the probabilities of
membership and sum to one, and the class allocation of every decider is
a latent variable that the sampler draws together with the parameters. A
class is occupied if at least one decider is allocated to it. Naming a
random effect in `latent_class_effects` replaces its normal distribution
by a finite mixture of normals, which yields the latent-class mixed
multinomial probit model of Oelschläger and Bauer
([2021](#ref-Oelschlaeger2021)). Random effects that are not named keep
one distribution for all deciders, and a named coefficient that is not
random takes one value per class and does not vary within it, as in the
classical latent class model ([Kamakura and Russell
1989](#ref-Kamakura1989); [Greene and Hensher 2003](#ref-Greene2003)).
`class_update` selects how the number of classes is treated:

- `class_update = "fixed"` when exactly $`K`$ substantively meaningful
  classes are assumed;
- `class_update = "sparse"` when `classes` is a generous upper bound and
  redundant classes should empty;
- `class_update = "dirichlet_process"` for a Dirichlet-process mixture
  whose number of occupied classes is itself random;
- `class_update = "weight_based"` for the split, remove, and merge
  heuristic of earlier **RprobitB** versions, which is a search
  procedure, not a Bayesian model.

All four updates are demonstrated on one simulated data set with two
classes. The fixed-class fit simulates the data; the other three are
refits through [`update()`](https://rdrr.io/r/stats/update.html), which
reuses the simulated data, so all updates are compared on identical
observations.

### A fixed number of classes

For `class_update = "fixed"`, `classes = K` fixes the number of classes,
and every class stays occupied. The class weights have the prior
$`(w_1,\ldots,w_K)\sim\operatorname{Dirichlet}(\delta,\ldots,\delta)`$,
where the default `class_concentration = 1` is uniform on the weight
simplex. The class labels are arbitrary: swapping them leaves the
likelihood unchanged, so the sampler may swap them during a run.
**RprobitB** leaves the sampler unconstrained and relabels the retained
draws afterwards. It derives a representative grouping of the deciders
from the posterior co-clustering matrix, whose entries are the posterior
probabilities that two deciders belong to the same class ([Dahl
2006](#ref-Dahl2006)), matches the allocation of every draw to this
grouping ([Papastamoulis and Iliopoulos 2010](#ref-Papastamoulis2010)),
and permutes weights, means, covariances, and allocations accordingly.
The classes are then numbered by decreasing weight, which is a naming
convention.

The following demonstration uses eight occasions per decider and two
well-separated classes: a majority with coefficients centered at `-1`
and a minority centered at `2`. The class-specific means and variances
mix slowly, so the two chains run twenty thousand iterations each and
retain every fifth draw.

``` r

mixture <- fit(
  choice ~ x | 0,
  random_effects = "x",
  latent_class_effects = "x",
  classes = 2,
  n_deciders = 80,
  n_occasions = 8,
  save_individual_draws = TRUE,
  dgp_parameters = list(
    beta = list(c(x = -1), c(x = 2)),
    Omega = list(matrix(0.2), matrix(0.2)),
    weights = c(0.6, 0.4)
  ),
  iterations = 20000,
  warmup = 10000,
  thin = 5,
  chains = 2,
  progress = FALSE
)
mixture_summary <- summary(mixture, variables = c(
  "weight[1]", "weight[2]", "mu[x,1]", "mu[x,2]",
  "Omega[x,x,1]", "Omega[x,x,2]"
))
mixture_summary
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 2000 retained per chain, 2 chains
#> 
#>      variable  dgp   mean   mode     sd rhat ess_bulk
#>     weight[1]  0.6  0.601  0.603 0.0619 1.00     1024
#>     weight[2]  0.4  0.399  0.397 0.0619 1.00     1024
#>       mu[x,1] -1.0 -0.765 -0.766 0.1118 1.00     1138
#>       mu[x,2]  2.0  2.198  2.172 0.3715 1.03      177
#>  Omega[x,x,1]  0.2  0.191  0.142 0.0888 1.00      705
#>  Omega[x,x,2]  0.2  0.491  0.231 0.4813 1.00      297
```

The relabeled weights and means can be read class by class. The class
near `-1` has a posterior mean weight of 0.6 against a population weight
of `0.6`, and the class near `2` the rest. The largest `rhat` in the
table is 1.033, and the smallest bulk effective sample size, 177,
belongs to `mu[x,2]`. Check both for every mixture fit, because
relabeling cannot compensate for poor mixing or weak class separation.

[`latent_class_diagnostics()`](https://loelschlaeger.de/RprobitB/reference/latent_class_diagnostics.md)
returns the posterior distribution of the number of occupied classes,
the membership probabilities of the deciders after relabeling, and the
co-clustering matrix, which does not depend on the class labels:

``` r

class_diagnostics <- latent_class_diagnostics(mixture)
class_diagnostics$occupancy
#>   n_classes probability
#> 1         2           1
class_diagnostics$membership[1:6, ]
#>   class_1 class_2
#> 1 0.99075 0.00925
#> 2 0.99575 0.00425
#> 3 0.99275 0.00725
#> 4 0.99325 0.00675
#> 5 0.00950 0.99050
#> 6 0.99325 0.00675
class_diagnostics$co_clustering[1:6, 1:6]
#>         1       2       3       4       5       6
#> 1 1.00000 0.98900 0.98600 0.98750 0.01875 0.98800
#> 2 0.98900 1.00000 0.99050 0.99050 0.01375 0.99150
#> 3 0.98600 0.99050 1.00000 0.98850 0.01675 0.98800
#> 4 0.98750 0.99050 0.98850 1.00000 0.01625 0.98800
#> 5 0.01875 0.01375 0.01675 0.01625 1.00000 0.01625
#> 6 0.98800 0.99150 0.98800 0.98800 0.01625 1.00000
```

The three refits below relabel their draws in the same way. When the
number of classes varies, a class exists only in part of the draws, and
the `occupied` column of
[`summary()`](https://rdrr.io/r/base/summary.html) reports this share.
The refits have no `dgp` column, so a small helper compares them with
the stored true parameters: the most probable number of occupied classes
and, after assigning every decider to the true class whose mean is
closer to their posterior mean coefficient, the share of deciders in the
second class and the average coefficient in each class.

``` r

truth <- mixture$simulation$dgp_parameters
class_means <- c(truth$beta[[1]][["x"]], truth$beta[[2]][["x"]])
recover_classes <- function(x) {
  occupancy <- latent_class_diagnostics(x)$occupancy
  individual <- coef(x, level = "individual")[, "x"]
  upper <- individual > mean(class_means)
  data.frame(
    variable = c("n_classes", "weight[2]", "mu[x,1]", "mu[x,2]"),
    dgp = c(length(truth$weights), truth$weights[2], class_means),
    estimate = c(
      occupancy$n_classes[which.max(occupancy$probability)],
      mean(upper), mean(individual[!upper]), mean(individual[upper])
    )
  )
}
mixture_sim <- recover_classes(mixture)
mixture_sim
#>    variable  dgp   estimate
#> 1 n_classes  2.0  2.0000000
#> 2 weight[2]  0.4  0.4000000
#> 3   mu[x,1] -1.0 -0.7714234
#> 4   mu[x,2]  2.0  2.1714848
```

Fitting two filled classes is not evidence that two meaningful
populations exist. When $`K`$ is not fixed by the research design, fit
each candidate value to the same data and compare the decider-level
predictive accuracy with the **loo** package ([Vehtari et al.
2026](#ref-Vehtari2026)), as the vignette [Bayesian model
evaluation](https://loelschlaeger.de/RprobitB/articles/v05_model_evaluation.html)
describes. The fits are thinned, because each
[`loo()`](https://loelschlaeger.de/RprobitB/reference/loo.RprobitB_fit.md)
call evaluates the panel likelihood of every decider under every
retained draw. This likelihood is a multivariate normal probability that
is simulated with the GHK simulator, and `ghk_draws = 50` reduces its
simulation draws from the default 500.

``` r

models_by_K <- lapply(1:3, function(K) {
  class_effects <- if (K > 1) "x" else character()
  update(
    mixture, classes = K, latent_class_effects = class_effects,
    iterations = 2000, warmup = 1000, thin = 20
  )
})
k_comparison <- loo::loo_compare(
  lapply(models_by_K, loo, ghk_draws = 50, progress = FALSE)
)
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
k_comparison
#>   model elpd_diff se_diff p_worse diag_diff      diag_elpd
#>  model2       0.0     0.0      NA           1 k_psis > 0.5
#>  model3      -0.5     0.5    0.88   N < 100 1 k_psis > 0.5
#>  model1     -11.3     3.5    1.00   N < 100 3 k_psis > 0.5
#> 
#> Diagnostic flags present.
#> See ?`loo-glossary` (sections `diag_diff` and `diag_elpd`)
#> or https://mc-stan.org/loo/reference/loo-glossary.html.
```

The one-class model falls short of the best model by 11, 3 times the
standard error of the difference, whereas the two- and the three-class
model differ by only 0.5 with a standard error of 0.5, so the third
class adds little. This is the expected result for data generated with
two classes.

### Class-specific coefficients

Do the train travelers of the vignette [Get started with
RprobitB](https://loelschlaeger.de/RprobitB/articles/v01_get_started.html)
all trade time against money at the same rate, or are there classes with
different values of time? The fit below uses the first 100 travelers,
fixes the price coefficient to `-1`, and gives the time coefficient two
class-specific values through `latent_class_effects` without a random
effect, so each class has its own value of travel time and the remaining
coefficients are common to both classes.

``` r

data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60
train_small <- Train[Train$id %in% unique(Train$id)[1:100], ]
train_classes <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = train_small,
  latent_class_effects = "time",
  classes = 2,
  column_decider = "id",
  column_occasion = "choiceid",
  scale = c(price = -1),
  iterations = 10000,
  warmup = 5000,
  thin = 2,
  chains = 2,
  progress = FALSE
)
train_classes_summary <- summary(train_classes, variables = c(
  "weight[1]", "weight[2]", "beta[time,1]", "beta[time,2]"
))
train_classes_summary
#> Bayesian probit choice model
#> Formula: choice ~ price + time + change + factor(comfort) | 0 | 0 
#> Samples: 2500 retained per chain, 2 chains
#> 
#>      variable   mean    mode     sd rhat ess_bulk
#>     weight[1]   0.87   0.886 0.0537 1.01      167
#>     weight[2]   0.13   0.114 0.0537 1.01      167
#>  beta[time,1]  -3.56  -3.437 0.6658 1.01      240
#>  beta[time,2] -21.96 -18.895 7.1465 1.02      101
```

With the price coefficient fixed at `-1`, the class-specific time
coefficients are values of travel time in euro per hour, which
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
reports class by class:

``` r

time_by_class <- interpret(train_classes, effects = "time")
time_by_class
#> Class 1: 1 `time` compensates -3.56 `price` (95% interval -4.86 to -2.26)
#> Class 2: 1 `time` compensates -22 `price` (95% interval -42.5 to -14)
```

The larger class, 87 percent of the travelers, values an hour at 3.6
euro, the smaller class at 22 euro. The smaller class chooses the faster
trip almost regardless of its price, a pattern that a single normal
distribution of the time coefficient would represent as a heavy tail.
The largest `rhat` of the four variables is 1.019 and the smallest bulk
effective sample size 101.

### Weight-based class updates

The weight-based update of Oelschläger and Bauer
([2021](#ref-Oelschlaeger2021)) is kept for reproducing earlier
**RprobitB** analyses. Every `buffer` warmup iterations, it removes the
smallest class if its weight is below `epsmin`, splits the largest class
if its weight is above `epsmax`, or merges the closest pair of classes
if the distance of their means is below `deltamin`, at most one
operation in this order. `weight_based_control` overrides the defaults
of these constants, and `max_classes` bounds the splitting. These
dimension changes correspond to no prior on the number of classes, and
they stop after warmup, so the reported `n_classes` is the outcome of a
search, not a posterior distribution. For inferential claims, prefer the
fixed, sparse finite, or Dirichlet-process specifications.

``` r

weight_based <- update(mixture, class_update = "weight_based")
weight_based_sim <- recover_classes(weight_based)
weight_based_sim
#>    variable  dgp   estimate
#> 1 n_classes  2.0  2.0000000
#> 2 weight[2]  0.4  0.4000000
#> 3   mu[x,1] -1.0 -0.7752978
#> 4   mu[x,2]  2.0  2.1244682
```

The run ends with 2 classes, and the share and the class means differ
from those of the fixed fit by at most 0.05. This is the case the
heuristic was designed for; the caveats above concern the interpretation
of its result.

### Sparse finite mixtures

A sparse finite mixture fixes a generous upper bound $`K`$, permits
empty classes, and places the symmetric Dirichlet prior with a small
concentration $`e_0`$ on the weights, which favors emptying redundant
classes ([Rousseau and Mengersen 2011](#ref-Rousseau2011);
[Frühwirth-Schnatter and Malsiner-Walli
2019](#ref-FruehwirthSchnatter2019)). The default
`class_concentration = c(shape = 1, rate = 200)` is the gamma hyperprior
$`e_0\sim\operatorname{Gamma}(1,200)`$ with mean `0.005`. Under a fixed
$`e_0`$, the prior expected number of occupied classes among $`N`$
deciders is

``` math
K\left[1-
\frac{\Gamma(Ke_0)\,\Gamma((K-1)e_0+N)}
     {\Gamma((K-1)e_0)\,\Gamma(Ke_0+N)}\right],
```

which translates $`e_0`$ into a statement about the number of classes.
At the prior mean $`e_0 = 0.005`$, with $`K = 6`$ and the 80 deciders of
the simulated data, it gives about 1.1 occupied classes: the prior
expects close to a single occupied class. The refit sets the upper bound
to six classes for the two-class data.

``` r

sparse <- update(mixture, classes = 6, class_update = "sparse")
sparse_summary <- summary(sparse)
sparse_summary
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 2000 retained per chain, 2 chains
#> 
#>             variable occupied     mean     mode      sd rhat ess_bulk
#>            weight[1]   1.0000  0.60628  0.62109 0.06037 1.00     1324
#>            weight[2]   1.0000  0.39106  0.38037 0.06108 1.00     1303
#>            weight[3]   0.0305  0.02573  0.00669 0.03845   NA       NA
#>            weight[4]   0.0185  0.04021  0.01468 0.04464   NA       NA
#>            weight[5]   0.0158  0.02666  0.00969 0.02630   NA       NA
#>            weight[6]   0.0095  0.02675  0.01257 0.02025   NA       NA
#>              mu[x,1]   1.0000 -0.75757 -0.74992 0.11076 1.00     1487
#>              mu[x,2]   1.0000  2.14223  2.14647 0.32039 1.01      234
#>              mu[x,3]   0.0305 -1.38106  1.13623 3.19781   NA       NA
#>              mu[x,4]   0.0185  1.68662  2.19947 2.10760   NA       NA
#>              mu[x,5]   0.0158  0.84980 -0.01850 1.92361   NA       NA
#>              mu[x,6]   0.0095  0.87222  1.56983 2.19091   NA       NA
#>         Omega[x,x,1]   1.0000  0.19835  0.14044 0.10440 1.01      773
#>         Omega[x,x,2]   1.0000  0.42822  0.19209 0.44007 1.00      370
#>         Omega[x,x,3]   0.0305  0.67483  0.24658 0.80323   NA       NA
#>         Omega[x,x,4]   0.0185  0.68901  0.31632 0.91526   NA       NA
#>         Omega[x,x,5]   0.0158  0.63857  0.36350 0.60428   NA       NA
#>         Omega[x,x,6]   0.0095  0.97210  0.33455 1.53646   NA       NA
#>  class_concentration   1.0000  0.00942  0.00459 0.00654 1.00      308
#>            n_classes   1.0000  2.07425  2.00000 0.26880 1.00      426
sparse_occupancy <- latent_class_diagnostics(sparse)$occupancy
sparse_occupancy
#>   n_classes probability
#> 1         2     0.92750
#> 2         3     0.07075
#> 3         4     0.00175
sparse_sim <- recover_classes(sparse)
sparse_sim
#>    variable  dgp   estimate
#> 1 n_classes  2.0  2.0000000
#> 2 weight[2]  0.4  0.4000000
#> 3   mu[x,1] -1.0 -0.7736973
#> 4   mu[x,2]  2.0  2.1049214
```

Although the prior favors a single class, the posterior assigns a
probability of 93 percent to exactly two occupied classes, and the class
means are recovered as by the fixed model, -0.77 and 2.1 against `-1`
and `2`. The smallest bulk effective sample size, 234, belongs to
`mu[x,2]`. Both $`K`$ and $`e_0`$ affect the posterior of the number of
occupied classes, so a sensitivity analysis should vary them. A less
sparse alternative is
$`e_0\sim\operatorname{Gamma}(2,4K)`$([Frühwirth-Schnatter and
Malsiner-Walli 2019](#ref-FruehwirthSchnatter2019)):

``` r

sparse_less <- update(
  mixture,
  classes = 6,
  class_update = "sparse",
  prior = list(class_concentration = c(shape = 2, rate = 24))
)
less_occupancy <- latent_class_diagnostics(sparse_less)$occupancy
less_occupancy
#>   n_classes probability
#> 1         2     0.46275
#> 2         3     0.34625
#> 3         4     0.15275
#> 4         5     0.03450
#> 5         6     0.00375
less_sim <- recover_classes(sparse_less)
less_sim
#>    variable  dgp  estimate
#> 1 n_classes  2.0  2.000000
#> 2 weight[2]  0.4  0.400000
#> 3   mu[x,1] -1.0 -0.782502
#> 4   mu[x,2]  2.0  2.141113
```

With a prior that empties classes less readily, the posterior of the
number of occupied classes spreads from 2 to 6 and assigns exactly two
classes a probability of only 46 percent. The class means are still
recovered, -0.78 and 2.14, because the extra classes contain few
deciders, but the estimated number of classes has changed with the
prior. Report the concentration prior together with the occupancy table.

### Dirichlet-process mixtures

For `class_update = "dirichlet_process"`, the precision $`\alpha`$
controls the prior tendency to open new classes, with the default
$`\alpha\sim\operatorname{Gamma}(2,4)`$ of mean `0.5`. Conditional on a
fixed $`\alpha`$, the prior expects
$`\sum_{i=1}^{N}\alpha/(\alpha+i-1)`$ occupied classes among $`N`$
deciders, which for $`\alpha = 0.5`$ and 80 deciders is about 3.2.
**RprobitB** updates $`\alpha`$ with the augmentation of Escobar and
West ([1995](#ref-Escobar1995)) and the allocations with the algorithm
of Neal ([2000](#ref-Neal2000)). `max_classes` caps the number of
classes the sampler may open, and
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) warns when
the draws reach the cap. The number of occupied classes mixes slowly,
because a decider opens a new class only rarely, so the refit runs
longer chains than the reference fit, thinned to keep the fitted object
small.

``` r

dynamic <- update(
  mixture, class_update = "dirichlet_process", max_classes = 15,
  iterations = 30000, warmup = 15000, thin = 10
)
dynamic_summary <- summary(dynamic)
dynamic_summary
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 1500 retained per chain, 2 chains
#> 
#>             variable occupied    mean    mode     sd rhat ess_bulk
#>            weight[1]  1.00000  0.5764  0.6006 0.0683 1.01     1067
#>            weight[2]  1.00000  0.3494  0.3909 0.0646 1.01      491
#>            weight[3]  0.66733  0.0713  0.0196 0.0647   NA       NA
#>            weight[4]  0.36000  0.0474  0.0144 0.0512   NA       NA
#>            weight[5]  0.16300  0.0384  0.0140 0.0437   NA       NA
#>            weight[6]  0.06600  0.0324  0.0129 0.0337   NA       NA
#>            weight[7]  0.02567  0.0276  0.0125 0.0277   NA       NA
#>            weight[8]  0.01100  0.0246  0.0125 0.0208   NA       NA
#>            weight[9]  0.00500  0.0225  0.0125 0.0212   NA       NA
#>           weight[10]  0.00133  0.0187  0.0125 0.0125   NA       NA
#>              mu[x,1]  1.00000 -0.7651 -0.7793 0.1280 1.00     1894
#>              mu[x,2]  1.00000  2.2365  2.1718 0.4526 1.01      330
#>              mu[x,3]  0.66733  1.1656  1.9625 2.2723   NA       NA
#>              mu[x,4]  0.36000  0.8415 -0.7902 2.1760   NA       NA
#>              mu[x,5]  0.16300  0.6768  1.3574 2.2878   NA       NA
#>              mu[x,6]  0.06600  1.0714  2.0517 2.2048   NA       NA
#>              mu[x,7]  0.02567  0.7316  2.1819 2.0304   NA       NA
#>              mu[x,8]  0.01100  0.9621  0.6255 1.9068   NA       NA
#>              mu[x,9]  0.00500  1.2651 -0.3654 3.1680   NA       NA
#>             mu[x,10]  0.00133 -0.5347 -1.4855 2.0850   NA       NA
#>         Omega[x,x,1]  1.00000  0.2061  0.1438 0.1315 1.00     1127
#>         Omega[x,x,2]  1.00000  0.4708  0.2067 0.5191 1.01      424
#>         Omega[x,x,3]  0.66733  0.8199  0.2278 1.9172   NA       NA
#>         Omega[x,x,4]  0.36000  0.9090  0.2282 1.9325   NA       NA
#>         Omega[x,x,5]  0.16300  0.8162  0.2633 1.5670   NA       NA
#>         Omega[x,x,6]  0.06600  0.8439  0.2791 1.8146   NA       NA
#>         Omega[x,x,7]  0.02567  0.6361  0.3245 0.5985   NA       NA
#>         Omega[x,x,8]  0.01100  0.7867  0.3789 0.8046   NA       NA
#>         Omega[x,x,9]  0.00500  0.5650  0.3526 0.3773   NA       NA
#>        Omega[x,x,10]  0.00133  0.4579  0.6688 0.3232   NA       NA
#>  class_concentration  1.00000  0.5191  0.3547 0.3093 1.01     1318
#>            n_classes  1.00000  3.2993  2.0000 1.3318 1.01      539
dynamic_occupancy <- latent_class_diagnostics(dynamic)$occupancy
dynamic_occupancy
#>   n_classes probability
#> 1         2 0.332666667
#> 2         3 0.307333333
#> 3         4 0.197000000
#> 4         5 0.097000000
#> 5         6 0.040333333
#> 6         7 0.014666667
#> 7         8 0.006000000
#> 8         9 0.003666667
#> 9        10 0.001333333
dynamic_sim <- recover_classes(dynamic)
dynamic_sim
#>    variable  dgp   estimate
#> 1 n_classes  2.0  2.0000000
#> 2 weight[2]  0.4  0.4000000
#> 3   mu[x,1] -1.0 -0.7861533
#> 4   mu[x,2]  2.0  2.2166363
```

The occupancy distribution is wider than under the sparse finite prior:
it assigns a posterior probability of 67 percent to three or more
classes, against 7 percent under the sparse finite prior. This reflects
the prior, which expects about three occupied classes for 80 deciders
and remains influential at this sample size. The label-invariant
comparison of the helper nevertheless recovers the two true groups,
-0.79 and 2.22, because the superfluous classes contain only a few
deciders. The `rhat` and the effective sample size of `n_classes`, here
1.012 and 539, decide whether the occupancy table is reliable. The
largest number of classes in any draw is 10, below the cap of 15. The
number of occupied classes, the concentration, and the co-clustering
probabilities do not depend on the class labels and are therefore the
safest summaries of such a fit; a class that exists in only a small
share of the draws describes a few deciders, not a group in the
population. Report the prior on $`\alpha`$ with the analysis, together
with the sensitivity to defensible alternatives.

## Ordered and ranked responses

Both mechanisms also work for ordered and ranked responses, which the
vignette [Model specification and
variants](https://loelschlaeger.de/RprobitB/articles/v02_model_variants.html)
introduces. They require a panel, because a population distribution is
estimated from deciders observed repeatedly. The following demonstration
uses a simulated panel of rankings: 100 deciders order three
alternatives five times each, with a coefficient that varies normally
around `-1`.

``` r

ranked_random <- fit(
  rank ~ x | 0,
  choice_type = "ranked",
  random_effects = "x",
  n_deciders = 100,
  n_occasions = 5,
  n_alternatives = 3,
  dgp_parameters = list(beta = c(x = -1), Omega = matrix(0.3)),
  iterations = 4000,
  warmup = 2000,
  chains = 2,
  progress = FALSE
)
summary(ranked_random)
#> Bayesian probit choice model
#> Formula: rank ~ x | 0 | 0 
#> Samples: 2000 retained per chain, 2 chains
#> 
#>    variable     dgp    mean   mode    sd rhat ess_bulk
#>       mu[x] -1.0000 -1.1193 -1.097 0.114 1.01      135
#>  Omega[x,x]  0.3000  0.5319  0.463 0.155 1.02      102
#>  Sigma[B,C]  0.0835 -0.0931 -0.121 0.124 1.00      417
#>  Sigma[C,C]  1.7228  1.6428  1.510 0.361 1.00      250
```

The posterior means of the population mean and variance deviate from
their true values by 1 and 1.5 posterior standard deviations. Ordered
responses accept the same arguments, and latent classes are requested in
the same way as above, with `latent_class_effects` and `classes`.

## Further reading

The vignette [Posterior
prediction](https://loelschlaeger.de/RprobitB/articles/v04_prediction.html)
uses the individual coefficients to compute choice probabilities for
each decider in the fit. The vignette [Bayesian model
evaluation](https://loelschlaeger.de/RprobitB/articles/v05_model_evaluation.html)
shows how to decide whether random coefficients improve a model.

## References

Allenby, Greg M., and Peter E. Rossi. 1998. “Marketing Models of
Consumer Heterogeneity.” *Journal of Econometrics* 89 (1-2): 57–78.
<https://doi.org/10.1016/S0304-4076(98)00055-4>.

Croissant, Yves. 2020. “Estimation of Random Utility Models in R: The
mlogit Package.” *Journal of Statistical Software* 95 (11): 1–41.
<https://doi.org/10.18637/jss.v095.i11>.

Dahl, David B. 2006. “Model-Based Clustering for Expression Data via a
Dirichlet Process Mixture Model.” In *Bayesian Inference for Gene
Expression and Proteomics*, edited by Kim-Anh Do, Peter Müller, and
Marina Vannucci. Cambridge University Press.
<https://doi.org/10.1017/CBO9780511584589.011>.

Escobar, Michael D., and Mike West. 1995. “Bayesian Density Estimation
and Inference Using Mixtures.” *Journal of the American Statistical
Association* 90 (430): 577–88.
<https://doi.org/10.1080/01621459.1995.10476550>.

Frühwirth-Schnatter, Sylvia, and Gertraud Malsiner-Walli. 2019. “From
Here to Infinity: Sparse Finite Versus Dirichlet Process Mixtures in
Model-Based Clustering.” *Advances in Data Analysis and Classification*
13 (1): 33–64. <https://doi.org/10.1007/s11634-018-0329-y>.

Greene, William H., and David A. Hensher. 2003. “A Latent Class Model
for Discrete Choice Analysis: Contrasts with Mixed Logit.”
*Transportation Research Part B: Methodological* 37 (8): 681–98.
<https://doi.org/10.1016/S0191-2615(02)00046-2>.

Huber, Joel, and Kenneth Train. 2001. “On the Similarity of Classical
and Bayesian Estimates of Individual Mean Partworths.” *Marketing
Letters* 12 (3): 259–69. <https://doi.org/10.1023/A:1011120928698>.

Kamakura, Wagner A., and Gary J. Russell. 1989. “A Probabilistic Choice
Model for Market Segmentation and Elasticity Structure.” *Journal of
Marketing Research* 26 (4): 379–90.

Neal, Radford M. 2000. “Markov Chain Sampling Methods for Dirichlet
Process Mixture Models.” *Journal of Computational and Graphical
Statistics* 9 (2): 249–65.
<https://doi.org/10.1080/10618600.2000.10474879>.

Oelschläger, Lennart. 2026. “Overcoming Challenges in Modeling Choice
Behavior Heterogeneity.” PhD thesis, Bielefeld University.
<https://pub.uni-bielefeld.de/record/3014719>.

Oelschläger, Lennart, and Dietmar Bauer. 2021. “Bayes Estimation of
Latent Class Mixed Multinomial Probit Models.” *Proceedings of the 100th
Annual Meeting of the Transportation Research Board* (Washington, DC).
<https://trid.trb.org/view/1759753>.

Papastamoulis, Panagiotis, and George Iliopoulos. 2010. “An Artificial
Allocations Based Solution to the Label Switching Problem in Bayesian
Analysis of Mixtures of Distributions.” *Journal of Computational and
Graphical Statistics* 19 (2): 313–31.
<https://doi.org/10.1198/jcgs.2010.09008>.

Rousseau, Judith, and Kerrie Mengersen. 2011. “Asymptotic Behaviour of
the Posterior Distribution in Overfitted Mixture Models.” *Journal of
the Royal Statistical Society: Series B (Statistical Methodology)* 73
(5): 689–710. <https://doi.org/10.1111/j.1467-9868.2011.00781.x>.

Vehtari, Aki, Jonah Gabry, Måns Magnusson, et al. 2026. *loo: Efficient
Leave-One-Out Cross-Validation and WAIC for Bayesian Models*.
<https://mc-stan.org/loo/>.

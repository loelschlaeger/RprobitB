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
2020](#ref-Croissant2020)).

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
is negative log-normal and uncorrelated. Travel time and comfort receive
correlated normal effects with a positive covariance.

``` r

mixing <- fit(
  choice ~ price + time + comfort | 0,
  random_effects = c(price = "ln-", time = "cn", comfort = "cn"),
  n_deciders = 100,
  n_occasions = 10,
  dgp_parameters = list(
    beta = c(price = -1, time = -0.8, comfort = 0.5),
    Omega = rbind(c(0.25, 0, 0), c(0, 0.4, 0.2), c(0, 0.2, 0.3))
  ),
  iterations = 4000,
  warmup = 2000,
  thin = 2,
  chains = 1,
  save_individual_draws = TRUE
)
summary(mixing)
#> Bayesian probit choice model
#> Formula: choice ~ price + time + comfort | 0 | 0 
#> Samples: 1000 retained per chain, 1 chain
#>                variable   dgp   mean   mode     sd rhat ess_bulk
#>               mu[price] -1.00 -0.967 -0.861 0.1761 1.02    16.78
#>                mu[time] -0.80 -0.874 -0.873 0.0972 1.05    29.15
#>             mu[comfort]  0.50  0.554  0.555 0.0814 1.00    69.99
#>      Omega[price,price]  0.25  0.311  0.181 0.1681 1.01    16.78
#>        Omega[time,time]  0.40  0.453  0.402 0.1434 1.17     6.19
#>     Omega[time,comfort]  0.20  0.260  0.236 0.0846 1.02    37.34
#>  Omega[comfort,comfort]  0.30  0.344  0.327 0.0961 1.02    43.95
```

The `dgp` column lists the parameters that generated the data. For the
price coefficient, `mu[price]` is the mean of the latent normal
variable, so the coefficient that enters the utility is minus its
exponential and negative for every decider. `time` and `comfort` share
the estimated covariance `Omega[time,comfort]`, while `price` has no
covariance entry with either of them.

[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
converts the coefficients into trade-offs. For a random effect it uses
the coefficient of the median decider, which for the log-normal price is
minus the exponential of `mu[price]`:

``` r

interpret(mixing, reference = "price")
#> 1 `time` compensates -2.33 `price` (95% interval -3.57 to -1.62)
#> 1 `comfort` compensates 1.48 `price` (95% interval 0.965 to 2.21)
```

The `dgp_parameters` set the latent mean of the price effect to `-1`, so
the true median price coefficient is `-exp(-1)`, about `-0.37`. Dividing
the true time and comfort coefficients by it gives the true trade-offs,
with which the posterior means above can be compared.

The panel structure makes individual coefficients estimable.
`coef(level = "individual")` returns the posterior mean coefficient of
every decider, on the latent normal scale for log-normal effects. The
histogram shows the price coefficient of each of the 100 deciders, all
negative as the log-normal specification enforces, with the left tail
containing the deciders who react most strongly to a higher price.

``` r

individual <- coef(mixing, level = "individual")
head(individual)
#>        price        time    comfort
#> 1 -1.0200039 -0.67214338  0.8740841
#> 2 -1.2903525 -1.34756825 -0.3577251
#> 3 -0.8659240 -0.25212699  0.8996742
#> 4 -0.8699187 -0.28498525  1.2180794
#> 5 -0.7378405  0.03161872  1.1608198
#> 6 -1.1649809 -1.76361959 -0.3922436
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
willingness to pay. The fit uses the first 100 households to keep the
computation short.

``` r

data("Electricity", package = "mlogit")
names(Electricity) <- sub("([a-z]+)([1-4])$", "\\1_\\2", names(Electricity))
Electricity$occasion <- ave(Electricity$id, Electricity$id, FUN = seq_along)
households <- Electricity[Electricity$id %in% unique(Electricity$id)[1:100], ]
electricity <- fit(
  choice ~ pf + cl + loc + wk + tod + seas | 0,
  data = households,
  random_effects = c("cl", "loc"),
  column_decider = "id",
  column_occasion = "occasion",
  scale = c(pf = -1),
  iterations = 4000,
  warmup = 2000,
  thin = 2,
  chains = 1,
  save_individual_draws = TRUE,
  progress = FALSE
)
summary(electricity)
#> Bayesian probit choice model
#> Formula: choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 
#> Samples: 1000 retained per chain, 1 chain
#>        variable   mean    mode     sd  rhat ess_bulk
#>        beta[wk]  1.624  1.5764 0.1391 1.003    159.7
#>       beta[tod] -8.980 -9.0026 0.1567 1.003    128.6
#>      beta[seas] -9.266 -9.2319 0.1518 1.012    245.2
#>          mu[cl] -0.276 -0.2658 0.0609 1.011    523.3
#>         mu[loc]  2.017  1.9528 0.2421 0.999    271.7
#>    Omega[cl,cl]  0.293  0.2564 0.0666 1.002    228.7
#>   Omega[cl,loc]  0.113  0.0905 0.1085 0.999    312.9
#>  Omega[loc,loc]  2.276  2.3401 0.5982 1.002    209.5
#>      Sigma[2,2]  7.522  7.0696 1.3642 1.016     63.1
#>      Sigma[2,3]  2.546  2.6579 0.9192 1.031     18.7
#>      Sigma[3,3]  5.704  5.2130 1.1423 1.013     66.0
#>      Sigma[2,4]  3.811  3.6332 0.9793 1.017     54.8
#>      Sigma[3,4]  1.974  1.6923 0.8011 1.099     12.5
#>      Sigma[4,4]  6.025  5.7407 1.3194 1.009     59.7
```

Because the price coefficient is fixed,
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
reports the coefficients directly as willingness to pay in cents per
kWh, with credible intervals:

``` r

interpret(electricity, effects = c("cl", "loc", "wk"))
#> 1 `cl` compensates -0.276 `pf` (95% interval -0.398 to -0.165)
#> 1 `loc` compensates 2.02 `pf` (95% interval 1.55 to 2.5)
#> 1 `wk` compensates 1.62 `pf` (95% interval 1.38 to 1.92)
```

On average, households would accept a price about 2 cents per kWh higher
for a local supplier, and would need a price about 0.28 cents lower for
each additional year of contract length.

## Latent classes

A single normal distribution is a strong assumption about the
distribution of preferences in a population. There may be commuters and
leisure travelers, or households that focus on price and households that
focus on service. `latent_class_effects` names the effects that differ
between `classes` latent classes. Every decider belongs to exactly one
class. The class weights $`w_1, \dots, w_K`$ are the probabilities of
membership and sum to one, and the class allocation of every decider is
a latent variable that the sampler draws together with the parameters.
Naming a random effect in `latent_class_effects` replaces its normal
distribution by a finite mixture of normals, which yields the
latent-class mixed multinomial probit model of Oelschläger and Bauer
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
  heuristic.

All four updates are demonstrated on one simulated data set with two
classes. The fixed-class fit simulates the data; the other three are
refits through [`update()`](https://rdrr.io/r/stats/update.html), which
reuses the simulated data.

### A fixed number of classes

For `class_update = "fixed"`, `classes = K` fixes the number of classes.
The class weights have the prior
$`(w_1,\ldots,w_K)\sim\operatorname{Dirichlet}(\delta,\ldots,\delta)`$,
where the default `class_concentration = 1` is uniform on the weight
simplex. The class labels are arbitrary: swapping them leaves the
likelihood unchanged, so the sampler may swap them during a run.
**RprobitB** therefore relabels the retained draws after sampling, so
that every draw uses the same labels ([Dahl 2006](#ref-Dahl2006);
[Papastamoulis and Iliopoulos 2010](#ref-Papastamoulis2010)), and
numbers the classes by decreasing weight.

The following demonstration uses eight occasions per decider and two
well-separated classes: a majority with coefficients centered at `-1`
and a minority centered at `2`.

``` r

mixture <- fit(
  choice ~ x | 0,
  random_effects = "x",
  latent_class_effects = "x",
  classes = 2,
  n_deciders = 100,
  n_occasions = 8,
  save_individual_draws = TRUE,
  dgp_parameters = list(
    beta = list(c(x = -1), c(x = 2)),
    Omega = list(matrix(0.2), matrix(0.2)),
    weights = c(0.6, 0.4)
  ),
  iterations = 1500,
  chains = 2,
  progress = FALSE
)
summary(mixture, variables = c(
  "weight[1]", "weight[2]", "mu[x,1]", "mu[x,2]",
  "Omega[x,x,1]", "Omega[x,x,2]"
))
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 750 retained per chain, 2 chains
#> 
#>      variable  dgp   mean   mode     sd rhat ess_bulk
#>     weight[1]  0.6  0.599  0.612 0.0558 1.02    228.0
#>     weight[2]  0.4  0.401  0.388 0.0558 1.02    228.0
#>       mu[x,1] -1.0 -0.970 -0.956 0.1171 1.03    117.4
#>       mu[x,2]  2.0  2.016  1.980 0.3179 1.07     33.6
#>  Omega[x,x,1]  0.2  0.278  0.183 0.1465 1.03     34.9
#>  Omega[x,x,2]  0.2  0.505  0.248 0.4485 1.04     24.6
```

[`latent_class_diagnostics()`](https://loelschlaeger.de/RprobitB/reference/latent_class_diagnostics.md)
returns the posterior distribution of the number of occupied classes,
the membership probabilities of the deciders after relabeling, and the
co-clustering matrix. Its entries are the posterior probabilities that
two deciders belong to the same class:

``` r

class_diagnostics <- latent_class_diagnostics(mixture)
class_diagnostics$occupancy
#>   n_classes probability
#> 1         1           0
#> 2         2           1
class_diagnostics$membership[1:6, ]
#>       class_1     class_2
#> 1 0.996000000 0.004000000
#> 2 0.005333333 0.994666667
#> 3 0.006666667 0.993333333
#> 4 0.994000000 0.006000000
#> 5 0.054666667 0.945333333
#> 6 0.994666667 0.005333333
class_diagnostics$co_clustering[1:6, 1:6]
#>             1           2          3          4          5          6
#> 1 1.000000000 0.009333333 0.01066667 0.99000000 0.05866667 0.99200000
#> 2 0.009333333 1.000000000 0.98800000 0.01133333 0.94133333 0.01066667
#> 3 0.010666667 0.988000000 1.00000000 0.01266667 0.94000000 0.01200000
#> 4 0.990000000 0.011333333 0.01266667 1.00000000 0.06066667 0.98866667
#> 5 0.058666667 0.941333333 0.94000000 0.06066667 1.00000000 0.06000000
#> 6 0.992000000 0.010666667 0.01200000 0.98866667 0.06000000 1.00000000
```

### Class-specific coefficients

Do the train travelers of the vignette [Get started with
RprobitB](https://loelschlaeger.de/RprobitB/articles/v01_get_started.html)
all trade time against money at the same rate, or are there classes with
different values of time? The fit below uses the first 60 travelers,
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
train_small <- Train[Train$id %in% unique(Train$id)[1:60], ]
train_classes <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = train_small,
  latent_class_effects = "time",
  classes = 2,
  column_decider = "id",
  column_occasion = "choiceid",
  scale = c(price = -1),
  iterations = 1500,
  chains = 2,
  progress = FALSE
)
summary(train_classes, variables = c(
  "weight[1]", "weight[2]", "beta[time,1]", "beta[time,2]"
))
#> Bayesian probit choice model
#> Formula: choice ~ price + time + change + factor(comfort) | 0 | 0 
#> Samples: 750 retained per chain, 2 chains
#> 
#>      variable    mean    mode     sd rhat ess_bulk
#>     weight[1]   0.737   0.747 0.0838 1.04     51.9
#>     weight[2]   0.263   0.253 0.0838 1.04     51.9
#>  beta[time,1]  -2.452  -2.292 0.9905 1.04    127.1
#>  beta[time,2] -17.957 -16.472 3.1601 1.06     38.8
```

With the price coefficient fixed at `-1`, the class-specific time
coefficients are values of travel time in euro per hour, which
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
reports class by class:

``` r

time_by_class <- interpret(train_classes, effects = "time")
time_by_class
#> Class 1: 1 `time` compensates -2.45 `price` (95% interval -4.39 to -0.457)
#> Class 2: 1 `time` compensates -18 `price` (95% interval -25.1 to -12.9)
```

The larger class values an hour at about 2 euro, the smaller class at
about 18 euro. The smaller class chooses the faster trip almost
regardless of its price.

### Weight-based class updates

Oelschläger and Bauer ([2021](#ref-Oelschlaeger2021)) presents a
weight-based update scheme for latent class analysis. Every `buffer`
warmup iterations, it removes the smallest class if its weight is below
`epsmin`, splits the largest class if its weight is above `epsmax`, or
merges the closest pair of classes if the distance of their means is
below `deltamin`, at most one operation in this order.
`weight_based_control` overrides the defaults of these constants, and
`max_classes` bounds the splitting. These dimension changes correspond
to no prior on the number of classes, and they stop after warmup, so the
reported `n_classes` is the outcome of a search, not a posterior
distribution.

This refit and the two in the following subsections change the class
update, so [`summary()`](https://rdrr.io/r/base/summary.html) has no
`dgp` column for them. The helper `recover_classes()` provides the
comparison with the true values instead. The classes are numbered by
decreasing weight, so the first class should be the majority with weight
`0.6` and mean `-1`, and the second class the minority with weight `0.4`
and mean `2`. The helper puts the posterior means of these four
variables and the most probable number of occupied classes beside the
true values. Applied to the fit with two fixed classes, it gives the
reference for the refits:

``` r

recover_classes <- function(x) {
  variables <- c("weight[1]", "weight[2]", "mu[x,1]", "mu[x,2]")
  occupancy <- latent_class_diagnostics(x)$occupancy
  data.frame(
    variable = c("n_classes", variables),
    dgp = c(2, 0.6, 0.4, -1, 2),
    estimate = round(c(
      occupancy$n_classes[which.max(occupancy$probability)],
      coef(x)[variables]
    ), 2),
    row.names = NULL
  )
}
recover_classes(mixture)
#>    variable  dgp estimate
#> 1 n_classes  2.0     2.00
#> 2 weight[1]  0.6     0.60
#> 3 weight[2]  0.4     0.40
#> 4   mu[x,1] -1.0    -0.97
#> 5   mu[x,2]  2.0     2.02
```

The weight-based refit is compared in the same way:

``` r

weight_based <- update(mixture, class_update = "weight_based")
recover_classes(weight_based)
#>    variable  dgp estimate
#> 1 n_classes  2.0     2.00
#> 2 weight[1]  0.6     0.59
#> 3 weight[2]  0.4     0.41
#> 4   mu[x,1] -1.0    -0.97
#> 5   mu[x,2]  2.0     2.07
```

The run ends with the two classes that generated the data, and their
weights and means agree closely with those of the fixed fit.

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
At the prior mean $`e_0 = 0.005`$, with $`K = 6`$ and the 100 deciders
of the simulated data, the prior expects close to a single occupied
class. The refit sets the upper bound to six classes for the two-class
data.

``` r

sparse <- update(mixture, classes = 6, class_update = "sparse")
summary(sparse)
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 750 retained per chain, 2 chains
#> 
#>             variable occupied     mean     mode      sd rhat ess_bulk
#>            weight[1]  1.00000  0.60878  0.62153 0.05298 1.00   316.69
#>            weight[2]  1.00000  0.39005  0.37736 0.05273 1.00   337.03
#>            weight[3]  0.02133  0.02858  0.01073 0.02138   NA       NA
#>            weight[4]  0.00733  0.01301  0.00363 0.02024   NA       NA
#>            weight[5]  0.00467  0.01266  0.00564 0.01420   NA       NA
#>            weight[6]  0.00133  0.00423  0.00731 0.00436   NA       NA
#>              mu[x,1]  1.00000 -0.95243 -0.95808 0.12464 1.03    92.87
#>              mu[x,2]  1.00000  2.13124  2.11263 0.35323 1.16     9.74
#>              mu[x,3]  0.02133  0.40818  0.39094 1.19553   NA       NA
#>              mu[x,4]  0.00733  2.32226  0.97869 2.43339   NA       NA
#>              mu[x,5]  0.00467 -0.54511  0.20298 1.04749   NA       NA
#>              mu[x,6]  0.00133 -0.81074 -2.33580 2.16523   NA       NA
#>         Omega[x,x,1]  1.00000  0.28914  0.21966 0.15896 1.07    40.23
#>         Omega[x,x,2]  1.00000  0.42953  0.22110 0.33864 1.05    29.34
#>         Omega[x,x,3]  0.02133  0.71605  0.28818 1.05517   NA       NA
#>         Omega[x,x,4]  0.00733  0.98367  0.45090 1.44551   NA       NA
#>         Omega[x,x,5]  0.00467  0.95121  0.61553 0.68835   NA       NA
#>         Omega[x,x,6]  0.00133  1.34552  1.73501 0.55299   NA       NA
#>  class_concentration  1.00000  0.01055  0.00548 0.00622 1.05    31.31
#>            n_classes  1.00000  2.03467  2.00000 0.18300 1.01   266.40
latent_class_diagnostics(sparse)$occupancy
#>   n_classes probability
#> 1         1  0.00000000
#> 2         2  0.96533333
#> 3         3  0.03466667
#> 4         4  0.00000000
#> 5         5  0.00000000
#> 6         6  0.00000000
recover_classes(sparse)
#>    variable  dgp estimate
#> 1 n_classes  2.0     2.00
#> 2 weight[1]  0.6     0.61
#> 3 weight[2]  0.4     0.39
#> 4   mu[x,1] -1.0    -0.95
#> 5   mu[x,2]  2.0     2.13
```

Although the prior favors a single class, the posterior concentrates on
two occupied classes, whose weights and means are close to the true
values. When the number of classes varies, a class exists only in part
of the draws, and the `occupied` column of
[`summary()`](https://rdrr.io/r/base/summary.html) reports this share.

### Dirichlet-process mixtures

For `class_update = "dirichlet_process"`, the precision $`\alpha`$
controls the prior tendency to open new classes, with the default
$`\alpha\sim\operatorname{Gamma}(2,4)`$ of mean `0.5`. Conditional on a
fixed $`\alpha`$, the prior expects
$`\sum_{i=1}^{N}\alpha/(\alpha+i-1)`$ occupied classes among $`N`$
deciders, which for $`\alpha = 0.5`$ and 100 deciders is about three.
**RprobitB** updates $`\alpha`$ with the augmentation of Escobar and
West ([1995](#ref-Escobar1995)) and the allocations with the algorithm
of Neal ([2000](#ref-Neal2000)). `max_classes` caps the number of
classes the sampler may open, and
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) warns when
the draws reach the cap.

``` r

dynamic <- update(
  mixture, class_update = "dirichlet_process", max_classes = 15,
  iterations = 1000
)
summary(dynamic)
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 500 retained per chain, 2 chains
#> 
#>             variable occupied    mean    mode      sd rhat ess_bulk
#>            weight[1]    1.000  0.5685  0.5958 0.05849 1.09    82.57
#>            weight[2]    1.000  0.3537  0.3901 0.06813 1.19     8.41
#>            weight[3]    0.742  0.0635  0.0168 0.06162   NA       NA
#>            weight[4]    0.451  0.0443  0.0141 0.04632   NA       NA
#>            weight[5]    0.224  0.0322  0.0108 0.03460   NA       NA
#>            weight[6]    0.106  0.0242  0.0100 0.02979   NA       NA
#>            weight[7]    0.041  0.0185  0.0100 0.01459   NA       NA
#>            weight[8]    0.012  0.0158  0.0102 0.00793   NA       NA
#>            weight[9]    0.004  0.0100  0.0100 0.00000   NA       NA
#>              mu[x,1]    1.000 -0.9258 -0.9635 0.13235 1.06    52.42
#>              mu[x,2]    1.000  2.2077  2.3205 0.38147 1.38     4.74
#>              mu[x,3]    0.742  0.1650  1.9745 2.40162   NA       NA
#>              mu[x,4]    0.451 -0.2067 -0.7589 2.40757   NA       NA
#>              mu[x,5]    0.224  0.3518  1.5561 2.36452   NA       NA
#>              mu[x,6]    0.106  0.1140  1.1443 2.56043   NA       NA
#>              mu[x,7]    0.041  0.5399  1.7911 2.93945   NA       NA
#>              mu[x,8]    0.012 -1.2729 -3.0764 2.64053   NA       NA
#>              mu[x,9]    0.004  0.1918  2.5965 4.25500   NA       NA
#>         Omega[x,x,1]    1.000  0.2387  0.1741 0.12861 1.14    42.60
#>         Omega[x,x,2]    1.000  0.5320  0.2344 0.57860 1.16    15.86
#>         Omega[x,x,3]    0.742  0.6916  0.2411 0.93024   NA       NA
#>         Omega[x,x,4]    0.451  0.7536  0.2373 2.28477   NA       NA
#>         Omega[x,x,5]    0.224  0.8659  0.2684 1.61849   NA       NA
#>         Omega[x,x,6]    0.106  0.6208  0.2596 1.06427   NA       NA
#>         Omega[x,x,7]    0.041  0.6214  0.2427 0.64990   NA       NA
#>         Omega[x,x,8]    0.012  1.5313  0.4921 2.05348   NA       NA
#>         Omega[x,x,9]    0.004  0.5778  0.4473 0.24483   NA       NA
#>  class_concentration    1.000  0.5463  0.4323 0.32123 1.04   132.92
#>            n_classes    1.000  3.5800  3.0000 1.41972 1.10    23.33
latent_class_diagnostics(dynamic)$occupancy
#>    n_classes probability
#> 1          1       0.000
#> 2          2       0.258
#> 3          3       0.291
#> 4          4       0.227
#> 5          5       0.118
#> 6          6       0.065
#> 7          7       0.029
#> 8          8       0.008
#> 9          9       0.004
#> 10        10       0.000
#> 11        11       0.000
#> 12        12       0.000
#> 13        13       0.000
#> 14        14       0.000
#> 15        15       0.000
recover_classes(dynamic)
#>    variable  dgp estimate
#> 1 n_classes  2.0     3.00
#> 2 weight[1]  0.6     0.57
#> 3 weight[2]  0.4     0.35
#> 4   mu[x,1] -1.0    -0.93
#> 5   mu[x,2]  2.0     2.21
```

The occupancy distribution is wider than under the sparse finite prior
and assigns most of its probability to three or more classes. This
reflects the prior, which expects about three occupied classes for 100
deciders. The two largest classes nevertheless match the two true
groups, because the superfluous classes contain only a few deciders.

## Ordered and ranked responses

Both mechanisms also work for ordered and ranked responses, which the
vignette [Model specification and
variants](https://loelschlaeger.de/RprobitB/articles/v02_model_variants.html)
introduces. The following demonstration uses a simulated panel of
rankings: 100 deciders order three alternatives five times each, with a
coefficient that varies normally around `-1`.

``` r

ranked_random <- fit(
  rank ~ x | 0,
  choice_type = "ranked",
  random_effects = "x",
  n_deciders = 100,
  n_occasions = 5,
  n_alternatives = 3,
  dgp_parameters = list(beta = c(x = -1), Omega = matrix(0.3)),
  iterations = 2000,
  chains = 1,
  progress = FALSE
)
summary(ranked_random)
#> Bayesian probit choice model
#> Formula: rank ~ x | 0 | 0 
#> Samples: 1000 retained per chain, 1 chain
#>    variable    dgp   mean   mode     sd rhat ess_bulk
#>       mu[x] -1.000 -1.080 -1.082 0.0965 1.02     43.6
#>  Omega[x,x]  0.300  0.319  0.315 0.0952 1.04     41.4
#>  Sigma[B,C] -0.112 -0.271 -0.233 0.1778 1.00    128.8
#>  Sigma[C,C]  3.001  3.516  3.104 0.7839 1.01     65.7
```

The summary shows the population mean and variance beside their true
values. Ordered responses accept the same arguments, and latent classes
are requested in the same way as above, with `latent_class_effects` and
`classes`.

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

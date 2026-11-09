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
background. Random coefficients and latent classes are first estimated
on simulated data, where the estimates can be compared with the
parameters that generated them, and then applied to data of the
**mlogit** package ([Croissant 2020](#ref-Croissant2020)).

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
attribute whose sign is fixed by theory: a higher price is not expected
to raise the utility of any decider. Log-normal effects are estimated on
the latent normal scale, so `mu`, `Omega`, and the individual draws
refer to the normal variable whose exponential enters the utility. The
second choice is whether an effect is correlated with the other
correlated effects. Deciders who value travel time may also value
comfort, and the `c` prefix adds the covariance between such effects to
the model. An uncorrelated effect has its own variance but no covariance
with any other effect, which appears as zeros in `Omega`.

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

The mixing distribution of an effect is the distribution of its
coefficient in the population. `plot(type = "mixture")` draws its
estimate, the density that `mu` and `Omega` imply, averaged over the
retained draws. For simulated data, dashed lines add the mixing
distributions that generated them:

``` r

plot(mixing, type = "mixture")
```

![](v03_heterogeneity_files/figure-html/mixing-plot-1.png)

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
the negated true time and comfort coefficients by it gives the true
trade-offs, with which the posterior means above can be compared.

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
#>        variable   mean   mode     sd rhat ess_bulk
#>        beta[wk]  1.630  1.625 0.1428 1.00    205.1
#>       beta[tod] -9.017 -9.027 0.1518 1.00    210.0
#>      beta[seas] -9.272 -9.232 0.1537 1.01    146.7
#>          mu[cl] -0.281 -0.282 0.0632 1.00    297.0
#>         mu[loc]  2.023  1.983 0.2381 1.00    287.1
#>    Omega[cl,cl]  0.300  0.293 0.0684 1.00    178.9
#>   Omega[cl,loc]  0.108  0.098 0.1122 1.00    313.4
#>  Omega[loc,loc]  2.267  2.093 0.6786 1.02    140.2
#>      Sigma[2,2]  7.462  6.918 1.3687 1.00     47.1
#>      Sigma[2,3]  2.009  1.866 0.7401 1.04     22.7
#>      Sigma[3,3]  5.485  5.021 1.1418 1.01    101.9
#>      Sigma[2,4]  4.060  4.168 1.0430 1.01     54.9
#>      Sigma[3,4]  1.980  2.040 0.7511 1.01     45.8
#>      Sigma[4,4]  6.413  6.079 1.3494 1.03     67.9
```

Because the price coefficient is fixed,
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
reports the coefficients directly as willingness to pay in cents per
kWh, with credible intervals:

``` r

interpret(electricity, effects = c("cl", "loc", "wk"))
#> 1 `cl` compensates -0.281 `pf` (95% interval -0.407 to -0.167)
#> 1 `loc` compensates 2.02 `pf` (95% interval 1.57 to 2.55)
#> 1 `wk` compensates 1.63 `pf` (95% interval 1.38 to 1.94)
```

On average, households would accept a price about 2 cents per kWh higher
for a local supplier, and would need a price about 0.28 cents lower for
each additional year of contract length.

The estimated mixing distributions show how these valuations vary
between households:

``` r

plot(electricity, type = "mixture") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
  ggplot2::labs(x = "willingness to pay in cents per kWh")
```

![](v03_heterogeneity_files/figure-html/electricity-plot-1.png)

``` r

draws <- posterior::as_draws_matrix(electricity)
longer <- mean(pnorm(
  0, draws[, "mu[cl]"], sqrt(draws[, "Omega[cl,cl]"]), lower.tail = FALSE
))
distant <- mean(pnorm(0, draws[, "mu[loc]"], sqrt(draws[, "Omega[loc,loc]"])))
```

Although the average household prefers shorter contracts, the estimated
mixing distribution assigns a positive valuation of contract length to
about 30 percent of the households, and about 9 percent prefer a
supplier that is not local.

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
#>     weight[1]  0.6  0.615  0.615 0.0551 1.01   264.74
#>     weight[2]  0.4  0.385  0.385 0.0551 1.01   264.74
#>       mu[x,1] -1.0 -0.993 -0.974 0.1065 1.02   118.37
#>       mu[x,2]  2.0  1.951  1.928 0.3029 1.08    39.07
#>  Omega[x,x,1]  0.2  0.205  0.148 0.0994 1.01    77.91
#>  Omega[x,x,2]  0.2  0.429  0.199 0.4127 1.25     7.33
```

For an effect with latent classes, the mixing distribution is the
mixture of the class-specific normal distributions, weighted by the
class weights:

``` r

plot(mixture, type = "mixture")
```

![](v03_heterogeneity_files/figure-html/classes-plot-1.png)

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
#> 1 0.995333333 0.004666667
#> 2 0.996000000 0.004000000
#> 3 0.982000000 0.018000000
#> 4 0.006666667 0.993333333
#> 5 0.017333333 0.982666667
#> 6 0.997333333 0.002666667
class_diagnostics$co_clustering[1:6, 1:6]
#>            1          2          3           4          5           6
#> 1 1.00000000 0.99133333 0.97866667 0.011333333 0.02200000 0.992666667
#> 2 0.99133333 1.00000000 0.98466667 0.010666667 0.02133333 0.993333333
#> 3 0.97866667 0.98466667 1.00000000 0.024666667 0.03533333 0.983333333
#> 4 0.01133333 0.01066667 0.02466667 1.000000000 0.97733333 0.009333333
#> 5 0.02200000 0.02133333 0.03533333 0.977333333 1.00000000 0.020000000
#> 6 0.99266667 0.99333333 0.98333333 0.009333333 0.02000000 1.000000000
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
#>     weight[1]   0.766   0.788 0.0936 1.02     69.5
#>     weight[2]   0.234   0.212 0.0936 1.02     69.5
#>  beta[time,1]  -2.825  -2.980 1.0695 1.01     77.9
#>  beta[time,2] -19.827 -17.744 5.3470 1.05     36.3
```

With the price coefficient fixed at `-1`, the class-specific time
coefficients give the values of travel time in euro per hour, which
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
reports class by class:

``` r

time_by_class <- interpret(train_classes, effects = "time")
time_by_class
#> Class 1: 1 `time` compensates -2.83 `price` (95% interval -4.78 to -0.548)
#> Class 2: 1 `time` compensates -19.8 `price` (95% interval -36.6 to -12.5)
```

The larger class values an hour at about 3 euro, the smaller class at
about 20 euro. The smaller class is willing to pay much more for a
faster trip.

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
#> 2 weight[1]  0.6     0.61
#> 3 weight[2]  0.4     0.39
#> 4   mu[x,1] -1.0    -0.99
#> 5   mu[x,2]  2.0     1.95
```

The weight-based refit is compared in the same way:

``` r

weight_based <- update(mixture, class_update = "weight_based")
recover_classes(weight_based)
#>    variable  dgp estimate
#> 1 n_classes  2.0     2.00
#> 2 weight[1]  0.6     0.62
#> 3 weight[2]  0.4     0.38
#> 4   mu[x,1] -1.0    -0.97
#> 5   mu[x,2]  2.0     1.97
```

The run ends with the two classes that generated the data, and their
weights and means agree closely with those of the fixed fit.

### Sparse finite mixtures

A sparse finite mixture fixes a generous upper bound $`K`$, permits
empty classes, and places the symmetric Dirichlet prior with a small
concentration $`e_0`$ on the weights, which favors emptying redundant
classes ([Rousseau and Mengersen 2011](#ref-Rousseau2011);
[Malsiner-Walli et al. 2016](#ref-MalsinerWalli2016);
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
#>            weight[1]  1.00000  0.61835  0.61433 0.05193 1.00    443.5
#>            weight[2]  1.00000  0.37919  0.38413 0.05271 1.00    388.7
#>            weight[3]  0.02067  0.04197  0.00889 0.05361   NA       NA
#>            weight[4]  0.02600  0.02985  0.00773 0.03217   NA       NA
#>            weight[5]  0.01533  0.02698  0.00978 0.02468   NA       NA
#>            weight[6]  0.00133  0.01523  0.02010 0.00691   NA       NA
#>              mu[x,1]  1.00000 -0.98887 -0.97461 0.10705 1.04     90.6
#>              mu[x,2]  1.00000  1.97024  1.99895 0.29271 1.11     16.8
#>              mu[x,3]  0.02067  1.98734  1.05723 1.92562   NA       NA
#>              mu[x,4]  0.02600  1.79189  2.42213 1.80330   NA       NA
#>              mu[x,5]  0.01533  0.68890  1.44201 1.23744   NA       NA
#>              mu[x,6]  0.00133 -0.76099 -0.55953 0.28602   NA       NA
#>         Omega[x,x,1]  1.00000  0.22229  0.17942 0.10520 1.05     51.6
#>         Omega[x,x,2]  1.00000  0.48744  0.21460 0.41360 1.18      8.3
#>         Omega[x,x,3]  0.02067  0.69516  0.30733 0.81896   NA       NA
#>         Omega[x,x,4]  0.02600  0.65970  0.35119 0.48719   NA       NA
#>         Omega[x,x,5]  0.01533  1.17043  0.44045 1.38167   NA       NA
#>         Omega[x,x,6]  0.00133  0.25652  0.31853 0.08805   NA       NA
#>  class_concentration  1.00000  0.00911  0.00615 0.00520 1.03     44.5
#>            n_classes  1.00000  2.06333  2.00000 0.25955 1.01    162.8
latent_class_diagnostics(sparse)$occupancy
#>   n_classes  probability
#> 1         1 0.0000000000
#> 2         2 0.9400000000
#> 3         3 0.0573333333
#> 4         4 0.0020000000
#> 5         5 0.0006666667
#> 6         6 0.0000000000
recover_classes(sparse)
#>    variable  dgp estimate
#> 1 n_classes  2.0     2.00
#> 2 weight[1]  0.6     0.62
#> 3 weight[2]  0.4     0.38
#> 4   mu[x,1] -1.0    -0.99
#> 5   mu[x,2]  2.0     1.97
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
West ([1995](#ref-Escobar1995)) and the allocations with the
auxiliary-parameter algorithm of Neal ([2000](#ref-Neal2000)).
`max_classes` caps the number of classes the sampler may open, and
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
#>            weight[1]    1.000  0.5674  0.6140 0.08093 1.18     8.66
#>            weight[2]    1.000  0.3219  0.3686 0.06864 1.05    46.30
#>            weight[3]    0.753  0.0719  0.0179 0.06182   NA       NA
#>            weight[4]    0.538  0.0581  0.0170 0.06005   NA       NA
#>            weight[5]    0.353  0.0375  0.0124 0.03904   NA       NA
#>            weight[6]    0.201  0.0374  0.0114 0.04670   NA       NA
#>            weight[7]    0.104  0.0271  0.0100 0.03513   NA       NA
#>            weight[8]    0.048  0.0213  0.0102 0.01886   NA       NA
#>            weight[9]    0.021  0.0219  0.0113 0.01887   NA       NA
#>           weight[10]    0.005  0.0320  0.0252 0.01924   NA       NA
#>           weight[11]    0.004  0.0100  0.0100 0.00000   NA       NA
#>           weight[12]    0.002  0.0150  0.0100 0.00707   NA       NA
#>           weight[13]    0.001  0.0100  0.0100      NA   NA       NA
#>              mu[x,1]    1.000 -0.9901 -0.9625 0.13175 1.03    85.78
#>              mu[x,2]    1.000  1.9511  1.9086 0.37249 1.14    18.81
#>              mu[x,3]    0.753  1.4727  1.8602 2.44370   NA       NA
#>              mu[x,4]    0.538  0.9529  1.9814 2.45653   NA       NA
#>              mu[x,5]    0.353  0.5896 -0.9046 2.57491   NA       NA
#>              mu[x,6]    0.201  0.7554  1.2375 2.44746   NA       NA
#>              mu[x,7]    0.104  0.3069 -0.4801 2.72347   NA       NA
#>              mu[x,8]    0.048  0.7130  2.6610 2.58592   NA       NA
#>              mu[x,9]    0.021  0.7823 -0.8749 2.97902   NA       NA
#>             mu[x,10]    0.005  0.3475 -1.1589 3.78488   NA       NA
#>             mu[x,11]    0.004  3.1974  5.6614 3.19311   NA       NA
#>             mu[x,12]    0.002  2.0908  6.0268 5.58832   NA       NA
#>             mu[x,13]    0.001  1.9332  1.9332      NA   NA       NA
#>         Omega[x,x,1]    1.000  0.2029  0.1460 0.12252 1.02    55.94
#>         Omega[x,x,2]    1.000  0.5947  0.2151 0.56409 1.22     7.62
#>         Omega[x,x,3]    0.753  0.6521  0.2248 0.99887   NA       NA
#>         Omega[x,x,4]    0.538  1.1820  0.5089 9.50227   NA       NA
#>         Omega[x,x,5]    0.353  0.7916  0.2739 1.44138   NA       NA
#>         Omega[x,x,6]    0.201  0.7411  0.2451 1.07469   NA       NA
#>         Omega[x,x,7]    0.104  0.8198  0.2583 1.27842   NA       NA
#>         Omega[x,x,8]    0.048  0.5669  0.2561 0.50974   NA       NA
#>         Omega[x,x,9]    0.021  0.8982  0.4377 0.71027   NA       NA
#>        Omega[x,x,10]    0.005  0.2674  0.2338 0.05980   NA       NA
#>        Omega[x,x,11]    0.004  1.5863  0.3630 2.44078   NA       NA
#>        Omega[x,x,12]    0.002  0.5082  0.8981 0.55351   NA       NA
#>        Omega[x,x,13]    0.001  0.4603  0.4603      NA   NA       NA
#>  class_concentration    1.000  0.6018  0.3300 0.36393 1.05    48.96
#>            n_classes    1.000  4.0300  2.0000 1.83316 1.14    11.67
latent_class_diagnostics(dynamic)$occupancy
#>    n_classes probability
#> 1          1       0.000
#> 2          2       0.247
#> 3          3       0.215
#> 4          4       0.185
#> 5          5       0.152
#> 6          6       0.097
#> 7          7       0.056
#> 8          8       0.027
#> 9          9       0.016
#> 10        10       0.001
#> 11        11       0.002
#> 12        12       0.001
#> 13        13       0.001
#> 14        14       0.000
#> 15        15       0.000
recover_classes(dynamic)
#>    variable  dgp estimate
#> 1 n_classes  2.0     2.00
#> 2 weight[1]  0.6     0.57
#> 3 weight[2]  0.4     0.32
#> 4   mu[x,1] -1.0    -0.99
#> 5   mu[x,2]  2.0     1.95
```

The occupancy distribution is wider than under the sparse finite prior
and assigns most of its probability to three or more classes. This
reflects the prior, which expects about three occupied classes for 100
deciders. The two largest classes nevertheless resemble the two true
groups, because the superfluous classes contain only a few deciders.

The estimated mixing distributions of the three refits also show the two
groups that generated the data:

``` r

bayesplot::bayesplot_grid(
  plot(weight_based, type = "mixture"),
  plot(sparse, type = "mixture"),
  plot(dynamic, type = "mixture"),
  titles = c("weight-based", "sparse", "Dirichlet process"),
  grid_args = list(nrow = 1)
)
```

![](v03_heterogeneity_files/figure-html/refits-plot-1.png)

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
#>    variable    dgp   mean   mode     sd  rhat ess_bulk
#>       mu[x] -1.000 -0.992 -0.963 0.0883 1.010     33.5
#>  Omega[x,x]  0.300  0.396  0.357 0.0957 1.048     17.4
#>  Sigma[B,C] -0.205 -0.244 -0.211 0.0758 0.999     96.7
#>  Sigma[C,C]  0.408  0.555  0.482 0.1272 1.049     30.8
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

Malsiner-Walli, Gertraud, Sylvia Frühwirth-Schnatter, and Bettina Grün.
2016. “Model-Based Clustering Based on Sparse Finite Gaussian Mixtures.”
*Statistics and Computing* 26 (1–2): 303–24.
<https://doi.org/10.1007/s11222-014-9500-2>.

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

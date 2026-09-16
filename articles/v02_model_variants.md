# Model specification and variants

Every model variant of **RprobitB** ([Oelschläger and Bauer
2026](#ref-Oelschlaeger2026d)) is requested through arguments of
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md): which
covariates get which kind of coefficient, whether the choice sets differ
between occasions, and whether the response is a choice, an ordered
category, or a ranking. This vignette starts with the two things every
probit fit rests on, the normalization and the prior, and then goes
through the variants one by one. Letting preferences differ between
deciders has a vignette of its own, and Oelschläger
([2026](#ref-Oelschlaeger2026c)) lays out the methodological background.

From the covariate types onward, each section first fits the variant to
simulated data and then to a data set of the **mlogit** package
([Croissant 2020](#ref-Croissant2020)) or the **MASS** package
([Venables and Ripley 2002](#ref-VenablesRipley2002)). With
`data = NULL`,
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) simulates
the requested model before estimating it, and
[`summary()`](https://rdrr.io/r/base/summary.html) prints the true
values in a `dgp` column beside the posterior summaries, converted to
the normalization of the fit; where the data were simulated by hand, a
small table does the same. The simulated fits use one chain, the
empirical ones two, so that their diagnostics can be read.

``` r

library(RprobitB)
set.seed(1)
```

## Normalization

Utilities are identified only up to level and scale: adding a constant
to all of them or multiplying them by a positive number changes nothing
about which alternative wins. **RprobitB** therefore works with
differences relative to a base alternative, chosen by `base` and
defaulting to the first alternative in alphabetical order, and fixes one
parameter through `scale`. How this is done is a central design question
of Bayesian multinomial probit models ([McCulloch and Rossi
1994](#ref-McCulloch1994); [Imai and van Dyk 2005a](#ref-Imai2005a)):

- `scale = NULL` fixes the error variance of the first utility
  difference to one. This is the default and yields the posterior
  variable names `Sigma[B,B]`, `Sigma[C,B]`, and so on.
- `scale = c(price = -1)` fixes a coefficient instead. All other
  coefficients are then measured in units of that coefficient, which is
  convenient when willingness-to-pay measures are the goal.
- `scale = c("Sigma_B,B" = 1)` names the fixed variance explicitly.

The sampler draws all parameters without restriction and rescales every
retained draw afterwards. Variables that the normalization fixes stay in
the draws but are left out of
[`summary()`](https://rdrr.io/r/base/summary.html),
[`coef()`](https://rdrr.io/r/stats/coef.html), and
[`vcov()`](https://rdrr.io/r/stats/vcov.html).

The proof of concept shows what the normalization does to the numbers.
Data generated with coefficients `1` for `x` and `-0.5` for `z` are
fitted once under the default scale and once with the coefficient of `z`
fixed to `-1`. Seeding both calls identically makes
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) simulate
the same data, so only the normalization differs.

``` r

set.seed(1)
scale_default <- fit(
  choice ~ x + z | 0,
  dgp_parameters = list(beta = c(x = 1, z = -0.5)),
  n_deciders = 300,
  chains = 1
)
summary(scale_default)
#> Bayesian probit choice model
#> Formula: choice ~ x + z | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>  variable  dgp   mean   mode     sd rhat ess_bulk
#>   beta[x]  1.0  1.124  1.079 0.1055    1     36.2
#>   beta[z] -0.5 -0.617 -0.604 0.0877    1     51.1
set.seed(1)
scale_z <- fit(
  choice ~ x + z | 0,
  dgp_parameters = list(beta = c(x = 1, z = -0.5)),
  scale = c(z = -1),
  n_deciders = 300,
  chains = 1
)
summary(scale_z)
#> Bayesian probit choice model
#> Formula: choice ~ x + z | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>    variable dgp mean mode    sd rhat ess_bulk
#>     beta[x]   2 1.85 1.79 0.253 1.01     98.8
#>  Sigma[B,B]   4 2.81 2.16 0.926 1.00     51.1
```

Under the default scale, the `dgp` column shows the coefficients as
specified, and `Sigma[B,B]` is fixed to one and therefore not listed.
Under the coefficient normalization, `beta[z]` is the fixed variable,
and everything else is expressed in units of it: the true `beta[x]`
becomes `2`, the true error variance `4`, and the estimates follow
within their posterior uncertainty.
[`summary()`](https://rdrr.io/r/base/summary.html) always converts the
truth to the normalization of the fit, so the columns are comparable in
both cases.

## Prior distribution

The prior is conjugate for every Gibbs update; the section “Prior
distribution” of
[`?fit`](https://loelschlaeger.de/RprobitB/reference/fit.md) lists all
of its components. Fixed coefficients and class means have normal
priors, covariance matrices have inverse Wishart priors, class weights
have a Dirichlet prior, and the log-increments between ordered
thresholds have a normal prior. The defaults for coefficients and
covariances are weakly informative. The defaults for the mixture
concentrations are a different matter: they encode assumptions about how
many classes are occupied and deserve a sensitivity analysis, as
discussed below. Single components are overridden through a named list,
and the complete prior of a fit is stored in its `prior` component. The
latent-utility data augmentation behind the sampler goes back to Albert
and Chib ([1993](#ref-Albert1993)); Imai and van Dyk
([2005a](#ref-Imai2005a)) developed its marginal augmentation for the
multinomial probit model.

The proof of concept simulates 100 deciders with a true coefficient of
`-1` under the default prior and then refits the same data under two
priors that expect the coefficient near `1`: a moderate one with
variance `0.5` and a tight one with variance `0.01`.
[`update()`](https://rdrr.io/r/stats/update.html) takes care of the
refits: it reuses the simulated data of the first fit and replaces only
the arguments that are named.

``` r

default_prior <- fit(
  choice ~ x | 0,
  dgp_parameters = list(beta = c(x = -1)),
  chains = 1
)
default_prior$prior
#> $fixed_mean
#> [1] 0
#> 
#> $fixed_covariance
#>      [,1]
#> [1,]   10
#> 
#> $error_covariance_df
#> [1] 3
#> 
#> $error_covariance_scale
#>      [,1]
#> [1,]    1
summary(default_prior)
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>  variable dgp  mean  mode    sd rhat ess_bulk
#>   beta[x]  -1 -1.08 -1.01 0.209 1.06     24.3
moderate_prior <- update(
  default_prior, prior = list(fixed_mean = 1, fixed_covariance = matrix(0.5))
)
tight_prior <- update(
  default_prior, prior = list(fixed_mean = 1, fixed_covariance = matrix(0.01))
)
data.frame(
  variable = "beta[x]", dgp = -1, default = coef(default_prior),
  moderate = coef(moderate_prior), tight = coef(tight_prior),
  row.names = NULL
)
#>   variable dgp   default   moderate     tight
#> 1  beta[x]  -1 -1.079866 -0.9457907 0.1108568
```

The default prior recovers the truth within the posterior uncertainty.
The moderate prior pulls the estimate about 0.13 towards its mean, a
visible but harmless nudge that a few hundred more observations would
erase. The tight prior is a different story: it insists on a value near
`1` with a standard deviation of `0.1`, and it drags the estimate all
the way to zero. Priors on coefficients are harmless as long as they are
honest about their uncertainty. The priors on mixture concentrations
further below deserve attention even at their defaults.

## Covariate types and alternative-specific constants

The formula `choice ~ A | B | C` distinguishes three kinds of
covariates: attributes of the alternatives with one shared coefficient
(`A`), characteristics of the decider with alternative-specific
coefficients (`B`), and attributes of the alternatives with
alternative-specific coefficients (`C`). Alternative-specific constants
are included by default and removed with `0` in the second part.

The proof of concept simulates all three types at once. Its summary
lines up the posterior estimates with the normalized true values.

``` r

covariate_types <- fit(
  choice ~ x | z | w,
  n_deciders = 500,
  dgp_parameters = list(beta = c(
    x = 0.5, z_B = -0.5, ASC_B = 0.25, w_A = -0.5, w_B = 0.5
  )),
  chains = 1
)
summary(covariate_types)
#> Bayesian probit choice model
#> Formula: choice ~ x | z | w 
#> Samples: 500 retained per chain, 1 chain
#>     variable   dgp   mean   mode     sd  rhat ess_bulk
#>      beta[x]  0.50  0.527  0.512 0.0604 1.012     80.2
#>    beta[z_B] -0.50 -0.593 -0.583 0.0785 0.998     97.7
#>  beta[ASC_B]  0.25  0.227  0.222 0.0668 0.999    155.4
#>    beta[w_A] -0.50 -0.536 -0.505 0.0817 1.003    122.8
#>    beta[w_B]  0.50  0.608  0.593 0.0774 1.002     82.0
```

All five coefficients are recovered with the right signs and sizes,
including the two alternative-specific effects of `w` and the constant
of alternative `B`. The remaining deviations amount to at most two and a
half posterior standard deviations, which is the sampling variation to
expect from one simulated data set.

`base` chooses which alternative the alternative-specific coefficients
are measured against. The default is the first in alphabetical order,
here `A`; switching to `B` mirrors the constant and the decider effect
without changing the model.

``` r

base_b <- update(covariate_types, base = "B")
coef(base_b)[c("beta[x]", "beta[z_A]", "beta[ASC_A]")]
#>     beta[x]   beta[z_A] beta[ASC_A] 
#>   0.5269383   0.5822533  -0.2248615
```

The shared coefficient of `x` is untouched, while `z` and the constant
now describe alternative `A` relative to `B` and therefore change sign.

Now for a real choice with several alternatives. Where do you go
fishing: the beach, a pier, a private boat, or a charter boat? The
`Fishing` data of the **mlogit** package ([Croissant
2020](#ref-Croissant2020)) contain this decision for 1182 US respondents
together with the cost of a trip and the expected catch rate of each
mode, and their monthly income ([Herriges and Kling
1999](#ref-Herriges1999)). Cost and catch rate vary across modes and
enter as type `A`; income is a property of the respondent and enters as
type `B`. The columns are named `price.beach`, `catch.pier`, and so on,
which `delimiter = "."` tells
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md). Every row
is one respondent, and `column_decider = NULL` numbers them. The first
alternative in alphabetical order, `beach`, is the base, so the
constants and the income coefficients describe the other modes relative
to fishing from the beach.

``` r

data("Fishing", package = "mlogit")
fishing <- fit(
  mode ~ price + catch | income,
  data = Fishing,
  delimiter = ".",
  column_decider = NULL,
  iterations = 15000,
  warmup = 7500,
  thin = 5,
  chains = 2,
  progress = FALSE
)
summary(fishing)
#> Bayesian probit choice model
#> Formula: mode ~ price + catch | income | 0 
#> Samples: 1500 retained per chain, 2 chains
#> 
#>                variable      mean      mode       sd rhat ess_bulk
#>             beta[price] -9.35e-03 -9.03e-03 1.44e-03 1.05     25.6
#>             beta[catch]  3.85e-01  3.81e-01 1.04e-01 1.01     65.9
#>       beta[income.boat]  5.12e-05  5.41e-05 2.39e-05 1.01    342.9
#>    beta[income.charter] -8.20e-05 -6.28e-05 4.89e-05 1.04     33.2
#>       beta[income.pier] -6.75e-05 -4.23e-05 4.83e-05 1.04     26.4
#>          beta[ASC.boat] -2.72e-01 -2.67e-01 1.77e-01 1.02     58.5
#>       beta[ASC.charter]  4.30e-01  3.27e-01 2.74e-01 1.03     35.3
#>          beta[ASC.pier]  4.54e-01  3.16e-01 2.68e-01 1.03     27.7
#>     Sigma[boat,charter] -7.49e-01 -6.78e-01 2.93e-01 1.08     20.7
#>  Sigma[charter,charter]  2.47e+00  1.37e+00 2.26e+00 1.06     20.0
#>        Sigma[boat,pier]  7.93e-02  1.39e-01 3.16e-01 1.06     18.5
#>     Sigma[charter,pier]  1.08e+00  2.68e-01 1.61e+00 1.06     21.8
#>        Sigma[pier,pier]  9.73e-01  4.68e-01 1.16e+00 1.04     27.4
```

Why fifteen thousand iterations for a model with eight coefficients?
Because of the five entries of `Sigma` below them. A free error
covariance between four alternatives is weakly identified when every
respondent is observed only once, and such parameters move slowly
through the posterior. At this length the coefficients are comfortable
and the covariance entries are usable; the default thousand iterations
left `rhat` near 1.5 for them. The fit takes a few seconds, which is
what a cross-section of this size costs.

Higher prices make a mode less and higher catch rates make it more
attractive, the two signs that Herriges and Kling
([1999](#ref-Herriges1999)) also report; they fitted nested logit models
to these data, so the error structure differs while the story does not.
How much is a better catch worth?
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
expresses the catch coefficient in units of the price, so the number is
the price increase that just offsets a one unit higher catch rate:

``` r

catch_value <- interpret(fishing, reference = "price", effects = "catch")
catch_value
#> 1 `catch` compensates 41.3 `price` (95% interval 23.9 to 60.4)
```

Anglers would accept a trip that costs about 41 dollars more for a catch
rate that is one unit higher, and the interval comes from the posterior
of the ratio, not from the ratio of two posterior means.

And what does income do? Its three coefficients are alternative-specific
and hard to read on the utility scale, so `interpret(type = "mea")`
translates them into probabilities at the average angler:

``` r

income_effects <- interpret(fishing, type = "mea")
income_effects[income_effects$covariate == "income", ]
#> Marginal effects at the average covariate values
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative   at      mean       sd     lower     upper
#>     income       beach 4099  9.80e-07 3.14e-06 -5.03e-06  7.35e-06
#>     income        boat 4099  2.72e-05 6.44e-06  1.44e-05  3.97e-05
#>     income     charter 4099 -1.89e-05 6.46e-06 -3.18e-05 -6.42e-06
#>     income        pier 4099 -9.20e-06 3.35e-06 -1.58e-05 -2.76e-06
```

Income is recorded in dollars, so the effects are per dollar and look
tiny. Per thousand dollars of monthly income, anglers move towards the
private boat by about 2.7 percentage points and away from the charter
boat by about 1.9 points. The four effects sum to zero, because an
angler has to fish somewhere.

## Heterogeneous preferences

Coefficients need not be the same for every decider. `random_effects`
lets them vary continuously over the population, and `classes` sorts
deciders into groups with their own coefficients. Both are covered in
[`vignette("v03_heterogeneity")`](https://loelschlaeger.de/RprobitB/articles/v03_heterogeneity.md),
together with the six available mixing distributions and the four ways
of updating latent classes.

## Individual choice sets

Not every alternative is on the menu at every occasion: a traveler
without a car cannot drive, and a route without a rail link has no train
option. Unordered choices may therefore be made from occasion-specific
subsets of the alternatives. In long format, an occasion simply lists
the rows of its available alternatives. The sampler imputes the latent
utilities of unavailable alternatives without any restriction, so they
do not affect the choice, and predictions assign them probability zero.

[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) simulates
complete choice sets only, so the proof of concept simulates its data by
hand, reproducibly through the
[`set.seed()`](https://rdrr.io/r/base/Random.html) call at the top.
Alternatives A and B are always available and C is available to every
second decider. Utilities use a coefficient of one; with A as the base,
the two estimable error differences have unit variances and zero
covariance. Availability is fixed before the choices are simulated.

``` r

choice_set_data <- expand.grid(
  deciderID = 1:500, alternative = LETTERS[1:3]
)
choice_set_data <- subset(
  choice_set_data, alternative != "C" | deciderID %% 2 == 0
)
choice_set_data$x <- rnorm(nrow(choice_set_data))
utility <- choice_set_data$x + ifelse(
  choice_set_data$alternative == "A", 0, rnorm(nrow(choice_set_data))
)
choice_set_data$choice <- ave(
  utility, choice_set_data$deciderID,
  FUN = function(x) as.integer(x == max(x))
)
choice_sets <- fit(
  choice ~ x | 0,
  data = choice_set_data,
  format = "long",
  column_alternative = "alternative",
  chains = 1
)
choice_set_truth <- c("beta[x]" = 1, "Sigma[C,B]" = 0, "Sigma[C,C]" = 1)
choice_set_summary <- summary(
  choice_sets, variables = names(choice_set_truth)
)$posterior
data.frame(
  variable = choice_set_summary$variable,
  dgp = unname(choice_set_truth[choice_set_summary$variable]),
  mean = choice_set_summary$mean,
  mode = choice_set_summary$mode,
  sd = choice_set_summary$sd
)
#>     variable dgp      mean      mode         sd
#> 1    beta[x]   1 1.0208049 1.0023058 0.07513509
#> 2 Sigma[C,B]   0 0.1576074 0.1510763 0.22755980
#> 3 Sigma[C,C]   1 1.5205880 1.3008830 0.43919633
```

The coefficient is recovered closely, and the free error parameters
within their posterior uncertainty, from choice sets of mixed size.

Between Montreal and Toronto, travelers can fly, take the bus, drive, or
take the train, but not every trip has all four on offer. The
`ModeCanada` data of the **mlogit** package ([Croissant
2020](#ref-Croissant2020)) cover 4324 trips in this corridor with two to
four available modes each ([Bhat 1995](#ref-Bhat1995)). Cost (`cost`),
in-vehicle time (`ivt`), out-of-vehicle time (`ovt`), and service
frequency (`freq`) vary across modes; household income and the number of
urban trip endpoints are trip-specific. The data are in long format with
one row per available mode, and the cost and the income are converted
from Canadian dollars to euro.

``` r

data("ModeCanada", package = "mlogit")
ModeCanada$cost <- ModeCanada$cost / 1.6151
ModeCanada$income <- ModeCanada$income / 1.6151
table(table(ModeCanada$case))
#> 
#>    2    3    4 
#>  231 1314 2779
canada <- fit(
  choice ~ cost + ivt + ovt + freq | income + urban,
  data = ModeCanada,
  format = "long",
  column_decider = "case",
  column_alternative = "alt",
  chains = 1
)
coef(canada)[1:4]
#>   beta[cost]    beta[ivt]    beta[ovt]   beta[freq] 
#> -0.026015395 -0.004127017 -0.012019120  0.030824764
```

The choice sets are read from the rows of each trip; no further argument
is needed. The estimates carry a well-known lesson of transport
research, and the one Bhat ([1995](#ref-Bhat1995)) drew from these very
data: a minute spent waiting or walking weighs several times as much as
a minute spent in the vehicle.
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
computes this ratio with its posterior uncertainty, and with the cost as
reference it turns both time coefficients into values of travel time.

``` r

time_ratio <- interpret(canada, reference = "ivt", effects = "ovt")
time_ratio
#> 1 `ovt` compensates -2.93 `ivt` (95% interval -3.58 to -2.21)
time_values <- interpret(canada, reference = "cost", effects = c("ivt", "ovt"))
time_values
#> 1 `ivt` compensates -0.166 `cost` (95% interval -0.286 to -0.118)
#> 1 `ovt` compensates -0.473 `cost` (95% interval -0.687 to -0.374)
```

A minute out of the vehicle counts about 2.9 times as much as a minute
in it, with a credible interval from 2.2 to 3.6. That is the one number
in this vignette that can be held against the literature rather than
only against a sign: mode choice studies of this corridor and of others
place the ratio at roughly two to three, and the interval covers that
range. In money, one more minute in the vehicle must be offset by a fare
that is 0.17 euro lower, an hour therefore by about 10 euro.

The two traveler characteristics enter with one coefficient per mode,
relative to the air travelers who form the base:

``` r

coef(canada)[grepl("income|urban", names(coef(canada)))]
#>   beta[income_bus]   beta[income_car] beta[income_train]    beta[urban_bus] 
#>        -0.04023657        -0.01537473        -0.02874091         0.18558113 
#>    beta[urban_car]  beta[urban_train] 
#>        -0.19375586         0.12975455
```

Income argues for flying: bus, car, and train all carry a negative
income coefficient relative to the plane. Urban trip endpoints work
differently. They speak for the bus and the train, whose stations sit in
the city centers, and against the car, which is where the parking
problem is.

Ordered and ranked models require complete choice sets.

## Ordered responses

Some responses are levels rather than picks: never, occasionally,
regularly, heavily. Ordered models estimate one utility per occasion and
compare it with increasing thresholds `gamma`; the response levels are
given through `alternatives` in increasing order. Latent-variable data
augmentation provides a direct Bayesian treatment of ordered probit
models ([Albert and Chib 1993](#ref-Albert1993)). The proof of concept
estimates a simulated three-category model and reports the coefficient
and the free threshold beside their true values.

``` r

ordered_recovery <- fit(
  choice ~ x | 0,
  alternatives = c("low", "middle", "high"),
  choice_type = "ordered",
  n_deciders = 500,
  dgp_parameters = list(beta = c(x = 1), gamma = c(0, 1)),
  chains = 1
)
summary(ordered_recovery)
#> Bayesian probit choice model
#> Formula: choice ~ x | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>  variable dgp mean mode     sd rhat ess_bulk
#>   beta[x]   1 1.04 1.08 0.0726    1      120
#>  gamma[2]   1 1.16 1.14 0.0631    1      104
```

Both the coefficient and the threshold between the middle and the high
category are recovered within a tenth of their true values.

The `survey` data of the **MASS** package ([Venables and Ripley
2002](#ref-VenablesRipley2002)) come from 237 statistics students at the
University of Adelaide who reported how often they smoke, together with
their age and how much they exercise. The survey is a teaching data set
rather than a choice experiment, so there is no earlier choice model to
compare the estimates with. The smoking level `Smoke` is stored as a
factor whose levels are in alphabetical order; `alternatives` puts them
in their natural order from never to heavy. The other survey questions
are not used and may contain missing values, which
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) ignores.

``` r

data("survey", package = "MASS")
levels(survey$Smoke)
#> [1] "Heavy" "Never" "Occas" "Regul"
smoking_levels <- c("Never", "Occas", "Regul", "Heavy")
smoking <- fit(
  Smoke ~ Age + Exer | 0,
  data = survey,
  alternatives = smoking_levels,
  choice_type = "ordered",
  column_decider = NULL,
  chains = 1
)
summary(smoking)
#> Bayesian probit choice model
#> Formula: Smoke ~ Age + Exer | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>        variable    mean    mode      sd  rhat ess_bulk
#>       beta[Age] -0.0291 -0.0303 0.00562 1.001      169
#>  beta[ExerNone] -0.1900 -0.0930 0.30380 1.012      157
#>  beta[ExerSome] -0.4736 -0.4704 0.18491 1.001      129
#>        gamma[2]  0.3718  0.3593 0.07167 1.000      160
#>        gamma[3]  0.8706  0.8399 0.13154 0.999      103
```

The first threshold is fixed to zero and the error variance to one; the
remaining thresholds `gamma[2]` and `gamma[3]` are estimated. The
picture behind these numbers: every student has one latent utility,
normally distributed around their systematic part, and the thresholds
cut it into the four levels. The area under the curve between two
thresholds is the probability of that level. The curve below belongs to
a student whose systematic utility is zero.

``` r

thresholds <- coef(smoking)[c("gamma[2]", "gamma[3]")]
cuts <- c(-Inf, 0, thresholds, Inf)
shades <- grey(seq(0.45, 0.9, length.out = length(smoking_levels)))
utility <- seq(-3.5, 3.5, length.out = 400)
plot(
  utility, dnorm(utility),
  type = "n", axes = FALSE, ylab = "",
  xlab = "latent utility of a student"
)
for (k in seq_along(smoking_levels)) {
  inside <- utility >= cuts[k] & utility <= cuts[k + 1]
  polygon(
    c(max(cuts[k], -3.5), utility[inside], min(cuts[k + 1], 3.5)),
    c(0, dnorm(utility[inside]), 0),
    col = shades[k], border = NA
  )
}
lines(utility, dnorm(utility), lwd = 2)
axis(1, at = c(-3, 0, 3))
legend(
  "topright", legend = smoking_levels, fill = shades, border = NA, bty = "n"
)
```

![](v02_model_variants_files/figure-html/ordered-figure-1.png)

The three smoking levels sit close together in the narrow region to the
right of the first threshold: a student whose utility moves one unit to
the right passes through all of them. A coefficient shifts the whole
curve sideways, which is why one number per covariate describes its
effect on all four levels at once. The factor `Exer` enters through its
dummy variables relative to the students who exercise frequently. Older
students report smoking less. Students who exercise only sometimes
appear to smoke less than the frequent exercisers as well, which is not
what one might expect; with fewer than 50 smokers among the 237
students, such contrasts rest on thin evidence.

What does a coefficient of an ordered model mean for the probabilities
of the levels? `interpret(type = "ame")` differentiates the probability
of every level with respect to age and averages the derivatives over the
students;
[`vignette("v04_prediction")`](https://loelschlaeger.de/RprobitB/articles/v04_prediction.md)
explains these marginal effects and their variant `type = "mea"` in more
detail:

``` r

age_effects <- interpret(smoking, type = "ame")
age_effects
#> Average marginal effects on the choice probabilities
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative     mean       sd    lower    upper
#>        Age       Never  0.00823 0.001356  0.00556  0.01094
#>        Age       Occas -0.00235 0.000599 -0.00364 -0.00126
#>        Age       Regul -0.00279 0.000726 -0.00430 -0.00146
#>        Age       Heavy -0.00309 0.000795 -0.00489 -0.00177
```

One more year of age raises the probability of never smoking by about
0.8 percentage points and lowers the probabilities of all other levels.
The four effects sum to zero, because the levels are exhaustive.

## Rankings

Ranked data record the complete order of the alternatives. In wide
format, the response columns combine the response name with each
alternative, for example `rank_Xbox` or, with `delimiter = "."`,
`ch.Xbox`. The model is the same probit model as for unordered choices,
but the likelihood uses the full ordering of the utilities. The proof of
concept simulates rankings of three alternatives and compares the
coefficient and the free covariance parameters with the truth.

``` r

ranked_recovery <- fit(
  rank ~ x | 0,
  choice_type = "ranked",
  n_deciders = 300,
  dgp_parameters = list(
    beta = c(x = 1),
    Sigma = rbind(c(0, 0, 0), c(0, 1, 0.2), c(0, 0.2, 1))
  ),
  chains = 1
)
summary(ranked_recovery, variables = c("beta[x]", "Sigma[C,B]", "Sigma[C,C]"))
#> Bayesian probit choice model
#> Formula: rank ~ x | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>    variable dgp   mean  mode     sd rhat ess_bulk
#>     beta[x] 1.0 1.1463 1.162 0.0795 1.00     52.5
#>  Sigma[C,B] 0.2 0.0891 0.136 0.1521 1.02     54.0
#>  Sigma[C,C] 1.0 1.2734 1.208 0.2968 1.00    106.5
```

The full ordering carries more information than a single choice: the
coefficient is recovered closely, and the covariance and the variance of
the error differences within their posterior uncertainty.

Which gaming platform would you rank first? The `Game` data of the
**mlogit** package ([Croissant 2020](#ref-Croissant2020)) contain
complete rankings of six platforms by 91 Dutch respondents, together
with whether they own each platform (`own`), their age, and their weekly
gaming hours. The source study develops a rank-ordered choice model for
these data ([Fok et al. 2012](#ref-Fok2012)). The ranks are stored in
the columns `ch.Xbox`, `ch.PlayStation`, and so on, so the response in
the formula is `ch`.

``` r

data("Game", package = "mlogit")
gaming <- fit(
  ch ~ own | age + hours,
  data = Game,
  alternatives = c(
    "Xbox", "PlayStation", "PSPortable", "GameCube", "GameBoy", "PC"
  ),
  choice_type = "ranked",
  delimiter = ".",
  column_decider = NULL,
  chains = 1
)
coef(gaming)[1:6]
#>             beta[own]    beta[age.GameCube]          beta[age.PC] 
#>          8.704695e-01          1.603362e-03          5.836835e-02 
#> beta[age.PlayStation]  beta[age.PSPortable]        beta[age.Xbox] 
#>          2.738191e-02          5.245495e-06          1.839244e-02
```

Owning a platform raises its rank, as Fok et al. ([2012](#ref-Fok2012))
also find. The alternative-specific constants and the coefficients of
`age` and `hours` are relative to the base alternative, which is
`GameBoy`, the first platform in alphabetical order, regardless of the
order in `alternatives`. Which platform gains from a gamer’s experience?
For a ranked model, the marginal effects of
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
refer to the probability of being ranked first. `type = "mea"` evaluates
them for a respondent of average age who plays the average number of
hours:

``` r

platform_effects <- interpret(gaming, type = "mea")
platform_effects
#> Marginal effects at the average covariate values
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative      at      mean      sd    lower     upper
#>        own     GameBoy  0.1319  0.030051 0.01337  0.00995  0.061164
#>        own    GameCube  0.0879  0.085871 0.02131  0.04795  0.129590
#>        own          PC  0.8791  0.196197 0.04376  0.12119  0.289795
#>        own PlayStation  0.3187  0.215567 0.04994  0.13114  0.328644
#>        own  PSPortable  0.0989  0.089787 0.02487  0.04768  0.140993
#>        own        Xbox  0.1319  0.225978 0.05441  0.13660  0.346109
#>        age     GameBoy 20.2308 -0.000636 0.00125 -0.00308  0.001670
#>        age    GameCube 20.2308 -0.002218 0.00652 -0.01495  0.010954
#>        age          PC 20.2308  0.009418 0.01518 -0.01932  0.039762
#>        age PlayStation 20.2308 -0.000310 0.01287 -0.02454  0.024360
#>        age  PSPortable 20.2308 -0.002746 0.00521 -0.01367  0.007609
#>        age        Xbox 20.2308 -0.003507 0.01190 -0.02579  0.017521
#>      hours     GameBoy  3.8846 -0.002069 0.00126 -0.00497 -0.000288
#>      hours    GameCube  3.8846 -0.005507 0.00391 -0.01406  0.001969
#>      hours          PC  3.8846  0.033947 0.01332  0.01073  0.064125
#>      hours PlayStation  3.8846 -0.003558 0.00797 -0.01853  0.012062
#>      hours  PSPortable  3.8846 -0.009021 0.00392 -0.01740 -0.003136
#>      hours        Xbox  3.8846 -0.013793 0.00784 -0.03036 -0.000296
```

The PC is the platform of the heavy gamers: every additional weekly hour
raises the probability of ranking it first by about 3.4 percentage
points, taken from the other platforms. The age effects are small and
uncertain; 91 respondents cannot say much about them.

## RprobitB among its neighbors

Several R packages estimate related models. What sets **RprobitB** apart
is the combination of Bayesian probit errors with panel random
coefficients and finite or nonparametric mixtures, together with
individual choice sets, posterior prediction, and decider-level model
evaluation. The comparison below concerns the documented estimation
routines of each package, not their data management or reporting
features.

| Package | Main estimator and response family | Heterogeneity relevant here |
|:---|:---|:---|
| **Rchoice** ([Sarrias 2016](#ref-Sarrias2016)) | Simulated maximum likelihood for binary, ordinal, and count responses | Continuous random parameters in cross-sectional or panel data |
| **mlogit** ([Croissant 2020](#ref-Croissant2020)) | Maximum likelihood for multinomial logit and extensions | Random-parameter logit, but no Bayesian probit mixture |
| **logitr** ([Helveston 2023](#ref-Helveston2023)) | Maximum likelihood for multinomial and mixed logit, in preference or willingness-to-pay space | Normal and log-normal random parameters, but no discrete mixture |
| **MNP** ([Imai and van Dyk 2005b](#ref-Imai2005b), [2025](#ref-Imai2025)) | Bayesian multinomial probit, including varying choice sets and rankings | No mixture on panel-level random coefficients |
| **bayesm** ([Rossi 2026](#ref-Rossi2026)) | Separate Bayesian multinomial probit and hierarchical multinomial logit routines | Finite and Dirichlet-process mixing for hierarchical logit, not hierarchical probit |
| **gmnl** ([Sarrias and Daziano 2017](#ref-Sarrias2017)) | Maximum or simulated maximum likelihood for multinomial logit variants | Continuous, finite latent-class, and mixed-mixed heterogeneity |
| **RprobitB** ([Oelschläger and Bauer 2026](#ref-Oelschlaeger2026d)) | Bayesian binary, multinomial, ordered, and ranked probit | Panel random coefficients with a fixed, sparse finite, or Dirichlet-process mixture, and a weight-based update of the class count |

## Where to go next

The specification is only half of the model. Whether the coefficients
are allowed to differ between deciders is the other half, and
[`vignette("v03_heterogeneity")`](https://loelschlaeger.de/RprobitB/articles/v03_heterogeneity.md)
covers it. Once a model is specified and fitted,
[`vignette("v04_prediction")`](https://loelschlaeger.de/RprobitB/articles/v04_prediction.md)
turns it into predictions and marginal effects, and
[`vignette("v05_model_evaluation")`](https://loelschlaeger.de/RprobitB/articles/v05_model_evaluation.md)
decides between competing specifications.

## References

Albert, James H., and Siddhartha Chib. 1993. “Bayesian Analysis of
Binary and Polychotomous Response Data.” *Journal of the American
Statistical Association* 88 (422): 669–79.
<https://doi.org/10.1080/01621459.1993.10476321>.

Bhat, Chandra R. 1995. “A Heteroscedastic Extreme Value Model of
Intercity Travel Mode Choice.” *Transportation Research Part B:
Methodological* 29 (6): 471–83.
<https://doi.org/10.1016/0191-2615(95)00015-6>.

Croissant, Yves. 2020. “Estimation of Random Utility Models in R: The
mlogit Package.” *Journal of Statistical Software* 95 (11): 1–41.
<https://doi.org/10.18637/jss.v095.i11>.

Fok, Dennis, Richard Paap, and Bram van Dijk. 2012. “A Rank-Ordered
Logit Model with Unobserved Heterogeneity in Ranking Capabilities.”
*Journal of Applied Econometrics* 27 (5): 831–46.
<https://doi.org/10.1002/jae.1223>.

Helveston, John Paul. 2023. “logitr: Fast Estimation of Multinomial and
Mixed Logit Models with Preference Space and Willingness-to-Pay Space
Utility Parameterizations.” *Journal of Statistical Software* 105 (10):
1–37. <https://doi.org/10.18637/jss.v105.i10>.

Herriges, Joseph A., and Catherine L. Kling. 1999. “Nonlinear Income
Effects in Random Utility Models.” *The Review of Economics and
Statistics* 81 (1): 62–72. <https://doi.org/10.1162/003465399767923827>.

Imai, Kosuke, and David A. van Dyk. 2005a. “A Bayesian Analysis of the
Multinomial Probit Model Using Marginal Data Augmentation.” *Journal of
Econometrics* 124 (2): 311–34.
<https://doi.org/10.1016/j.jeconom.2004.02.002>.

Imai, Kosuke, and David A. van Dyk. 2005b. “MNP: R Package for Fitting
the Multinomial Probit Model.” *Journal of Statistical Software* 14 (3):
1–32. <https://doi.org/10.18637/jss.v014.i03>.

Imai, Kosuke, and David A. van Dyk. 2025. *MNP: Fitting the Multinomial
Probit Model*. <https://CRAN.R-project.org/package=MNP>.

McCulloch, Robert E., and Peter E. Rossi. 1994. “An Exact Likelihood
Analysis of the Multinomial Probit Model.” *Journal of Econometrics* 64
(1-2): 207–40. <https://doi.org/10.1016/0304-4076(94)90064-7>.

Oelschläger, Lennart. 2026. “Overcoming Challenges in Modeling Choice
Behavior Heterogeneity.” PhD thesis, Bielefeld University.
<https://pub.uni-bielefeld.de/record/3014719>.

Oelschläger, Lennart, and Dietmar Bauer. 2026. *RprobitB: Bayesian
Probit Choice Modeling*. <https://CRAN.R-project.org/package=RprobitB>.

Rossi, Peter E. 2026. *bayesm: Bayesian Inference for
Marketing/Micro-Econometrics*.
<https://CRAN.R-project.org/package=bayesm>.

Sarrias, Mauricio. 2016. “Discrete Choice Models with Random Parameters
in R: The Rchoice Package.” *Journal of Statistical Software* 74 (10):
1–31. <https://doi.org/10.18637/jss.v074.i10>.

Sarrias, Mauricio, and Ricardo Daziano. 2017. “Multinomial Logit Models
with Continuous and Discrete Individual Heterogeneity in R: The gmnl
Package.” *Journal of Statistical Software* 79 (2): 1–46.
<https://doi.org/10.18637/jss.v079.i02>.

Venables, William N., and Brian D. Ripley. 2002. *Modern Applied
Statistics with s*. 4th ed. Springer.

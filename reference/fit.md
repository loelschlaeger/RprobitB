# Fit a Bayesian probit choice model

Fits a Bayesian probit choice model to a `data.frame` of choice data. If
`data = NULL`, probit choice data are simulated with the
[**choicedata**](https://loelschlaeger.de/choicedata/) package before
estimation.

## Usage

``` r
fit(
  formula,
  data = NULL,
  random_effects = character(),
  latent_class_effects = character(),
  alternatives = NULL,
  base = NULL,
  choice_type = c("unordered", "ordered", "ranked"),
  format = c("wide", "long"),
  column_decider = "deciderID",
  column_occasion = NULL,
  column_alternative = NULL,
  delimiter = "_",
  scale = NULL,
  prior = NULL,
  classes = 1L,
  class_update = c("fixed", "sparse", "dirichlet_process", "weight_based"),
  max_classes = 10L,
  weight_based_control = NULL,
  iterations = 1000L,
  warmup = iterations%/%2L,
  thin = 1L,
  chains = 4L,
  save_individual_draws = FALSE,
  n_deciders = 100L,
  n_occasions = 1L,
  n_alternatives = NULL,
  covariates = NULL,
  dgp_parameters = NULL,
  progress = interactive()
)
```

## Arguments

- formula:

  \[`formula`\]  
  A symbolic description of the choice model, see the details on
  specifying the model formula.

- data:

  \[`data.frame` \| `NULL`\]  
  Empirical choice data. `NULL` simulates data before fitting. In long
  format, a choice occasion may list only its available alternatives,
  see the details on individual choice sets.

- random_effects:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Named vector defining random effects, see the details on specifying
  random effects.

- latent_class_effects:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Names of covariates whose effects differ between latent classes, see
  the details on specifying latent class effects.

- alternatives:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Alternative labels. Required if `choice_type = "ordered"`, then in
  increasing order of the response levels. Otherwise, `NULL` takes them
  from `data` or, if `data = NULL`, uses capital letters.

- base:

  \[`character(1)` \| `NULL`\]  
  The alternative whose utility is subtracted from all others, see the
  details on the normalization. Coefficients that vary across
  alternatives are then expressed relative to it, and none is estimated
  for it. `NULL` uses the first model alternative. Not used if
  `choice_type = "ordered"`, which has a single utility per occasion.

- choice_type:

  \[`character(1)`\]  
  What the response records, and how the model explains it:

  - `"unordered"`: the chosen alternative. Every alternative has its own
    utility, and the alternative with the greatest utility is chosen.

  - `"ordered"`: a level of the ordered scale given by `alternatives`.
    One utility per choice occasion is compared with increasing
    thresholds, and the level is the interval it falls into.

  - `"ranked"`: a complete ranking of the alternatives. Every
    alternative has its own utility, and the ranking orders them by
    utility.

- format:

  \[`character(1)`\]  
  The layout of `data`:

  - `"wide"` has one row per choice occasion, where covariate columns
    that vary across alternatives end in the alternative name.

  - `"long"` has one row per choice occasion and alternative, named in
    `column_alternative`.

- column_decider:

  \[`character(1)` \| `NULL`\]  
  Column name with decider identifiers. `NULL` treats every row of wide
  `data` as its own decider and adds the identifiers as column
  `deciderID`.

- column_occasion:

  \[`character(1)` \| `NULL`\]  
  Column name with occasion identifiers. Set to `NULL` in
  cross-sectional data.

- column_alternative:

  \[`character(1)` \| `NULL`\]  
  Column name with alternative identifiers when `format = "long"`.

- delimiter:

  \[`character(1)`\]  
  Delimiter separating alternative identifiers from covariate names when
  `format = "wide"`.

- scale:

  \[`NULL` \| named `numeric(1)`\]  
  Utility scale normalization, see the details on the normalization.
  `NULL` fixes the error variance of the first utility difference to
  one. Otherwise one named value:

  - `c(<effect> = <value>)` fixes a non-random coefficient, e.g.
    `scale = c(price = -1)`.

  - `c("Sigma_<alternative>,<alternative>" = <value>)` fixes the error
    variance of the utility difference between a non-base alternative
    and the base alternative to a positive value, e.g.
    `scale = c("Sigma_B,B" = 1)`.

  - `c(Sigma = <value>)` fixes the error variance if
    `choice_type = "ordered"`.

- prior:

  \[`named list()` \| `NULL`\]  
  Parameters of the prior distributions that replace the defaults, see
  the details on the prior distribution for the component names and
  default values.

- classes:

  \[`integer(1)`\]  
  Number of latent classes between which the `latent_class_effects`
  differ. For `class_update = "dirichlet_process"` or `"weight_based"`,
  this is the number of classes the sampler starts from.

- class_update:

  \[`character(1)`\]  
  Mixture specification:

  - `"fixed"` fits a finite mixture with `classes` classes.

  - `"sparse"` fits an overfitted finite mixture with `classes` classes
    and infers the occupied number through a sparse Dirichlet weight
    prior.

  - `"dirichlet_process"` samples the occupied class count with a
    Dirichlet process allocation sampler.

  - `"weight_based"` adapts the number of classes during warmup with a
    weight-threshold split, removal, and merge heuristic.

- max_classes:

  \[`integer(1)`\]  
  Largest number of latent classes the sampler may reach when
  `class_update = "dirichlet_process"` or `"weight_based"` changes their
  number. `fit()` warns if the retained draws reach it, then refit with
  a larger value.

- weight_based_control:

  \[`named list()` \| `NULL`\]  
  Tuning constants of `class_update = "weight_based"`, see the details
  on the number of latent classes. `NULL` uses the defaults:

  - `buffer = 50`: minimum number of iterations between two updates.

  - `epsmin = 0.01`: remove the smallest class if its weight falls below
    this value.

  - `epsmax = 0.7`: split the largest class if its weight exceeds this
    value.

  - `deltamin = 0.1`: merge the closest pair of classes if the distance
    of their means falls below this value.

  - `deltashift = 0.5`: displacement of the two means after a split, in
    within-class standard deviations.

- iterations:

  \[`integer(1)`\]  
  Total MCMC iterations per chain, including warmup.

- warmup:

  \[`integer(1)`\]  
  Initial iterations discarded from each chain.

- thin:

  \[`integer(1)`\]  
  Interval between retained post-warmup draws.

- chains:

  \[`integer(1)`\]  
  Number of independent MCMC chains.

- save_individual_draws:

  \[`logical(1)`\]  
  Retain the posterior draws of the individual random coefficients? They
  are required for `coef(level = "individual")` and
  `predict(type = "conditional")`.

  Enable this only when you need them, because they dominate the memory
  the fitted object occupies.

- n_deciders:

  \[`integer(1)`\]  
  Number of deciders to simulate when `data = NULL`.

- n_occasions:

  \[`integer(1)` \| `integer(n_deciders)`\]  
  Simulated occasions for each decider.

- n_alternatives:

  \[`integer(1)` \| `NULL`\]  
  Number of simulated alternatives. `NULL` uses `length(alternatives)`
  or, if `alternatives = NULL`, two unordered or three ordered or ranked
  alternatives labeled with capital letters.

- covariates:

  \[`named list()` \| `NULL`\]  
  Optional covariate values for the simulated data. Names are covariate
  columns of the simulated wide `data.frame`, such as `price_A`, and
  each element is a vector with one value per simulated choice occasion.
  Unspecified covariates are generated by
  [`choicedata::generate_choice_covariates()`](https://loelschlaeger.de/choicedata/reference/choice_covariates.html).

- dgp_parameters:

  \[`named list()` \| `NULL`\]  
  The parameters that generate the simulated data, named like the
  arguments of
  [`choicedata::choice_parameters()`](https://loelschlaeger.de/choicedata/reference/choice_parameters.html):

  - `beta`: the coefficient vector, or a list of one vector per latent
    class. A named vector is matched to the effects by name and may omit
    effects, whose coefficients are then drawn.

  - `Omega`: the covariance matrix of the random effects, or a list of
    one matrix per latent class.

  - `Sigma`: the error covariance matrix, or the error variance if
    `choice_type = "ordered"`.

  - `gamma`: the thresholds if `choice_type = "ordered"`.

  - `weights`: the class weights if `classes > 1`.

  Unspecified parameters are drawn at random.

- progress:

  \[`logical(1)`\]  
  Show progress?

## Value

An `RprobitB_fit` object, which is a `list` with the components:

- `call`: the matched call.

- `data`: the data used.

- `model`: the model specification.

- `prior`: the prior specification.

- `draws`: the posterior draws.

- `sampler`: iterations, warmup, thinning, chains, and elapsed times.

- `simulation`: the `dgp_parameters` that generated the data and the
  simulation sizes if `data = NULL`, otherwise `NULL`.

## Normalization

Utilities are identified only up to level and scale. `base` selects the
alternative whose utility is subtracted from all others, and `scale`
fixes one parameter to identify the scale. The sampler fixes the error
variance of the first utility difference at one and draws the error
covariance by the marginal data augmentation of Imai and van Dyk (2005),
which expands the scale of the latent utilities in every iteration and
returns to the fixed one afterwards, so that the covariance and the
coefficients move freely. Every retained draw is then rescaled to the
normalization in `scale`.

## Individual choice sets

Unordered choices may be made from occasion-specific subsets of the
alternatives. In long format, an occasion lists only the rows of its
available alternatives. The latent utilities of unavailable alternatives
are imputed from their conditional distribution without a truncation, so
they do not restrict the choice. Predictions assign probability zero to
unavailable alternatives. Ordered and ranked models require complete
choice sets.

## Random effects and mixture models

An unnamed `random_effects` vector is a shorthand for correlated normal
effects, so `random_effects = c("price", "time")` is the same as
`random_effects = c(price = "cn", time = "cn")`.

A mixture model divides the deciders into `classes` latent classes and
allocates every decider to exactly one of them. `latent_class_effects`
decides which mixture model this is:

- A random effect named there follows a class-specific normal
  distribution. This is the latent class mixed probit model, reported as
  `mu[<effect>,<class>]` and `Omega[<effect>,<effect>,<class>]`.

- An effect that is named there but has no random effect is a single
  coefficient per class. This is the classical latent class model,
  reported as `beta[<effect>,<class>]`.

## Number of latent classes

`class_update` decides how many of the latent classes are used:

- `"fixed"` keeps all `classes` classes occupied, so the posterior is
  the finite-mixture posterior given that all of them are used.

- `"sparse"` starts from `classes` classes, deliberately more than
  expected, and empties the superfluous ones through a small symmetric
  Dirichlet weight prior (Rousseau and Mengersen 2011;
  Frühwirth-Schnatter and Malsiner-Walli 2019).

- `"dirichlet_process"` creates and removes occupied classes with
  Neal's (2000) auxiliary-parameter allocation update, up to
  `max_classes`. Its precision hyperparameter is updated with the
  beta-gamma augmentation of Escobar and West (1995).

- `"weight_based"` splits, removes, and merges classes during warmup and
  then keeps their number fixed. Every `buffer` iterations it removes
  the class below `epsmin`, splits the class above `epsmax`, or merges
  the closest pair of class means below `deltamin`, attempting at most
  one change in that order. A split moves the two means by `deltashift`
  times the leading within-class standard deviation. Updates stop after
  warmup, and retained iterations condition on the dimension selected
  separately by each chain.

## Class labels

Numbering the latent classes differently describes the same mixture,
every fit with more than one possible class therefore relabels its
retained draws before they are summarized. The representative assignment
of deciders to classes is the draw that is closest to the posterior
co-clustering matrix in the least-squares sense (Dahl 2006). Every draw
is then renumbered to agree with it, using the equivalence classes
representatives assignment of Papastamoulis and Iliopoulos (2010), and
the same permutation is applied to weights, means, covariances, and
allocations. Finally the classes are numbered by decreasing posterior
mean weight.

## Prior distribution

The prior is conjugate where available; the finite-mixture concentration
is updated by a log-scale Metropolis-Hastings step when it has a gamma
hyperprior. `prior` is a named list that overrides the following
defaults, where `P_f`, `P_l`, and `P_r` count the fixed effects without
latent classes, the latent class effects, and the random effects, and
`J` the alternatives:

- `fixed_mean` \[`numeric(P_f)`\] and `fixed_covariance`
  \[`matrix(P_f, P_f)`\]: normal prior for the fixed coefficients
  without latent classes, default `rep(0, P_f)` and `10 * diag(P_f)`.

- `latent_class_mean` \[`numeric(P_l)`\] and `latent_class_covariance`
  \[`matrix(P_l, P_l)`\]: normal prior for the class-specific values of
  the latent class effects, the same for every class, default
  `rep(0, P_l)` and `10 * diag(P_l)`.

- `random_mean` \[`numeric(P_r)`\] and `random_mean_covariance`
  \[`matrix(P_r, P_r)`\]: normal prior for the means of the random
  coefficients, default `rep(0, P_r)` and `10 * diag(P_r)`.

- `random_covariance_df` \[`integer(1)`\] and `random_covariance_scale`
  \[`matrix(P_r, P_r)`\]: inverse Wishart prior for the covariance
  matrices of the random coefficients, default `P_r + 2` and
  `diag(P_r)`. Entries between uncorrelated random effects must be zero.
  In a mixture, both priors apply to the block of the random effects
  with latent classes in every class and to the block without latent
  classes once.

- `class_concentration` \[`numeric(1)` \| named `numeric(2)`\]: fixed
  symmetric Dirichlet concentration for finite weights or precision of
  the Dirichlet process. A named `c(shape = ..., rate = ...)` instead
  places a gamma hyperprior on it. Defaults are `1` for a fixed finite
  or weight-based mixture, `c(shape = 1, rate = 200)` for a sparse
  finite mixture, and `c(shape = 2, rate = 4)` for a Dirichlet process
  mixture.

- `error_covariance_df` \[`integer(1)`\] and `error_covariance_scale`
  \[`matrix(J - 1, J - 1)`\]: inverse Wishart prior for the unrestricted
  error covariance of the utility differences, default `J + 1` and
  `diag(J - 1)`. The prior of the identified covariance is that of the
  unrestricted covariance divided by its first diagonal element. Not
  used for ordered models.

- `threshold_mean` \[`numeric(J - 2)`\] and `threshold_covariance`
  \[`matrix(J - 2, J - 2)`\]: normal prior for the logarithmic
  increments between the ordered thresholds, default `rep(0, J - 2)` and
  `diag(J - 2)`. Only used for ordered models.

## Specifying the model formula

The structure of `formula` is `choice ~ A | B | C`, i.e., a standard
[`formula`](https://rdrr.io/r/stats/formula.html) object but with three
parts on the right-hand side, separated by `|`, where

- `choice` is the name of the discrete response variable,

- `A` are names of **alternative-specific covariates** with **a
  coefficient that is constant across alternatives**,

- `B` are names of **covariates that are constant across alternatives**,

- and `C` are names of **alternative-specific covariates** with
  **alternative-specific coefficients**.

The following rules apply:

1.  By default, intercepts (referred to as alternative-specific
    constants, ASCs) are added to the model. They can be removed by
    adding `+ 0` in the second part, e.g., `choice ~ A | B + 0 | C`. To
    not include any covariates of the second type but to estimate ASCs,
    add `1` in the second part, e.g., `choice ~ A | 1 | C`. The
    expression `choice ~ A | 0 | C` is interpreted as no covariates of
    the second type and no ASCs.

2.  To not include covariates of any type, add `0` in the respective
    part, e.g., `choice ~ 0 | B | C`.

3.  Some parts of the formula can be omitted when there is no ambiguity.
    For example, `choice ~ A` is equivalent to `choice ~ A | 1 | 0`.

4.  Multiple covariates in one part are separated by a `+` sign, e.g.,
    `choice ~ A1 + A2`.

5.  Arithmetic transformations of covariates in all three parts of the
    right-hand side are possible via the function
    [`I()`](https://rdrr.io/r/base/AsIs.html), e.g.,
    `choice ~ I(A1^2 + A2 * 2)`. In this case, a random effect can be
    defined for the transformed covariate, e.g.,
    `random_effects = c("I(A1^2 + A2 * 2)" = "cn")`.

6.  Ordered choice models have a single utility per choice occasion.
    Their covariates must be placed in the first part and ASCs must be
    removed, e.g., `choice ~ age + income | 0`.

## Specifying random effects

Specify random effects as `"<covariate>" = "<distribution>"`. Each
covariate must appear explicitly on the right-hand side of `formula`;
use `"ASC"` for alternative-specific constants.

Available distributions are:

- `"cn"`: correlated normal

- `"n"`: uncorrelated normal

- `"cln"`: positively signed correlated log-normal

- `"ln"`: positively signed uncorrelated log-normal

- `"cln-"`: negatively signed correlated log-normal

- `"ln-"`: negatively signed uncorrelated log-normal

## Specifying latent class effects

The covariates in `latent_class_effects` have effects that differ
between the latent classes of a mixture model; use `"ASC"` for
alternative-specific constants. A random effect named here has a
class-specific mean and covariance, any other effect a class-specific
coefficient. Effects that are not named are the same in every class, and
random effects with and without latent class effects are uncorrelated. A
model with more than one latent class needs at least one latent class
effect.

## References

Dahl DB (2006). “Model-Based Clustering for Expression Data via a
Dirichlet Process Mixture Model.” In Do K, Müller P, Vannucci M (eds.),
*Bayesian Inference for Gene Expression and Proteomics*, 201–218.
Cambridge University Press.
[doi:10.1017/CBO9780511584589.011](https://doi.org/10.1017/CBO9780511584589.011)
.

Escobar MD, West M (1995). “Bayesian Density Estimation and Inference
Using Mixtures.” *Journal of the American Statistical Association*,
**90**(430), 577–588.
[doi:10.1080/01621459.1995.10476550](https://doi.org/10.1080/01621459.1995.10476550)
.

Frühwirth-Schnatter S, Malsiner-Walli G (2019). “From Here to Infinity:
Sparse Finite versus Dirichlet Process Mixtures in Model-Based
Clustering.” *Advances in Data Analysis and Classification*, **13**(1),
33–64.
[doi:10.1007/s11634-018-0329-y](https://doi.org/10.1007/s11634-018-0329-y)
.

Greene WH, Hensher DA (2003). “A latent class model for discrete choice
analysis: contrasts with mixed logit.” *Transportation Research Part B:
Methodological*, **37**(8), 681–698.
[doi:10.1016/S0191-2615(02)00046-2](https://doi.org/10.1016/S0191-2615%2802%2900046-2)
.

Imai K, van Dyk DA (2005). “A Bayesian Analysis of the Multinomial
Probit Model Using Marginal Data Augmentation.” *Journal of
Econometrics*, **124**(2), 311–334.
[doi:10.1016/j.jeconom.2004.02.002](https://doi.org/10.1016/j.jeconom.2004.02.002)
.

Neal RM (2000). “Markov Chain Sampling Methods for Dirichlet Process
Mixture Models.” *Journal of Computational and Graphical Statistics*,
**9**(2), 249–265.
[doi:10.1080/10618600.2000.10474879](https://doi.org/10.1080/10618600.2000.10474879)
.

Oelschläger L, Bauer D (2021). “Bayes Estimation of Latent Class Mixed
Multinomial Probit Models.” In *Proceedings of the 100th Annual Meeting
of the Transportation Research Board*.
<https://trid.trb.org/view/1759753>.

Oelschläger L (2026). *Overcoming Challenges in Modeling Choice Behavior
Heterogeneity*. Ph.D. thesis, Bielefeld University, Bielefeld, Germany.
<https://pub.uni-bielefeld.de/record/3014719>.

Papastamoulis P, Iliopoulos G (2010). “An Artificial Allocations Based
Solution to the Label Switching Problem in Bayesian Analysis of Mixtures
of Distributions.” *Journal of Computational and Graphical Statistics*,
**19**(2), 313–331.
[doi:10.1198/jcgs.2010.09008](https://doi.org/10.1198/jcgs.2010.09008) .

Rousseau J, Mengersen K (2011). “Asymptotic Behaviour of the Posterior
Distribution in Overfitted Mixture Models.” *Journal of the Royal
Statistical Society: Series B (Statistical Methodology)*, **73**(5),
689–710.
[doi:10.1111/j.1467-9868.2011.00781.x](https://doi.org/10.1111/j.1467-9868.2011.00781.x)
.

## Examples

``` r
### Fit a probit model to panel choice data
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371 # price in Euro
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60 # time in hours
Train$time_B <- Train$time_B / 60
model <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = Train,
  column_decider = "id",
  column_occasion = "choiceid",
  scale = c(price = -1), # other coefficients are willingness-to-pay
  chains = 1
)
summary(model)
#> Bayesian probit choice model
#> Formula: choice ~ price + time + change + factor(comfort) | 0 | 0 
#> Samples: 500 retained per chain, 1 chain
#>                variable  mean  mode    sd rhat ess_bulk
#>              beta[time] -5.36 -5.33 0.457 1.00    162.2
#>            beta[change] -1.05 -1.04 0.172 1.01    160.2
#>  beta[factor(comfort)1] -1.99 -2.05 0.222 1.04     44.4
#>  beta[factor(comfort)2] -6.80 -6.81 0.405 1.00    101.3
#>              Sigma[B,B] 26.00 25.34 2.316 1.01    124.4
interpret(model)
#> 1 `time` compensates -5.36 `price` (95% interval -6.2 to -4.42)
#> 1 `change` compensates -1.05 `price` (95% interval -1.39 to -0.716)
#> 1 `factor(comfort)1` compensates -1.99 `price` (95% interval -2.41 to -1.57)
#> 1 `factor(comfort)2` compensates -6.8 `price` (95% interval -7.6 to -6.02)
### Simulate choice data and compare the estimates with the truth
set.seed(1)
simulated <- fit(
  choice ~ x | y,
  dgp_parameters = list(beta = c(x = 1, y_B = -1)),
  covariates = list(y = rpois(400, lambda = 3)),
  iterations = 2000,
  chains = 1,
  n_deciders = 400
)
head(model.frame(simulated))
#>   deciderID choice y        x_A        x_B
#> 1         1      A 2  0.4094018  1.6888733
#> 2         2      A 2 -0.3309078 -2.2852355
#> 3         3      A 3  0.6670662  0.5413273
#> 4         4      A 5  0.5101084 -0.1643758
#> 5         5      A 2 -0.4002467 -1.3702079
#> 6         6      A 5  1.5197450 -0.3087406
summary(simulated)
#> Bayesian probit choice model
#> Formula: choice ~ x | y | 0 
#> Samples: 1000 retained per chain, 1 chain
#>     variable    dgp  mean   mode    sd rhat ess_bulk
#>      beta[x]  1.000  1.24  1.378 0.195 1.30      2.8
#>    beta[y_B] -1.000 -1.08 -0.970 0.211 1.06     11.0
#>  beta[ASC_B] -0.893 -0.96 -0.968 0.379 1.05     47.6
confint(simulated)
#>                   2.5%      97.5%
#> beta[x]      0.8751327  1.5579284
#> beta[y_B]   -1.4893335 -0.7338935
#> beta[ASC_B] -1.7009446 -0.2294631
```

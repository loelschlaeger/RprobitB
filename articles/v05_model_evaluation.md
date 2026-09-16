# Bayesian model evaluation

Which of two models should you believe? **RprobitB** ([Oelschläger and
Bauer 2026](#ref-Oelschlaeger2026d)) offers three complementary answers:
the widely applicable information criterion (WAIC), Pareto-smoothed
importance sampling leave-one-out cross-validation (PSIS-LOO), and Bayes
factors. The first two estimate how well a model would predict new data,
using the posterior log-likelihood draws. The third compares the
marginal likelihoods of two models, that is, how well each model
predicted the data before seeing them ([Watanabe
2010](#ref-Watanabe2010); [Vehtari et al. 2017](#ref-Vehtari2017); [Kass
and Raftery 1995](#ref-Kass1995)). In panel data, the log-likelihood is
evaluated per decider, so the repeated choices of one person count as
one independent unit. The examples use data sets of the **AER** package
([Kleiber and Zeileis 2008](#ref-Kleiber2008)), the **choicedata**
package ([Oelschläger 2026](#ref-Oelschlaeger2026a)), the **MASS**
package ([Venables and Ripley 2002](#ref-VenablesRipley2002)), and the
**mlogit** package ([Croissant 2020](#ref-Croissant2020)). Fitting and
reading a model is the subject of
[`vignette("v01_get_started")`](https://loelschlaeger.de/RprobitB/articles/v01_get_started.md),
the specifications compared here come from
[`vignette("v02_model_variants")`](https://loelschlaeger.de/RprobitB/articles/v02_model_variants.md)
and
[`vignette("v03_heterogeneity")`](https://loelschlaeger.de/RprobitB/articles/v03_heterogeneity.md),
and what they predict is the subject of
[`vignette("v04_prediction")`](https://loelschlaeger.de/RprobitB/articles/v04_prediction.md).

## Does income change how people travel?

In 1987, 210 travelers between Sydney and Melbourne reported which of
four modes they had taken: air, train, bus, or car. The `TravelMode`
data of the **AER** package ([Kleiber and Zeileis
2008](#ref-Kleiber2008)) are in long format with one row per mode. They
are a standard benchmark for discrete choice models, used among others
in Greene’s econometrics textbook ([Greene 2003](#ref-Greene2003b)),
where higher income is likewise found to favor the faster and more
expensive modes. Terminal waiting time (`wait`), in-vehicle cost
(`vcost`), and travel time vary across modes; household income and the
size of the traveling party belong to the traveler. The choice indicator
is coded as `"yes"` and `"no"`, which
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md) expects as
a logical or `0`/`1` indicator, and the cost and the income are
converted from Australian dollars to euro. Cost and time surely matter,
but does income also tilt travelers towards particular modes? The full
model lets income and party size shift the utilities of the modes
relative to the base alternative, the reduced model omits them. Thinning
keeps 250 of the 2000 post-warmup draws, plenty for the information
criteria and short enough for the bridge sampling below.

``` r

library(RprobitB)
set.seed(1)
data("TravelMode", package = "AER")
TravelMode$choice <- TravelMode$choice == "yes"
TravelMode$vcost <- TravelMode$vcost / 1.6196
TravelMode$income <- TravelMode$income / 1.6196

full_model <- fit(
  choice ~ wait + vcost + travel | income + size,
  data = TravelMode,
  format = "long",
  column_decider = "individual",
  column_alternative = "mode",
  iterations = 2000,
  warmup = 1000,
  thin = 8,
  chains = 2,
  progress = FALSE
)
reduced_model <- fit(
  choice ~ wait + vcost + travel | 0,
  data = TravelMode,
  format = "long",
  column_decider = "individual",
  column_alternative = "mode",
  iterations = 2000,
  warmup = 1000,
  thin = 8,
  chains = 2,
  progress = FALSE
)
```

[`logLik()`](https://rdrr.io/r/stats/logLik.html) evaluates the
log-likelihood at the posterior mean parameters. Its `df` attribute
counts the free parameters after normalization, and `nobs` counts the
independent likelihood units, here the 210 travelers.

``` r

logLik(full_model)
#> 'log Lik.' -175.3296 (df=17)
logLik(reduced_model)
#> 'log Lik.' -235.375 (df=8)
```

**RprobitB** defines no [`AIC()`](https://rdrr.io/r/stats/AIC.html) or
[`BIC()`](https://rdrr.io/r/stats/AIC.html) methods of its own. Because
[`logLik()`](https://rdrr.io/r/stats/logLik.html) returns a standard
`logLik` object, the generics of R’s `stats` package ([R Core Team
2026](#ref-RCoreTeam2026)) compute both criteria directly.

``` r

AIC(full_model, reduced_model)
#>               df      AIC
#> full_model    17 384.6593
#> reduced_model  8 486.7499
BIC(full_model, reduced_model)
#>               df      BIC
#> full_model    17 441.5601
#> reduced_model  8 513.5268
```

These values are plug-in summaries at the posterior mean. They are handy
for continuity with an existing analysis, but WAIC and PSIS-LOO use the
full posterior and are the primary tools here. The distinction matters
most for finite mixtures, which are singular models: the large-sample
argument behind AIC and BIC can fail for them ([Watanabe
2010](#ref-Watanabe2010)). For a substantively fixed class count, fit
filled finite models with `classes = 1, 2, ...` to the same observations
and compare them with the workflow below. Sparse finite and
Dirichlet-process mixtures report a posterior over the occupied count
instead; assess them with label-invariant predictive criteria and check
their sensitivity to the concentration prior.

The weight-based update is a different case again. It selects a
dimension heuristically during warmup, so its `n_classes` draws do not
represent posterior uncertainty. WAIC or PSIS-LOO can still assess the
resulting fitted model, but they do not turn the preceding search into
Bayesian inference about the number of classes.

## WAIC and PSIS-LOO

[`WAIC()`](https://loelschlaeger.de/RprobitB/reference/WAIC.md) and
[`loo()`](https://loelschlaeger.de/RprobitB/reference/loo.RprobitB_fit.md)
return objects of the `loo` package ([Vehtari et al.
2026](#ref-Vehtari2026)). Both report the expected log predictive
density `elpd`, an effective number of parameters, and the criterion on
the deviance scale. Lower `waic` and `looic`, or equivalently higher
`elpd`, mean better predictive accuracy. Watanabe
([2010](#ref-Watanabe2010)) introduced WAIC and showed that it
approximates Bayesian cross-validation asymptotically; Vehtari et al.
([2017](#ref-Vehtari2017)) and Vehtari et al. ([2024](#ref-Vehtari2024))
developed the practical PSIS-LOO approximation and its diagnostics.

``` r

WAIC(full_model)
#> Warning: 
#> 9 (4.3%) p_waic estimates greater than 0.4. We recommend trying loo instead.
#> 
#> Computed from 250 by 210 log-likelihood matrix.
#> 
#>           Estimate   SE
#> elpd_waic   -193.1 16.0
#> p_waic        18.9  3.5
#> waic         386.2 31.9
#> 
#> 9 (4.3%) p_waic estimates greater than 0.4. We recommend trying loo instead.
WAIC(reduced_model)
#> Warning: 
#> 8 (3.8%) p_waic estimates greater than 0.4. We recommend trying loo instead.
#> 
#> Computed from 250 by 210 log-likelihood matrix.
#> 
#>           Estimate   SE
#> elpd_waic   -242.8  9.8
#> p_waic        11.9  1.8
#> waic         485.7 19.6
#> 
#> 8 (3.8%) p_waic estimates greater than 0.4. We recommend trying loo instead.
```

[`WAIC()`](https://loelschlaeger.de/RprobitB/reference/WAIC.md) warns
about large `p_waic` values: some travelers are influential enough to
make the WAIC approximation shaky, which is exactly the situation in
which PSIS-LOO is preferable. It comes with Pareto-k diagnostics. Values
below the printed threshold mean that the importance sampling for that
traveler is reliable, and a few larger values usually point to travelers
whose choices are surprising under the model ([Vehtari et al.
2024](#ref-Vehtari2024)).

``` r

loo_full <- loo(full_model)
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
loo_reduced <- loo(reduced_model)
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
loo_full
#> 
#> Computed from 250 by 210 log-likelihood matrix.
#> 
#>          Estimate   SE
#> elpd_loo   -193.7 16.1
#> p_loo        19.4  3.6
#> looic       387.3 32.1
#> ------
#> MCSE of elpd_loo is NA.
#> MCSE and ESS estimates assume independent draws (r_eff=1).
#> 
#> Pareto k diagnostic values:
#>                           Count Pct.    Min. ESS
#> (-Inf, 0.58]   (good)     203   96.7%   55      
#>    (0.58, 1]   (bad)        6    2.9%   <NA>    
#>     (1, Inf)   (very bad)   1    0.5%   <NA>    
#> See help('pareto-k-diagnostic') for details.
```

The `loo` package plots these diagnostics per traveler. Points below the
dashed line are unproblematic; the few above it are the travelers whose
choices the model finds hardest to reproduce when they are left out.

``` r

plot(loo_full)
```

![](v05_model_evaluation_files/figure-html/loo-plot-1.png)

[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
ranks the models by `elpd` and reports the difference to the best model
with its standard error. A difference of several standard errors, as
here, is decisive: knowing a traveler’s income and party size clearly
improves the prediction of their mode.

``` r

loo::loo_compare(loo_full, loo_reduced)
#>   model elpd_diff se_diff p_worse diag_diff       diag_elpd
#>  model1       0.0     0.0      NA           7 k_psis > 0.58
#>  model2     -49.3    10.9    1.00           2 k_psis > 0.58
#> 
#> Diagnostic flags present.
#> See ?`loo-glossary` (sections `diag_diff` and `diag_elpd`)
#> or https://mc-stan.org/loo/reference/loo-glossary.html.
```

What is it that income changes? `interpret(type = "mea")` differentiates
the probability of every mode at the average traveler, so the rows for
`income` say where an additional thousand euro of household income
shifts the choice:

``` r

mode_effects <- interpret(full_model, type = "mea")
mode_effects
#> Marginal effects at the average covariate values
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative     at      mean       sd    lower     upper
#>       wait         air  61.01 -0.013778 0.002286 -0.01894 -0.009596
#>       wait         bus  41.66 -0.011028 0.002550 -0.01712 -0.006898
#>       wait         car   0.00 -0.018855 0.003858 -0.02757 -0.012222
#>       wait       train  35.69 -0.017943 0.003070 -0.02374 -0.012457
#>      vcost         air  52.64 -0.003201 0.001989 -0.00723  0.000360
#>      vcost         bus  20.66 -0.002597 0.001733 -0.00693  0.000157
#>      vcost         car  12.96 -0.004405 0.002714 -0.00939  0.000422
#>      vcost       train  31.70 -0.004216 0.002648 -0.01043  0.000396
#>     travel         air 133.71 -0.000778 0.000149 -0.00109 -0.000543
#>     travel         bus 629.46 -0.000630 0.000187 -0.00107 -0.000327
#>     travel         car 573.20 -0.001075 0.000279 -0.00164 -0.000588
#>     travel       train 608.29 -0.001035 0.000294 -0.00170 -0.000565
#>     income         air  21.33  0.010011 0.003537  0.00343  0.017459
#>     income         bus  21.33  0.001412 0.002142 -0.00239  0.005741
#>     income         car  21.33  0.008443 0.003467  0.00187  0.014894
#>     income       train  21.33 -0.019866 0.004050 -0.02862 -0.012960
#>       size         air   1.74 -0.158998 0.045610 -0.24191 -0.080349
#>       size         bus   1.74 -0.009493 0.032160 -0.07223  0.051383
#>       size         car   1.74  0.058514 0.042685 -0.01625  0.151655
#>       size       train   1.74  0.109977 0.044122  0.02704  0.194997
```

A higher income moves travelers away from the train, by about 2
percentage points per thousand euro, and towards the plane and the car,
by 1 and 0.8 points. A larger traveling party has the opposite effect on
the plane and the train, which fits the economics of the trip: the fare
is paid per person, the car is paid per trip.

## Bayes factors

[`bayes_factor()`](https://loelschlaeger.de/RprobitB/reference/bayes_factor.md)
estimates the marginal likelihood of each model with the
`bridgesampling` package ([Gronau et al. 2020](#ref-Gronau2020)) and
returns their ratio; values above one favor the first model. Bayes
factors weigh the prior predictive performance of the models, so they
are sensitive to the prior, and the priors of both models should be
chosen deliberately when a Bayes factor informs a substantive decision
([Kass and Raftery 1995](#ref-Kass1995)). Bridge sampling estimates the
required ratio of normalizing constants ([Meng and Wong
1996](#ref-Meng1996)); Gronau et al. ([2017](#ref-Gronau2017)) give a
practical tutorial. **RprobitB** deliberately avoids the harmonic-mean
estimator: it is easy to compute from posterior likelihood draws, but it
can have infinite variance and converge far too slowly for practical use
([Wolpert and Schmidler 2012](#ref-Wolpert2012)). The current
implementation requires fixed single-class models; their normalization
does not matter, because all draws are brought to the same one before
bridge sampling. Repeated bridge runs should agree before a result is
reported.

``` r

set.seed(1)
log_bf <- bayes_factor(full_model, reduced_model, log = TRUE)
log_bf
#> Estimated log Bayes factor in favor of model1 over model2: 23.89108
```

A log Bayes factor of about 24 leaves no doubt either: the data are
$`e^{24}`$ times more probable under the full model, even though a Bayes
factor weighs how well each model predicted the data before seeing them
and the nine extra parameters of the full model have to earn their keep
against the prior. Information criteria and Bayes factors answer
different questions and need not agree; here they do.

Bridge sampling is a Monte Carlo estimate, so the number should be
checked before it is reported. `repetitions` reruns the bridge estimate
several times and reports the spread, which shows how much the number
depends on any single run:

``` r

set.seed(2)
bayes_factor(full_model, reduced_model, log = TRUE, repetitions = 3)
#> Estimated log Bayes factor (based on medians of log marginal likelihood estimates)
#>  in favor of model1 over model2: 24.08667
#> Range of estimates: 23.94648 to 24.08920
#> Interquartile range: 0.07136
```

The second estimate agrees with the first to within a fraction of a log
unit, so the conclusion does not rest on the accident of one bridge run.

## When the likelihood is an integral

For models with random coefficients, the likelihood of a decider
integrates over the coefficient distribution jointly for all their
occasions. The result is a multivariate normal probability with one
dimension per occasion and alternative difference, which for a panel of
moderate length quickly means dozens of dimensions.
[`oeli::pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md)
evaluates probabilities of up to three dimensions exactly and higher
dimensions with the GHK simulator on a fixed sequence of `ghk_draws`
quasi-random points, so results are reproducible and smooth in the
parameters. More draws reduce the simulation error at a proportional
cost in time. The GHK simulator is reviewed and evaluated by
Hajivassiliou et al. ([1996](#ref-Hajivassiliou1996)); Genz and Bretz
([2002](#ref-Genz2002)) compare the alternative methods for multivariate
normal probabilities.

In Mejia, Ecuador, 98 dairy farmers took part in a choice experiment on
water conservation ([Ortiz et al. 2023](#ref-Ortiz2023)). Each farmer
faced four choice tasks with two conservation plans and the status quo.
The plans differ in irrigation technology, manure and waste management,
whether training is offered, and a cost-share payment to the farmer;
farm size, milk production, and cattle density describe the farmer.
Ortiz et al. ([2023](#ref-Ortiz2023)) collected these data to measure
what farmers require in return for conservation practices and report
that the cost-share payment raises the acceptance of a plan, which the
estimates below reproduce. Do farmers differ in how much the payment
sways them? [`update()`](https://rdrr.io/r/stats/update.html) refits the
first model with a normal random effect on the payment coefficient and
keeps everything else, including the data. The panel likelihood of a
farmer then integrates over the coefficient, which makes the evaluation
of the criteria noticeably slower than for the fixed model.

``` r

data("water_conservation_choice", package = "choicedata")
water_effects <- choice ~ irrigation + manure + waste + training + payment |
  farm_size + milk_production + cattle_density
fixed_water <- fit(
  water_effects,
  data = water_conservation_choice,
  format = "long",
  column_decider = "farmer",
  column_occasion = "occasion",
  column_alternative = "alternative",
  iterations = 1000,
  warmup = 500,
  chains = 2,
  progress = FALSE
)
mixed_water <- update(fixed_water, random_effects = "payment")
loo_fixed <- loo(fixed_water)
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
loo_mixed <- loo(mixed_water)
#> Warning: Some Pareto k diagnostic values are too high. See help('pareto-k-diagnostic') for details.
loo::loo_compare(loo_fixed, loo_mixed)
#>   model elpd_diff se_diff p_worse diag_diff       diag_elpd
#>  model1       0.0     0.0      NA           3 k_psis > 0.67
#>  model2     -89.2    23.9    1.00   N < 100 1 k_psis > 0.67
#> 
#> Diagnostic flags present.
#> See ?`loo-glossary` (sections `diag_diff` and `diag_elpd`)
#> or https://mc-stan.org/loo/reference/loo-glossary.html.
```

Both models are compared on the same farmer-level scale, and the answer
is sobering: letting the payment coefficient vary does not improve the
expected predictive accuracy, it lowers it. The `N < 100` flag and the
large standard error of the difference caution that a comparison based
on 98 farmers is rough, and the warnings about high Pareto-k values
concern a few farmers whose importance weights are unstable.
[`loo::loo_moment_match()`](https://mc-stan.org/loo/reference/loo_moment_match.html)
would tighten the comparison. Still, the message makes sense: four
choices per farmer carry little information about individual payment
sensitivities, and the extra flexibility costs more than it returns.

What do the farmers actually want? With the model chosen,
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
reads the coefficients as the payment that offsets each practice:

``` r

interpret(fixed_water, reference = "payment", effects = "trainingTRUE")
#> 1 `trainingTRUE` compensates 246 `payment` (95% interval 82.1 to 454)
```

Training is worth a few hundred dollars of cost share to these farmers.
The other practices come with intervals several thousand dollars wide,
which is worth a warning about ratios in general:
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
divides by the reference coefficient draw by draw, and when that
coefficient is not safely away from zero, as the payment coefficient is
not here, a few draws blow the ratio up and the interval becomes
useless. Check the reference coefficient before reading a compensation.

## Ordered and ranked responses

Nothing about the criteria is specific to unordered choices. The ordered
model of
[`vignette("v02_model_variants")`](https://loelschlaeger.de/RprobitB/articles/v02_model_variants.md)
asked whether older students smoke less; the comparison below asks
whether the exercise variable earns its three dummy coefficients.

``` r

data("survey", package = "MASS")
smoking_full <- fit(
  Smoke ~ Age + Exer | 0,
  data = survey,
  alternatives = c("Never", "Occas", "Regul", "Heavy"),
  choice_type = "ordered",
  column_decider = NULL,
  iterations = 2000,
  warmup = 1000,
  chains = 2,
  progress = FALSE
)
smoking_age <- update(smoking_full, . ~ Age | 0)
loo::loo_compare(
  loo(smoking_full, progress = FALSE), loo(smoking_age, progress = FALSE)
)
#>   model elpd_diff se_diff p_worse       diag_diff diag_elpd
#>  model1       0.0     0.0      NA                          
#>  model2      -1.1     2.7    0.66 |elpd_diff| < 4
#> 
#> Diagnostic flags present.
#> See ?`loo-glossary` (sections `diag_diff` and `diag_elpd`)
#> or https://mc-stan.org/loo/reference/loo-glossary.html.
```

The difference is smaller than its standard error, so the exercise
dummies do not improve the prediction of how much a student smokes. With
fewer than fifty smokers among the 237 students that is the expected
outcome, and it agrees with the wide credible intervals that the
coefficients carried in
[`vignette("v02_model_variants")`](https://loelschlaeger.de/RprobitB/articles/v02_model_variants.md).

The same works for rankings. Because a ranking of six platforms carries
more information than a single choice, the likelihood units are the 91
respondents, and [`logLik()`](https://rdrr.io/r/stats/logLik.html)
counts them as such.

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
  iterations = 2000,
  warmup = 1000,
  chains = 2,
  progress = FALSE
)
logLik(gaming)
#> 'log Lik.' -474.5142 (df=30)
nobs(gaming)
#> [1] 91
```

## Where to go next

A model that survives this comparison is ready to be read for its
content.
[`vignette("v04_prediction")`](https://loelschlaeger.de/RprobitB/articles/v04_prediction.md)
turns it into choice probabilities and marginal effects, and
[`vignette("v03_heterogeneity")`](https://loelschlaeger.de/RprobitB/articles/v03_heterogeneity.md)
offers the richer specifications that a comparison like the one above
can then judge.

## References

Croissant, Yves. 2020. “Estimation of Random Utility Models in R: The
mlogit Package.” *Journal of Statistical Software* 95 (11): 1–41.
<https://doi.org/10.18637/jss.v095.i11>.

Genz, Alan, and Frank Bretz. 2002. “Comparison of Methods for the
Computation of Multivariate $`t`$ Probabilities.” *Journal of
Computational and Graphical Statistics* 11 (4): 950–71.
<https://doi.org/10.1198/106186002394>.

Greene, William H. 2003. *Econometric Analysis*. 5th ed. Prentice Hall.

Gronau, Quentin F., Alexandra Sarafoglou, Dora Matzke, et al. 2017. “A
Tutorial on Bridge Sampling.” *Journal of Mathematical Psychology* 81:
80–97. <https://doi.org/10.1016/j.jmp.2017.09.005>.

Gronau, Quentin F., Henrik Singmann, and Eric-Jan Wagenmakers. 2020.
“bridgesampling: An R Package for Estimating Normalizing Constants.”
*Journal of Statistical Software* 92 (10): 1–29.
<https://doi.org/10.18637/jss.v092.i10>.

Hajivassiliou, Vassilis A., Daniel L. McFadden, and Paul A. Ruud. 1996.
“Simulation of Multivariate Normal Rectangle Probabilities and Their
Derivatives: Theoretical and Computational Results.” *Journal of
Econometrics* 72 (1-2): 85–134.
<https://doi.org/10.1016/0304-4076(94)01716-6>.

Kass, Robert E., and Adrian E. Raftery. 1995. “Bayes Factors.” *Journal
of the American Statistical Association* 90 (430): 773–95.
<https://doi.org/10.1080/01621459.1995.10476572>.

Kleiber, Christian, and Achim Zeileis. 2008. *Applied Econometrics with
R*. Springer. <https://doi.org/10.1007/978-0-387-77318-6>.

Meng, Xiao-Li, and Wing Hung Wong. 1996. “Simulating Ratios of
Normalizing Constants via a Simple Identity: A Theoretical Exploration.”
*Statistica Sinica* 6 (4): 831–60.
<https://www3.stat.sinica.edu.tw/statistica/j6n4/j6n43/j6n43.htm>.

Oelschläger, Lennart. 2026. *choicedata: Working with Choice Data*.
<https://github.com/loelschlaeger/choicedata>.

Oelschläger, Lennart, and Dietmar Bauer. 2026. *RprobitB: Bayesian
Probit Choice Modeling*. <https://CRAN.R-project.org/package=RprobitB>.

Ortiz, Christian A., Juan José Avila-Santamaría, and Adán L.
Martinez-Cruz. 2023. “Dairy Farmers’ Willingness to Adopt Cleaner
Production Practices for Water Conservation: A Discrete Choice
Experiment in Mejia, Ecuador.” *Agricultural Water Management* 278:
108168. <https://doi.org/10.1016/j.agwat.2023.108168>.

R Core Team. 2026. *R: A Language and Environment for Statistical
Computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>.

Vehtari, Aki, Jonah Gabry, Måns Magnusson, et al. 2026. *loo: Efficient
Leave-One-Out Cross-Validation and WAIC for Bayesian Models*.
<https://mc-stan.org/loo/>.

Vehtari, Aki, Andrew Gelman, and Jonah Gabry. 2017. “Practical Bayesian
Model Evaluation Using Leave-One-Out Cross-Validation and WAIC.”
*Statistics and Computing* 27 (5): 1413–32.
<https://doi.org/10.1007/s11222-016-9696-4>.

Vehtari, Aki, Daniel Simpson, Andrew Gelman, Yuling Yao, and Jonah
Gabry. 2024. “Pareto Smoothed Importance Sampling.” *Journal of Machine
Learning Research* 25 (72): 1–58.
<https://www.jmlr.org/papers/v25/19-556.html>.

Venables, William N., and Brian D. Ripley. 2002. *Modern Applied
Statistics with s*. 4th ed. Springer.

Watanabe, Sumio. 2010. “Asymptotic Equivalence of Bayes Cross Validation
and Widely Applicable Information Criterion in Singular Learning
Theory.” *Journal of Machine Learning Research* 11: 3571–94.
<https://www.jmlr.org/papers/v11/watanabe10a.html>.

Wolpert, Robert L., and Scott C. Schmidler. 2012. “$`\alpha`$-Stable
Limit Laws for Harmonic Mean Estimators of Marginal Likelihoods.”
*Statistica Sinica* 22 (3): 1233–51.
<https://doi.org/10.5705/ss.2010.221>.

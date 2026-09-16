# Posterior prediction

A fitted choice model earns its keep by answering questions. What
happens to the support for a wind-power project if the compensation is
doubled? Which travelers are likely to book the faster train? Does the
model still know what it is doing when it meets people it has never
seen? [`predict()`](https://rdrr.io/r/stats/predict.html) in
**RprobitB** ([Oelschläger and Bauer 2026](#ref-Oelschlaeger2026d))
answers such questions as a posterior predictive method: it evaluates
the choice probabilities of every occasion under each retained posterior
draw and averages the result. Parameter uncertainty is thereby carried
through to the predictions, which also makes them a natural basis for
checking model fit ([Gelman et al. 1996](#ref-Gelman1996)). This
vignette works through predictions for the population and for individual
deciders, uncertainty summaries, what-if scenarios in a stated choice
experiment, out-of-sample prediction of revealed choices, and residuals.
The parts use data sets of the **mlogit** package ([Croissant
2020](#ref-Croissant2020)), the **choicedata** package ([Oelschläger
2026](#ref-Oelschlaeger2026a)), and the **MASS** package ([Venables and
Ripley 2002](#ref-VenablesRipley2002)). It builds on the fitted model of
[`vignette("v01_get_started")`](https://loelschlaeger.de/RprobitB/articles/v01_get_started.md)
and puts the individual coefficients of
[`vignette("v03_heterogeneity")`](https://loelschlaeger.de/RprobitB/articles/v03_heterogeneity.md)
to work.

## Not everyone minds the price equally

The `Train` data of the **mlogit** package contain about a dozen choices
by each of 235 Dutch travelers between two hypothetical train trips that
differ in price, travel time, number of changes, and comfort class
([Ben-Akiva et al. 1993](#ref-BenAkiva1993)); as in
[`vignette("v01_get_started")`](https://loelschlaeger.de/RprobitB/articles/v01_get_started.md),
the prices are converted to euro and the travel times to hours.
[`vignette("v01_get_started")`](https://loelschlaeger.de/RprobitB/articles/v01_get_started.md)
fits a model with one price coefficient for everyone, and
[`vignette("v03_heterogeneity")`](https://loelschlaeger.de/RprobitB/articles/v03_heterogeneity.md)
splits the travelers into two classes. Here the price coefficient gets a
normal random effect instead, so every traveler has their own price
sensitivity. Thinning keeps 50 of the 750 post-warmup draws, which is
enough for stable posterior means and keeps the predictions that follow
fast.

``` r

library(RprobitB)
set.seed(1)
data("Train", package = "mlogit")
Train$price_A <- Train$price_A / 100 / 2.20371
Train$price_B <- Train$price_B / 100 / 2.20371
Train$time_A <- Train$time_A / 60
Train$time_B <- Train$time_B / 60

model <- fit(
  choice ~ price + time + change + factor(comfort) | 0,
  data = Train,
  random_effects = "price",
  column_decider = "id",
  column_occasion = "choiceid",
  iterations = 1500,
  warmup = 750,
  thin = 15,
  chains = 2,
  save_individual_draws = TRUE,
  progress = FALSE
)
summary(model)
#> Bayesian probit choice model
#> Formula: choice ~ price + time + change + factor(comfort) | 0 | 0 
#> Samples: 50 retained per chain, 2 chains
#> 
#>                variable    mean    mode     sd  rhat ess_bulk
#>              beta[time] -1.7814 -1.7580 0.1205 1.093     92.5
#>            beta[change] -0.3376 -0.3500 0.0427 0.988     91.8
#>  beta[factor(comfort)1] -0.6803 -0.6775 0.0540 1.002    130.6
#>  beta[factor(comfort)2] -1.9188 -1.9416 0.0993 0.997     81.0
#>               mu[price] -0.3923 -0.3941 0.0278 1.045     48.7
#>      Omega[price,price]  0.0966  0.0931 0.0154 1.078     26.9
```

`mu[price]` and `Omega[price,price]` describe how the price coefficient
is spread across the population. The square root of its posterior mean
is 0.311, not far from the size of the mean itself: price sensitivity
clearly differs between travelers, and the predictions below can put
that to use. The variance parameter is also the one that mixes worst, as
its `rhat` shows. Two short, heavily thinned chains are not enough for a
real analysis of the heterogeneity, but they are enough to demonstrate
prediction.

## Predicting for the population

By default, [`predict()`](https://rdrr.io/r/stats/predict.html) returns
one row per choice occasion with the identifiers, the most probable
alternative in `.prediction`, and the posterior mean probability of
every alternative. These population predictions integrate over the
estimated distribution of the random coefficients, so they hold for any
traveler from the population, not just for those in the data.

``` r

population <- predict(model)
head(population)
#>   id choiceid .prediction probability_A probability_B
#> 1  1        1           A     0.8758825     0.1241175
#> 2  1        2           A     0.7089943     0.2910057
#> 3  1        3           A     0.8103538     0.1896462
#> 4  1        4           B     0.1584512     0.8415488
#> 5  1        5           A     0.6889428     0.3110572
#> 6  1        6           B     0.1649000     0.8351000
```

`uncertainty = TRUE` adds the posterior standard deviation and an
equal-tailed credible interval of every probability. These intervals
express uncertainty about the parameters, not the randomness of the
choice itself.

``` r

head(predict(model, uncertainty = TRUE, level = 0.9))
#>   id choiceid .prediction probability_A probability_B       sd_A       sd_B
#> 1  1        1           A     0.8758825     0.1241175 0.01820053 0.01820053
#> 2  1        2           A     0.7089943     0.2910057 0.02078762 0.02078762
#> 3  1        3           A     0.8103538     0.1896462 0.02204639 0.02204639
#> 4  1        4           B     0.1584512     0.8415488 0.01807750 0.01807750
#> 5  1        5           A     0.6889428     0.3110572 0.02306812 0.02306812
#> 6  1        6           B     0.1649000     0.8351000 0.02045077 0.02045077
#>     lower_A    lower_B   upper_A   upper_B
#> 1 0.8458549 0.09741844 0.9025816 0.1541451
#> 2 0.6738162 0.25751461 0.7424854 0.3261838
#> 3 0.7753907 0.15454084 0.8454592 0.2246093
#> 4 0.1319420 0.80994462 0.1900554 0.8680580
#> 5 0.6537258 0.27240732 0.7275927 0.3462742
#> 6 0.1305137 0.80171684 0.1982832 0.8694863
```

## Predicting for the people you observed

A mixed model retains the posterior draws of every individual
coefficient, and `coef(level = "individual")` summarizes them. Once a
traveler has answered a dozen questions, we know quite a bit about their
personal price sensitivity, and `predict(type = "conditional")` uses
that knowledge instead of the population distribution. Conditional
predictions exist only for deciders that were part of the fit, and they
are usually sharper because they exploit the decider’s own choice
history. This partial pooling is the classic argument for hierarchical
Bayesian choice models ([Allenby and Rossi 1998](#ref-Allenby1998);
[Huber and Train 2001](#ref-Huber2001)).

``` r

head(coef(model, level = "individual"))
#>         price
#> 1 -0.26940692
#> 2 -0.75192882
#> 3 -0.63361658
#> 4 -0.42940736
#> 5 -0.01203728
#> 6 -0.06736097
conditional <- predict(model, type = "conditional")
head(conditional)
#>   id choiceid .prediction probability_A probability_B
#> 1  1        1           A     0.9472914    0.05270857
#> 2  1        2           A     0.6403897    0.35961026
#> 3  1        3           A     0.8512142    0.14878583
#> 4  1        4           B     0.1582555    0.84174454
#> 5  1        5           A     0.6095824    0.39041762
#> 6  1        6           B     0.1071964    0.89280359
```

How much does the personal information help? The share of correctly
predicted choices in the fitted data gives a first answer.

``` r

observed <- model.frame(model)$choice
c(
  population = mean(population$.prediction == observed, na.rm = TRUE),
  conditional = mean(conditional$.prediction == observed, na.rm = TRUE)
)
#>  population conditional 
#>   0.7108228   0.7869580
```

A traveler’s own history lifts the hit rate noticeably. The hit rate
judges the predictions at a single cut, a probability of one half. A ROC
curve compares them at every cut: as the threshold for predicting trip
`B` moves from one down to zero, it traces the share of `B` choices that
are predicted correctly against the share of `A` choices that are
wrongly predicted as `B`, and the area under the curve is the
probability that a randomly chosen `B` occasion receives a higher `B`
probability than a randomly chosen `A` occasion. The `plotROC` package
([Sachs 2017](#ref-Sachs2017)) draws the curves with `ggplot2` ([Wickham
2016](#ref-Wickham2016)).

``` r

library(ggplot2)
library(plotROC)
roc_data <- rbind(
  data.frame(
    prediction = "population", chose_B = as.integer(observed == "B"),
    probability = population$probability_B
  ),
  data.frame(
    prediction = "conditional", chose_B = as.integer(observed == "B"),
    probability = conditional$probability_B
  )
)
roc_data$prediction <- factor(
  roc_data$prediction, levels = c("population", "conditional")
)
roc_plot <- ggplot(
  roc_data, aes(d = chose_B, m = probability, color = prediction)
) +
  geom_roc(n.cuts = 0) +
  style_roc()
roc_plot
```

![](v04_prediction_files/figure-html/roc-1.png)

The conditional curve lies above the population curve, and the area
under it rises from 0.77 to 0.87.

## What if the compensation were doubled?

`newdata` accepts a data frame in the layout of the fitted data, with or
without the response column. A scenario analysis thus comes down to
editing a data frame, which is how stated choice experiments are
typically put to use.

Near Setskog in Norway, 308 residents were asked six times to choose
between two plans for a proposed wind-power project and the status quo
without the project, alternative `1` ([Dugstad et al.
2024](#ref-Dugstad2024)). The plans varied the number and height of the
turbines, the routing of the power line, and an annual reduction in
municipal taxes offered as compensation. The study also measured each
respondent’s collective psychological ownership of the affected area, a
score of how strongly they feel that the landscape belongs to the
residents. That score is the point of the original study ([Dugstad et
al. 2024](#ref-Dugstad2024)), which reports that a stronger feeling of
ownership makes residents less willing to accept a project. The
estimates below say the same. The score does not vary across
alternatives and therefore enters with alternative-specific
coefficients.

``` r

data("wind_power_choice", package = "choicedata")
wind <- fit(
  choice ~ turbines + height + powerline + compensation |
    psychological_ownership,
  data = wind_power_choice,
  column_decider = "respondent",
  column_occasion = "occasion",
  iterations = 600,
  warmup = 300,
  chains = 2,
  progress = FALSE
)
coef(wind)[c("beta[compensation]", "beta[psychological_ownership_2]")]
#>              beta[compensation] beta[psychological_ownership_2] 
#>                     0.001086941                    -0.441554485
```

Compensation makes a plan more attractive, and residents with a stronger
feeling of ownership are less willing to leave the status quo. How much
money does that feeling stand for? Since the compensation is money, it
is the natural reference for
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md),
and the ownership score is standardized, so the result is the annual tax
reduction that offsets one standard deviation more ownership:

``` r

ownership_value <- interpret(
  wind,
  reference = "compensation",
  effects = c("psychological_ownership_2", "psychological_ownership_3")
)
ownership_value
#> 1 `psychological_ownership_2` compensates 414 `compensation` (95% interval 305 to 566)
#> 1 `psychological_ownership_3` compensates 397 `compensation` (95% interval 291 to 537)
```

For either plan, a resident who feels one standard deviation more
strongly that the area belongs to the residents would need about 405
euro more compensation per year to be equally likely to accept it. That
is of the order of the largest reduction on offer in the experiment, 368
euro, and shows why money alone goes only so far here.

So what would happen if the municipality doubled the compensation? The
scenario below doubles it for both plans in the first four choice tasks
and compares the probability of the status quo before and after.

``` r

scenario <- model.frame(wind)[1:4, ]
scenario$choice <- NULL
scenario$compensation_2 <- 2 * scenario$compensation_2
scenario$compensation_3 <- 2 * scenario$compensation_3
cbind(
  before = predict(wind)$probability_1[1:4],
  after = predict(wind, newdata = scenario)$probability_1
)
#>         before     after
#> [1,] 0.4241701 0.2810360
#> [2,] 0.3682594 0.2603746
#> [3,] 0.3742080 0.3016910
#> [4,] 0.4405111 0.3740548
```

The status quo loses between five and nine percentage points in every
task. Money helps, but it is not the only thing that matters to the
neighbors of a wind farm.

## Do the chess players behave as the model expects?

Predictive performance is best judged on deciders that played no part in
the estimation; this is the principle behind out-of-sample and
leave-one-out model assessment ([Vehtari et al.
2017](#ref-Vehtari2017)). The `lichess_berserk_choice` data offer a
large playground for it. In an arena tournament on Lichess, a player may
go Berserk at the start of a game: their clock is halved, and a win
earns one extra tournament point. Players on a winning streak collect
double points, so for them a loss is particularly costly. The data
record this decision for 5852 players in the Lichess Yearly Rapid Arena
of April 2026, game by game, together with the playing color, the
player’s rating, the rating difference to the opponent, the remaining
tournament time, and whether the player was on a streak. Unlike the
other data sets here, these are revealed choices from a live tournament
and have not been analyzed before, so there is nothing to compare the
estimates with except the hold-out games below. All of these are
game-specific and constant across the two alternatives, so they enter as
type `B` covariates with a coefficient for the alternative `TRUE`, for
example `beta[rating_TRUE]`; logical covariates appear as dummy
variables such as `streakTRUE`. The fit below keeps the first 300
players and lets
[`train_test()`](https://loelschlaeger.de/choicedata/reference/train_test.html)
split their games. The function is re-exported from **choicedata** and
splits by decider, not by row: `test_number = 60` puts the games of 60
players into the test set and everything else into the training set, so
no player contributes to both. `test_proportion` takes a share instead
of a count, and `by_occasion = TRUE` splits within deciders, which is
what you want when the question is forecasting later choices of known
deciders rather than choices of new ones.

``` r

data("lichess_berserk_choice", package = "choicedata")
players <- unique(lichess_berserk_choice$deciderID)
first_players <- lichess_berserk_choice$deciderID %in% players[1:300]
split <- train_test(lichess_berserk_choice[first_players, ], test_number = 60)
berserk <- fit(
  berserk ~ 0 | white + rating + ratingDifference + minutesRemaining + streak,
  data = split$train,
  column_occasion = "occasionID",
  iterations = 1000,
  warmup = 500,
  chains = 2,
  progress = FALSE
)
coef(berserk)
#>        beta[whiteTRUE_TRUE]           beta[rating_TRUE] 
#>                2.961391e-02                1.510856e-03 
#> beta[ratingDifference_TRUE] beta[minutesRemaining_TRUE] 
#>                5.454093e-05               -1.800991e-04 
#>       beta[streakTRUE_TRUE]              beta[ASC_TRUE] 
#>               -7.983852e-02               -3.271449e+00
```

Stronger players go Berserk more often, players on a streak less often,
and everyone more often towards the end of the tournament, when little
time remains to make up for a lost game. How large are these effects in
probabilities? `interpret(type = "mea")` differentiates the Berserk
probability at the average game:

``` r

berserk_effects <- interpret(berserk, type = "mea")
berserk_effects
#> Marginal effects at the average covariate values
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>         covariate alternative     at      mean       sd     lower     upper
#>            rating       FALSE 1707.8 -4.56e-04 4.75e-05 -5.46e-04 -3.67e-04
#>            rating        TRUE 1707.8  4.56e-04 4.75e-05  3.67e-04  5.46e-04
#>  ratingDifference       FALSE   14.8 -1.64e-05 3.82e-05 -9.12e-05  5.82e-05
#>  ratingDifference        TRUE   14.8  1.64e-05 3.82e-05 -5.82e-05  9.12e-05
#>  minutesRemaining       FALSE  296.6  5.47e-05 9.11e-05 -1.13e-04  2.34e-04
#>  minutesRemaining        TRUE  296.6 -5.47e-05 9.11e-05 -2.34e-04  1.13e-04
```

A player rated 100 points higher is about 4.6 percentage points more
likely to go Berserk, and every hour that remains in the tournament
lowers the probability by about 0.3 percentage points. How well does
this carry over to the 60 unseen players? Accuracy alone is a blunt
instrument here, because always predicting `FALSE` already scores
whatever share of the games went without Berserk.

``` r

holdout_prediction <- predict(berserk, newdata = split$test)
holdout_choice <- as.character(split$test$berserk)
c(
  accuracy = mean(holdout_prediction$.prediction == holdout_choice),
  share_berserk = mean(split$test$berserk)
)
#>      accuracy share_berserk 
#>     0.6810700     0.3497942
```

The value of the model lies in its probabilities rather than in its hit
rate. A calibration table sorts the hold-out games by their predicted
Berserk probability and compares it with the observed Berserk rate in
each group; groups with fewer than 50 games are too small to judge and
dropped.

``` r

predicted <- holdout_prediction$probability_TRUE
bins <- cut(predicted, breaks = seq(0, 1, by = 0.1))
calibration <- data.frame(
  games = as.vector(table(bins)),
  predicted = as.vector(tapply(predicted, bins, mean)),
  observed = as.vector(tapply(split$test$berserk, bins, mean)),
  row.names = levels(bins)
)
round(calibration[calibration$games >= 50, ], 2)
#>           games predicted observed
#> (0.1,0.2]    56      0.14     0.18
#> (0.2,0.3]   199      0.25     0.26
#> (0.3,0.4]    75      0.35     0.33
#> (0.4,0.5]    83      0.43     0.57
```

A calibration plot says the same thing at a glance. Points on the
diagonal mean that the predicted probability is the observed rate;
points below it mean the model predicts more Berserk than happens.

``` r

large <- calibration[calibration$games >= 50, ]
plot(
  large$predicted, large$observed,
  xlim = c(0, 1), ylim = c(0, 1), pch = 19,
  cex = 0.5 + 2 * large$games / max(large$games),
  xlab = "predicted Berserk probability",
  ylab = "observed Berserk rate"
)
abline(0, 1, lwd = 2, col = "grey50")
```

![](v04_prediction_files/figure-html/calibration-plot-1.png)

The point sizes are proportional to the number of games in a group. The
observed rate rises with the predicted probability, which is what a
useful model must deliver: where it sees a low risk appetite, few
players go Berserk, and where it predicts something close to a coin
flip, more than half of them do. The points sit close to the diagonal in
the middle groups and above it at the top, so the model ranks the games
well and is, if anything, a little too cautious about the games it
already considers risky.

## Residuals

[`residuals()`](https://rdrr.io/r/stats/residuals.html) returns the
observed choice indicators minus the posterior mean probabilities, one
row per choice occasion and one column per alternative. The rows of
observed occasions sum to zero, and occasions with a missing response
yield `NA`. For ranked data, the indicator marks the first-ranked
alternative.

``` r

model_residuals <- residuals(model)
head(model_residuals)
#>              A          B
#> 1:1  0.1241175 -0.1241175
#> 1:2  0.2910057 -0.2910057
#> 1:3  0.1896462 -0.1896462
#> 1:4 -0.1584512  0.1584512
#> 1:5 -0.6889428  0.6889428
#> 1:6 -0.1649000  0.1649000
colMeans(model_residuals, na.rm = TRUE)
#>            A            B 
#>  0.004725969 -0.004725969
```

Average residuals near zero say that the predicted choice shares match
the observed ones. The more revealing use is to group them. Averaging by
decider shows whose choices the model reproduces and whose it does not:

``` r

by_decider <- tapply(
  model_residuals[, "A"], model.frame(model)$id, mean, na.rm = TRUE
)
round(quantile(by_decider, c(0, 0.25, 0.5, 0.75, 1)), 3)
#>     0%    25%    50%    75%   100% 
#> -0.337 -0.070  0.001  0.088  0.347
```

Half of the travelers have an average residual within about a tenth of
zero, while the extremes reach a third: for those travelers the model
predicts trip `A` too often or too rarely across all their questions.
Grouping by a covariate is the second useful cut, here by the price of
trip `A` in four groups of equal size:

``` r

price_group <- cut(
  model.frame(model)$price_A,
  breaks = quantile(model.frame(model)$price_A, seq(0, 1, 0.25)),
  include.lowest = TRUE
)
round(tapply(model_residuals[, "A"], price_group, mean, na.rm = TRUE), 3)
#> [0.454,11.3]  (11.3,14.7]  (14.7,18.2]  (18.2,56.7] 
#>        0.008        0.004       -0.011        0.017
```

No group stands out, so the linear price term is doing its job. A
systematic pattern here, for example positive residuals at both ends,
would argue for a nonlinear term or an unmodeled preference class.

## Marginal effects

How much does one euro more change the probability of booking a trip?
Coefficients do not answer that directly, because the probit link is not
linear.
[`interpret()`](https://loelschlaeger.de/RprobitB/reference/interpret.md)
differentiates the predicted probabilities instead. `type = "mea"`
evaluates the derivative for one occasion whose covariates equal the
observed averages, which the column `at` reports; `type = "ame"`
evaluates it for every observed occasion and averages, which weighs the
occasions as they occur in the data and is usually the more relevant
summary. Both use every posterior draw, so they come with credible
intervals.

``` r

interpret(model, type = "mea")
#> Marginal effects at the average covariate values
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative     at   mean     sd  lower  upper
#>       time           A  2.125 -0.711 0.0481 -0.799 -0.595
#>       time           B  2.119 -0.711 0.0481 -0.799 -0.595
#>     change           A  0.664 -0.135 0.0170 -0.167 -0.100
#>     change           B  0.681 -0.135 0.0170 -0.167 -0.100
#>      price           A 15.283 -0.156 0.0111 -0.178 -0.136
#>      price           B 15.280 -0.156 0.0111 -0.178 -0.136
average_effects <- interpret(model, type = "ame")
average_effects
#> Average marginal effects on the choice probabilities
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative    mean      sd   lower   upper
#>       time           A -0.4347 0.02632 -0.4763 -0.3683
#>       time           B -0.4347 0.02632 -0.4763 -0.3683
#>     change           A -0.0824 0.01006 -0.1023 -0.0613
#>     change           B -0.0824 0.01006 -0.1023 -0.0613
#>      price           A -0.0813 0.00391 -0.0879 -0.0737
#>      price           B -0.0813 0.00391 -0.0879 -0.0737
```

The `at` argument replaces the average of selected covariates. Naming
the alternative matters: a price that is common to both trips cancels in
the utility difference and leaves the effects unchanged, while a much
cheaper alternative B pushes the choice towards certainty, where one
euro more hardly moves the probability at all.

``` r

interpret(model, type = "mea", at = c(price_A = 30, price_B = 10))
#> Marginal effects at the given covariate values
#> Change in the probability of the alternative per unit of the covariate, with 95% interval 
#>  covariate alternative     at      mean       sd     lower     upper
#>       time           A  2.125 -0.051552 0.005575 -0.061439 -0.040998
#>       time           B  2.119 -0.051552 0.005575 -0.061439 -0.040998
#>     change           A  0.664 -0.009766 0.001483 -0.012510 -0.007030
#>     change           B  0.681 -0.009766 0.001483 -0.012510 -0.007030
#>      price           A 30.000 -0.000284 0.000037 -0.000362 -0.000219
#>      price           B 10.000 -0.000284 0.000037 -0.000362 -0.000219
```

With two alternatives, a change in the attributes of one trip shifts
probability to the other, so the two rows of each covariate mirror each
other. Averaged over the observed trips, one euro more lowers the
probability of a trip by about 8 percentage points and one hour more by
about 43 points, in line with the compensations in
[`vignette("v01_get_started")`](https://loelschlaeger.de/RprobitB/articles/v01_get_started.md).

## Ordered responses predict a distribution over levels

For an ordered model,
[`predict()`](https://rdrr.io/r/stats/predict.html) returns the
probability of every level rather than of every alternative. The smoking
model of
[`vignette("v02_model_variants")`](https://loelschlaeger.de/RprobitB/articles/v02_model_variants.md)
predicts how often a student smokes from their age and exercise habits.

``` r

data("survey", package = "MASS")
smoking <- fit(
  Smoke ~ Age + Exer | 0,
  data = survey,
  alternatives = c("Never", "Occas", "Regul", "Heavy"),
  choice_type = "ordered",
  column_decider = NULL,
  iterations = 1500,
  warmup = 750,
  chains = 2,
  progress = FALSE
)
head(predict(smoking))
#>   deciderID .prediction probability_Never probability_Occas probability_Regul
#> 1         1       Never         0.8390417        0.07112815        0.05719618
#> 2         2       Never         0.7559041        0.09376456        0.08646459
#> 3         3       Never         0.7499603        0.09525566        0.08851250
#> 4         4       Never         0.7796000        0.08752184        0.07821333
#> 5         5       Never         0.8743092        0.05868116        0.04427316
#> 6         6       Never         0.8577127        0.06469947        0.05036270
#>   probability_Heavy
#> 1        0.03263398
#> 2        0.06386672
#> 3        0.06627151
#> 4        0.05466481
#> 5        0.02273646
#> 6        0.02722512
```

Every row carries four probabilities that sum to one, and `.prediction`
names the most probable level. Because four out of five students never
smoke, that level wins almost everywhere, and the model earns its keep
through the probabilities rather than through the predicted level. The
residuals have one column per level and are read the same way as above.

``` r

colMeans(residuals(smoking), na.rm = TRUE)
#>        Never        Occas        Regul        Heavy 
#>  0.021360441 -0.006862634 -0.006485781 -0.008012026
```

A scenario works as before: `newdata` takes a data frame in the layout
of the fitted data. Ten years of age shift the levels as the
coefficients promised.

``` r

students <- model.frame(smoking)[1:3, ]
students$Smoke <- NULL
older <- students
older$Age <- older$Age + 10
cbind(
  before = predict(smoking, newdata = students)$probability_Never,
  after = predict(smoking, newdata = older)$probability_Never
)
#>         before     after
#> [1,] 0.8390417 0.8994418
#> [2,] 0.7559041 0.8351532
#> [3,] 0.7499603 0.8304814
```

## Where to go next

Predictions say what a model expects; they do not say whether the model
deserves to be believed over another one. That comparison, on the same
decider-level scale that the hold-out check uses here, is the subject of
[`vignette("v05_model_evaluation")`](https://loelschlaeger.de/RprobitB/articles/v05_model_evaluation.md).

## References

Allenby, Greg M., and Peter E. Rossi. 1998. “Marketing Models of
Consumer Heterogeneity.” *Journal of Econometrics* 89 (1-2): 57–78.
<https://doi.org/10.1016/S0304-4076(98)00055-4>.

Ben-Akiva, Moshe, Denis Bolduc, and Mark Bradley. 1993. “Estimation of
Travel Choice Models with Randomly Distributed Values of Time.”
*Transportation Research Record* 1413: 88–97.
<https://trid.trb.org/View/385096>.

Croissant, Yves. 2020. “Estimation of Random Utility Models in R: The
mlogit Package.” *Journal of Statistical Software* 95 (11): 1–41.
<https://doi.org/10.18637/jss.v095.i11>.

Dugstad, Anders, Roy Brouwer, Kristine Grimsrud, Gorm Kipperberg, Henrik
Lindhjem, and Ståle Navrud. 2024. “Nature Is Ours! Psychological
Ownership and Preferences for Wind Energy.” *Energy Economics* 129:
107239. <https://doi.org/10.1016/j.eneco.2023.107239>.

Gelman, Andrew, Xiao-Li Meng, and Hal Stern. 1996. “Posterior Predictive
Assessment of Model Fitness via Realized Discrepancies.” *Statistica
Sinica* 6 (4): 733–807.
<https://www3.stat.sinica.edu.tw/statistica/j6n4/j6n41/j6n41.htm>.

Huber, Joel, and Kenneth Train. 2001. “On the Similarity of Classical
and Bayesian Estimates of Individual Mean Partworths.” *Marketing
Letters* 12 (3): 259–69. <https://doi.org/10.1023/A:1011120928698>.

Oelschläger, Lennart. 2026. *choicedata: Working with Choice Data*.
<https://github.com/loelschlaeger/choicedata>.

Oelschläger, Lennart, and Dietmar Bauer. 2026. *RprobitB: Bayesian
Probit Choice Modeling*. <https://CRAN.R-project.org/package=RprobitB>.

Sachs, Michael C. 2017. “plotROC: A Tool for Plotting ROC Curves.”
*Journal of Statistical Software, Code Snippets* 79 (2): 1–19.
<https://doi.org/10.18637/jss.v079.c02>.

Vehtari, Aki, Andrew Gelman, and Jonah Gabry. 2017. “Practical Bayesian
Model Evaluation Using Leave-One-Out Cross-Validation and WAIC.”
*Statistics and Computing* 27 (5): 1413–32.
<https://doi.org/10.1007/s11222-016-9696-4>.

Venables, William N., and Brian D. Ripley. 2002. *Modern Applied
Statistics with s*. 4th ed. Springer.

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
Springer. <https://doi.org/10.1007/978-3-319-24277-4>.

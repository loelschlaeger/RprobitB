# Compute point estimates

This function computes the point estimates of an
[`RprobitB_fit`](https://loelschlaeger.de/RprobitB/reference/RprobitB_fit.md).
Per default, the `mean` of the Gibbs samples is used as a point
estimate. However, any statistic that computes a single numeric value
out of a vector of Gibbs samples can be specified for `FUN`.

## Usage

``` r
point_estimates(x, FUN = mean)
```

## Arguments

- x:

  An object of class
  [`RprobitB_fit`](https://loelschlaeger.de/RprobitB/reference/RprobitB_fit.md).

- FUN:

  A function that computes a single numeric value out of a vector of
  numeric values.

## Value

An object of class
[`RprobitB_parameter`](https://loelschlaeger.de/RprobitB/reference/RprobitB_parameter.md).

## Examples

``` r
data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
model <- fit_model(data)
#> Computing sufficient statistics - 0 of 4  
#> Computing sufficient statistics - 1 of 4  
#> Computing sufficient statistics - 2 of 4  
#> Computing sufficient statistics - 3 of 4  
#> Computing sufficient statistics - 4 of 4  
#> Gibbs sampler - 1 of 1000 iterations 
#> Gibbs sampler - 10 of 1000 iterations 
#> Gibbs sampler - 20 of 1000 iterations 
#> Gibbs sampler - 30 of 1000 iterations 
#> Gibbs sampler - 40 of 1000 iterations 
#> Gibbs sampler - 50 of 1000 iterations 
#> Gibbs sampler - 60 of 1000 iterations 
#> Gibbs sampler - 70 of 1000 iterations 
#> Gibbs sampler - 80 of 1000 iterations 
#> Gibbs sampler - 90 of 1000 iterations 
#> Gibbs sampler - 100 of 1000 iterations 
#> Gibbs sampler - 110 of 1000 iterations 
#> Gibbs sampler - 120 of 1000 iterations 
#> Gibbs sampler - 130 of 1000 iterations 
#> Gibbs sampler - 140 of 1000 iterations 
#> Gibbs sampler - 150 of 1000 iterations 
#> Gibbs sampler - 160 of 1000 iterations 
#> Gibbs sampler - 170 of 1000 iterations 
#> Gibbs sampler - 180 of 1000 iterations 
#> Gibbs sampler - 190 of 1000 iterations 
#> Gibbs sampler - 200 of 1000 iterations 
#> Gibbs sampler - 210 of 1000 iterations 
#> Gibbs sampler - 220 of 1000 iterations 
#> Gibbs sampler - 230 of 1000 iterations 
#> Gibbs sampler - 240 of 1000 iterations 
#> Gibbs sampler - 250 of 1000 iterations 
#> Gibbs sampler - 260 of 1000 iterations 
#> Gibbs sampler - 270 of 1000 iterations 
#> Gibbs sampler - 280 of 1000 iterations 
#> Gibbs sampler - 290 of 1000 iterations 
#> Gibbs sampler - 300 of 1000 iterations 
#> Gibbs sampler - 310 of 1000 iterations 
#> Gibbs sampler - 320 of 1000 iterations 
#> Gibbs sampler - 330 of 1000 iterations 
#> Gibbs sampler - 340 of 1000 iterations 
#> Gibbs sampler - 350 of 1000 iterations 
#> Gibbs sampler - 360 of 1000 iterations 
#> Gibbs sampler - 370 of 1000 iterations 
#> Gibbs sampler - 380 of 1000 iterations 
#> Gibbs sampler - 390 of 1000 iterations 
#> Gibbs sampler - 400 of 1000 iterations 
#> Gibbs sampler - 410 of 1000 iterations 
#> Gibbs sampler - 420 of 1000 iterations 
#> Gibbs sampler - 430 of 1000 iterations 
#> Gibbs sampler - 440 of 1000 iterations 
#> Gibbs sampler - 450 of 1000 iterations 
#> Gibbs sampler - 460 of 1000 iterations 
#> Gibbs sampler - 470 of 1000 iterations 
#> Gibbs sampler - 480 of 1000 iterations 
#> Gibbs sampler - 490 of 1000 iterations 
#> Gibbs sampler - 500 of 1000 iterations 
#> Gibbs sampler - 510 of 1000 iterations 
#> Gibbs sampler - 520 of 1000 iterations 
#> Gibbs sampler - 530 of 1000 iterations 
#> Gibbs sampler - 540 of 1000 iterations 
#> Gibbs sampler - 550 of 1000 iterations 
#> Gibbs sampler - 560 of 1000 iterations 
#> Gibbs sampler - 570 of 1000 iterations 
#> Gibbs sampler - 580 of 1000 iterations 
#> Gibbs sampler - 590 of 1000 iterations 
#> Gibbs sampler - 600 of 1000 iterations 
#> Gibbs sampler - 610 of 1000 iterations 
#> Gibbs sampler - 620 of 1000 iterations 
#> Gibbs sampler - 630 of 1000 iterations 
#> Gibbs sampler - 640 of 1000 iterations 
#> Gibbs sampler - 650 of 1000 iterations 
#> Gibbs sampler - 660 of 1000 iterations 
#> Gibbs sampler - 670 of 1000 iterations 
#> Gibbs sampler - 680 of 1000 iterations 
#> Gibbs sampler - 690 of 1000 iterations 
#> Gibbs sampler - 700 of 1000 iterations 
#> Gibbs sampler - 710 of 1000 iterations 
#> Gibbs sampler - 720 of 1000 iterations 
#> Gibbs sampler - 730 of 1000 iterations 
#> Gibbs sampler - 740 of 1000 iterations 
#> Gibbs sampler - 750 of 1000 iterations 
#> Gibbs sampler - 760 of 1000 iterations 
#> Gibbs sampler - 770 of 1000 iterations 
#> Gibbs sampler - 780 of 1000 iterations 
#> Gibbs sampler - 790 of 1000 iterations 
#> Gibbs sampler - 800 of 1000 iterations 
#> Gibbs sampler - 810 of 1000 iterations 
#> Gibbs sampler - 820 of 1000 iterations 
#> Gibbs sampler - 830 of 1000 iterations 
#> Gibbs sampler - 840 of 1000 iterations 
#> Gibbs sampler - 850 of 1000 iterations 
#> Gibbs sampler - 860 of 1000 iterations 
#> Gibbs sampler - 870 of 1000 iterations 
#> Gibbs sampler - 880 of 1000 iterations 
#> Gibbs sampler - 890 of 1000 iterations 
#> Gibbs sampler - 900 of 1000 iterations 
#> Gibbs sampler - 910 of 1000 iterations 
#> Gibbs sampler - 920 of 1000 iterations 
#> Gibbs sampler - 930 of 1000 iterations 
#> Gibbs sampler - 940 of 1000 iterations 
#> Gibbs sampler - 950 of 1000 iterations 
#> Gibbs sampler - 960 of 1000 iterations 
#> Gibbs sampler - 970 of 1000 iterations 
#> Gibbs sampler - 980 of 1000 iterations 
#> Gibbs sampler - 990 of 1000 iterations 
#> Gibbs sampler - 1000 of 1000 iterations 
point_estimates(model)
#> alpha : double vector of length 2 
#> -0.36 0.71
#> 
#> C : NA
#> 
#> s : NA
#> 
#> b : NA
#> 
#> Omega : NA
#> 
#> Sigma : 1
#> 
#> Sigma_full : 2 x 2 matrix of doubles 
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    0
#> 
#> 
#> beta : NA
#> 
#> z : NA
#> 
#> d : NA
#> 
point_estimates(model, FUN = median)
#> alpha : double vector of length 2 
#> -0.36 0.7
#> 
#> C : NA
#> 
#> s : NA
#> 
#> b : NA
#> 
#> Omega : NA
#> 
#> Sigma : 1
#> 
#> Sigma_full : 2 x 2 matrix of doubles 
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    0
#> 
#> 
#> beta : NA
#> 
#> z : NA
#> 
#> d : NA
#> 
```

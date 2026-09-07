# Compute choice probabilities

This function returns the choice probabilities of an `RprobitB_fit`
object.

## Usage

``` r
choice_probabilities(x, data = NULL, par_set = mean)
```

## Arguments

- x:

  An object of class `RprobitB_fit`.

- data:

  Either `NULL` or an object of class `RprobitB_data`. In the former
  case, choice probabilities are computed for the data that was used for
  model fitting. Alternatively, a new data set can be provided.

- par_set:

  Specifying the parameter set for calculation and either

  - a function that computes a posterior point estimate (the default is
    [`mean()`](https://rdrr.io/r/base/mean.html)),

  - `"true"` to select the true parameter set,

  - an object of class `RprobitB_parameter`.

## Value

A data frame of choice probabilities with choice situations in rows and
alternatives in columns. The first two columns are the decider
identifier `"id"` and the choice situation identifier `"idc"`.

## Examples

``` r
data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
x <- fit_model(data)
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
choice_probabilities(x)
#>     id idc            A            B
#> 1    1   1 2.089896e-13 1.0000000000
#> 2    1   2 9.970550e-01 0.0029449686
#> 3    1   3 9.997945e-01 0.0002054899
#> 4    1   4 3.279793e-02 0.9672020670
#> 5    1   5 9.320639e-04 0.9990679361
#> 6    1   6 7.470546e-04 0.9992529454
#> 7    1   7 5.060648e-05 0.9999493935
#> 8    1   8 6.973517e-01 0.3026482916
#> 9    1   9 7.485863e-01 0.2514136635
#> 10   1  10 1.758866e-07 0.9999998241
#> 11   2   1 9.691980e-01 0.0308020087
#> 12   2   2 9.998299e-01 0.0001700808
#> 13   2   3 9.901710e-01 0.0098290111
#> 14   2   4 9.973911e-01 0.0026089365
#> 15   2   5 7.954455e-01 0.2045545118
#> 16   2   6 7.212530e-01 0.2787470226
#> 17   2   7 9.459992e-01 0.0540008356
#> 18   2   8 3.897571e-01 0.6102428689
#> 19   2   9 3.538920e-05 0.9999646108
#> 20   2  10 4.191720e-06 0.9999958083
#> 21   3   1 5.721567e-01 0.4278432986
#> 22   3   2 2.296303e-03 0.9977036971
#> 23   3   3 1.188076e-02 0.9881192375
#> 24   3   4 3.467276e-06 0.9999965327
#> 25   3   5 1.553236e-08 0.9999999845
#> 26   3   6 9.554959e-02 0.9044504095
#> 27   3   7 8.331518e-01 0.1668482486
#> 28   3   8 7.829084e-04 0.9992170916
#> 29   3   9 9.183065e-01 0.0816934722
#> 30   3  10 2.234958e-01 0.7765041887
#> 31   4   1 4.303903e-01 0.5696096600
#> 32   4   2 9.681514e-01 0.0318485898
#> 33   4   3 3.919543e-07 0.9999996080
#> 34   4   4 7.783281e-01 0.2216718581
#> 35   4   5 8.069421e-07 0.9999991931
#> 36   4   6 2.997398e-11 1.0000000000
#> 37   4   7 1.813531e-07 0.9999998186
#> 38   4   8 3.302675e-01 0.6697324633
#> 39   4   9 1.086940e-05 0.9999891306
#> 40   4  10 2.964407e-08 0.9999999704
#> 41   5   1 4.977701e-04 0.9995022299
#> 42   5   2 3.726890e-09 0.9999999963
#> 43   5   3 8.070675e-02 0.9192932527
#> 44   5   4 1.170905e-02 0.9882909535
#> 45   5   5 7.404776e-04 0.9992595224
#> 46   5   6 3.343100e-09 0.9999999967
#> 47   5   7 2.868247e-09 0.9999999971
#> 48   5   8 5.446098e-01 0.4553902456
#> 49   5   9 9.992058e-01 0.0007942198
#> 50   5  10 1.976181e-01 0.8023819085
#> 51   6   1 6.995632e-08 0.9999999300
#> 52   6   2 9.955018e-01 0.0044982353
#> 53   6   3 6.362889e-03 0.9936371107
#> 54   6   4 8.475081e-06 0.9999915249
#> 55   6   5 2.436040e-05 0.9999756396
#> 56   6   6 9.796385e-01 0.0203615063
#> 57   6   7 2.905126e-01 0.7094874392
#> 58   6   8 9.995363e-01 0.0004636912
#> 59   6   9 4.138592e-07 0.9999995861
#> 60   6  10 7.029959e-03 0.9929700410
#> 61   7   1 9.322913e-03 0.9906770875
#> 62   7   2 2.138165e-02 0.9786183482
#> 63   7   3 5.507464e-03 0.9944925358
#> 64   7   4 7.135439e-02 0.9286456131
#> 65   7   5 5.146782e-02 0.9485321808
#> 66   7   6 3.829855e-10 0.9999999996
#> 67   7   7 4.821002e-04 0.9995178998
#> 68   7   8 9.328323e-05 0.9999067168
#> 69   7   9 5.528081e-02 0.9447191907
#> 70   7  10 1.317266e-13 1.0000000000
#> 71   8   1 1.348879e-01 0.8651120980
#> 72   8   2 6.011067e-05 0.9999398893
#> 73   8   3 8.540193e-06 0.9999914598
#> 74   8   4 9.994802e-01 0.0005197716
#> 75   8   5 2.411907e-01 0.7588092585
#> 76   8   6 1.764051e-05 0.9999823595
#> 77   8   7 2.789796e-12 1.0000000000
#> 78   8   8 5.260345e-01 0.4739654816
#> 79   8   9 4.180090e-04 0.9995819910
#> 80   8  10 3.313074e-10 0.9999999997
#> 81   9   1 4.834690e-05 0.9999516531
#> 82   9   2 4.888606e-03 0.9951113941
#> 83   9   3 3.326073e-09 0.9999999967
#> 84   9   4 3.076711e-04 0.9996923289
#> 85   9   5 8.792712e-02 0.9120728779
#> 86   9   6 1.634683e-02 0.9836531661
#> 87   9   7 8.540135e-06 0.9999914599
#> 88   9   8 6.969744e-02 0.9303025638
#> 89   9   9 6.800931e-01 0.3199068650
#> 90   9  10 1.985398e-02 0.9801460173
#> 91  10   1 6.032186e-01 0.3967813703
#> 92  10   2 7.503242e-01 0.2496757614
#> 93  10   3 1.939624e-01 0.8060376182
#> 94  10   4 1.459884e-02 0.9854011601
#> 95  10   5 4.942762e-01 0.5057238389
#> 96  10   6 3.971489e-01 0.6028510649
#> 97  10   7 9.998959e-01 0.0001041308
#> 98  10   8 2.298834e-03 0.9977011663
#> 99  10   9 1.183627e-01 0.8816372636
#> 100 10  10 2.751237e-06 0.9999972488
```

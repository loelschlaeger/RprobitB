# Split choice data into train and test subset

This function splits choice data into a train and a test part.

## Usage

``` r
train_test(
  x,
  test_proportion = NULL,
  test_number = NULL,
  by = "N",
  random = FALSE
)
```

## Arguments

- x:

  An object of class `RprobitB_data`.

- test_proportion:

  \[`numeric(1)`\]  
  The proportion of the test subset.

- test_number:

  \[`integer(1)`\]  
  The number of observations in the test subset.

- by:

  \[`character(1)`\]  
  Either `"N"` (split by deciders) and `"T"` (split by occasions).

- random:

  \[`logical(1)`\]  
  Build subsets randomly?

## Value

A list with two objects of class `RprobitB_data`, named `"train"` and
`"test"`.

## Examples

``` r
### simulate choices for demonstration
x <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)

### 70% of deciders in the train subsample,
### 30% of deciders in the test subsample
train_test(x, test_proportion = 0.3, by = "N")
#> $train
#> Simulated data of 70 choices.
#> 
#> $test
#> Simulated data of 30 choices.
#> 

### 2 randomly chosen choice occasions per decider in the test subsample,
### the rest in the train subsample
train_test(x, test_number = 2, by = "T", random = TRUE)
#> $train
#> Simulated data of 80 choices.
#> 
#> $test
#> Simulated data of 20 choices.
#> 
```

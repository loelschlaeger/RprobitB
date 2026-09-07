# Stated Preferences for Train Traveling

Data set of 2929 stated choices by 235 Dutch individuals deciding
between two virtual train trip options `"A"` and `"B"` based on the
price, the travel time, the number of rail-to-rail transfers (changes),
and the level of comfort.

The data were obtained in 1987 by Hague Consulting Group for the
National Dutch Railways. Prices were recorded in Dutch guilder and in
this data set transformed to Euro at an exchange rate of 2.20371
guilders = 1 Euro.

## Usage

``` r
train_choice
```

## Format

A `data.frame` with 2929 rows and 11 columns:

- deciderID:

  `integer` identifier for the decider

- occasionID:

  `integer` identifier for the choice occasion

- choice:

  `character` for the chosen alternative (either `"A"` or `"B"`)

- price_A:

  `numeric` price for alternative `"A"` in Euro

- time_A:

  `numeric` travel time for alternative `"A"` in hours

- change_A:

  `integer` number of changes for alternative `"A"`

- comfort_A:

  `integer` comfort level (in decreasing comfort order) for alternative
  `"A"`

- price_B:

  `numeric` price for alternative `"B"` in Euro

- time_B:

  `numeric` travel time for alternative `"B"` in hours

- change_B:

  `integer` number of changes for alternative `"B"`

- comfort_B:

  `integer` comfort level (in decreasing comfort order) for alternative
  `"B"`

## References

Ben-Akiva M, Bolduc D, Bradley M (1993). “Estimation of travel choice
models with randomly distributed values of time.” *Transportation
Research Record*, **1413**.

Meijer E, Rouwendal J (2006). “Measuring welfare effects in models with
random coefficients.” *Journal of Applied Econometrics*, **21**(2).

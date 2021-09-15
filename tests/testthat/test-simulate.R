test_that("data simulation works", {
  form = choice ~ cost | income | travel_time
  re = "cost"
  N = 100
  T = 10
  J = 3
  alternatives = c("car", "bus", "train")
  distr = list("cost" = list("name" = "rnorm", sd = 3),
               "income" = list("name" = "sample", x = (1:10)*1e3,
                               replace = TRUE),
               "travel_time_car" = list("name" = "rlnorm", meanlog = 1),
               "travel_time_bus" = list("name" = "rlnorm", meanlog = 2))
  standardize = c("income", "travel_time_car", "travel_time_bus",
                  "travel_time_train")
  data = simulate(form = form, N = N, T = T, J = J, re = re,
                  alternatives = alternatives, distr = distr,
                  standardize = standardize, seed = 1, "C" = 2,
                  "s" = c(0.5,0.5))
  expect_snapshot(data$data)
  expect_snapshot(summary(data))
})

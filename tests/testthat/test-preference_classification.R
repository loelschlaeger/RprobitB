test_that("preference classification works", {
  data <- simulate_choices(
    form = choice ~ cost | income | time,
    N = 30,
    T = 10,
    J = 3,
    re = c("cost", "ASC"),
    alternatives = c("train", "bus", "car"),
    seed = 1,
    C = 2
  )
  model <- mcmc(data,
    R = 1000, print_progress = FALSE, seed = 1,
    latent_classes = list("C" = 2)
  )
  expect_snapshot(preference_classification(model, true = TRUE))
})

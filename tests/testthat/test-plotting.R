test_that("plotting choice data works", {
  data <- simulate_choices(
    form = choice ~ cost | 0,
    N = 100,
    T = 10,
    J = 2,
    alternatives = c("bus", "car"),
    true_parameter = list("alpha" = -1)
  )
  pdf(NULL)
  out <- plot(data, by_choice = TRUE)
  dev.off()
  expect_true(gtable::is.gtable(out))
})

test_that("plotting mixture contour works", {
  means <- list(c(0, 0), c(2, 2))
  covs <- list(diag(2), 0.5 * diag(2))
  weights <- c(0.7, 0.3)
  names <- c("A", "B")
  plot <- plot_mixture_contour(means, covs, weights, names)
  expect_true(ggplot2::is_ggplot(plot))
})

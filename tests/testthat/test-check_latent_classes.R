test_that("checks for latent classes work", {
  expect_snapshot(check_latent_classes(latent_classes = NULL))
  expect_snapshot(
    check_latent_classes(
      latent_classes = list("C" = 2, "update" = TRUE, "Cinit" = 5, "Cmax" = 10,
                            "buffer" = 100, "epsmin" = 0.01, "epsmax" = 0.99,
                            "distmin" = 0.1))
  )
  expect_error(check_latent_classes(latent_classes = list("update" = TRUE, "Cinit" = 5, "Cmax" = 4)))
})

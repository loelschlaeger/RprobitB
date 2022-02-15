test_that("latent_class setting works", {
  expect_error(
    RprobitB_latent_classes("not a list")
  )
  expect_snapshot(
    RprobitB_latent_classes(list("C" = 2))
  )
  expect_error(
    RprobitB_latent_classes(list("C" = -1))
  )
  expect_snapshot(
    (out <- RprobitB_latent_classes(list(
      "weight_update" = TRUE,
      "dp_update" = TRUE
    )))
  )
  expect_snapshot(str(out))
})

test_that("can draw from prior distribution", {
  prior <- check_prior(P_f = 1, P_r = 2, J = 2, ordered = FALSE)
  checkmate::expect_list(draw_from_prior(prior, C = 2))
})

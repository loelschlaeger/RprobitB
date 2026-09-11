run_cpp_tests("RprobitB")

test_that("compiled sampler tests are registered", {
  expect_true(is.loaded("run_testthat_tests", PACKAGE = "RprobitB"))
})

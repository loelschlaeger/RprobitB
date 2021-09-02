test_that("empirical choice data preparation works", {
  form = choice ~ price | 0 | time + comfort + change
  data("Train", package = "mlogit")
  choice_data = Train
  alternatives = NULL
  re = c("price","time")
  id = "id"
  standardize = "all"
  out = prepare(form = form, choice_data = choice_data,
                alternatives = alternatives, re = re, id = id,
                standardize = standardize)
  expect_s3_class(out, "RprobitB_data")
  expect_snapshot(out$data)
  #choice_data_bug = choice_data
  #choice_data_bug[1,1] = NA
})

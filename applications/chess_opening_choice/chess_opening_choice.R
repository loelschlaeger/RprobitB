
choice_chess_opening <- create_lagged_cov(
  choice_data = RprobitB::choice_chess_opening,
  column = "w1",
  k = 1,
  id = "fideid_w")

choice_chess_opening <- fastDummies::dummy_cols(choice_chess_opening, "w1.1")

choice_chess_opening$rating_diff <- choice_chess_opening$rating_w - choice_chess_opening$rating_b

data <- prepare_data(
  form = w1 ~ w1.1 | sex_w + byear_w + rating_w + rating_diff + 1,
  choice_data = choice_chess_opening,
  id = "fideid_w",
  idc = "fideid_b",
  alternatives = c("e4","d4","b3"),
  standardize = c("byear_w","rating_w","rating_diff"),
  impute = "complete_cases"
)

plot(data, by_choice = TRUE)
saveRDS(data, "applications/chess_opening_choice/data.rds", compress = "xz")

model_chess_opening <- fit_model(data)
saveRDS(model_chess_opening, "applications/chess_opening_choice/model_chess_opening.rds", compress = "xz")

model_chess_opening_sparse <- nested_model(model_chess_opening,
                                           form = w1 ~ w1.1 | 1)
saveRDS(model_chess_opening_sparse, "applications/chess_opening_choice/model_chess_opening_sparse.rds", compress = "xz")

coef(model_chess_opening)
coef(model_chess_opening_sparse)

model_selection(model_chess_opening, model_chess_opening_sparse)



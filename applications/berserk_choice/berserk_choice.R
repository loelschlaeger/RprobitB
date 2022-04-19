
choice_berserk <- create_lagged_cov(
  choice_data = RprobitB::choice_berserk,
  column = c("berserk","lost"),
  k = 1,
  id = "player_id"
)

data <- prepare_data(
  form = berserk ~ 0 | white + rating + rating_diff + min_rem + streak + berserk.1 + lost.1 + 1,
  re = c("rating","min_rem","lost.1"),
  choice_data = choice_berserk,
  id = "player_id",
  idc = "game_id",
  standardize = c("rating","rating_diff","min_rem"),
  impute = "zero_out"
)

plot(data, by_choice = TRUE)
saveRDS(data, "applications/berserk_choice/data.rds", compress = "xz")

model_berserk <- mcmc(data)
saveRDS(model_berserk, "applications/berserk_choice/model_berserk.rds", compress = "xz")

coef(model_berserk)

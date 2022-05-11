
choice_berserk <- create_lagged_cov(
  choice_data = RprobitB::choice_berserk,
  column = c("berserk","lost"),
  k = 1,
  id = "player_id"
)

data <- prepare_data(
  form = berserk ~ 0 | white + rating + rating_diff + min_rem + streak + berserk.1 + lost.1 + 1,
  re = c("rating_diff","lost.1"),
  choice_data = choice_berserk,
  id = "player_id",
  idc = "game_id",
  standardize = c("rating","rating_diff","min_rem"),
  impute = "zero"
)

plot(data, by_choice = TRUE)
saveRDS(data, "applications/berserk_choice/data.rds", compress = "xz")

model_berserk <- fit_model(data,
                           latent_classes = list("dp_update" = TRUE, "C" = 10),
                           R = 5000)
saveRDS(model_berserk, "applications/berserk_choice/model_berserk.rds", compress = "xz")

coef(model_berserk)
summary(model_berserk)
plot(model_berserk, "mixture")
plot(model_berserk, "class_seq")

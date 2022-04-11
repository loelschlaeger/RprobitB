### berserk choice

data("choice_berserk")
choice_berserk <- create_lagged_cov(
  choice_data = choice_berserk,
  column = c("berserk","lost"),
  k = 1,
  id = "player_id"
)
data <- prepare_data(
  form = berserk ~ 0 | white + rating + rating_diff + lost + min_rem + streak + berserk.1 + lost.1 + 1,
  choice_data = choice_berserk,
  id = "player_id",
  idc = "game_id",
  standardize = c("rating","rating_diff","min_rem"),
  impute = "complete_cases"
)
# plot(data, by_choice = TRUE)
# saveRDS(data, "applications/berserk_choice/data.rds", compress = "xz")
mod_fix <- mcmc(data)
saveRDS(mod_fix, "applications/berserk_choice/mod_fix.rds", compress = "xz")
mod_re <- nested_model(mod_fix,
                       re = c("white","rating","rating_diff","lost","min_rem","streak","berserk.1","lost.1"))
saveRDS(mod_re, "applications/berserk_choice/mod_re.rds", compress = "xz")
mod_cl <- nested_model(mod_re,
                       latent_classes = list("dp_update" = TRUE))
saveRDS(mod_cl, "applications/berserk_choice/mod_cl.rds", compress = "xz")


### chess opening choice

data("choice_chess_opening")
choice_chess_opening <- subset(choice_chess_opening, startsWith(game, "1. e4"))
choice_chess_opening <- create_lagged_cov(choice_chess_opening, "b1", id = "fideid_b")
choice_chess_opening <- fastDummies::dummy_cols(choice_chess_opening, "b1.1")
choice_chess_opening$rating_diff <- choice_chess_opening$rating_b - choice_chess_opening$rating_w
data <- prepare_data(
  form = b1 ~ b1.1 | sex_b + byear_b + rating_b + rating_diff,
  choice_data = choice_chess_opening,
  id = "fideid_b",
  idc = "fideid_w",
  alternatives = c("c5","e5","c6","e6"),
  standardize = c("byear_b","rating_b","rating_diff"),
  impute = "complete_cases"
)
# plot(data, by_choice = TRUE)
# saveRDS(data, "applications/chess_opening_choice/data.rds", compress = "xz")
data_split <- train_test(data, test_proportion = 0.5, random = TRUE, seed = 1)
mod_fix <- mcmc(data_split$train)
saveRDS(mod_fix, "applications/chess_opening_choice/mod_fix.rds", compress = "xz")
mod_base <- nested_model(mod_fix, form = b1 ~ b1.1)
saveRDS(mod_base, "applications/chess_opening_choice/mod_base.rds", compress = "xz")
mod_re <- nested_model(mod_fix,
                       re = c("color","rating","rating_diff","min_rem","streak"))
saveRDS(mod_re, "applications/chess_opening_choice/mod_re.rds", compress = "xz")
mod_cl <- nested_model(mod_re,
                       latent_classes = list("dp_update" = TRUE))
saveRDS(mod_cl, "applications/chess_opening_choice/mod_cl.rds", compress = "xz")

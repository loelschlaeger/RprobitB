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

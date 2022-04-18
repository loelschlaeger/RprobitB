
# Berserk choice ----------------------------------------------------------

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
                       prior = list("delta" = 0.1),
                       latent_classes = list("dp_update" = TRUE))
saveRDS(mod_cl, "applications/berserk_choice/mod_cl.rds", compress = "xz")



# Chess opening choice ----------------------------------------------------


data("choice_chess_opening")
choice_chess_opening <- create_lagged_cov(choice_chess_opening, "w1", id = "fideid_w")
choice_chess_opening <- fastDummies::dummy_cols(choice_chess_opening, "w1.1")
choice_chess_opening$rating_diff <- choice_chess_opening$rating_w - choice_chess_opening$rating_b
data <- prepare_data(
  form = w1 ~ w1.1 | sex_w + byear_w + rating_w + rating_diff,
  choice_data = choice_chess_opening,
  id = "fideid_w",
  idc = "fideid_b",
  alternatives = c("e4","d4","b3"),
  standardize = c("byear_w","rating_w","rating_diff"),
  impute = "complete_cases"
)
# plot(data, by_choice = TRUE)
# saveRDS(data, "applications/chess_opening_choice/data.rds", compress = "xz")

mod_fix <- mcmc(data)
mod_fix <- compute_p_si(mod_fix)
saveRDS(mod_fix, "applications/chess_opening_choice/mod_fix.rds", compress = "xz")

mod_base <- nested_model(mod_fix, form = w1 ~ w1.1)
mod_base <- compute_p_si(mod_base)
saveRDS(mod_base, "applications/chess_opening_choice/mod_base.rds", compress = "xz")

mod_re1 <- nested_model(mod_fix, re = c("w1.1"))
mod_re1 <- compute_p_si(mod_re1)
saveRDS(mod_re1, "applications/chess_opening_choice/mod_re1.rds", compress = "xz")

mod_re2 <- nested_model(mod_fix, re = c("w1.1","rating_w"))
mod_re2 <- compute_p_si(mod_re2)
saveRDS(mod_re2, "applications/chess_opening_choice/mod_re2.rds", compress = "xz")

model_selection(mod_base, mod_fix, mod_re1, mod_re2)



# Child wish --------------------------------------------------------------

contraception_data <- readRDS("applications/contraception_choice/data/data_encoded.rds")
data <- prepare_data(
  form = child_wish ~ 0 | child_cond1 + child_cond2 + child_cond3 +
    child_cond4 + child_cond5 + child_cond6 + child_cond7 + child_cond8,
  choice_data = contraception_data,
  id = "id",
  idc = "wave",
  impute = "complete_cases"
)
# plot(data, by_choice = TRUE)
# saveRDS(data, "applications/contraception_choice/data.rds", compress = "xz")

mod_fix <- mcmc(data)
saveRDS(mod_fix, "applications/contraception_choice/mod_fix.rds", compress = "xz")

mod_re <- nested_model(mod_fix,
                       re = paste0("child_cond", 1:8))
saveRDS(mod_re, "applications/contraception_choice/mod_re.rds", compress = "xz")

mod_cl1 <- nested_model(mod_re,
                        prior = list("delta" = 0.1),
                        latent_classes = list("dp_update" = TRUE))
saveRDS(mod_cl1, "applications/contraception_choice/mod_cl1.rds", compress = "xz")

mod_cl2 <- nested_model(mod_re,
                        prior = list("delta" = 0.2),
                        latent_classes = list("dp_update" = TRUE))
saveRDS(mod_cl2, "applications/contraception_choice/mod_cl2.rds", compress = "xz")

mod_cl3 <- nested_model(mod_re,
                        prior = list("delta" = 0.3),
                        latent_classes = list("dp_update" = TRUE))
saveRDS(mod_cl3, "applications/contraception_choice/mod_cl3.rds", compress = "xz")




# Contraception choice ----------------------------------------------------



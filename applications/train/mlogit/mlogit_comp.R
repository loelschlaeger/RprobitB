data("Train", package = "mlogit")

Train$choiceid = 1:nrow(Train)

### normalize
Train[c("price_A","price_B")]     = (Train[c("price_A","price_B")]     - mean(c(Train$price_A,Train$price_B)))     / sd(c(Train$price_A,Train$price_B))
Train[c("time_A","time_B")]       = (Train[c("time_A","time_B")]       - mean(c(Train$time_A,Train$time_B)))       / sd(c(Train$time_A,Train$time_B))
Train[c("change_A","change_B")]   = (Train[c("change_A","change_B")]   - mean(c(Train$change_A,Train$change_B)))   / sd(c(Train$change_A,Train$change_B))
Train[c("comfort_A","comfort_B")] = (Train[c("comfort_A","comfort_B")] - mean(c(Train$comfort_A,Train$comfort_B))) / sd(c(Train$comfort_A,Train$comfort_B))

Train = dfidx::dfidx(Train, shape = "wide", varying = 4:11, sep = "_", 
                     idx = list(c("choiceid", "id")), idnames = c(NA, "alt"))

mlogit::mlogit(formula = choice ~ price + time + change + comfort, data = Train)

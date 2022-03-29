### metadata about dataset
tid <- "RibHfoX6"
exdir <- "applications/berserk_choice"

### get tournament info
url_ti <- paste0("https://lichess.org/api/tournament/",tid)
res <- httr::GET(url_ti)
write(rawToChar(res$content), paste0(exdir,"/tournament_info.json"))
tournament_info <- jsonlite::stream_in(file(paste0(exdir,"/tournament_info.json")))
t_start <- as.POSIXct(tournament_info$startsAt, format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
t_end <- t_start + tournament_info$minutes*60

### scrape data
url <- paste0("https://lichess.org/api/tournament/",tid,"/games")
h <- "application/x-ndjson"
names(h) <- "Accept"
res <- httr::GET(url, httr::add_headers(h))
write(rawToChar(res$content), paste0(exdir,"/data.json"))
data_all <- jsonlite::flatten(jsonlite::stream_in(file(paste0(exdir,"/data.json"))))

### remove games that failed to start
data_all <- data_all[!data_all$status %in% c("noStart","timeout"), ]

### remove games with no winner
data_all <- data_all[!is.na(data_all$winner), ]

### bring 'data_full' to long format
choice_berserk <- data.frame()
for(i in 1:nrow(data_all)) {
  cat(sprintf("%.0f %% \r", i/nrow(data_all)*100))
  N <- nrow(choice_berserk)
  min_rem <- as.numeric(difftime(t_end, as.POSIXct(data_all[i,"createdAt"]/1000, origin="1970-01-01", tz = "GMT"), units = "min"))
  ### white data
  choice_berserk[N+1, "player_id"] <- data_all[i,"players.white.user.id"]
  choice_berserk[N+1, "game_id"] <-  data_all[i,"id"]
  choice_berserk[N+1, "berserk"] <- ifelse(is.na(data_all[i,"players.white.berserk"]), FALSE, TRUE)
  choice_berserk[N+1, "color"] <- "white"
  choice_berserk[N+1, "rating"] <- data_all[i,"players.white.rating"]
  choice_berserk[N+1, "rating_diff"] <- data_all[i,"players.white.rating"] - data_all[i,"players.black.rating"]
  choice_berserk[N+1, "lost"] <- ifelse(data_all[i,"winner"] != "white", TRUE, FALSE)
  choice_berserk[N+1, "min_rem"] <- min_rem
  ### black data
  choice_berserk[N+2, "player_id"] <- data_all[i,"players.black.user.id"]
  choice_berserk[N+2, "game_id"] <-  data_all[i,"id"]
  choice_berserk[N+2, "berserk"] <- ifelse(is.na(data_all[i,"players.black.berserk"]), FALSE, TRUE)
  choice_berserk[N+2, "color"] <- "black"
  choice_berserk[N+2, "rating"] <- data_all[i,"players.black.rating"]
  choice_berserk[N+2, "rating_diff"] <- data_all[i,"players.black.rating"] - data_all[i,"players.white.rating"]
  choice_berserk[N+2, "lost"] <- ifelse(data_all[i,"winner"] != "black", TRUE, FALSE)
  choice_berserk[N+2, "min_rem"] <- min_rem
}

### reverse order (first games first)
choice_berserk <- choice_berserk[order(choice_berserk$min_rem, decreasing = TRUE),]

### add info if player is on streak
player_ids <- unique(choice_berserk$player_id)
for(pid in player_ids){
  games <- which(choice_berserk$player_id == pid)
  for(g in seq_along(games)){
    if(g %in% 1:2){
      choice_berserk[games[g],"streak"] <- FALSE
    } else {
      if(choice_berserk[games[g-1],"lost"] || choice_berserk[games[g-2],"lost"]) {
        choice_berserk[games[g],"streak"] <- FALSE
      } else {
        choice_berserk[games[g],"streak"] <- TRUE
      }
    }
  }
}

### save R dataset
usethis::use_data(choice_berserk, overwrite = TRUE, compress = 'xz')

### create documentation
doc <- paste0("#' Choice of berserking
#'
#' @description
#' This dataset includes the binary 'berserking' choice of participants in the
#' yearly bullet arena 2022 on the online chess platform \\url{https://lichess.org}.
#' Berserking is a choice each player has at the beginning of each game:
#' When a player clicks the 'Berserk button', they lose half of their clock time,
#' but the win is worth one extra tournament point.
#'
#' @details
#' To 'berserk' is a feature on the online chess platform \\url{https://lichess.org}.
#' Before the game starts, each player can click a button, after which
#' they lose half of their clock time, but the win is worth one extra tournament
#' point.
#'
#' The considered tournament had the following characteristics:
#' \\itemize{
#'   \\item The tournament startet at ", t_start, " and lasted ", tournament_info$minutes, " minutes.
#'   \\item The time control was 1 minute per player per game (bullet format).
#'   \\item The players are automatically and immediately paired again after a game has finished,
#'         which is the so-called 'arena tournament' modus.
#'   \\item The players can pause their participation at any time.
#'   \\item A win has a base score of 2 points, a draw 1 point, and a loss is worth no points.
#'   \\item If a player wins two games consecutively, they will start a double point streak,
#'         which means the following games will continue to be worth double points
#'         until they fail to win a game.
#' }
#'
#' @references
#' See \\url{https://lichess.org/tournament/help?system=arena}
#' for more information on the tournament format.
#'
#' @docType data
#'
#' @usage data(choice_berserk)
#'
#' @format
#' A data frame containing berserking choices of ", length(unique(choice_berserk$player_id)), " chess players
#' in ", nrow(choice_berserk), " online bullet (1+0) games.
#' It consists of the following columns:
#' \\itemize{
#'   \\item \\code{player_id}, unique lichess username of the chess player
#'   \\item \\code{game_id}, unique lichess identification of the game
#'   \\item \\code{berserk}, a boolean whether the player berserked
#'   \\item \\code{color}, the color of the player (either 'white' or 'black')
#'   \\item \\code{rating}, the player's lichess bullet rating at the start of the game
#'   \\item \\code{rating_diff}, the rating difference to the opponent,
#'   \\item \\code{lost}, a boolean whether the player lost their game (and hence lost his streak)
#'   \\item \\code{min_rem}, the number of minutes left in the tournament
#'   \\item \\code{streak}, a boolean whether the player is on a streak (see the details)
#' }
#'
#' @source
#' The data was obtained via the lichess API \\url{https://lichess.org/api}
#' with the tournament id '", tid, "' on ", Sys.Date(), ".
#'
#' @keywords
#' dataset
#'
'choice_berserk'")
fileConn <- file("R/data_choice_berserk.R")
writeLines(doc, fileConn)
close(fileConn)

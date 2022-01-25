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

### build dataset
berserk_choice <- data.frame(
  "id_white" = data_all$players.white.user.id,
  "id_black" = data_all$players.black.user.id,
  "rating_white" = data_all$players.white.rating,
  "rating_black" = data_all$players.black.rating,
  "berserk_white" = ifelse(is.na(data_all$players.white.berserk), FALSE, TRUE),
  "berserk_black" = ifelse(is.na(data_all$players.black.berserk), FALSE, TRUE),
  "winner" = ifelse(is.na(data_all$winner), "draw", data_all$winner),
  "minutes_remaining" = as.numeric(difftime(t_end, as.POSIXct(data_all$createdAt/1000, origin="1970-01-01", tz = "GMT"), units = "min")),
  "game_id" = data_all$id)

### remove games that failed to start
berserk_choice <- berserk_choice[!data_all$status %in% c("noStart","timeout"), ]

### save R dataset
save(berserk_choice, file="data/berserk_choice.RData")

### create documentation
doc <- paste0("#' Choice of berserking
#'
#' @description
#' This dataset includes the binary berserking choice of participants in the yearly
#' bullet arena on the online chess platform \\url{https://lichess.org}.
#' The tournament startet at ", t_start, " and lastet ", tournament_info$minutes, " minutes.
#'
#' @details
#' To 'berserk' is a feature on the online chess platform \\url{https://lichess.org}.
#' Before the game starts, each player can click a 'berserk' button, after which
#' they lose half of their clock time, but the win is worth one extra tournament
#' point. See \\url{https://lichess.org/tournament/help?system=arena}
#' for more information.
#'
#' @docType data
#'
#' @usage data(berserk_choice)
#'
#' @format
#' A data frame containing berserking choices of the White and the Black player
#' in ", nrow(berserk_choice), " bullet (1+0) games. It consists of the following columns:
#' \\itemize{
#'   \\item \\code{id_white} and \\code{id_black}, unique lichess usernames of the White and the Black players for each game,
#'   \\item \\code{rating_white} and \\code{rating_black}, both player's lichess bullet rating at the start of each game,
#'   \\item \\code{berserk_white} and \\code{berserk_black}, a boolean whether the players berserked,
#'   \\item \\code{winner}, the winner of the game, either \\code{'white'}, \\code{'black'} or \\code{'draw'},
#'   \\item \\code{minutes_remaining}, the number of minutes left in the tournament,
#'   \\item \\code{game_id}, the game's id on lichess (each game can be accessed via \\url{https://lichess.org/<game_id>}).
#' }
#'
#' @source
#' The data was obtained via the lichess API \\url{https://lichess.org/api}
#' with the tournament id '", tid, "' on ", Sys.Date(), ".
#'
#' @keywords
#' dataset
#'
'berserk_choice'")
fileConn <- file("R/berserk_choice-data.R")
writeLines(doc, fileConn)
close(fileConn)

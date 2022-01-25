### metadata about dataset
date_today <- Sys.Date()
twic_issues <- 1416:1420
exdir <- "applications/chess_opening"

### get player information from FIDE
file_dest <- tempfile()
utils::download.file("http://ratings.fide.com/download/players_list_xml.zip", file_dest)
file <- utils::unzip(file_dest, exdir = exdir)
x <- xml2::read_xml(file)
data_list <- list()
items_player <- c("fideid","name","country","sex","birthday","rating")
all_items_player <- c("fideid","name","country","sex","title","rating","games",
                      "rapid_rating","rapid_games","blitz_rating","blitz_games",
                      "birthday")
for(i in seq_along(items_player)){
  data_list[[i]] <- xml2::xml_text(xml2::xml_find_all(x, paste0("//player//",items_player[i])))
}
players <- data.frame(data_list)
colnames(players) <- items_player

### clean-up player information
players$sex <- as.factor(players$sex)
players$country <- as.factor(players$country)
players$age <- as.numeric(players$birthday)
players <- players[complete.cases(players), ]
players$rating <- as.numeric(players$rating)
players$rating[is.na(players$rating)] <- 0

### get games from 'The Week in Chess'
games_full <- NULL
for(twic in twic_issues){
  file_dest <- tempfile()
  url <- paste0("https://theweekinchess.com/zips/twic",twic,"g.zip")
  utils::download.file(url, file_dest)
  file <- utils::unzip(file_dest, exdir = exdir)
  games_full <- rbind(games_full, bigchess::read.pgn(file))
}

### clean-up games information
items_games <- c("Date", "White", "Black", "Result", "W1", "B1")
all_items_games <- c('Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result', 'Movetext', 'NMoves', 'W1', 'B1', 'W2', 'B2', 'W3', 'B3', 'W4', 'B4', 'W5', 'B5', 'W6', 'B6', 'W7', 'B7', 'W8', 'B8', 'W9', 'B9', 'W10', 'B10', 'last.move', 'complete.movetext', 'W_B_moves', 'W_K_moves', 'W_N_moves', 'W_O_moves', 'W_Q_moves', 'W_R_moves', 'B_B_moves', 'B_K_moves', 'B_N_moves', 'B_O_moves', 'B_Q_moves', 'B_R_moves', 'B_moves', 'K_moves', 'N_moves', 'O_moves', 'Q_moves', 'R_moves')
games <- games_full[items_games]
games$Date <- as.Date(games$Date, format = "%Y.%m.%d")
games$White <- gsub(",([[:alpha:]])", ", \\1", games$White)
games$Black <- gsub(",([[:alpha:]])", ", \\1", games$Black)

### merge information from 'players' and 'games'
opening_choice <- data.frame()
for(i in 1:nrow(games)){
  name_white <- games[i, "White"]
  index_white <- stringr::str_which(players$name, name_white)
  name_black <- games[i, "Black"]
  index_black <- stringr::str_which(players$name, name_black)
  ### skip if one of names is ambiguous
  if(length(index_white) != 1 || length(index_black) != 1){
    next()
  } else {
    N <- nrow(opening_choice) + 1
    opening_choice[N, "fideid"] <- players[index_white, "fideid"]
    opening_choice[N, "move"] <- games[i, "W1"]
    opening_choice[N, "country"] <- players[index_white, "country"]
    opening_choice[N, "sex"] <- players[index_white, "sex"]
    opening_choice[N, "age"] <- players[index_white, "age"]
    opening_choice[N, "rating"] <- players[index_white, "rating"]
    opening_choice[N, "opponent_country"] <- players[index_black, "country"]
    opening_choice[N, "opponent_sex"] <- players[index_black, "sex"]
    opening_choice[N, "age_difference"] <- players[index_white, "age"]- players[index_black, "age"]
    opening_choice[N, "rating_difference"] <- players[index_white, "rating"]- players[index_black, "rating"]
    info <- paste(players[index_white, "name"], "against", players[index_black, "name"], "on", games[i, "Date"], "with result", games[i, "Result"])
    opening_choice[N, "info"] <- info
  }
}

### save R dataset
save(opening_choice, file="data/opening_choice.RData")

### create documentation
doc <- paste0("#' Choice of a chess opening
#'
#' @description
#' This dataset includes first move choices of the White player in chess games.
#'
#' @docType data
#'
#' @usage data(opening_choice)
#'
#' @format
#' A data frame containing first move choices of the White player in ", nrow(opening_choice), " chess games.
#' It consists of the following columns:
#' \\itemize{
#'   \\item \\code{fideid}, unique identifier for each player provided by FIDE,
#'   \\item \\code{move}, the chosen first move,
#'   \\item \\code{country}, the player's federation,
#'   \\item \\code{sex}, the player's gender,
#'   \\item \\code{age}, the player's age,
#'   \\item \\code{rating}, the player's rating,
#'   \\item \\code{opponent_country}, the opponent's federation,
#'   \\item \\code{opponent_sex}, the opponent's gender,
#'   \\item \\code{opponent_country}, the opponent's federation,
#'   \\item \\code{age_difference}, the player's age minus the opponent's age,
#'   \\item \\code{info}, a short description about the pairing.
#' }
#'
#' @source
#' The opening moves were scraped from The Week in Chess Archive
#' \\url{https://theweekinchess.com/twic} issues ", paste(twic_issues,collapse=", "), ".
#' The player's metadata (country, sex, age, rating) were added by matching the pairings names
#' with the official FIDE rating list \\url{http://ratings.fide.com} on ", date_today, ".
#' Ambiguous matches were dropped.
#'
'choice_opening'")
fileConn <- file("R/opening_choice-data.R")
writeLines(doc, fileConn)
close(fileConn)

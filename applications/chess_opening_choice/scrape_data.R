### metadata about dataset
date_today <- Sys.Date()
twic_issues <- 1417:1428
exdir <- "applications/chess_opening_choice"

### get player information from FIDE
file_dest <- tempfile()
utils::download.file("http://ratings.fide.com/download/players_list_xml.zip", file_dest)
file <- utils::unzip(file_dest, exdir = exdir)
x <- xml2::read_xml(file)
data_list <- list()
items_player <- c("fideid","name","sex","birthday","rating")
### all items:
### "fideid","name","country","sex","title","rating","games", "rapid_rating",
### "rapid_games","blitz_rating","blitz_games", "birthday"
for(i in seq_along(items_player)){
  cat(i,"of",length(items_player),"\r")
  data_list[[i]] <- xml2::xml_text(xml2::xml_find_all(x, paste0("//player//",items_player[i])))
}
players <- data.frame(data_list)
colnames(players) <- items_player

### clean-up player information
players$sex[which(players$sex == "M")] <- 0
players$sex[which(players$sex == "F")] <- 1
players$byear <- as.numeric(players$birthday)
players$birthday <- NULL
players$rating <- as.numeric(players$rating)
players <- players[complete.cases(players), ]

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
items_games <- c("Date", "White", "Black", "Result", "Movetext",
                 "complete.movetext", "W1", "B1", "NMoves", "Event")
### all items
### 'Event', 'Site', 'Date', 'Round', 'White', 'Black', 'Result', 'Movetext',
### 'NMoves', 'W1', 'B1', 'W2', 'B2', 'W3', 'B3', 'W4', 'B4', 'W5', 'B5', 'W6',
### 'B6', 'W7', 'B7', 'W8', 'B8', 'W9', 'B9', 'W10', 'B10', 'last.move',
### 'complete.movetext', 'W_B_moves', 'W_K_moves', 'W_N_moves', 'W_O_moves',
### 'W_Q_moves', 'W_R_moves', 'B_B_moves', 'B_K_moves', 'B_N_moves', 'B_O_moves',
### 'B_Q_moves', 'B_R_moves', 'B_moves', 'K_moves', 'N_moves', 'O_moves',
### 'Q_moves', 'R_moves'
games <- games_full[items_games]
games <- games[games$complete.movetext == TRUE,]
games$complete.movetext <- NULL
games <- games[games$NMoves >= 10, ]
games$NMoves <- NULL
games$Date <- as.Date(games$Date, format = "%Y.%m.%d")
games$White <- gsub(",([[:alpha:]])", ", \\1", games$White)
games$Black <- gsub(",([[:alpha:]])", ", \\1", games$Black)
games <- games[complete.cases(games),]
games <- games[!grepl("blitz", games$Event, ignore.case = TRUE), ]
games <- games[!grepl("rapid", games$Event, ignore.case = TRUE), ]
games$Event <- NULL
games <- droplevels(games)

### merge information from 'players' and 'games'
choice_chess_opening <- data.frame()
for(i in 1:nrow(games)){
  cat(sprintf("%.0f %% \r", i/nrow(games)*100))
  name_white <- games[i, "White"]
  index_white <- stringr::str_which(players$name, name_white)
  name_black <- games[i, "Black"]
  index_black <- stringr::str_which(players$name, name_black)
  ### skip if one of names is ambiguous
  if(length(index_white) != 1 || length(index_black) != 1){
    next()
  } else {
    N <- nrow(choice_chess_opening) + 1
    choice_chess_opening[N, "fideid_w"] <- players[index_white, "fideid"]
    choice_chess_opening[N, "fideid_b"] <- players[index_black, "fideid"]
    choice_chess_opening[N, "w1"] <- games[i, "W1"]
    choice_chess_opening[N, "b1"] <- games[i, "B1"]
    choice_chess_opening[N, "sex_w"] <- players[index_white, "sex"]
    choice_chess_opening[N, "sex_b"] <- players[index_black, "sex"]
    choice_chess_opening[N, "byear_w"] <- players[index_white, "byear"]
    choice_chess_opening[N, "byear_b"] <- players[index_black, "byear"]
    choice_chess_opening[N, "rating_w"] <- players[index_white, "rating"]
    choice_chess_opening[N, "rating_b"] <- players[index_black, "rating"]
    choice_chess_opening[N, "date"] <- games[i, "Date"]
    choice_chess_opening[N, "result"] <- games[i, "Result"]
    choice_chess_opening[N, "game"] <- games[i, "Movetext"]
  }
}

### save R dataset
use_data(choice_chess_opening, overwrite = TRUE, compress = 'xz')

### create documentation
doc <- paste0("#' Choice of a chess opening
#'
#' @description
#' This dataset includes opening choices in ", nrow(choice_chess_opening), " chess games.
#'
#' @docType data
#'
#' @usage data(choice_chess_opening)
#'
#' @format
#' A data frame with the following columns:
#' \\itemize{
#'   \\item \\code{fideid_w}, FIDE identifier for the White player
#'   \\item \\code{fideid_b}, FIDE identifier for the Black player
#'   \\item \\code{w1}, first move of the White player
#'   \\item \\code{b1}, first move of the Black player
#'   \\item \\code{sex_w}, gender of the White player (0 for male and 1 for female)
#'   \\item \\code{sex_b}, gender of the Black player (0 for male and 1 for female)
#'   \\item \\code{byear_w}, birth year of the White player
#'   \\item \\code{byear_b}, birth year of the Black player
#'   \\item \\code{rating_w}, FIDE rating of the White player
#'   \\item \\code{rating_b}, FIDE rating of the Black player
#'   \\item \\code{date}, date of the chess game
#'   \\item \\code{result}, result of the chess game
#'   \\item \\code{game}, full notation of the chess game
#' }
#'
#' @source
#' The opening moves were scraped from 'The Week in Chess Archive'
#' \\url{https://theweekinchess.com/twic} issues ", paste(twic_issues,collapse=", "), ".
#' The player's metadata (fideid, sex, byear, rating) were added by matching the pairings names
#' with the official FIDE rating list \\url{http://ratings.fide.com} on ", date_today, ".
#' Ambiguous matches were dropped. Chess games with blitz or rapid time control are ignored.
#'
#' @keywords
#' dataset
#'
'choice_chess_opening'")
fileConn <- file("R/data_choice_chess_opening.R")
writeLines(doc, fileConn)
close(fileConn)

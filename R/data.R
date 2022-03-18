#' Choice of berserking
#'
#' @description
#' This dataset includes the binary berserking choice of participants in the yearly
#' bullet arena on the online chess platform \url{https://lichess.org}.
#' The tournament startet at 2022-01-10 17:00:25 and lasted 240 minutes.
#'
#' @details
#' To 'berserk' is a feature on the online chess platform \url{https://lichess.org}.
#' Before the game starts, each player can click a 'berserk' button, after which
#' they lose half of their clock time, but the win is worth one extra tournament
#' point. See \url{https://lichess.org/tournament/help?system=arena}
#' for more information.
#'
#' @docType data
#'
#' @usage data(choice_berserk)
#'
#' @format
#' A data frame containing berserking choices of the White and the Black player
#' in 65811 bullet (1+0) games. It consists of the following columns:
#' \itemize{
#'   \item \code{id_white} and \code{id_black}, unique lichess usernames of the White and the Black players for each game,
#'   \item \code{rating_white} and \code{rating_black}, both player's lichess bullet rating at the start of each game,
#'   \item \code{berserk_white} and \code{berserk_black}, a boolean whether the players berserked,
#'   \item \code{winner}, the winner of the game, either \code{'white'}, \code{'black'} or \code{'draw'},
#'   \item \code{minutes_remaining}, the number of minutes left in the tournament,
#'   \item \code{game_id}, the game's id on lichess (each game can be accessed via \url{https://lichess.org/<game_id>}).
#' }
#'
#' @source
#' The data was obtained via the lichess API \url{https://lichess.org/api}
#' with the tournament id 'RibHfoX6' on 2022-01-25.
#'
#' @keywords
#' dataset
#'
"choice_berserk"

#' Choice of a chess opening
#'
#' @description
#' This dataset includes first move choices of the White player in chess games.
#'
#' @docType data
#'
#' @usage data(choice_chess_opening)
#'
#' @format
#' A data frame containing first move choices of the White player in 549 chess games.
#' It consists of the following columns:
#' \itemize{
#'   \item \code{fideid}, unique identifier for each player provided by FIDE,
#'   \item \code{move}, the chosen first move,
#'   \item \code{country}, the player's federation,
#'   \item \code{sex}, the player's gender,
#'   \item \code{age}, the player's age,
#'   \item \code{rating}, the player's rating,
#'   \item \code{opponent_country}, the opponent's federation,
#'   \item \code{opponent_sex}, the opponent's gender,
#'   \item \code{opponent_country}, the opponent's federation,
#'   \item \code{age_difference}, the player's age minus the opponent's age,
#'   \item \code{info}, a short description about the pairing.
#' }
#'
#' @source
#' The opening moves were scraped from The Week in Chess Archive
#' \url{https://theweekinchess.com/twic} issues 1416, 1417, 1418, 1419, 1420.
#' The player's metadata (country, sex, age, rating) were added by matching the pairings names
#' with the official FIDE rating list \url{http://ratings.fide.com} on 2022-01-25.
#' Ambiguous matches were dropped.
#'
#' @keywords
#' dataset
#'
"choice_chess_opening"

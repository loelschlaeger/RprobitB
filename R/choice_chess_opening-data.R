#' Choice of a chess opening
#'
#' @description
#' This dataset includes opening choices in 37229 chess games.
#'
#' @docType data
#'
#' @usage data(choice_chess_opening)
#'
#' @format
#' A data frame with the following columns:
#' \itemize{
#'   \item \code{fideid_w}, FIDE identifier for the White player
#'   \item \code{fideid_b}, FIDE identifier for the Black player
#'   \item \code{w1}, first move of the White player
#'   \item \code{b1}, first move of the Black player
#'   \item \code{sex_w}, gender of the White player (0 for male and 1 for female)
#'   \item \code{sex_b}, gender of the Black player (0 for male and 1 for female)
#'   \item \code{byear_w}, birth year of the White player
#'   \item \code{byear_b}, birth year of the Black player
#'   \item \code{rating_w}, FIDE rating of the White player
#'   \item \code{rating_b}, FIDE rating of the Black player
#'   \item \code{date}, date of the chess game
#'   \item \code{result}, result of the chess game
#'   \item \code{game}, full notation of the chess game
#' }
#'
#' @source
#' The opening moves were scraped from 'The Week in Chess Archive'
#' \url{https://theweekinchess.com/twic} issues 1417, 1418, 1419, 1420, 1421, 1422, 1423, 1424, 1425, 1426, 1427, 1428.
#' The player's metadata (fideid, sex, byear, rating) were added by matching the pairings names
#' with the official FIDE rating list \url{http://ratings.fide.com} on 2022-03-28.
#' Ambiguous matches were dropped. Chess games with blitz or rapid time control are ignored.
#'
#' @keywords
#' dataset
#'
'choice_chess_opening'

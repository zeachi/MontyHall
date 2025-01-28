#' @title
#'   Create a new Monty Hall Problem game (3 doors).
#'
#' @description
#'   `create_game()` generates a new Monty Hall game with 3 doors:
#'   2 goats and 1 car placed in a random order.
#'
#' @details
#'   This setup replicates the TV show "Let's Make a Deal," 
#'   where there are three doors for a contestant to choose from: 
#'   behind one door is a car, and behind the other two are goats.
#'   The contestant picks a door, the host opens a different door 
#'   to reveal a goat, and the contestant then has the option 
#'   to stay with the original door or switch to the other unopened door.
#'
#' @param ... No arguments are used by this function.
#'
#' @return 
#'   A length-3 character vector, e.g. \code{c("goat","goat","car")}, 
#'   indicating the random positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample(x=c("goat","goat","car"), size=3, replace=FALSE)
  return(a.game)
}


#' @title
#'   Randomly select one door
#'
#' @description
#'   `select_door()` chooses exactly one door among the three 
#'   with equal probability.
#'
#' @details
#'   Since there are 3 doors labeled 1, 2, 3, the function 
#'   simply samples one door index with uniform probability.
#'
#' @param ... No arguments are used by this function.
#'
#' @return 
#'   An integer \code{1}, \code{2}, or \code{3}, representing 
#'   the chosen door.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function()
{
  doors <- c(1, 2, 3)
  a.pick <- sample(doors, size=1)
  return(a.pick)  # number between 1 and 3
}


#' @title
#'   Host opens a goat door
#'
#' @description
#'   `open_goat_door()` simulates the host revealing exactly one goat 
#'   door, following Monty Hall constraints.
#'
#' @details
#'   If the contestant's chosen door has the car, the host randomly picks 
#'   one of the two goat doors to open. If the contestant has a goat, 
#'   the host must open the only other goat door.
#'
#' @param game A length-3 character vector (from \code{\link{create_game}}), 
#'   containing "goat"/"car" in a random order.
#' @param a.pick An integer 1, 2, or 3, representing the door 
#'   the contestant initially chose (from \code{\link{select_door}}).
#'
#' @return 
#'   An integer \code{1}, \code{2}, or \code{3} for the door opened by the host.
#'
#' @examples
#'   current_game <- create_game()
#'   my_pick <- select_door()
#'   open_goat_door(current_game, my_pick)
#'
#' @export
open_goat_door <- function(game, a.pick)
{
  doors <- c(1,2,3)
  
  # If contestant selected car, randomly select one of two goats 
  if (game[a.pick] == "car")
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample(goat.doors, size=1)
  }
  # If contestant selected goat, open the other goat
  if (game[a.pick] == "goat")
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return(opened.door) # number between 1 and 3
}


#' @title
#'   Change Doors (Stay or Switch)
#'
#' @description
#'   `change_door()` gives the contestant's final choice after 
#'   deciding whether to stay with the initial pick or switch to 
#'   the unopened door.
#'
#' @details
#'   If \code{stay=TRUE}, the function returns the original door (\code{a.pick}).
#'   If \code{stay=FALSE}, it returns the other unopened door (neither \code{a.pick} 
#'   nor \code{opened.door}).
#'
#' @param stay A logical indicating whether to stay (\code{TRUE}) 
#'   or switch (\code{FALSE}).
#' @param opened.door An integer 1, 2, or 3 that the host opened 
#'   (revealing a goat).
#' @param a.pick An integer for the door the contestant originally picked.
#'
#' @return 
#'   An integer \code{1}, \code{2}, or \code{3} representing the final door choice.
#'
#' @examples
#'   g <- create_game()
#'   pick <- select_door()
#'   opened <- open_goat_door(g, pick)
#'   change_door(TRUE, opened, pick)
#'   change_door(FALSE, opened, pick)
#'
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick)
{
  doors <- c(1,2,3) 
  
  if (stay) {
    final.pick <- a.pick
  } else {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }
  
  return(final.pick)  # number between 1 and 3
}


#' @title
#'   Determine if the final pick wins
#'
#' @description
#'   `determine_winner()` checks whether the final chosen door 
#'   has a car or a goat.
#'
#' @details
#'   It compares the door index in \code{final.pick} to the 
#'   arrangement in \code{game}. If that door holds a car, 
#'   the result is "WIN". Otherwise it's "LOSE".
#'
#' @param final.pick An integer for the final chosen door (1, 2, or 3).
#' @param game A length-3 character vector with "goat"/"car" 
#'   (from \code{\link{create_game}}).
#'
#' @return 
#'   A character string: \code{"WIN"} if it's a car, 
#'   or \code{"LOSE"} if it's a goat.
#'
#' @examples
#'   g <- create_game()
#'   determine_winner(final.pick = 2, g)
#'
#' @export
determine_winner <- function(final.pick, game)
{
  if (game[ final.pick ] == "car")
  {
    return("WIN")
  } 
  else
  {
    return("LOSE")
  }
}


#' @title
#'   Play one Monty Hall game (3 doors)
#'
#' @description
#'   `play_game()` calls the five helper functions once each:
#'   \code{\link{create_game}}, \code{\link{select_door}}, 
#'   \code{\link{open_goat_door}}, \code{\link{change_door}}, 
#'   \code{\link{determine_winner}}, returning results 
#'   for both "stay" and "switch" strategies.
#'
#' @details
#'   1. Create a random 3-door arrangement (\code{create_game()}).
#'   2. Pick one door (\code{select_door()}).
#'   3. Host opens a goat door (\code{open_goat_door()}).
#'   4. Get final picks for stay and switch (\code{change_door}).
#'   5. Determine outcome of each strategy (\code{determine_winner}).
#'
#' @param ... No arguments are used by the function.
#'
#' @return 
#'   A 2-row data frame with columns \code{strategy} and \code{outcome}, 
#'   one row for "stay" and one for "switch."
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function()
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)
  
  final.pick.stay <- change_door(stay=TRUE, opened.door, first.pick)
  final.pick.switch <- change_door(stay=FALSE, opened.door, first.pick)
  
  outcome.stay <- determine_winner(final.pick.stay, new.game)
  outcome.switch <- determine_winner(final.pick.switch, new.game)
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame(strategy, outcome, stringsAsFactors=FALSE)
  return(game.results)
}


#' @title
#'   Play many Monty Hall games
#'
#' @description
#'   `play_n_games()` repeats \code{\link{play_game}} \code{n} times, 
#'   collecting the outcomes for each strategy, 
#'   and prints row proportions (WIN/LOSE).
#'
#' @details
#'   By default, it loops 100 times, but you can specify 
#'   a larger \code{n} for more precise estimates 
#'   (e.g., 10,000 or 1e5).
#'
#' @param n An integer specifying how many Monty Hall games to simulate.
#'
#' @return 
#'   A data frame with \code{2*n} rows (because each game 
#'   has 2 rows: "stay" and "switch"), plus prints a table 
#'   of row proportions (WIN vs. LOSE) for each strategy.
#'
#' @examples
#' \dontrun{
#'   results_100 <- play_n_games(100)
#'   results_10000 <- play_n_games(10000)
#' }
#'
#' @export
play_n_games <- function(n = 100)
{
  library(dplyr)
  results.list <- list()   # collector
  loop.count <- 1
  
  for (i in 1:n) {
    game.outcome <- play_game()
    results.list[[loop.count]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows(results.list)
  
  # Print row proportions of (WIN, LOSE) for each strategy
  table(results.df) %>%
    prop.table(margin=1) %>%
    round(2) %>%
    print()
  
  return(results.df)
}


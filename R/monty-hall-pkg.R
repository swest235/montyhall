#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#' Contestant selects the door they believe the car is behind.
#' @description
#' In line with the game show flow, the contestant selects the doors they 
#' believe the car is behind. The select_door function allows the contestant to randomly select one of the
#' available doors
#' @details
#' This function serves as the contestants guess as to which door holds the car. It randomly selects a door from 1-3. 
#' It works by calling the 'sample()' on a vector of doors and returns the value of the door number the contestant chose.
#' @param ... no arguments are used by this function
#' @return 
#' This function returns a single numeric value representing the door randomly selected by the contestant and
#'  ranging between 1 and 3.
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens a door, revealing a goat.
#' @description
#' The host, after the contestants initial pick, reveals one of the doors that has a goat behind it. This door
#' may not be the door the contestant selected, and of course, must not be the door with the car behind it. The
#' door must reveal a goat.
#' @details
#' After the contestant makes a selection, the host must open one of the remaining doors that has a goat behind it.
#' The host must not pick the door the contestant picked, and must also only pick a door containing a goat. By reducing
#' the hosts qualified options, the host is able to randomly select between the 2 remaining goat doors - assuming the
#' contestant selected the car to begin with, or the single goat door left if the contestant picked one of the two goat
#' doors to begin with.
#' @param 
#' This function uses arguments 'game' and 'a.pick' which include the game setup created from create_game that randomly
#' establishes which doors contain which items (2 goats, 1 car) and the contestants initiall selection from select_door
#' that is used to isolate what doors are available to the host to pick from
#' @return 
#' This function returns a single numeric value representing which door the host chose to open, based on the criteria described
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' open_goat_door(game, a.pick)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' The contestant chooses to retain his initial selection or switch to the last available door
#' @description
#' After the host reveals a goat door, the contestant is able to choose between switching his selection to the only
#' other door remaining, or to retain his initial selection. 
#' @details
#' The contestant must decide at this point to stay or switch doors. There are only two doors left at this point: one
#' with a goat, and one with a car. The contestant chooses to switch to the last remaining door or keep his initial 
#' selection. The available doors are determined by previous functions and used as arguments. The initially chosen door
#' by the contestant, the opened door by the host revealing a goat, provide the final 2 doors available to pick from. 
#' @param 
#' This function uses the arguments stay=, opened.door, and a.pick which represent the choice to STAY = T (retain initial 
#' door selection), STAY = F (switch doors), opened.door (which door the host opened), a.pick (the door the contestant 
#' initially chose).
#' @return 
#' The function returns the door the contestant selected - a single numeric representing which door was chosen. 
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' opened.door <- open_goat_door(game.a.pick)
#' change_door(stay = TRUE, opened.door, a.pick)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if contestant picked the door with the car (win) or the goat (lose)
#' @description
#' The function checks if the selected door contained the car, any other situation is a loss.
#' @details
#' determine_winner() checks the contestants final selection against the create_game() arrangement to determine 
#' if the selected door contains a goat or a car. If the door with the car was chosen, the contestant wins, if not, they lose. 
#' @param 
#' The function uses final.pick and game as arguments where final.pick is the end result of the stay or switch function
#' and game is the originally produced set and order of doors with corresponding prizes (goat or car)
#' @return 
#' Returns a string of characters stating "WIN" if the contestant chose the door with the car or or "LOSE" if any
#' other door was chosen
#' @examples
#' game <- create_game()
#' a.pick <- select_door()
#' opened.door <- open_goat_door(game, a.pick)
#' final.pick <- change_door(stay = TRUE, opened.door, a.pick)
#' determine_winner(final.pick, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play a single game of "Let's Make a Deal" or more commonly called, the Monty Hall game. 
#' @description
#' The function runs through an entire game of "Let's Make a Deal" where each round is random.
#' @details
#` The function simulates an entire play-by-play of a single game of "Let's Make a Deal" where the doors are randomly
#' assigned car or goats, the contestant selects a door, the host reveals a goat door, the contestant then chooses to
#' keep the original door or switch. Finally, win or lose is presented depdening on the outcome of the door selection.
#' @param ... no arguments are used by this function
#' @return 
#' This returns a data frame with 2 rows and 2 columns. THe dataframe includes a column representing 
#' the strategy (stay/switch) and another for the outcomes (win/lose)
#' 
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Play more than one rounds of the Monty Hall Game
#' @description
#' The function plays multiple rounds of the game and stores the results in a data frame to analyze
#' @details
#' This function enables users to simulate numerous rounds of the game in order to analyze the most effective strategies. 
#'  By choosing 'n' games, you can review both strategies and compare the results of each in a prop.table..
#' @param 
#' The function uses n as a parameter which represents the number of games to be simulated.
#' @return 
#' Returns a data frame that holds the win/lose for each game as well as the strategy stay/switch.
#' @examples
#' play_n_games(n = 1,000)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}

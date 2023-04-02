library(tidyverse)
library(ggforce)

# sets up the matrix that records piece locations 
initialize_matrix <- function() {
  
  matrix(0, nrow = 6, ncol = 7, 
         dimnames = list(str_c("row_", 1:6),
                         c("a", "b", "c", "d", "e", "f", "g")))
}

update_matrix <- function(loc, locs, color) {
  
  value <- if_else(color == "red", -1, 1)
  locs[loc$row_num, loc$column] <- value
  return(locs)
  
}

check_win <- function(locs, color) {
  win <- FALSE
  win_sum <- if_else(color == "red", -4, 4)
  
  if(
     #vertical four
     sum(locs[1:4,1]) == win_sum |
     sum(locs[1:4,2]) == win_sum |
     sum(locs[1:4,3]) == win_sum |
     sum(locs[1:4,4]) == win_sum |
     sum(locs[1:4,5]) == win_sum |
     sum(locs[1:4,6]) == win_sum |
     sum(locs[1:4,7]) == win_sum |
     
     sum(locs[2:5,1]) == win_sum |
     sum(locs[2:5,2]) == win_sum |
     sum(locs[2:5,3]) == win_sum |
     sum(locs[2:5,4]) == win_sum |
     sum(locs[2:5,5]) == win_sum |
     sum(locs[2:5,6]) == win_sum |
     sum(locs[2:5,7]) == win_sum |
     
     sum(locs[3:6,1]) == win_sum |
     sum(locs[3:6,2]) == win_sum |
     sum(locs[3:6,3]) == win_sum |
     sum(locs[3:6,4]) == win_sum |
     sum(locs[3:6,5]) == win_sum |
     sum(locs[3:6,6]) == win_sum |
     sum(locs[3:6,7]) == win_sum |
     
     #horizontal 4
     sum(locs[1,1:4]) == win_sum |
     sum(locs[2,1:4]) == win_sum |
     sum(locs[3,1:4]) == win_sum |
     sum(locs[4,1:4]) == win_sum |
     sum(locs[5,1:4]) == win_sum |
     sum(locs[6,1:4]) == win_sum |
      
     sum(locs[1,2:5]) == win_sum |
     sum(locs[2,2:5]) == win_sum |
     sum(locs[3,2:5]) == win_sum |
     sum(locs[4,2:5]) == win_sum |
     sum(locs[5,2:5]) == win_sum |
     sum(locs[6,2:5]) == win_sum | 
     
     sum(locs[1,3:6]) == win_sum |
     sum(locs[2,3:6]) == win_sum |
     sum(locs[3,3:6]) == win_sum |
     sum(locs[4,3:6]) == win_sum |
     sum(locs[5,3:6]) == win_sum |
     sum(locs[6,3:6]) == win_sum |
     
     sum(locs[1,4:7]) == win_sum |
     sum(locs[2,4:7]) == win_sum |
     sum(locs[3,4:7]) == win_sum |
     sum(locs[4,4:7]) == win_sum |
     sum(locs[5,4:7]) == win_sum |
     sum(locs[6,4:7]) == win_sum |
    
    # diagonal 4 top left - bottom right
    sum(locs[1,1], locs[2,2], locs[3,3], locs[4,4]) == win_sum |
    sum(locs[2,1], locs[3,2], locs[4,3], locs[5,4]) == win_sum |
    sum(locs[3,1], locs[4,2], locs[5,3], locs[6,4]) == win_sum |
    
    sum(locs[1,2], locs[2,3], locs[3,4], locs[4,5]) == win_sum |
    sum(locs[2,2], locs[3,3], locs[4,4], locs[5,5]) == win_sum |
    sum(locs[3,2], locs[4,3], locs[5,4], locs[6,5]) == win_sum |
    
    sum(locs[1,3], locs[2,4], locs[3,5], locs[4,6]) == win_sum |
    sum(locs[2,3], locs[3,4], locs[4,5], locs[5,6]) == win_sum |
    sum(locs[3,3], locs[4,4], locs[5,5], locs[6,6]) == win_sum |
    
    sum(locs[1,4], locs[2,5], locs[3,6], locs[4,7]) == win_sum |
    sum(locs[2,4], locs[3,5], locs[4,6], locs[5,7]) == win_sum |
    sum(locs[3,4], locs[4,5], locs[5,6], locs[6,7]) == win_sum |
    
    #diagonal 4 top right - bottom left
    sum(locs[1,7], locs[2,6], locs[3,5], locs[4,4]) == win_sum |
    sum(locs[2,7], locs[3,6], locs[4,5], locs[5,4]) == win_sum |
    sum(locs[3,7], locs[4,6], locs[5,5], locs[6,4]) == win_sum |
    
    sum(locs[1,6], locs[2,5], locs[3,4], locs[4,3]) == win_sum |
    sum(locs[2,6], locs[3,5], locs[4,4], locs[5,3]) == win_sum |
    sum(locs[3,6], locs[4,5], locs[5,4], locs[6,3]) == win_sum |
    
    sum(locs[1,5], locs[2,4], locs[3,3], locs[4,2]) == win_sum |
    sum(locs[2,5], locs[3,4], locs[4,3], locs[5,2]) == win_sum |
    sum(locs[3,5], locs[4,4], locs[5,3], locs[6,2]) == win_sum |
    
    sum(locs[1,4], locs[2,3], locs[3,2], locs[4,1]) == win_sum |
    sum(locs[2,4], locs[3,3], locs[4,2], locs[5,1]) == win_sum |
    sum(locs[3,4], locs[4,3], locs[5,2], locs[6,1]) == win_sum) {
    
    win <- TRUE
  }
  return(win)
  
}


# creates an object for the plot of a blank board
initialize_board <- function() {
  
  the_board <- tibble(x = c(0,7), y = c(0,6)) %>% 
    ggplot() +
    geom_blank(aes(x, y)) +
    coord_equal() + 
    scale_x_continuous(name = "Column", 
                       breaks = seq(.5, 6.5, 1), 
                       labels = c("A", "B", "C", "D", "E", "F", "G"),
                       limits=c(0, 7), expand = c(0, 0)) +
    scale_y_continuous(name = "", 
                       breaks = seq(0, 6),
                       labels = c(" "," "," "," "," "," ", " "),
                       limits=c(0, 6), expand = c(0, 0)) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) 
  
  return(the_board)
  
}

# plots a red or yellow circle onto the board
update_board <- function(the_board, loc, piece_color) {
  
  df <- tibble(y0 = (7 - loc$row_num) - .5,
               x0 = case_when(loc$column == "a" ~ 0.5,
                              loc$column == "b" ~ 1.5,              
                              loc$column == "c" ~ 2.5,
                              loc$column == "d" ~ 3.5,
                              loc$column == "e" ~ 4.5,
                              loc$column == "f" ~ 5.5,
                              loc$column == "g" ~ 6.5),
              r = .4)
  the_board +
    geom_circle(aes(x0 = x0, y0 = y0, r = r), data = df, fill = piece_color)
  
}

input_column <- function(color){
  valid_columns <- c("a", "b", "c", "d", "e", "f", "g", "intellectual")
  
  need_valid_column <- TRUE
  while(need_valid_column) {
    column <- readline(prompt = str_c("Pick a column (A - G) for the ",
                                      color, " piece: "))
    column <- tolower(column)
    if (column %in% valid_columns) {
      need_valid_column <- FALSE
    } else {
      message ("Column ", column, " is not a valid column.  Try again!")
    }
  }
  message("You placed a ", color, " piece in column ", column)
  return(column)
}


# asks player to input column and updates piece matrix
get_coords <- function(column, locs) {
  
  where_zero <- which(locs[, column] == 0)
  row_num <- where_zero[length(where_zero)]
  
  loc <- list(column = column, row_num = row_num)
  return(loc)
}



# ------------------------------------------------------------------------
# Start Game


locs <-  initialize_matrix()
the_board <- initialize_board()
the_board


game_over <- FALSE
n_move <- 0
color_move <- "red"
  
while(!game_over) {
  
  n_move <- n_move + 1
  the_column <- input_column(color_move)
  
  if(the_column == "intellectual") {
    message("you are intellectual. nice work")
    n_move <- n_move - 1 
    color_move <- if_else(color_move == "red", "yellow", "red")
  } else {
    

    loc <- get_coords(the_column, locs)
    the_board <- update_board(the_board, loc, color_move)
    print(the_board)
    locs <- update_matrix(loc, locs, color_move)
    game_over <- check_win(locs, color_move)
    
    if(game_over) {
      message(color_move, " wins. good game!!!")  
    } else {
      if (n_move == 42) {
        game_over <- TRUE
        message ("Tie Game.  Too Bad.  You both suck!!")
      } else {
        color_move <- if_else(color_move == "red", "yellow", "red")
      }
    }
  }
}



###########################################################################################
# Day 9  : How many positions does the tail of the rope visit at least once?
###########################################################################################

#Track the position of the rope Head and Tail via a series of motion.
#Keep track of all coordinates where the tail has been (in a list), including the starting position.
#Find the number of unique coordinates.
library(tidyverse)

day9 <- function(){
  
  #input <- readLines("Day 9/test input.txt")  
  input <- readLines("Day 9/input.txt") 
  
  #starting position
  Head <- c(0,0)
  Tail <- c(0,0)
  
  #Loop thought the list of movements (R 4). Directions are Up, Down,Left, Right.
  # R : xdiff=1,  ydiff=0, 
  # L : xdiff=-1, ydiff=0.
  # U : xdiff=0,  ydiff=-1.
  # D : xdiff=0,  ydiff=1.
  
  #Move H one step at a time. Check if T is close to H (no diff > 1) : if so do not move T. 
  #If not, move T closer to H: one step in the direction where distance > 1. 
  #In addition : Set the tail other coortinate ( x or y) to the sme as H (i.e. move diaginally if required.)
  tail_positions <- list()
  tail_positions <- append(tail_positions, list(Tail))
  
  for(line in 1:length(input)){
    
    x <- unlist(str_split(input[line], " "))
    dir <- x[1]
    distance <- as.integer(x[2])
    
    xydir <- case_when(
      dir == 'R' ~ c(1,  0),
      dir == 'L' ~ c(-1, 0),
      dir == 'U' ~ c(0, -1),  
      dir == 'D' ~ c(0  ,1)  
    )
    
    
    for(i in 1:distance){
      
      #Move the head
      Head <- Head + xydir 
      
      #Check if distance in any axis > 1
      axis  <- which(abs(Head - Tail) > 1)
      if(! length(axis) == 0){
        
          #Move the Tail one step towards the Head in the axis
          if(Head[axis] > Tail[axis]){Tail[axis] <- Tail[axis] + 1} else {Tail[axis] <- Tail[axis] - 1}
          
          #Move diagonally in the other axis if required
          if(axis == 1) otheraxis <- 2 else otheraxis <- 1
          Tail[otheraxis] <- Head[otheraxis]
          
          #Record this position
          tail_positions <- append(tail_positions, list(Tail))
      }
    }
  }
  
  #List all unique tail positions
  length(unique(tail_positions))
}



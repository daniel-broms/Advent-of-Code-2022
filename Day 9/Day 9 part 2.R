
###########################################################################################
# Day 9 part 2 : How many positions does the tail of the rope visit at least once? The rope now has 10 knots.
###########################################################################################

#Track the position of the rope Head and Tail via a series of motion.
#Keep track of all coordinates where the tail has been (in a list), including the starting position.
#Find the number of unique coordinates.
library(tidyverse)

day9p2 <- function(){
  
  #input <- readLines("Day 9/test input 2.txt")  
  input <- readLines("Day 9/input.txt") 
  
  #starting position
  Knots <- rep(list(c(0,0)),10)  #Head is Knots[1], Tail is Knots[10]
  
  #Loop thought the list of movements (R 4). Directions are Up, Down,Left, Right.
  # R : xdiff=1,  ydiff=0, 
  # L : xdiff=-1, ydiff=0.
  # U : xdiff=0,  ydiff=-1.
  # D : xdiff=0,  ydiff=1.
  
  #Move H one step at a time. Check if T is close to H (no diff > 1) : if so do not move T. 
  #If not, move T closer to H: one step in the direction where distance > 1. 
  #In addition : Set the tail other coortinate ( x or y) to the sme as H (i.e. move diaginally if required.)
  tail_positions <- list()
  tail_positions <- append(tail_positions, list(Knots[[10]]))
  
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
      Knots[[1]] <- Knots[[1]] + xydir 
      
      #Check ALL nive tails, move as required.
      for(Knot in 1:9){
        Head <- Knots[[Knot]]
        Tail <- Knots[[Knot+1]]
        
        #Check if distance in any axis > 1       #TODO : handle when both X and Y differ > 1!
        axis  <- which(abs(Head - Tail) > 1)
        if(! length(axis) == 0){
          
            #Move the Tail one step towards the Head in the axis
            if(Head[axis] > Tail[axis]){Knots[[Knot+1]][axis] <- Tail[axis] + 1} else {Knots[[Knot+1]][axis] <- Tail[axis] - 1}
            
            #Move diagonally in the other axis if required
            if(axis == 1) otheraxis <- 2 else otheraxis <- 1
           
             #Knots[[Knot+1]][otheraxis] <- Head[otheraxis]
            if(Head[otheraxis] > Tail[otheraxis]){
              Knots[[Knot+1]][otheraxis] <- Tail[otheraxis] + 1
              } 
            else if(Head[otheraxis] < Tail[otheraxis]){
              Knots[[Knot+1]][otheraxis] <- Tail[otheraxis] - 1
              }

        }
      }
      #printrope(Knots)
      
      #Record the position if the last knot
      tail_positions <- append(tail_positions, list(Tail))

    }
  }
  
  #List all unique tail positions. Add one to get the right answer for part 2!
  length(unique(tail_positions))
}

#print rope matrix : Add 1 to x, add 5 to y.
printrope <- function(Knots){
  m <- matrix(rep('.', 30), nrow=5, ncol=6)
  for(i in 10:1){
    m[Knots[[i]][2] + 5, Knots[[i]][1] + 1 ] <- case_when(i==10 ~ "T", i==1 ~"H", TRUE ~ as.character(i-1) )
  }
  print(m, quote=F)
}

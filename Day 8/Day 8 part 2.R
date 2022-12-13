###########################################################################################
# Day 8  : What is the highest scenic score possible for any tree?
###########################################################################################
library(tidyverse)


input <- readLines("Day 8/test input.txt")  
columns <- 5  #Test input : real input has 99 columns and 99 rows.


input <- readLines("Day 8/input.txt") 
columns <- 99

#Convert input to a matrix
m <- input |>
  lapply( function(x) as.integer(str_split_fixed( x ,"", columns)) ) |> 
  unlist()|>
  matrix(ncol=columns, byrow=T)


#function to calculate the viewing distance for a tree.
ivtd2 <- function(m, x, y, xdiff, ydiff){
  if(x==1 | y==1 |x==columns | y==columns) return(0)   #return zero for all border trees
  
  cx <- x
  cy <- y
  distance <- 0
  
  while(cx > 1 & cx < columns & cy > 1 & cy < columns){
    cx <- cx + xdiff
    cy <- cy + ydiff
    distance <- distance + 1
    if(m[y,x] <= m[cy, cx]) return(distance)
  }
  return(distance)
}

#Function to calculate a tree scenic score
iv2 <- function (m, x, y){
  return(ivtd2(m,x,y,0,1) * ivtd2(m,x,y,0,-1) * ivtd2(m,x,y,1,0) * ivtd2(m,x,y,-1,0) )
}

#find the best scenic score
cvt <- function(m){
  best_score <- 0
  for(x in 1:columns){
    for(y in 1:columns){
      score <- iv2(m,x,y)
      if(score > best_score) best_score <- score
    }
  }
  return(best_score)
}

#get the result : 8 for test input, 486540 for real input
cvt(m)


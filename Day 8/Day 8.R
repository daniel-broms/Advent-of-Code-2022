###########################################################################################
# Day 8  : Count visible trees
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

#function to check if a given tree at x,y is visible in a specific direction (xdiff, ydiff)
ivtd <- function(m, x, y, xdiff, ydiff){
  if(x==1 | y==1 |x==columns | y==columns) return(T)   #return true for all border trees
  
  cx <- x
  cy <- y
  
  while(cx > 1 & cx < columns & cy > 1 & cy < columns){
    cx <- cx + xdiff
    cy <- cy + ydiff
    if(m[y,x] <= m[cy, cx]) return(F)
  }
  return(T)
}

#Function to check if a tree is visible or not.
iv <- function (m, x, y){
  if(ivtd(m,x,y,0,1) | ivtd(m,x,y,0,-1) | ivtd(m,x,y,1,0) | ivtd(m,x,y,-1,0)) return(T)
  else return(F)

}

#Count the visible trees in m
cvt <- function(m){
  vt <- 0
  for(x in 1:columns){
    for(y in 1:columns)
      if(iv(m,x,y)) vt <- vt + 1
  }
  return(vt)
}

#get the result : 21 for test input, 1684 for real input
cvt(m)





      

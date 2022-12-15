###########################################################################################
# Day 14 : Using your scan, simulate the falling sand. How many units of sand come to rest before sand starts flowing into the abyss below?
###########################################################################################
library(tidyverse)
library(collections)
library(rlang)


input <- readLines("Day 14/test input.txt") 
#input <- readLines("Day 14/input.txt") 
maxy <- 200 #Bottom of the cave

#Count nr of sand that comes to rest.
preparematrix(input, maxy)
movesand()        #719

#show matrix 
m2 <- m[1:10, 493:503 ]
view(m2)

#Prepare a global matrix m with the cave
preparematrix <- function(input, maxy){

  #Make the matrix 1000 wide.
  m <<-matrix(rep('.', 1000 * (maxy+1)), nrow=(maxy+1))
  
  #parse input to a list of list of coordinates
  s <- readLines("Day 14/test input.txt") |>
    str_split(' -> ')                     |>
    lapply(function(x) str_split(x, ',')) |>
    lapply(function(x) lapply (x, as.integer))

  #Render rocks in matrix
  for(l in s){
    for(i in 2:(length(l))){
      p1 <- l[[i-1]]
      p2 <- l[[i]]
      
      #vertical line
      if(p1[1] == p2[1]){
        x <- p1[1]
        for(y in p1[2] : p2[2]){
          m[y,x] <<- '#'
        }
      } else { #horizontal line
        y <- p1[2]
        for(x in p1[1] : p2[1]){
          m[y,x] <<- '#'
        }
      }
    }
  }
}

#Simulate falling sand from (500,1). Drop new sand from 500,0 until sand drops from the abyss (sand2[2] > maxy)
movesand <- function(){
  
  restingsand <- 0
  
  repeat{
    sand <- c(500,0)
    
    #Move sand until it stops
    repeat{
      sand2 <- movesand1(sand)
      if(sand2[2]>maxy) return(restingsand)  #Exit function when sand starts falling out
      else if(all(sand2==sand)){             #Next sand when this sand stops
        m[sand[2], sand[1]] <<- 'o'
        restingsand <- restingsand + 1
        break
      }
      sand <- sand2
    }
  }
}

#Move sand one step in m.
movesand1 <- function(sand){
  
  #move down if possible, else down left, else down right.
  if(m[sand[2] + 1, sand[1]] == '.'){
    return(c(sand[1], sand[2] + 1))
  } else if(m[sand[2] + 1, sand[1]-1] == '.'){
    return(c(sand[1] -1, sand[2] + 1))
  } else if(m[sand[2] + 1, sand[1] + 1] == '.'){
    return(c(sand[1] + 1, sand[2] + 1))
  } else return(sand)
}


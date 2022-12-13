###########################################################################################
# Day 5
###########################################################################################
#After the rearrangement procedure completes, what crate ends up on top of each stack?
library(tidyverse)
library(collections)


run <- function(){
  #input <- readLines("Day 5/input.txt") 
  
  input1 <- readLines("Day 5/input 1.txt")  
  input2 <- readLines("Day 5/input 2.txt")  
  
  #input1 <- readLines("Day 5/test input 1.txt")  
  #input2 <- readLines("Day 5/test input 2.txt")  
  
  nr_stacks <- 9
  stack_height <- 8
  
  #Input1 contains three/9 stacks. import as three stacks? Obs rowwise order.
  #t1 <- read_fwf("Day 5/test input 1.txt",col_positions = fwf_widths(rep(4,nr_stacks)) ) 
  t1 <- read_fwf("Day 5/input 1.txt",col_positions = fwf_widths(rep(4,nr_stacks)) ) 
  
  # syntax : separate(data frame, col=target column in tibble, into=names of new columns        , sep= separator expression : Can be a regular expression!)
  t2 <- tibble(input2)
  t2 <-  separate(t2,          col=input2, into=c('d1', 'move', 'd2', 'from', 'd3', 'to' )     , sep= ' ' )    
  t2 <- t2 %>% mutate(across(c(2,4,6), as.integer))
  

  
  #create an array for each stack, put all in a parent array.
  #l <- list(list(), list(), list())
  l <- list(list(), list(), list(), list(), list(), list(), list(), list(), list())   #9 stacks
  for(c in 1:nr_stacks){
    for (r in 1:stack_height){
      crate <- t1[[c]][[r]]
      if(!is.na(crate)) {  
        l[[c]] <- append(l[[c]], crate)
      }
      
    }
  }
  
  #reverse all lists so that the top crate is at the end if each list.
  for(i in 1:nr_stacks){l[[i]] <- rev(l[[i]])}
  
  #Move around creates according to instructions.
  for(i in 1:nrow(t2)){
    fromstack <- t2[[4]][[i]]
    tostack <- t2[[6]][[i]]
    for(j in 1:t2[[2]][[i]]){  #nr of crates to move
      crate <- l[[fromstack]][[length(l[[fromstack]])]]
      l[[fromstack]][[length(l[[fromstack]])]] <- NULL
      l[[tostack]] <- append(l[[tostack]], crate)
    }
  }
  
  #List the top of each stack : TDCHVHJTG
  for(i in 1:nr_stacks){
    print(l[[i]][[length(l[[i]])]])
  }
}



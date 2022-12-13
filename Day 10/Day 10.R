###########################################################################################
# Day 10 : Track x via CPU cycles
###########################################################################################
library(tidyverse)


day10 <- function(){
  
  input <- readLines("Day 10/input.txt") 
  #input <- readLines("Day 10/test input.txt")  
  
  #Starting values
  x <- 1
  cycle <- 0
  pointer <- 1
  tot_strength <- 0
  

  #Loop through the program instructions
  for(pointer in 1:length(input)){
   
    #Get the next instruction
    instr <- input[pointer]
    
    #execute instructions and spend one cycle on noop, two cycles on addx.
    if(instr == "noop"){
      cycle <- cycle + 1
      tot_strength <- tot_strength + regcycle(cycle, x)
    }
    else {
        value <- as.integer(str_sub(instr, 6,-1))
        cycle <- cycle + 1
        tot_strength <- tot_strength + regcycle(cycle, x)
        
        cycle <- cycle + 1
        tot_strength <- tot_strength + regcycle(cycle, x)
        
        x <- x + value  #Update the value AFTER two cycles
    }
  }
  
  print(tot_strength)
  
} #end of day10()


regcycle <- function(cycle, x){
  
  
  if(cycle %in% c(20, 60, 100, 140, 180, 220)){
    print(c(cycle, x))
    return(cycle * x)
  }
  else return(0)
    
}
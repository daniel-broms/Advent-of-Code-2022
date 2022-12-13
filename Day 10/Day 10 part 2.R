###########################################################################################
# Day 10 : Track x via CPU cycles. Render to a CRT screen.
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
  crt <- matrix(rep(' ', 240), nrow=6)
  

  #Loop through the program instructions
  for(pointer in 1:length(input)){
   
    #Get the next instruction
    instr <- input[pointer]
    
    #execute instructions and spend one cycle on noop, two cycles on addx.
    #the CRT draws a single pixel DURING each cycle.
    if(instr == "noop"){
      cycle <- cycle + 1
      crt <-  regcycle(cycle, x, crt)
    }
    else {
        value <- as.integer(str_sub(instr, 6,-1))
        cycle <- cycle + 1
        crt <-  regcycle(cycle, x, crt)
        
        cycle <- cycle + 1
        crt <-  regcycle(cycle, x, crt)
        
        x <- x + value  #Update the value AFTER two cycles
    }
  }
  
  #Render the final CRT : It shows eight letters.
  print(crt, quote=F)

} #end of day10()


#Render CRT pixels. Cycle gives us the row and column to render. x gives the middle position of the "sprite". If CRT column is between (x-1) and (x+1) then render a "#", else render a "."
#note that R matrix is 1-based to add 1 to the pixel being drawn (pixel 0 => crt column 1)
regcycle <- function(cycle, x, crt){
  
  row <- floor((cycle-1)/40) + 1
  col <- cycle - (40 * (row - 1)) -1  #col is zero-based
  
  if(between(col, x-1, x+1)){
    crt[row, col+1] <- '#'
  } else {
    crt[row, col+1] <- '.'
  }

  #print(crt, quote=F)
  return(crt)
 
}

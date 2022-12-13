###########################################################################################
# Day 2
###########################################################################################
library(tidyverse)

#Import a fixed-width table using read_fwf():
#t <- read_fwf("Day 2/test input.txt",fwf_cols(P1 = c(1, 1), P2 = c(3, 3)))          
input <- readLines("Day 2/input.txt")  
#input <- readLines("Day 2/test input.txt")  

view(input)

#Calculate scores:
#A=Rock=1      X
#B=Paper=2     Y
#C=Scissors=3  Z

#Draw : "A X", "B Y", "C Z"    3 points
#Win  : "A Y", "B X", "C Y"    6 points
#Loss : (all others)           0 points

#Part 1 
score <- 0
for(i in 1:length(input)){
  #Score for the drawn figure
  score <- score + switch(substr(input[i],3,3), "X"=1, "Y"=2, "Z"=3)
  
  if(input[i] %in% c("A Y", "B Z", "C X" )){score <- score + 6}  #win
  if(input[i] %in% c("A X", "B Y", "C Z" )){score <- score + 3}  #draw
  print(score)

}
score #12740



#Part 2 : X=Lose, Y=Draw, Z=Win.
score <- 0
for(i in 1:length(input)){
  #Score for win or draw
  score <- score + switch(substr(input[i],3,3), "X"=0, "Y"=3, "Z"=6)
  
  #Score for my drawn figure
  score <- score + switch(input[i], "A Y"=1, "A Z"=2, "A X"=3,   "B Y"=2, "B Z"=3, "B X"=1,  "C Y"=3, "C Z"=1, "C X" = 2)
  print(score)
  
}
score #11980



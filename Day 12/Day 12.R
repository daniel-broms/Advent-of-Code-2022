###########################################################################################
# Day 12 : What is the fewest steps required to move from your current position to the location that should get the best signal?
###########################################################################################
library(tidyverse)
library(collections)

#input <- readLines("Day 12/input.txt") 
input <- readLines("Day 12/test input.txt")  
input <- str_split(input, "")
input <- unlist(input)       #test : 40 characters
m <- matrix(input, nrow=5)   #Test : 5 rows, each is 8 wide

m[40]


#Build a node/edge graph with the input for possible paths. The cost of each step is always one.
#dist is array with nodes and distance (nr of steps from the source to this node.) Initialize to infinty.
#pq is a priority queue with seen but not processed nodes.
#sptset is a list of nodes with final distances from 
#Run Djikistra to find the shortest path.


#Keep track of visited states and their distance, we do not want to revisit already visited states.
#The keys are the square identifiers, the values are the total cost from starting state.
visitedstates  <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)
seenstates     <- new.env(hash = TRUE, parent = emptyenv(), size = 100L)
seenstates_pq  <- priority_queue()  #states to process

#initialize with square 1.
seenstates[[1]] <- 0
seenstates_pq$push(1,0)             #initialize with first square, distance = zero.

#Dijkstra's algorithm:
#process seen squares until we see no more squares. Process
iteration <- 0
while(seenstates_pq$size() > 0){
  
  #get next square to process
  currentstate       <- seenstates_pq$pop()
  currentstatecost   <- seenstates[[currentstate]]
  iteration          <- iteration +1
  
  
  
}


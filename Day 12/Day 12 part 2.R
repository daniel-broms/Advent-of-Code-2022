###########################################################################################
# Day 12 : What is the fewest steps required to move from your current position to the location that should get the best signal?
###########################################################################################
library(tidyverse)
library(collections)
library(rlang)

input <- readLines("Day 12/input.txt") 
matrix_rows <-41

input <- readLines("Day 12/test input.txt") 
matrix_rows <-5

input <- str_split(input, "")
input <- unlist(input)       #test : 40 characters
m <- matrix(input, nrow=matrix_rows, byrow=T)   #Test : 5 rows, each is 8 wide. dim(m) = (5, 8) Realinput: 41 rows, 179 columns.


#Build a node/edge graph with the input for possible paths. The cost of each step is always one.
#dist is array with nodes and distance (nr of steps from the source to this node.) Initialize to infinty.
#pq is a priority queue with seen but not processed nodes.
#sptset is a list of nodes with final distances from 
#Run Djikistra to find the shortest path.

#Keep track of visited states and their distance, we do not want to revisit already visited states.
#The keys are the square identifiers, the values are the total cost from starting state.

nodes    <- new.env(hash = TRUE, parent = emptyenv(), size = length(m))   #Nodes. key= matrix ID (1-40), data = distance (to start, Inf at start), fromnode = Null
visited  <- new.env(hash = TRUE, parent = emptyenv(), size = length(m))   #Nodes which have been visited. Do not re-visit these.
pq       <- priority_queue()  #nodes to process

#initialize with square "S". Find min steps to sqare "E".
endnode   <-match('E', m)
startnode <-match('S', m)

#Add all nodes and the inithial distance : 0 for start node, Inf for the rest.
#nodes[['1']] <- c(dDistance=0, fromnode=-1)   #Add the first node with distance 0.
for(i in 1:length(m)){
  nodes[[as.character(i)]] <- c(distance=Inf, fromnode=-1)
}
nodes[[as.character(startnode)]] <- c(distance=0, fromnode=-1)  

pq$push(as.character(startnode), -Inf)             #initialize with first square, distance = zero.

#Dijkstra's algorithm:
#process seen squares until we see no more squares. 

 
day12()   #484 

day12 <- function(){
   
  while(pq$size() > 0){
    
    #get next square to process
    currentnode          <- as.integer(pq$pop())

    if(!exists(as.character(currentnode), envir=visited)){               #Skip this node if already visited - there may be duplicates
      currentnodedist      <- nodes[[as.character(currentnode)]][1]      #Get the total distance of this node to start node
      visited[[as.character(currentnode)]] <- currentnode                #Flag this node as visited
      #print(c(currentnode, currentnodedist))
    
      #Look around at surrounding squares. If we see a an unvisited square within allowed height diff, calculate the distance to this, update Nodes add this square to pq if the calcualted distance > current node distance.
      mcol <- floor((currentnode-1)/matrix_rows)+1
      mrow <- currentnode - (mcol-1)*matrix_rows
      dummy <- checksurrouningsquares(currentnode=currentnode, mrow=mrow, mcol=mcol, m=m)
    }
  }

  #print(nodes[[as.character(endnode)]])
} #end of Day12

#Return a list of valid, unvisited neighbors to a given square
checksurrouningsquares <- function(currentnode, mrow, mcol, m){
  if(mrow >1 )         {checksquare(currentnode,mrow, mcol, mrow-1, mcol, m)}        #up
  if(mrow < dim(m)[1]) {checksquare(currentnode,mrow, mcol, mrow+1, mcol, m)}        #down
  if(mcol >1 )         {checksquare(currentnode,mrow, mcol, mrow, mcol-1, m)}        #left
  if(mcol < dim(m)[2]) {checksquare(currentnode,mrow, mcol, mrow, mcol+1, m)}        #right
}

#Process one neighbor square.
checksquare <- function(currentnode, mrow, mcol,y,x,m){
  candidate_height <- convert_symbol(m[y, x])
  current_height   <- convert_symbol(m[mrow, mcol])
  if(candidate_height - current_height <= 1){
    if(!exists(as.character(xytoi(x,y)), envir=visited)){
      candidate_distance <- nodes[[as.character(currentnode)]][1] + 1
      candidate_key      <- as.character(xytoi(x,y))
      
      #If the calculated candidate distance is lower than the currently recorded distance : update the new with new distance and fromnode. 
      if(candidate_distance < nodes[[candidate_key]][1]){
        nodes[[candidate_key]][1] <<- candidate_distance
        nodes[[candidate_key]][2] <<- currentnode
      }
      pq$push(candidate_key, -candidate_distance)   #add the node to pq (i.e.queue for processing). The largest priority is selected next so we need to use the negative distance!
    }
  }
}

#Convert matrix symbol to height
convert_symbol <- function(c){
  h <- case_when (c=='S' ~ 'a', c=='E' ~'z', TRUE ~ c) |> charToRaw() |> strtoi(16L)
}

#Convert matrix coordinates to matrix index
xytoi<- function(x,y){
  (x-1) * matrix_rows + y
}

#####################################################################################################################################
#Part 2: Find th shortest path from any "a" to E. Walk backwards from E until we reach an 'a', substract the distance of this 'a' cell from total E distance? NO, this assumes we begin as S which is no longer true.
#Do we need to brute-force this by testing ALL 'a' as starting positions?
#maybe we can record every 'a' we see in each path, and their distance, add add these to "visited" so that we do not re-calculate them again? Also stop any round if we reach an already visited 'a'.

visited_a  <- new.env(hash = TRUE, parent = emptyenv(), size = length(m)) 
endnode    <- match('E', m)
#length(which(m %in% c('a', 'S'))) #2024 'a', about 1/3 of all tiles (7339) are 'a'!!
shortest_a_path <- Inf

#Loop all a nodes.
for(anode in which(m %in% c('a', 'S'))){
  
  if(!exists(as.character(anode), envir=visited_a)){
  
    #re-initialize 
    nodes    <- new.env(hash = TRUE, parent = emptyenv(), size = length(m))   #Nodes. key= matrix ID (1-40), data = distance (to start, Inf at start), fromnode = Null
    visited  <- new.env(hash = TRUE, parent = emptyenv(), size = length(m))   #Nodes which have been visited. Do not re-visit these.
    pq       <- priority_queue()  #nodes to process
    
    #initialize with square anode. Find min steps to square "E".
    
    startnode <- anode
    
    #Add all nodes and the inithial distance : 0 for start node, Inf for the rest.
    #nodes[['1']] <- c(dDistance=0, fromnode=-1)   #Add the first node with distance 0.
    for(i in 1:length(m)){
      nodes[[as.character(i)]] <- c(distance=Inf, fromnode=-1)
    }
    nodes[[as.character(startnode)]] <- c(distance=0, fromnode=-1)  
    
    #initialize priority queue with first square, distance = -Inf.
    pq$push(as.character(startnode), -Inf)            
    
    #Run the search
    day12()
    
    #Now find all 'a' in the path, add them to 'visited'. Also keep track of the shortest path yet.
    
    totaldistance <- nodes[[as.character(endnode)]][1]
    
    currentnode   <- endnode
    while(!is_null(nodes[[as.character(currentnode)]][2])){
      if(m[currentnode] %in% c('a', 'S')){
        a_distance <- nodes[[as.character(currentnode)]][1]
        print(c(node=currentnode, dist=(totaldistance - a_distance) ))
        if(totaldistance - a_distance < shortest_a_path) shortest_a_path <- totaldistance - a_distance
        visited_a[[as.character(currentnode)]] <- currentnode 
      }
      #find the next node in this path
      currentnode <- nodes[[as.character(currentnode)]][2]
    }
  }
  
 print(shortest_a_path) #the final result : 478

}



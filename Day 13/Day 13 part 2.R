###########################################################################################
# Day 13 : Organize all of the packets into the correct order. What is the decoder key for the distress signal?
###########################################################################################
library(tidyverse)
library(collections)
library(rlang)

#input <- readLines("Day 13/input.txt") 
input <- readLines("Day 13/test input.txt") 


#Import input into pairs of lists. (8 for test input).
processinput <- function(input){
  row <- 1
  index_sum <- 0
  
  for(pair in 1:(length(input)/3)){
    
    l1 <- input[row]
    l2 <- input[row+1]
    row <- row + 3
    
    l1 <- str_to_list(l1)
    l2 <- str_to_list(l2)
    
    if(compare_lists(l1, l2) == "Yes") index_sum <- index_sum + pair
    print(index_sum)  #5717
  
  }
}

#Compare two lists according to rules. Return "Yes" if  correct order, No" if wrong order, "Equal" if , for each pair.
#Is this recursive, so that we call this function again for sub-lists?
compare_lists <-function(left, right){
  
  #Compare left and right lists one element at a time. Compare sub-lists recursively.
  i <- 1
  answer <- "Equal"
  
  repeat{
    
    #Check if value i exists in both lists. If it exists in both : Continue. If it does not exist in any: return "Equal". If Left has run out of items: return Yes. Else: Retunr No.
    if(length(left) < i | length(right) < i){
      if(length(left) < i & length(right) < i) return("Equal")
      if(length(left) < length(right)) return("Yes") else return("No")
      }
    
    #Get next element
    l1 <- left[[i]]
    r1 <- right[[i]]
    
    #If both are integers : Compare them.
    if (!is.list(l1) & !is.list(r1)){
      if(l1 < r1){return("Yes")}
      if(l1 > r1){return("No")}
    } else { #Both are not integers.
      
      #If mixed: convert the integer to a list and then compare the two lists.
      if(!is.list(l1)){l1 <- list(l1)}
      if(!is.list(r1)){r1 <- list(r1)}

      #If both are lists: Call compare_lists recursively,
      if (is.list(l1) & is.list(r1)){
        r <- compare_lists(l1, r1)
        if(! r == "Equal") {return(r)}
      } else {
        print("ERROR : We should not get here!")
      }
    }
    
    i <- i + 1
    if(i>1000) break
  }
}

#Convert (parse & eval) string input to list
str_to_list <- function(s){
  s <- str_replace_all(s, ']', ')')
  s <- str_replace_all(s, '\\[', 'list(' )
  eval(parse(text=s))
}

##########################################
#Part 2:
##########################################
#Add two divider packets to the list of packets 
#Put all the packets in the correct order - build a bubble sort? Or can we call a sort algo with custom comparer?
#Find the idexes of the divider packets, multiply these.

#input <- readLines("Day 13/test input.txt") 
input <- readLines("Day 13/input.txt") 

#remove empty lines from input
for(i in 1:length(input)){
  if(input[[i]] == "") input <- input[-i]
  if(i>=length(input)) break
}

#Add divisors
input <- append(input,"[[2]]" )
input <- append(input,"[[6]]" )

#Sort the packets
input <- bubble_sort(input)

#Find indexes of divisors after ordering packets
match("[[2]]" , input) * match("[[6]]" , input)  #25935

#bubble sort where we use compare_lists() as customer comparator:
bubble_sort <- function(x)
{
  # calculate the length of array
  n <- length(x)
  # run loop n-1 times
  for (i in 1 : (n - 1)) {
    # run loop (n-i) times
    for (j in 1 : (n - i)) {
      # compare elements
      #if (x[j] > x[j + 1]) {
      if(compare_lists(str_to_list(x[j]), str_to_list(x[j + 1]) ) == "No") {
        temp <- x[j]
        x[j] <- x[j + 1]
        x[j + 1] <- temp
      }
    }
  }
  x
}



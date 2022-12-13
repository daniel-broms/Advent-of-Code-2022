###########################################################################################
# Day 6
###########################################################################################
#Find where we have four unique characters.
library(tidyverse)

input <- readLines("Day 6/input.txt") 
#input <- readLines("Day 6/test input.txt")  

#split characters into a list
a <- unlist(str_split(input, ''))

n <- 14 #nr of unique chars to find (4 in part1, 14 in part2)

#Find the first slice which does not have any duplicates.
for(i in n: str_length(input)) {
  d <- sum(duplicated(a[(i-n+1):i]))
  if(d == 0 ) {
    print(i,d)
    break
    }
}

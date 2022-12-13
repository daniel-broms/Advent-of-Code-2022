###########################################################################################
# Day 1 
###########################################################################################

#Import text file lines to a character vector using readlines:
input <- readLines("Day 1/input.txt")                           #Simply imports text lines, separated by newline.

#Process this vector iteratively, top-down. Accumulate groups (elves) until we reach a blank line.
elve <- vector(mode='integer')
acc <-0
for(i in 1:length(input)) {
  if(input[i] == '') {
    elve <- c(elve,acc)
    acc <-0
  }
  else {
    acc <- acc + as.numeric(input[i])
  }
}

#Part 1: Find the top elf:
max(elve)

#Part 2 : sum the top three elves.
sum(sort(elve, decreasing = T)[1:3])

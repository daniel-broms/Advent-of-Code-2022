###########################################################################################
# Day 1 :Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
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

#Part 1: Find the top elf: 74394
max(elve)

#Part 2 : sum the top three elves.
sum(sort(elve, decreasing = T)[1:3])

############## Alternative base r formulation, without loops ########

input |> 
  as.integer() |> 
  aggregate(by = list(cumsum(input == '')), 
             FUN=sum, 
             na.rm = T ) |> 
  pull(x) |> 
  sort(decreasing = T) |> 
  head(3) |> 
  sum()


############## dplyr version ############## 
input |> 
  as.integer() |> 
  as_tibble() |> 
  group_by(cumsum(is.na(value))) |> 
  filter(!is.na(value)) |> 
  summarize( value=sum(value), na.rm = TRUE) |> 
  arrange(value) |> 
  head(3) |> 
  sum()
  
  
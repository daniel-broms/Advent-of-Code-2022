###########################################################################################
# Day 4
###########################################################################################
#In how many assignment pairs does one range fully contain the other?
library(tidyverse)

input <- readLines("Day 4/input.txt") 
input <- readLines("Day 4/test input.txt")  
 
#Split input to 4 columns in a tibble
t <- tibble(input)


#Use tidyr Separate() to split each string into four columns using a regular expression (, OR -).
t <- separate(t,  col=input,  into=c('A1', 'A2', 'B1', 'B2' ) , sep= ',|-' )

#Convert all columns (1-4) to integers
t <- t %>% mutate(across(c(1:4), as.integer))

#List rows where A contains B, or the other way around 
t2 <- filter(t, (A1 <= B1 & A2 >= B2) | (B1 <= A1 & B2 >= A2 ))

#Part 2 : List rows with any overlap
t2 <- filter(t, (A1>=B1 & A1<=B2) | (A2>=B1 & A2<=B2) | (B1>=A1 & B1<=A2)  | (B2>=A1 & B2<=A2)  )  #938

######################################################################################

#NOTES:
#Alternative conversion method. Note that we need double brackets to access the individual values!! t[] returns a tibble.
t[['A1']] <- as.integer(t[['A1']])


#test Stringr to split strings instead. Works with regular expression pattern, returns a matrix of string 
x <- str_split_fixed(input, ',|-', n=4)
x <- tibble(x)
x <- as.data.frame(x)


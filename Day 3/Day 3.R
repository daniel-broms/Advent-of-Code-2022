###########################################################################################
# Day 3
###########################################################################################

dummy <- function(){   #Enclose all code in a dummy function, just to improve debugging a code sourcing. execute all code by writing "dummy()" in the console.
  
  input <- readLines("Day 3/test input.txt")  
  #input <- readLines("Day 3/input.txt")  
  
  #Part 1: Find the character which is common to first and second halv of each string, sum the ascii codes.
  
  tot <- 0
  for(i in 1:length(input)){
    l <- nchar(input[i])
    v1 <- vector()
    v2 <- vector()
    for(j in 1:floor(l/2)){
      k <- j + l/2
      v1 <- c(v1, convert(substr(input[i],j,j)))
      v2 <- c(v2, convert(substr(input[i],k,k)))
    }
    #if(i == 2) browser()     We can conditionally stop code and inspect data by calling "browser()"
    i <- intersect(v1, v2)
    tot <- tot + intersect(v1, v2)
  }
  print(tot)
  
  
  
  #Part 2 : Find the value which is common for each group of three lines
  tot <- 0
  for(i in seq(1, length(input), by=3)){
  
    v1 <- tovec(input[i])
    v2 <- tovec(input[i+1])
    v3 <- tovec(input[i+2])
    
    i1 <- intersect(v1, v2)
    i2 <- intersect(i1, v3)
  
    tot <- tot + i2
  }
  print(tot)

}


#Support functions

#Convert one character to a number
convert <-function(x){
  a <- asc(x)
  if(a<=90) a-64+26 else a-96
}
asc <- function(x) { strtoi(charToRaw(x),16L) }

#convert a string to a vector of numbers
tovec <- function(x){
  l <- nchar(x)
  v <- vector(length=l)
  for(i in 1:l){
    v[i] <- convert(substr(x,i,i))
  }
  v
}



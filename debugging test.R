
#write the code as a function in a separate file. This way we can "source" the file without running code, and place breakpoints in it.
#Call the function from some other file, or from the console.
#Debugging works much better this way!


part1 <- function(){
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
    #if(i == 2) browser()
    i <- intersect(v1, v2)
    tot <- tot + intersect(v1, v2)
  }
  tot
}
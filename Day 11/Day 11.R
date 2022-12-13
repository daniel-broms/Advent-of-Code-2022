###########################################################################################
# Day 11 : Calculate monkey business.
###########################################################################################


day11 <- function(){
  
  #input <- readLines("Day 11/test input.txt") 
  input <- readLines("Day 11/input.txt") 
  
  #Model as monkey objects? Or is each monkey a list with members?
  
  monkeys <- list()
  monkey  <- list()
  row <- 1
  
  nr_of_monkeys <- length(input) / 7
  
  #read data into monkeys structures. Note that monkey list index = monkey index + 1!!
  for(i in 0: (nr_of_monkeys - 1)){
    
    monkey <- list()
    monkey$monkey_index <- i 
    monkey$items <- input[i*7 + 2] |>
      str_sub( 19, -1) |>
      str_split(',', simplify = TRUE) |>
      lapply(as.integer)
    
    divisible <- input[i*7 + 4] |>  str_sub( 22, -1) |> as.integer()
    monkey$test <- make_test(divisible)
    monkey$if_true  <- input[i*7 + 5] |>  str_sub( 30, -1) |> as.integer()
    monkey$if_false <- input[i*7 + 6] |>  str_sub( 31, -1) |> as.integer()
    monkey$inspected_items <- 0
    
    monkeys <- append(monkeys,list(monkey))
    
  }
  
  #Add operation functions manually, takes too long time to parse.
  #TEST DATA#
  #monkeys[[1]]$operation <- function(x) {x * 19}
  #monkeys[[2]]$operation <- function(x) {x + 6}
  #monkeys[[3]]$operation <- function(x) {x * x}
  #monkeys[[4]]$operation <- function(x) {x + 3}
  
  monkeys[[1]]$operation <- function(x) {x * 7}
  monkeys[[2]]$operation <- function(x) {x * 13}
  monkeys[[3]]$operation <- function(x) {x + 1}
  monkeys[[4]]$operation <- function(x) {x * x}
  monkeys[[5]]$operation <- function(x) {x + 7}
  monkeys[[6]]$operation <- function(x) {x + 6}
  monkeys[[7]]$operation <- function(x) {x + 4}
  monkeys[[8]]$operation <- function(x) {x + 8}
  
  
  #run 20 rounds
  for(round in 1:20){
    
    #process eah monkey in turn
    for(m in 0:(nr_of_monkeys - 1)){
      
      #cat("monkey:", m)
      #process each item in order
      for(i in seq_along(monkeys[[m+1]]$items)){
        #print(monkeys[[m+1]]$items[[i]])
        monkeys[[m+1]]$inspected_items <- monkeys[[m+1]]$inspected_items + 1               #tally the item inspection total
        new_level <- monkeys[[m+1]]$operation(monkeys[[m+1]]$items[[i]])   #calculate new worry level
        new_level <- floor(new_level/3)                                    #divide by three
        if(monkeys[[m+1]]$test(new_level)){
          to_monkey <- monkeys[[m+1]]$if_true
        } else {
          to_monkey <- monkeys[[m+1]]$if_false
        }
        
        #throw the item
        monkeys[[to_monkey+1]]$items <- append(monkeys[[to_monkey+1]]$items, new_level)   
        
  
      }
      monkeys[[m+1]]$items <- list() #empty this monkey item list - all are throw.
      
      
    }
  
  }

  for(m in 1:nr_of_monkeys){
    print(monkeys[[m]]$inspected_items)
  }
  
}

make_test <- function(x){
  divisor <- x
  function(y){y %% divisor == 0}
}

day11()

280*282  #78960 : Correct!

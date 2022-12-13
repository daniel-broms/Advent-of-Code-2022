###########################################################################################
# Day 7  V2 : track complete paths, not just folder name.
###########################################################################################
library(tidyverse)
#Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?

ImportData <- function(){
    
  
  input <- readLines("Day 7/input.txt") 
  #input <- readLines("Day 7/test input.txt")  
  
  #Add ending instruction
  input[length(input)+1] <- 'END'

  #1; execute commands to build directory structure (as nested list?). 
  #  Only track directory name, size, parent and subdirectories - not files.
  #  Record data in two tibbles: 
  #   t1 with one row per directory: Name, Parent, Size,
  #   t2 with Parent, Child (to record directory structur)
  
  current_dir_name    <- '/'
  current_dir_parent  <- ''
  current_dir_size    <- 0
  current_dir_subdirs <- vector()
  
  t1 <- tibble(name=character(), parent=character(), size=numeric(), accsize=numeric(), leaf=logical())
  t2 <- tibble(parent=character(), child=character())
  
  #Parse commands and output
  row <- 2
  while(row <= length(input)-1){
    
    #handle command '$ ls'
    if(input[row] == '$ ls') {
      
      #read the current directory content until we reach the next command $ 
      current_dir_size    <- 0
      current_dir_subdirs <- vector()
      
      row <- row + 1
      while(! substr(input[row],1,1) == '$' & ! input[row] == 'END'){
        if(str_sub(input[row],1,3) == 'dir') { #add a subdirectory
          current_dir_subdirs <- append(current_dir_subdirs, str_sub(input[row], 5, -1))
        } 
        else {   #add file size to directory : find file size with regex
          size <- str_extract(input[row], '[0-9]+' )
          size <- as.integer(size)
          current_dir_size <- current_dir_size + size
        }
        row <- row + 1
      }
      
      #record this directory in t1 and t2 ( assume that we only do ls once per directory!!)
      if(length(current_dir_subdirs) > 0) {
        t2 <- add_row(t2, parent=current_dir_name, child=current_dir_subdirs)
        t1 <- add_row(t1, name=current_dir_name, parent=current_dir_parent, size=current_dir_size, accsize=NULL, leaf=FALSE) 
      }
      else {
        t1 <- add_row(t1, name=current_dir_name, parent=current_dir_parent, size=current_dir_size, accsize=current_dir_size, leaf=TRUE )
      }
    }
      
    #handle command '$ cd ..' : Move up one level
    else if(input[row] == '$ cd ..') {
      current_dir_name <- current_dir_parent
      current_dir_parent <- filter(t1, name == current_dir_name) %>% pull(parent)   
      row <- row + 1
  
    }
    
    #handle command '$ cd dirname' : Move down one level
    else if(str_sub(input[row],1,5) == '$ cd ') {
      current_dir_parent <- current_dir_name
      current_dir_name <- paste(current_dir_name, '/', str_sub(input[row], 6, -1), sep='')
      row <- row + 1
    }
    
    else if(input[row] == 'END'){
      row <- row + 1  #just fall out of the looo
    }
      
    
    else{
      print("ERROR : We should not get here, unhandled command?")
      browser()
      row <- row + 1
    }
  }
  
  return(t1)

} #End of function ImportData  

#Separate function to calculate total size of each folder.
#Iterate all folders. If accsize=NA, and all child folders of a folder have an acc_size : update the folder accsize as the sum of these children.
#Repeat this until all folders have accsize.
CalcualteSizes <- function() {
  
  #repeat until  all folders have an accsize 
  while(t1 %>% filter(is.na(accsize)) %>% nrow() > 0){
    t1 <<- mutate(t1, accsize = CalcualteFolderSizeV(name, accsize, size))
    print('CalcualteFolderSizeV')
  }
}

CalcualteFolderSize <- function(folder, accsize, thisfoldersize){
  
  #If accsize is calcualted, just return this.
  if(!is.na(accsize)){
    return(accsize)
  }
  else {
    #find all children (via t1.parent.. if ALL have accsize: return sum of child accsize plus the current folder size. Otherwise return NA.
    
    if(t1 %>% filter( parent==folder & is.na(accsize)) %>%nrow() > 0){
      return(NA)
    } else {
      return(t1 %>% filter(parent==folder) %>% summarise(size = sum(accsize)) %>%pull(size)  + thisfoldersize)
    }
  }
}
CalcualteFolderSizeV <- Vectorize(CalcualteFolderSize) 

#Run all the above code:
t1 <- ImportData()
CalcualteSizes()

#Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?
t1 %>% filter(accsize<=100000) %>%  summarise(size = sum(accsize)) %>%pull(size) #1642503

################################################################################
#Part 2: Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update. What is the total size of that directory?
#  total disk size = 70000000
#  needed size = 30000000
#  currently used size = 46592386
#  currently available size = 23407614
#  needed more size = 6592386 : Find the smallest folder bigger than this.

t1 %>% filter(accsize>=6592386) %>% arrange(accsize)  #Smallest matching folder size is 6999588 



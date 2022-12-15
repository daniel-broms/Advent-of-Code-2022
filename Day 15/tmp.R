#Find intersections in seacrh scope.
for(i in 1:length(lines)){
  for( j in 1:length(lines)){
    if(!i==j){
      line1 <- Line$new(A=c(lines[[i]][1], lines[[i]][2]), B=c(lines[[i]][3], lines[[i]][4]), F, F)
      line2 <- Line$new(A=c(lines[[j]][1], lines[[j]][2]), B=c(lines[[j]][3], lines[[j]][4]), F, F)
      intersect <- intersectionLineLine(line1, line2)
      if(!is.null(intersect) & is.numeric(intersect) & !any(is.na(intersect))){
        #browser()
        intersect <- as.integer(intersect)
        if(all(intersect > 0 ) & all(intersect < boundary)){
          if(!checkpoint(intersect[1],intersect[2]+1,s2)) print(c(intersect[1],intersect[2]+1))
          if(!checkpoint(intersect[1]+1,intersect[2],s2)) print(c(intersect[1]+1,intersect[2]))
          if(!checkpoint(intersect[1],intersect[2]-1,s2)) print(c(intersect[1],intersect[2]-1))
          if(!checkpoint(intersect[1]-1,intersect[2],s2)) print(c(intersect[1]-1,intersect[2]))
        }
      }
    }
  }
}

###########################################################################################
# Day 15 : Consult the report from the sensors you just deployed. In the row where y=2000000, how many positions cannot contain a beacon?
###########################################################################################
#Real input coordinates are in the millions - too large to make a matrix out of.
library(tidyverse)
library(collections)
library(rlang)
library(PlaneGeometry)

input <- readLines("Day 15/test input.txt") 
testrow<- 10
boundary <- 20

input <- readLines("Day 15/input.txt") 
testrow<- 2000000
boundary <- 4000000

#Model the sensors and their reach (manhattan distance to closest beacon).
#Find the sensor with lowest x reach at row 2 000 000 : this is the first x position which cannot contain a beacon.
#Find the sensor with largest x reach at row 2 000 000 : this is the last x position which cannot contain a beacon.
#loop through all positions between these endpoints, count how many positions are covered by any sensor + reach.


#Convert to input to a list of vectors with 4 numbers.
s <- str_replace(input, 'Sensor at ', '')
s <- str_replace(s, ': closest beacon is at', '')
s <- str_split(s, ' ')
s <-lapply(s, function (x) str_replace(x, '\\w=', ''))
s <-lapply(s, function (x) str_replace(x, ',', ''))
s <-lapply(s, as.integer)

#convert this to a list of sensor positions + reach.
#s2 <-lapply(s, function(x) c(x=x[1], y=x[2], d= abs(x[1] - x[3]) + abs(x[2] - x[4]) ))
s2 <-lapply(s, function(x) c(x[1], x[2], abs(x[1] - x[3]) + abs(x[2] - x[4]) ))

#convert this to a list of min/max x reach for each sensor on testrow.
s3 <- lapply(s2, function(x) rowspan(x[1], x[2], x[3], testrow ))
s3 <- s3[!is.na(s3)]  #remove NA rows which do not touch testrow

#find min and max x in this list
xmin <- Inf
xmax <- -Inf
for(s in s3){
  xmin <- min(xmin, s[1], s[2])
  xmax <- max(xmax, s[1], s[2])
}

#Check all positions between xmin and xmax : is this position covered by a sensor or not?
cp <- 0
for(x in xmin:xmax){
  for(s in s3)
    if(between(x, s[1], s[2])){
      cp <- cp + 1
      print(x)
      break
    }
}
cp-1 #5394423 : Correct!

#calculate the min and max x reach of sensor at x/y with reach d on row testrow:
rowspan <- function(x,y,d,testrow){
  span <- max( 0, d - abs(y-testrow))
  if(span == 0) return(NA)
  return(c(x-span, x+span))
}

#Check if a point is covered by ANY sensor.
checkpoint <- function(px, py, s2){
  Covered <- F
  for(s in s2){
    if(checkpointsensor(px, py, s[1],  s[2], s[3] )) {Covered <- T }
  }
  Covered
}

#Check if a point (px, py) is covered by  sensor(sx, sy, sd).
checkpointsensor <- function(px, py, sx, sy, sd){
  (abs(px-sx) + abs(py-sy)) <= sd
}

#Find the intersect for two lines
get_intersect <- function(L1, L2){
  slope1 <- (L1[2] - L1[4]) / (L1[1] - L1[3])
  slope2 <- (L2[2] - L2[4]) / (L2[1] - L2[3])
  
  if(slope1 == slope2) return(NA)
  
  if(slope1==1) {
    A=L1
    B=L2
  } else {
    B=L1
    A=L2
  }
  
  Iy <- ((A[2] - A[1]) + (B[2] + B[1])) / 2   #18
  Ix <- ((A[1] - A[2]) + (B[2] + B[1])) / 2   #9
  return(c(Ix, Iy))
}

#######################################################################################
#Part 2:
#Find the only possible position for the distress beacon. What is its tuning frequency?
#######################################################################################

##################################################################
#Plot sensors in 2D space.

plot(x=c(0,boundary), y=c(0,boundary), type='l')
for(s in s2){
  shape(s[1],  s[2], s[3])
}

#return shape of a sensor as an x and y vector.
shape <- function(x, y, d){
  x <- c(x, x+d, x, x-d, x)
  y <- c(y-d, y, y+d, y, y-d)
  #line(x, y, type='l')
  polygon(x, y, col = c('red'))
}

##################################################################

#Find all line intersections. Check the points surrounding the intersection : Is it outside all sensors?
#make a list of all lines. Compare each line with each other line. If they intersect, checkpoint the surrounding points (up, down, left, right).
lines <- list()
for(s in s2){
  lines <- append(lines, list(c(s[1], s[2] - s[3],   s[1] + s[3], s[2])))  #first line
  lines <- append(lines, list(c(s[1] + s[3], s[2],   s[1], s[2] + s[3])))  #second line
  lines <- append(lines, list(c(s[1], s[2] + s[3],   s[1] - s[3], s[2])))  #third line
  lines <- append(lines, list(c(s[1] - s[3], s[2],   s[1], s[2] - s[3])))  #fourth line
}

#Find intersections in search scope. Check coordinates around the intersections. Return if we find an uncovered coordinate.
findbeacon <- function(){
  for(i in 1:length(lines)){
    for( j in 1:length(lines)){
      if(!i==j){
        intersect <- get_intersect(lines[[i]], lines[[j]])
        if(!is.null(intersect) & is.numeric(intersect) & !any(is.na(intersect))){
          if(all(intersect > 0 ) & all(intersect < boundary)){
            intersect <- as.integer(intersect)
            if(!checkpoint(intersect[1],intersect[2]+1,s2)) return(c(intersect[1],intersect[2]+1))
            if(!checkpoint(intersect[1]+1,intersect[2],s2)) return(c(intersect[1]+1,intersect[2]))
            if(!checkpoint(intersect[1],intersect[2]-1,s2)) return(c(intersect[1],intersect[2]-1))
            if(!checkpoint(intersect[1]-1,intersect[2],s2)) return(c(intersect[1]-1,intersect[2]))
          }
        }
      }
    }
  }
}

findbeacon()

print(2960219 * 4000000 +  3211051, digits = 20)  #11840879211051


#Zoom in to the plot at this area to see the beacon
plot(x=c(2960219-50,2960219+50), y=c(3211051-50,3211051+50), type='l', col='white')
for(s in s2){
  shape(s[1],  s[2], s[3])
}





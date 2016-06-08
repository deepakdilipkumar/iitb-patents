library(readxl)

 ee <- read_excel("..//data//patents.xlsx",1,skip=1)
 health <- read_excel("..//data//patents.xlsx",2,skip=1)
 ict <- read_excel("..//data//patents.xlsx",3,skip=1)
 manchem <- read_excel("..//data//patents.xlsx",4,skip=1)
other <- read_excel("..//data//patents.xlsx",5,col_types=c("text","text","text","text","text","text"),skip=1)
 granted <- read_excel("..//data//patents.xlsx",6,col_types=c("text","text","text","text","text","text"),skip=1)

index <- match("International Patent filed",ee[,1])
ee <- ee[-index,1:6]
ee[1:index-1,7] <- "Indian"
ee[index:dim(ee)[1],7] <- "International"
ee[,8] <- "EE"
names(ee)[7:8] <- c("Level","Sector")

index <- match("International Patent filed",health[,1])
health <- health[-index,1:6]
health[1:index-1,7] <- "Indian"
health[index:dim(health)[1],7] <- "International"
health[,8] <- "Healthcare"
names(health)[7:8] <- c("Level","Sector")

index <- match("International Patent filed",ict[,1])
ict <- ict[-index,1:6]
ict[1:index-1,7] <- "Indian"
ict[index:dim(ict)[1],7] <- "International"
ict[,8] <- "ICT"
names(ict)[7:8] <- c("Level","Sector")

index <- match("International Patent filed",manchem[,1])
manchem <- manchem[-index,1:6]
manchem[1:index-1,7] <- "Indian"
manchem[index:dim(manchem)[1],7] <- "International"
manchem[,8] <- "Manufacturing/Chemical"
names(manchem)[7:8] <- c("Level","Sector")

index <- match("International Patent filed",other[,1])
other <- other[-index,1:6]
other[1:index-1,7] <- "Indian"
other[index:dim(other)[1],7] <- "International"
other[,8] <- "Other"
names(other)[7:8] <- c("Level","Sector")

filed <- rbind(ee,health,ict,manchem,other)

index <- match("International Patent Grant",granted[,1])
granted <- granted[-index,1:6]
granted[1:index-1,7] <- "Indian"
granted[index:dim(granted)[1],7] <- "International"
print(granted[,6])

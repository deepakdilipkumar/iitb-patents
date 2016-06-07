library(readxl)

 ee <- read_excel("..//data//patents.xlsx",1,skip=1)
 health <- read_excel("..//data//patents.xlsx",2,skip=1)
 ict <- read_excel("..//data//patents.xlsx",3,skip=1)
 manchem <- read_excel("..//data//patents.xlsx",4,skip=1)
 other <- read_excel("..//data//patents.xlsx",5)
 granted <- read_excel("..//data//patents.xlsx",6,skip=1)

index <- match("International Patent filed",ee[,1])
ee <- ee[-index,1:6]
eeInd <- ee[1:index-1,]
eeInt <- ee[index:dim(ee)[1],]

index <- match("International Patent filed",health[,1])
health <- health[-index,1:6]
healthInd <- health[1:index-1,]
healthInt <- health[index:dim(health)[1],]

index <- match("International Patent filed",ict[,1])
ict <- ict[-index,1:6]
ictInd <- ict[1:index-1,]
ictInt <- ict[index:dim(ict)[1],]

index <- match("International Patent filed",manchem[,1])
manchem <- manchem[-index,1:6]
manchemInd <- manchem[1:index-1,]
manchemInt <- manchem[index:dim(manchem)[1],]

index <- match("International Patent filed",other[,1])
other <- other[-index,1:6]
otherInd <- other[1:index-1,]
otherInt <- other[index:dim(other)[1],]

index <- match("International Patent Grant",granted[,1])
granted <- granted[-index,1:6]
grantedInd <- granted[1:index-1,]
grantedInt <- granted[index:dim(granted)[1],]
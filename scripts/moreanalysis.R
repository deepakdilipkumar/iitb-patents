
filed <- read.csv("..//data//filed.csv")
filed$Date1 <- as.Date(filed$Date)
filed$Date1[is.na(filed$Date1)] <- as.Date(paste0(filed$Date,"-01-01"))

granted <- read.csv("..//data//granted.csv")
granted$Date1 <- as.Date(granted$Date)
granted$Date1[is.na(granted$Date1)] <- as.Date(paste0(granted$Date,"-01-01"))


dfCombined <- merge(filed, granted, by=c('Title', 'Level'), all.x=F, all.y=F)
counttable <- table(dfCombined$Title)
dfCombined <- dfCombined[!dfCombined$Title %in% names(counttable[counttable>1]), ]

dfCombined$Timetogrant <- as.Date(dfCombined$Date1.y) - as.Date(dfCombined$Date1.x)

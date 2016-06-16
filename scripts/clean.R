library(readxl)
library(lubridate)

 ee <- read_excel("..//data//patents.xlsx",1,col_types=c("text","text","text","text","text","date","text","text"),skip=1)
 health <- read_excel("..//data//patents.xlsx",2,col_types=c("text","text","text","text","text","date","text"),skip=1)
 ict <- read_excel("..//data//patents.xlsx",3,col_types=c("text","text","text","text","text","date","text"),skip=1)
 manchem <- read_excel("..//data//patents.xlsx",4,col_types=c("text","text","text","text","text","date"),skip=1)
other <- read_excel("..//data//patents.xlsx",5,col_types=c("text","text","text","text","text","date"),skip=1)
 granted <- read_excel("..//data//patents.xlsx",6,col_types=c("text","text","text","text","text","date"),skip=1)

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

names(filed)[5:6] <- c("Number","Date")

index <- match("International Patent Grant",granted[,1])
granted <- granted[-index,1:6]
granted[1:index-1,7] <- "Indian"
granted[index:dim(granted)[1],7] <- "International"
granted <- granted[1:167,]

granted[,8] <- ""
names(granted)[5:8] <- c("Number","Date","Level","Sector")

l1 <- length(filed$Title)
l2 <- length(granted$Title)

filed <- rbind(filed,granted)

filed[,9:10]=""
filed[,11]="No"

#names(filed)
names(filed)[9:11]=c("Dept1","Dept2","Multidiscipline")

for (index in 1:length(filed$Department)){
	if (grepl("aerospace",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Aerospace"
		}
		else{
			filed$Dept2[index]="Aerospace"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("electrical",filed$Department[index], ignore.case = TRUE)|grepl("EE",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Electrical"
		}
		else{
			filed$Dept2[index]="Electrical"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("biosc",filed$Department[index], ignore.case = TRUE)|grepl("SBB",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Bio"
		}
		else{
			filed$Dept2[index]="Bio"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("chemistry",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Chemistry"
		}
		else{
			filed$Dept2[index]="Chemistry"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("chemical",filed$Department[index], ignore.case = TRUE)|grepl("chem engg",filed$Department[index],ignore.case=TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Chemical"
		}
		else{
			filed$Dept2[index]="Chemical"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("mech",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Mechanical"
		}
		else{
			filed$Dept2[index]="Mechanical"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("civil",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Civil"
		}
		else{
			filed$Dept2[index]="Civil"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("computer",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CSE"
		}
		else{
			filed$Dept2[index]="CSE"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("metall",filed$Department[index], ignore.case = TRUE)|grepl("MEMS",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="MEMS"
		}
		else{
			filed$Dept2[index]="MEMS"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("physics",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Physics"
		}
		else{
			filed$Dept2[index]="Physics"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("environ",filed$Department[index], ignore.case = TRUE)|grepl("CESE",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CESE"
		}
		else{
			filed$Dept2[index]="CESE"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("resource",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CSRE"
		}
		else{
			filed$Dept2[index]="CSRE"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("CTARA",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CTARA"
		}
		else{
			filed$Dept2[index]="CTARA"
			filed$Multidiscipline[index]="Yes"
		}
	}
}


for (index in 1:length(filed$Department)){
	if (grepl("earth",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Earth Sciences"
		}
		else{
			filed$Dept2[index]="Earth Sciences"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("energy",filed$Department[index], ignore.case = TRUE)|grepl("ESE",filed$Department[index],ignore.case=FALSE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Energy"
		}
		else{
			filed$Dept2[index]="Energy"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("SOM",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="SOM"
		}
		else{
			filed$Dept2[index]="SOM"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("design",filed$Department[index], ignore.case = TRUE)|grepl("IDC",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="IDC"
		}
		else{
			filed$Dept2[index]="IDC"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("system",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Syscon"
		}
		else{
			filed$Dept2[index]="Syscon"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("TTSL",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="TTSL"
		}
		else{
			filed$Dept2[index]="TTSL"
			filed$Multidiscipline[index]="Yes"
		}
	}
}

filed[,12] <- year(date(filed[,6]))
filed[,13] <- month.abb[month(date(filed[,6]))]
names(filed)[12:13] <- c("Year","Month")

filed$Year[filed$Year<1950&!is.na(filed$Year)] <- filed$Year[filed$Year<1950&!is.na(filed$Year)]+100

print(dim(filed))
print(l1)
print(l2)
print(l1+l2)
print(filed[1:l1+l2,6])

granted <- data.frame(filed[l1+1:l1+l2,])
filed <- data.frame(filed[1:l1,])

#print(granted$Department)
#deptwise <- data.frame(table(filed$Dept1))
#extra <- data.frame(table(filed$Dept2))
#for (dept in extra[,1]){
#	deptwise[deptwise[,1]==dept,2]<- deptwise[deptwise[,1]==dept,2]+extra[extra[,1]==dept,2]
#} 
#names(deptwise)=c("Department","Count")
#deptwise <- deptwise[-1,]  #Remove blank entries
#deptplot <- deptwise[,2]
#names(deptplot) <- deptwise[,1]

write.csv(filed,"..//data//filed.csv")
write.csv(granted,"..//data//granted.csv")

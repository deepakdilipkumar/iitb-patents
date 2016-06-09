library(readxl)
library(ggplot2)

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

filed[,9:10]=""

#names(filed)
names(filed)[9:10]=c("Dept1","Dept2")

for (index in 1:length(filed$Department)){
	if (grepl("erospace",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Aerospace"
		}
		else{
			filed$Dept2[index]="Aerospace"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("lectr",filed$Department[index])|grepl("EE",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Electrical"
		}
		else{
			filed$Dept2[index]="Electrical"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("iosc",filed$Department[index])|grepl("SBB",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Bio"
		}
		else{
			filed$Dept2[index]="Bio"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("hemistry",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Chemistry"
		}
		else{
			filed$Dept2[index]="Chemistry"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("hemical",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Chemical"
		}
		else{
			filed$Dept2[index]="Chemical"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("ech",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Mechanical"
		}
		else{
			filed$Dept2[index]="Mechanical"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("ivil",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Civil"
		}
		else{
			filed$Dept2[index]="Civil"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("omputer",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CSE"
		}
		else{
			filed$Dept2[index]="CSE"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("etall",filed$Department[index])|grepl("MEMS",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="MEMS"
		}
		else{
			filed$Dept2[index]="MEMS"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("hysics",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Physics"
		}
		else{
			filed$Dept2[index]="Physics"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("nviron",filed$Department[index])|grepl("CESE",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CESE"
		}
		else{
			filed$Dept2[index]="CESE"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("esource",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CSRE"
		}
		else{
			filed$Dept2[index]="CSRE"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("CTARA",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="CTARA"
		}
		else{
			filed$Dept2[index]="CTARA"
		}
	}
}


for (index in 1:length(filed$Department)){
	if (grepl("earth",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Earth Sciences"
		}
		else{
			filed$Dept2[index]="Earth Sciences"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("nergy",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Energy"
		}
		else{
			filed$Dept2[index]="Energy"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("SOM",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="SOM"
		}
		else{
			filed$Dept2[index]="SOM"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("esign",filed$Department[index])|grepl("IDC",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="IDC"
		}
		else{
			filed$Dept2[index]="IDC"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("ystems",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Syscon"
		}
		else{
			filed$Dept2[index]="Syscon"
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("TTSL",filed$Department[index])) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="TTSL"
		}
		else{
			filed$Dept2[index]="TTSL"
		}
	}
}

#filed$Dept2

deptwise <- data.frame(table(filed$Dept1))
deptwise

extra <- data.frame(table(filed$Dept2))
extra

for (dept in extra[,1]){
	deptwise[deptwise[,1]==dept,2]<- deptwise[deptwise[,1]==dept,2]+extra[extra[,1]==dept,2]
} 

names(deptwise)=c("Department","Count")
ggplot(filed,aes(Dept1))+geom_bar()

index <- match("International Patent Grant",granted[,1])
granted <- granted[-index,1:6]
granted[1:index-1,7] <- "Indian"
granted[index:dim(granted)[1],7] <- "International"

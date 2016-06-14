library(readxl)
library(ggplot2)
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

filed[,9:10]=""

#names(filed)
names(filed)[9:10]=c("Dept1","Dept2")
names(filed)[6]="Date"

for (index in 1:length(filed$Department)){
	if (grepl("aerospace",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Aerospace"
		}
		else{
			filed$Dept2[index]="Aerospace"
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
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("chemical",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Chemical"
		}
		else{
			filed$Dept2[index]="Chemical"
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
		}
	}
}

for (index in 1:length(filed$Department)){
	if (grepl("energy",filed$Department[index], ignore.case = TRUE)) {
		if(filed$Dept1[index]==""){
			filed$Dept1[index]="Energy"
		}
		else{
			filed$Dept2[index]="Energy"
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
		}
	}
}

filed[,11] <- year(date(filed[,6]))
filed[,12] <- month(date(filed[,6]))
names(filed)[11:12] <- c("Year","Month")
granted <- data.frame(granted[1:167,])
filed <- data.frame(filed)

#deptwise <- data.frame(table(filed$Dept1))
#extra <- data.frame(table(filed$Dept2))
#for (dept in extra[,1]){
#	deptwise[deptwise[,1]==dept,2]<- deptwise[deptwise[,1]==dept,2]+extra[extra[,1]==dept,2]
#} 
#names(deptwise)=c("Department","Count")
#deptwise <- deptwise[-1,]  #Remove blank entries
#deptplot <- deptwise[,2]
#names(deptplot) <- deptwise[,1]

filedbar <- filed
filedbar$Dept1 <- filedbar$Dept2
filedbar <- rbind(filed,filedbar)
oldfiled<-filed
filed<-filedbar
#ggplot(filedbar,aes(Dept1))+geom_bar()+ theme(axis.text.x = element_text(angle=45))

index <- match("International Patent Grant",granted[,1])
granted <- granted[-index,1:6]
granted[1:index-1,7] <- "Indian"
granted[index:dim(granted)[1],7] <- "International"

pdf("..//output//Deptwise across years.pdf")
dat=filed[filed$Year>2009&filed$Year<2016&!is.na(filed$Year)&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"|filed$Dept1=="Energy"|filed$Dept1=="Civil"|filed$Dept1=="CSE"),]
ggplot(dat,aes(x=Dept1))+geom_bar(stat="count",fill="grey50")+facet_wrap(~Year,nrow=2)+theme(axis.text.x=element_text(angle=45))+labs(x="Department",title="Patents by Key Departments after 2009")
#qplot(Dept1,data=filed[filed$Year>2009&filed$Year<2016&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"),],facets=Year~2)
dev.off()

pdf("..//output//all.pdf")
ggplot(filed[filed$Dept1!="",],aes(x=Dept1))+geom_bar(stat="count",fill="grey50")+theme(axis.text.x=element_text(angle=45))+labs(x="Department")+labs(title="All IITB Patents")
dev.off()

pdf("..//output//sectors.pdf")
ggplot(oldfiled[oldfiled$Year>2009&oldfiled$Year<2016&!is.na(oldfiled$Year),],aes(x=Sector))+geom_bar(stat="count",fill="grey50")+theme(axis.text.x=element_text(angle=45))+
labs(x="Sector",title="Sectorwise Patent Distribution after 2009")+facet_wrap(~Year,nrow=2)
dev.off()

pdf("..//output//level.pdf")
ggplot(oldfiled[oldfiled$Year>1999,],aes(x=Year,color=Level))+geom_line(stat="count")+labs(title="Patents Filed")
dev.off()

pdf("..//output//depts.pdf")
ggplot(filed[filed$Year>1999&filed$Dept1!=""&!is.na(filed$Dept1)&filed$Dept1!="SOM"&filed$Dept1!="TTSL"&filed$Dept1!="Earth Sciences"&filed$Dept1!="CSRE"&filed$Dept1!="CTARA",],aes(x=Year))+
geom_line(stat="count")+facet_wrap(~Dept1,ncol=3,scales="free_y")+labs(title="Patent variation across years for all departments")
dev.off()

pdf("..//output//Dept sector year.pdf")
ggplot(filed[filed$Dept1!="",],aes(x=Dept1))+geom_bar(stat="count",aes(fill=Sector))+theme(axis.text.x=element_text(angle=45))+labs(x="Department")+labs(title="All IITB Patents")
dev.off()

pdf("..//output//dept sector year 2.pdf")
dat=filed[filed$Year>2009&filed$Year<2016&!is.na(filed$Year)&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"|filed$Dept1=="Energy"|filed$Dept1=="Civil"|filed$Dept1=="CSE"),]
ggplot(dat,aes(x=Dept1))+geom_bar(stat="count",aes(fill=Sector))+facet_wrap(~Year,nrow=2)+theme(axis.text.x=element_text(angle=45))+labs(x="Department",title="Patents by Key Departments after 2009")
#qplot(Dept1,data=filed[filed$Year>2009&filed$Year<2016&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"),],facets=Year~2)
dev.off()

count=0
#for (index1 in 1:length(granted$Title)){
#	for (index2 in 1:length(filed$Title)){
#		if (grepl(granted$Title[index1],filed$Title[index2],ignore.case=TRUE)){
			#granted[index1,8] <- as.period(date(granted[index2,6])-date(filed[index1,6]))
#		}
#	}
#}

#granted[,8]
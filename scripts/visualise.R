library(ggplot2)

filed <- read.csv("..//data//filed.csv")
granted <- read.csv("..//data//granted.csv")

filedbar <- filed
filedbar$Dept1 <- filedbar$Dept2
filedbar <- rbind(filed,filedbar)
oldfiled<-filed
filed<-filedbar
#ggplot(filedbar,aes(Dept1))+geom_bar()+ theme(axis.text.x = element_text(angle=45))

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
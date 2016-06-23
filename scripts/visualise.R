library(ggplot2)
library(RColorBrewer)

filed <- read.csv("..//data//filed.csv")
granted <- read.csv("..//data//granted.csv")

filedbar <- filed
filedbar$Dept1 <- filedbar$Dept2
filedbar <- rbind(filed,filedbar)
oldfiled<-filed
filed<-filedbar

filed <- filed[filed$Dept1!="", ]

#ggplot(filedbar,aes(Dept1))+geom_bar()+ theme(axis.text.x = element_text(angle=45))



dfAggregatedByYear <- aggregate(Title ~ Dept1 + Year, FUN = length, data=filed)
colnames(dfAggregatedByYear)[3] <- 'Count'


dfAggregatedByYear <- within(dfAggregatedByYear,
                          Dept1 <- factor(Dept1, levels = rev(sort(unique(Dept1)))))

p <- ggplot(data= dfAggregatedByYear[dfAggregatedByYear$Year>=2005 &
                                       !dfAggregatedByYear$Dept1 %in% c('TTSL', 'SOM', 'Earth Sciences', 'CSRE' ), ],
       aes(x = Year,
           y = reorder(Dept1, Count, FUN = sum),
           color = Dept1,
           size = Count)) +
  geom_point()

p + scale_size(range = c(4, 10), breaks=c(1,5,10,25)) +
  scale_x_continuous(breaks=2005:2015) +
  scale_colour_hue(guide = "none") +
  theme(legend.position="bottom") +
  theme(panel.grid.minor = element_blank()) +
  ylab('Department')  +
  guides(size = guide_legend(title = "", label.position="bottom", label.hjust = 0.5))
  


###------------------------------------------------------------------------------------###










png("..//output//dept comparision across years bar.png",width=800,height=700)
dat=filed[filed$Year>2006&filed$Year<2016&!is.na(filed$Year)&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"|filed$Dept1=="Energy"|filed$Dept1=="Civil"|filed$Dept1=="CSE"),]
ggplot(dat,aes(x=Dept1))+geom_bar(stat="count",fill="grey50")+facet_wrap(~Year,nrow=2)+theme(axis.text.x=element_text(angle=45))+labs(x="Department",title="Patents Filed by Key Departments after 2006")
#qplot(Dept1,data=filed[filed$Year>2009&filed$Year<2016&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"),],facets=Year~2)
dev.off()

png("..//output//all patents filed.png",width=800,height=700)
ggplot(filed[filed$Dept1!="",],aes(x=Dept1))+geom_bar(stat="count",fill="grey50")+theme(axis.text.x=element_text(angle=45))+labs(x="Department")+labs(title="All IITB Patents Filed")
dev.off()

png("..//output//sectorwise distribution.png",width=800,height=700)
ggplot(oldfiled[oldfiled$Year>2006&oldfiled$Year<2016&!is.na(oldfiled$Year),],aes(x=Sector))+geom_bar(stat="count",fill="grey50")+theme(axis.text.x=element_text(angle=45))+
labs(x="Sector",title="Sectorwise Patent Filing Distribution after 2006")+facet_wrap(~Year,nrow=2)
dev.off()

png("..//output//patents filed across years.png",width=800,height=700)
ggplot(oldfiled,aes(x=Year,color=Level))+geom_line(stat="count")+labs(title="Patents Filed")
dev.off()

png("..//output//deptwise variation across years.png",width=800,height=700)
ggplot(filed[filed$Dept1!=""&!is.na(filed$Dept1)&filed$Dept1!="SOM"&filed$Dept1!="TTSL"&filed$Dept1!="Earth Sciences"&filed$Dept1!="CSRE"&filed$Dept1!="CTARA",],aes(x=Year))+
geom_line(stat="count")+facet_wrap(~Dept1,ncol=3,scales="free_y")+labs(title="Patent filing variation across years for all departments")
dev.off()

png("..//output//sectorwise across departments.png",width=800,height=700)
ggplot(filed[filed$Dept1!="",],aes(x=Dept1))+geom_bar(stat="count",aes(fill=Sector))+theme(axis.text.x=element_text(angle=45))+labs(x="Department")+labs(title="All IITB Patents Filed")
dev.off()

png("..//output//sectorwise across departments and years.png",width=800,height=700)
dat=filed[filed$Year>2006&filed$Year<2016&!is.na(filed$Year)&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"|filed$Dept1=="Energy"|filed$Dept1=="Civil"|filed$Dept1=="CSE"),]
ggplot(dat,aes(x=Dept1))+geom_bar(stat="count",aes(fill=Sector))+facet_wrap(~Year,nrow=2)+theme(axis.text.x=element_text(angle=45))+labs(x="Department",title="Patents Filed by Key Departments after 2006")
#qplot(Dept1,data=filed[filed$Year>2009&filed$Year<2016&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="MEMS"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"),],facets=Year~2)
dev.off()

png("..//output//multidiscplinary.png",width=800,height=700)
ggplot(oldfiled,aes(x=Year,color=Multidiscipline))+geom_line(stat="count")+labs(title="Multidisciplinary Patents Filed")
dev.off()

png("..//output//all patents granted.png",width=800,height=700)
ggplot(granted,aes(x=Department))+geom_bar(stat="count")+labs(title="All IITB Patents Granted")+theme(axis.text.x=element_text(angle=45))
dev.off()

png("..//output//dept comparison across years line.png",width=800,height=700)
dat=filed[filed$Year>2006&filed$Year<2016&!is.na(filed$Year)&(filed$Dept1=="Electrical"|filed$Dept1=="Mechanical"|filed$Dept1=="Bio"|filed$Dept1=="Chemical"|filed$Dept1=="Chemistry"),]
ggplot(dat,aes(x=Year,color=Dept1))+geom_line(stat="count")+labs(title="Patents Filed by Key Departments after 2006")+theme(axis.text.x=element_text(angle=45))
dev.off()

png("..//output//sectorwise across years.png",width=800,height=700)
ggplot(oldfiled,aes(x=Year,fill=Sector))+geom_bar(stat="count")+labs(title="Patents Filed in different Sectors")+theme(axis.text.x=element_text(angle=45))
dev.off()

png("..//output//sectorwise across years percentage.png",width=800,height=700)
ggplot(oldfiled,aes(x=Year,fill=Sector))+geom_bar(stat="count")+
labs(title="Patents Filed in different Sectors")+theme(axis.text.x=element_text(angle=45))
dev.off()

png("..//output//monthwise.png",width=800,height=700)
ggplot(oldfiled[!is.na(oldfiled$Month),],aes(x=factor(Month,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),fill=Sector))+
geom_bar(stat="count")+labs(title="Patents filed by Month",x="Month")+theme(axis.text.x=element_text(angle=45))
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
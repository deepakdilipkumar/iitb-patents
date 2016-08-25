library(ggplot2)
library(RColorBrewer)
library(lubridate)

filed <- read.csv("..//data//filed.csv")
granted <- read.csv("..//data//granted.csv")

filedbar <- filed
filedbar$Dept1 <- filedbar$Dept2
filedbar <- rbind(filed,filedbar)
oldfiled<-filed
filed<-filedbar

filed <- filed[filed$Dept1!="", ]

dfAggregatedByYear <- aggregate(Title ~ Dept1 + Year, FUN = length, data=filed)
colnames(dfAggregatedByYear)[3] <- 'Count'


dfAggregatedByYear <- within(dfAggregatedByYear,
                          Dept1 <- factor(Dept1, levels = rev(sort(unique(Dept1)))))


#Department and year wise
p <- ggplot(data= dfAggregatedByYear[dfAggregatedByYear$Year>=2005 &
                                       !dfAggregatedByYear$Dept1 %in% c('TTSL', 'SOM', 'Earth Sciences', 'CSRE' ), ],
       aes(x = Year,
           y = reorder(Dept1, Count, FUN = sum),
           color = Dept1,
           size = Count)) +
  geom_point()

p + scale_size(range = c(4, 20), breaks=c(1,5,10,25)) +
  scale_x_continuous(breaks=2005:2015) +
  scale_colour_hue(guide = "none") +
  theme(legend.position="bottom") +
  theme(panel.grid.minor = element_blank()) +
  ylab('Department')  +
  guides(size = guide_legend(title = "", label.position="bottom", label.hjust = 0.5)) +
  theme(axis.title = element_blank(), axis.text=element_text(size=16)) + 
  

  
#Month wise bar chart  
ggplot(oldfiled[!is.na(oldfiled$Month),],
       aes(x = as.Date(paste0("2015-",Month,"-01"))
       )) +
  geom_bar(width = 22, stat="count", fill="#2783bd") + 
  labs(title="Patents filed by Month",x="Month") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(limits = c(0,75),expand = c(0, 0)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(axis.title = element_blank(), axis.text=element_text(size=16)) + 
  theme(plot.title = element_text(size = 26))


#Counts over years
ggplot(filed[complete.cases(oldfiled), ],aes(x=Year,color=Level)) +
  geom_line(stat="count", size=2) +
  labs(title="# Patents Filed") + 
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(0,105),expand = c(.02, 0),breaks=seq(0,100,20)) +
  scale_x_continuous(limits = c(2000,2015),expand = c(.02, .08),breaks=seq(2000,2015,3)) +
  theme(axis.title = element_blank(), axis.text=element_text(size=16)) + 
  theme(plot.title = element_text(size = 23)) +
  theme(legend.position=c(0.15, .9), legend.title = element_blank(), legend.text = element_text(size = 16))


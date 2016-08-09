library(dplyr)
library(ggplot2)
library(reshape2)

internet <- read.csv("UNdata.csv",header=TRUE)
int_gp <- internet %>% group_by(Year)
int_gp <- arrange(int_gp, desc(Value))
int_gp <- mutate(int_gp,position_in_year=row_number())

india <- int_gp %>% filter(Value, Country.or.Area=="India")

int_sum <- int_gp %>% summarise(avg_usage = mean (Value), min_usage = min(Value), max_usage = max(Value) , total = n())
india_sum <- inner_join (int_sum,india)

int_sub<-select(india_sum,Year,avg_usage,max_usage,Value)
dfmelt <- melt(int_sub,id.vars="Year")

p <-ggplot(dfmelt,aes(x=Year,y=value,color=variable))+geom_line()+theme_bw()+theme(plot.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+theme(axis.line.x=element_line(color="black"), axis.line.y=element_line(color="black"))+labs(title = "India Vs The world in percentage of internet users \n", x= "Year", y="Population using Internet (%)", color="Internet Usage")+scale_color_manual(labels = c("Avg Usage % - World", "Max Usage % - World", "Usage In India %"), values=c("red","green","blue"))
print(p)

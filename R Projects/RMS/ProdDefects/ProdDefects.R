library(ggplot2)
setwd("C:/RMS Laptop/RMS/Actuos Global/Status Reports")
ProdDefects <- read.csv("SourceProdDefects.csv")

#Remove spaces in the column names
colnames(ProdDefects) <- gsub(" ","",colnames(ProdDefects))
colnames(ProdDefects)

write.csv(ProdDefects,file = "GlobalProdDefects.csv")

 
#Remove records created by US team        
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Holly Justice',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Josh Houghton',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='sean.leadum',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Tim Perdue',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='rajmohann',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Nick Bowman',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='nair.raman',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Balaji Ramamoorthy',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Kyle DeVaney',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='reddy.vatti',]
ProdDefects <- ProdDefects[!ProdDefects$Created.By=='Ryan Kirkland',]
nrow(ProdDefects)
colnames(ProdDefects)

write.csv(ProdDefects,file = "GlobalProdDefects.csv")
StatusTable <-table(ProdDefects$State)
#jpeg(paste0("DefectsByState",Sys.Date(), ".jpg"))
Status = data.frame(StatusTable)

ggplot(Status, aes(x=Var1, y=Freq)) + geom_bar(stat='identity', fill = c("light green","orange","brown","tomato")) +
  geom_text(aes(label=Freq), size=3,position=position_stack(vjust=0.5))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Defects Inflow Vs Outflow By Month")

#bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
#text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
#title(main="Production Defects By State",ylab="Count")
#dev.off()

ProdDefects$LoggedMonth <- format(as.Date(strptime(ProdDefects$Created.Date,"%m/%d/%Y")), "%y-%m")
ProdDefects$ClosedMonth <- format(as.Date(strptime(ProdDefects$Closed.Date,"%m/%d/%Y")), "%y-%m")

ProdDefects$LoggedDate <- format(as.Date(strptime(ProdDefects$Created.Date,"%m/%d/%Y")), "%y-%m-%d")
ProdDefects$ClosedDate <- format(as.Date(strptime(ProdDefects$Closed.Date,"%m/%d/%Y")), "%y-%m-%d")

ProdDefects$LoggedYear <- format(as.Date(strptime(ProdDefects$Created.Date,"%m/%d/%Y")), "%y")

LoggedMonthTable <- table(ProdDefects$LoggedMonth)
LoggedMonth <- as.data.frame(LoggedMonthTable)

ClosedMonthTable <- table(ProdDefects$ClosedMonth)
ClosedMonth <- as.data.frame(ClosedMonthTable)

DefectTable <- merge(LoggedMonth, ClosedMonth, by.x="Var1", by.y="Var1", all = T)
colnames(DefectTable) <- c("Month", "LoggedCount", "ClosedCount")

write.csv(DefectTable, file="defectstable.csv")

DefectFlow = read.csv("defectstable.csv")

#Working code, but using ggplot instead as below
#install.packages("plotly")
# library(plotly)
# p <- plot_ly(DefectTable, x = ~Month, y = ~LoggedCount, type = 'bar', name = 'Logged Date') %>%
#   add_trace(y = ~ClosedCount, name = 'Closed Date') %>%
#   layout(yaxis = list(title = 'Count'), barmode = 'group',
#   annotations = list(text = rownames(data),xanchor = 'center', yanchor = 'bottom', showarrow = FALSE))
# p

library ("ggplot2")

# ggplot(DefectFlow, aes(x=Month, y=LoggedCount)) + geom_bar( stat="identity", position="dodge") + geom_text(aes(label=LoggedCount, y=LoggedCount+0.5),position = position_dodge(0.9), vjust=0.5)

library(reshape2)
myvars = c("LoggedCount","ClosedCount","Month")
DefectFlow = DefectFlow[myvars]

melteddata = melt(DefectFlow, id.vars = 'Month')
jpeg(paste0("DefectInflowVsOutflowByMonth",Sys.Date(), ".jpg"),width = 6, height = 6, units = 'in', res = 360)
ggplot(melteddata, aes(x=Month, y=value, fill=variable)) + geom_bar(stat='identity') +
  geom_text(aes(label=value), size=3,position=position_stack(vjust=0.5))+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Defects Inflow Vs Outflow By Month")
dev.off()


### In-flow vs out-flow By Date ###
ProdDefects = subset ( ProdDefects, ProdDefects$LoggedMonth == "17-09" | ProdDefects$LoggedMonth == "17-10" |  ProdDefects$ClosedMonth == "17-09" | ProdDefects$ClosedMonth == "17-10")

LoggedDateTable <- table(ProdDefects$LoggedDate)
LoggedDate <- as.data.frame(LoggedDateTable)

ClosedDateTable <- table(ProdDefects$ClosedDate)
ClosedDate <- as.data.frame(ClosedDateTable)

DefectDateTable <- merge(LoggedDate, ClosedDate, by.x="Var1", by.y="Var1", all = T)
colnames(DefectDateTable) <- c("Date", "LoggedCount", "ClosedCount")

write.csv(DefectDateTable, file="defectsdatetable.csv")

DefectDateFlow = read.csv("defectsdatetable.csv")

library(reshape2)
myvars = c("LoggedCount","ClosedCount","Date")
DefectDateFlow = DefectDateFlow[myvars]

melteddata = melt(DefectDateFlow, id.vars = 'Date')
jpeg(paste0("DefectInflowVsOutflowByDate",Sys.Date(), ".jpg"),width = 6, height = 6, units = 'in', res = 360)
ggplot(melteddata, aes(x=Date, y=value, fill=variable)) + geom_bar(stat='identity') +
  geom_text(aes(label=value), size=3,position=position_stack(vjust=0.5)) +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Defects Inflow Vs Outflow By Date")
dev.off()


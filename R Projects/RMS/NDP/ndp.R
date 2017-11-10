setwd("C:/RMS Laptop/RMS/NDP")
library(ggplot2)
NDP <- read.csv("NDPData4Sep2017.csv")


NDP = subset(NDP, NDP$Work.Item.Type== "Task" & NDP$Created.By != "Ravichandran Ramachandran" & NDP$Created.By != "Veerappan Palaniappan" & NDP$Assigned.To != "Veerappan Palaniappan"  & NDP$Assigned.To != "Sunilbabu Chimata" )
NDP$Age <- as.numeric(difftime(Sys.Date(),as.Date(strptime(NDP$Created.Date,"%m/%d/%Y")), units="days"))
NDPByTag = subset(NDP, NDP$Work.Item.Type == 'Task')

#Bar Chart by State 
NDPByState = subset(NDP, NDP$State != 'Done')
jpeg(paste0("ByState",Sys.Date(), ".jpg"))
StatusTable <-table(NDPByState$State)
bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
title(main="Defects By State",ylab="Count")
dev.off() 

#Bar Chart By Person By Priority
DevAndQA = c("B.Kabita","Imam Baig","karthikeyanm", "Praneeth Thopram", "Sirisha Parlapalli","Rajakumar Sudalaikan", "Sunilbabu Chimata", "Kalyan.Ramisetti", "Sivashankar Jagadeesan", "v.mohan", "vinothk", "Chris Charlton","Dominik Kouker")
jpeg(paste0("ByPerson",Sys.Date(), ".jpg"))
NDPByUser = subset(NDP, NDP$State != 'Done' & NDP$State != 'Removed' & NDP$Assigned.To %in% DevAndQA)
str(NDPByUser)
write.csv (NDPByUser, file="testing.csv")
NDPByUser = read.csv("testing.csv")
ggplot(NDPByUser, aes(Age, fill=Assigned.To, label = 'Assigned.To')) + geom_histogram(binwidth=1)

counts <- table(NDPByUser$Priority, NDPByUser$Assigned.To)
bp=barplot(counts, main="Defects by person and priority",
           xlab="", col=cm.colors(3),
           legend = rownames(counts), beside=3, las=3)
text(bp,0, round(counts,1),cex=0.8, pos=3)
dev.off()

jpeg(paste0("Age",Sys.Date(), ".jpg"))
hist(NDPByUser$Age, breaks = 10, col = "orange", labels=TRUE) 
dev.off()


#Bar Chart by Priority 
jpeg(paste0("ByPriority",Sys.Date(), ".jpg"))
NDPByPriority = subset(NDP, NDP$State != 'Done' & NDP$State != 'Removed' )
StatusTable <-table(NDPByPriority$Priority)
bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
title(main="Defects by Priority",ylab="Count")
dev.off()

#By Tag
library(stringr)
NDPByTag$Status <- "Blank"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "in development") == 1),"Status"] <- "In dev"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "ready for development") == 1),"Status"] <- "for dev"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "ready for staging deployment") == 1),"Status"] <- "for sta dep"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "ready for staging test") == 1),"Status"] <- "for sta test"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "in staging test") == 1),"Status"] <- "In sta test"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "ready for uat deployment") == 1),"Status"] <- "for uat dep"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "ready for uat test") == 1),"Status"] <- "for uat test"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "ready for production deployment") == 1),"Status"] <- "for prod dep"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "ready for customer uat") == 1),"Status"] <- "for cust uat"
NDPByTag[which(str_count(str_to_lower(str_replace_all(NDPByTag$Tags,"  "," ")), "failed in staging") == 1),"Status"] <- "failed - sta"

jpeg(paste0("ByTag",Sys.Date(), ".jpg"))
StatusTable <-table(NDPByTag$Status)
StatusNames<-unique(NDPByTag$Status)
bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
title(main="PBIs By Status",ylab="Count")
dev.off()

#Clustered Column Chart for Closed and open defects in last 10 days
NDP$ClosedInDays <- difftime(Sys.Date(),as.Date(strptime(NDP$Closed.Date,"%m/%d/%Y")), units="days")



NDP$LoggedDate <- format(as.Date(strptime(NDP$Created.Date,"%m/%d/%Y")), "%y-%m-%d")
NDP$ClosedDate <- format(as.Date(strptime(NDP$Closed.Date,"%m/%d/%Y")), "%y-%m-%d")

NDPLoggedDate <- subset ( NDP, NDP$Age <=10)

LoggedDateTable <- table(NDPLoggedDate$LoggedDate)
LoggedDate <- as.data.frame(LoggedDateTable)

NDPClosedDate <- subset ( NDP, NDP$ClosedInDays <=10)

ClosedDateTable <- table(NDPClosedDate$ClosedDate)
ClosedDate <- as.data.frame(ClosedDateTable)

DefectTable <- merge(LoggedDate, ClosedDate, by.x="Var1", by.y="Var1", all = T)
colnames(DefectTable) <- c("Month", "LoggedCount", "ClosedCount")
DefectTable
#install.packages("plotly")

library(plotly)
p <- plot_ly(DefectTable, x = ~Month, y = ~LoggedCount, type = 'bar', name = 'Logged') %>%
  add_trace(y = ~ClosedCount, name = 'Closed') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group',
         annotations = list(title = 'Count', showarrow = FALSE), barmode='group')
p
  
  


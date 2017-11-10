setwd("C:/RMS Laptop/RMS/Dynamos")
library(ggplot2)
Dynamos <- read.csv("DynamosData7Sep2017.csv")


Dynamos = subset(Dynamos, Dynamos$Work.Item.Type== "Task" & 
               Dynamos$Created.By != "Ravichandran Ramachandran" & 
               Dynamos$Created.By != "Veerappan Palaniappan" & 
               Dynamos$Assigned.To != "Veerappan Palaniappan"  & 
               Dynamos$Assigned.To != "Sunilbabu Chimata" )

Dynamos$Age <- as.numeric(difftime(Sys.Date(),as.Date(strptime(Dynamos$Created.Date,"%m/%d/%Y")), units="days"))
DynamosByTag = subset(Dynamos, Dynamos$Work.Item.Type == 'Task')

#Bar Chart by State 
DynamosByState = subset(Dynamos, Dynamos$State != 'Done')
jpeg(paste0("ByState",Sys.Date(), ".jpg"))
StatusTable <-table(DynamosByState$State)
bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
title(main="Defects By State",ylab="Count")
dev.off() 

#Bar Chart By Person By Priority
DevAndQA = c("B.Kabita","Imam Baig","karthikeyanm", "Praneeth Thopram", "Sirisha Parlapalli","Rajakumar Sudalaikan", "Sunilbabu Chimata", "Kalyan.Ramisetti", "Sivashankar Jagadeesan", "v.mohan", "vinothk", "Chris Charlton","Dominik Kouker")
jpeg(paste0("ByPerson",Sys.Date(), ".jpg"))
DynamosByUser = subset(Dynamos, Dynamos$State != 'Done' & Dynamos$State != 'Removed' & Dynamos$Assigned.To %in% DevAndQA)
str(DynamosByUser)
write.csv (DynamosByUser, file="testing.csv")
DynamosByUser = read.csv("testing.csv")
ggplot(DynamosByUser, aes(Age, fill=Assigned.To, label = 'Assigned.To')) + geom_histogram(binwidth=1)

counts <- table(DynamosByUser$Priority, DynamosByUser$Assigned.To)
bp=barplot(counts, main="Defects by person and priority",
           xlab="", col=cm.colors(3),
           legend = rownames(counts), beside=3, las=3)
text(bp,0, round(counts,1),cex=0.8, pos=3)
dev.off()

jpeg(paste0("Age",Sys.Date(), ".jpg"))
hist(DynamosByUser$Age, breaks = 10, col = "orange", labels=TRUE) 
dev.off()


#Bar Chart by Priority 
jpeg(paste0("ByPriority",Sys.Date(), ".jpg"))
DynamosByPriority = subset(Dynamos, Dynamos$State != 'Done' & Dynamos$State != 'Removed' )
StatusTable <-table(DynamosByPriority$Priority)
bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
title(main="Defects by Priority",ylab="Count")
dev.off()

#By Tag
library(stringr)
DynamosByTag$Status <- "Blank"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "in development") == 1),"Status"] <- "In dev"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "ready for development") == 1),"Status"] <- "for dev"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "ready for staging deployment") == 1),"Status"] <- "for sta dep"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "ready for staging test") == 1),"Status"] <- "for sta test"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "in staging test") == 1),"Status"] <- "In sta test"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "ready for uat deployment") == 1),"Status"] <- "for uat dep"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "ready for uat test") == 1),"Status"] <- "for uat test"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "ready for production deployment") == 1),"Status"] <- "for prod dep"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "ready for customer uat") == 1),"Status"] <- "for cust uat"
DynamosByTag[which(str_count(str_to_lower(str_replace_all(DynamosByTag$Tags,"  "," ")), "failed in staging") == 1),"Status"] <- "failed - sta"

jpeg(paste0("ByTag",Sys.Date(), ".jpg"))
StatusTable <-table(DynamosByTag$Status)
StatusNames<-unique(DynamosByTag$Status)
bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
title(main="PBIs By Status",ylab="Count")
dev.off()

#Clustered Column Chart for Closed and open defects in last 10 days
Dynamos$ClosedInDays <- difftime(Sys.Date(),as.Date(strptime(Dynamos$Closed.Date,"%m/%d/%Y")), units="days")



Dynamos$LoggedDate <- format(as.Date(strptime(Dynamos$Created.Date,"%m/%d/%Y")), "%y-%m-%d")
Dynamos$ClosedDate <- format(as.Date(strptime(Dynamos$Closed.Date,"%m/%d/%Y")), "%y-%m-%d")

DynamosLoggedDate <- subset ( Dynamos, Dynamos$Age <=10)

LoggedDateTable <- table(DynamosLoggedDate$LoggedDate)
LoggedDate <- as.data.frame(LoggedDateTable)

DynamosClosedDate <- subset ( Dynamos, Dynamos$ClosedInDays <=10)

ClosedDateTable <- table(DynamosClosedDate$ClosedDate)
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




setwd("C:/RMS Laptop/RMS/NDP")

# NDP19Jun <- read.csv("NDPData19Jun2017.csv")
# NDP19Jun$Date = Sys.Date()-11
# NDP20Jun <- read.csv("NDPData20Jun2017.csv")
# NDP20Jun$Date = Sys.Date()-10
# NDP21Jun <- read.csv("NDPData21Jun2017.csv")
# NDP21Jun$Date = Sys.Date()-9
# NDP22Jun <- read.csv("NDPData22Jun2017.csv")
# NDP22Jun$Date = Sys.Date()-8
# NDP23Jun <- read.csv("NDPData23Jun2017.csv")
# NDP23Jun$Date = Sys.Date()-7
# NDP27Jun <- read.csv("NDPData27Jun2017.csv")
# NDP27Jun$Date = Sys.Date()-6
# NDP28Jun <- read.csv("NDPData28Jun2017.csv")
# NDP28Jun$Date = Sys.Date()-5
# NDP29Jun <- read.csv("NDPData29Jun2017.csv")
# NDP29Jun$Date = Sys.Date()-4
# NDP30Jun <- read.csv("NDPData30Jun2017.csv")
# NDP30Jun$Date = Sys.Date()-3
NDP11Jul <- read.csv("NDPData11Jul2017.csv")
NDP11Jul$Date = Sys.Date()-1
NDPCurrent <- read.csv("NDPData12Jul2017.csv")
NDPCurrent$Date = Sys.Date()



# NDPOld = read.csv("Daywisepriority.csv")
# NDPOld[1] = NULL
# NDPOld[,NDPOld$Date] <- as.Date(NDPOld[,c(20)], format = "%m-%d-%Y")
# str(NDPOld)


# NDP = rbind (NDPOld, NDPCurrent)
NDP = rbind(NDP11Jul, NDPCurrent)
NDPWhole = subset(NDP, NDP$Priority == 2 & NDP$State !='Done' & NDP$State != 'Removed' & NDP$Work.Item.Type=='Task' & NDP$Created.By != "Ravichandran Ramachandran")
write.csv(NDPWhole, "Daywisepriority.csv" )

#StatusTable = table(NDPWhole$Date,NDPWhole$Priority)
jpeg("PriorityCountDayWise.jpg")
StatusTable <-table(NDPWhole$Date)
StatusTable
bp <- barplot(StatusTable, width=.4,col=terrain.colors(7),las=3,beside=3)
text(bp,0, round(StatusTable,1),cex=0.8, pos=3)
title(main="Priority 2 Issues Day-wise",ylab="Count")
dev.off()


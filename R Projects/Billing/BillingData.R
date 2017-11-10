# Requirements
# 1. P and L

#install.packages("readxl")
library("readxl")
library("ggplot2")
# PENDING Issues
#1 Include Pydi's UK invoices - later as this is not required for GM
#2 Senthil, Kirthi, Pydi - Pay is duplicated - done
#3 Update exchange rate excel sheet with correct values
#4 Projection for rest of the year

finyear201718 = c('17-04','17-05','17-06','17-07','17-08','17-09','17-10',
                  '17-11','17-12','18-01','18-02','18-03')

finyear201617 = c('16-04','16-05','16-06','16-07','16-08','16-09','16-10',
                  '16-11','16-12','17-01','17-02','17-03')

#Function to copy records from one month and create new set of records for forecasting P and L for rest of the year
CreateRecordsFromCurrentMonth <- function (FromMonth, ToMonth){
  CurrentMonthRecords = billingdata[which(billingdata$MonthYear==FromMonth),]
  CurrentMonthRecords$MonthYear = ToMonth
  CurrentMonthRecords[which (CurrentMonthRecords$MonthYear == ToMonth),"BillableHours"] = 160
  CurrentMonthRecords[which (CurrentMonthRecords$MonthYear == ToMonth & CurrentMonthRecords$MonthYear == "17-09"),"BillableHours"] = 152
  CurrentMonthRecords[which (CurrentMonthRecords$MonthYear == ToMonth & CurrentMonthRecords$MonthYear == "17-12"),"BillableHours"] = 152
  CurrentMonthRecords[which (CurrentMonthRecords$MonthYear == ToMonth & CurrentMonthRecords$MonthYear == "18-01"),"BillableHours"] = 155
  CurrentMonthRecords[which (CurrentMonthRecords$MonthYear == ToMonth & CurrentMonthRecords$MonthYear == "18-02"),"BillableHours"] = 153
  CurrentMonthRecords[which (CurrentMonthRecords$MonthYear == ToMonth & CurrentMonthRecords$MonthYear == "18-03"),"BillableHours"] = 166
  CurrentMonthRecords$BillingAmount = as.numeric(CurrentMonthRecords$BillableHours) * as.numeric(CurrentMonthRecords$BillingRate)
  
  return (CurrentMonthRecords)
  
}

#Function to Load the data from three billing sheets to a single data frame
GetMonthlyBillingData <- function (MonYr,PSFileName, MBODFileName, RMSFileName, FilePath){
  setwd (FilePath)
  
  PSbillingdata <- read_excel(PSFileName, sheet = "Product Engineering")
  MBODbillingdata <- read_excel(MBODFileName, sheet = "MBOD")
  RMSbillingdata <- read_excel(RMSFileName, sheet = "Product Engineering")
  
#Discard all the columns except first 10 coulmns
  PSbillingdata <- PSbillingdata [,c(1:10)]
  MBODbillingdata <- MBODbillingdata [,c(1:10)]
  RMSbillingdata <- RMSbillingdata [,c(1:10)]

  billingdata <- rbind(PSbillingdata, MBODbillingdata, RMSbillingdata)
#Remove the line with total which has NA in most of the columns
  billingdata <- billingdata [!is.na(billingdata$SlNo),]
  billingdata$InsertedMonYr = MonYr
  
  return (billingdata)
}

#Load monthly billing data
billingdataJan2016 <- GetMonthlyBillingData ("16-01",'RMS-Billing-January -16 - Production Support.xlsx','RMS-Billing-January -16 - MBOD.xlsx','RMS-Billing-January -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-01-2016')
billingdataFeb2016 <- GetMonthlyBillingData ("16-02","RMS-Billing-February -16 - Production Support.xlsx","RMS-Billing-February-16 - MBOD.xlsx","RMS-Billing-February -16- RMS.xlsx","C:/RMS Laptop/Billing/RMS/2016/31-02-2016")
billingdataMar2016 <- GetMonthlyBillingData ("16-03","RMS-Billing-March -16 - Production Support.xlsx","RMS-Billing-March-16 - MBOD.xlsx","RMS-Billing-March -16- RMS.xlsx","C:/RMS Laptop/Billing/RMS/2016/31-03-2016")
billingdataApr2016 <- GetMonthlyBillingData ("16-04","RMS-Billing-April -16 - Production Support.xlsx","RMS-Billing-April-16 - MBOD.xlsx","RMS-Billing-April -16- RMS.xlsx","C:/RMS Laptop/Billing/RMS/2016/31-04-2016")
billingdataMay2016 <- GetMonthlyBillingData ("16-05",'RMS-Billing-May -16 - Production Support.xlsx','RMS-Billing-May-16 - MBOD.xlsx','RMS-Billing-May -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-05-2016')
billingdataJun2016 <- GetMonthlyBillingData ("16-06",'RMS-Billing-June -16 - Production Support.xlsx','RMS-Billing-June-16 - MBOD.xlsx','RMS-Billing-June -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-06-2016')
billingdataJul2016 <- GetMonthlyBillingData ("16-07",'RMS-Billing-July -16 - Production Support.xlsx','RMS-Billing-July-16 - MBOD.xlsx','RMS-Billing-July -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-07-2016')
billingdataAug2016 <- GetMonthlyBillingData ("16-08",'RMS-Billing-August -16 - Production Support.xlsx','RMS-Billing-Augus-16 - MBOD.xlsx','RMS-Billing-August -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-08-2016')
billingdataSep2016 <- GetMonthlyBillingData ("16-09",'RMS-Billing-September-16 - Production Support.xlsx','RMS-Billing-September-16 - MBOD.xlsx','RMS-Billing-September -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-09-2016')
billingdataOct2016 <- GetMonthlyBillingData ("16-10",'RMS-Billing-October-16 - Production Support.xlsx','RMS-Billing-October-16 - MBOD.xlsx','RMS-Billing-October -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-10-2016')
billingdataNov2016 <- GetMonthlyBillingData ("16-11",'RMS-Billing-November-16 - Production Support.xlsx','RMS-Billing-November-16 - MBOD.xlsx','RMS-Billing-November -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-11-2016')
billingdataDec2016 <- GetMonthlyBillingData ("16-12",'RMS-Billing-December-16 - Production Support.xlsx','RMS-Billing-December-16 - MBOD.xlsx','RMS-Billing-December -16- RMS.xlsx','C:/RMS Laptop/Billing/RMS/2016/31-12-2016')

billingdataJan2017 <- GetMonthlyBillingData ("17-01",'RMS-Billing-Jan17-ProdSupport.xlsx','RMS-Billing-Jan17-MBOD.xlsx','RMS-Billing-Jan17-RMS.xlsx','C:/RMS Laptop/Billing/RMS/2017/31-01-2017')
billingdataFeb2017 <- GetMonthlyBillingData ("17-02",'RMS-Billing-Feb17-ProdSupport.xlsx','RMS-Billing-Feb17-MBOD.xlsx','RMS-Billing-Feb17-RMS.xlsx','C:/RMS Laptop/Billing/RMS/2017/31-02-2017')
billingdataMar2017 <- GetMonthlyBillingData ("17-03",'RMS-Billing-Mar17-ProdSupport.xlsx','RMS-Billing-Mar17-MBOD.xlsx','RMS-Billing-Mar17-RMS.xlsx','C:/RMS Laptop/Billing/RMS/2017/31-03-2017')
billingdataApr2017 <- GetMonthlyBillingData ("17-04",'RMS-Billing-Apr17-ProdSupport.xlsx','RMS-Billing-Apr17-MBOD.xlsx','RMS-Billing-Apr17-RMS.xlsx','C:/RMS Laptop/Billing/RMS/2017/31-04-2017')
billingdataMay2017 <- GetMonthlyBillingData ("17-05",'RMS-Billing-May17-ProdSupport.xlsx','RMS-Billing-May17-MBOD.xlsx','RMS-Billing-May17-RMS.xlsx','C:/RMS Laptop/Billing/RMS/2017/31-05-2017')
billingdataJun2017 <- GetMonthlyBillingData ("17-06",'RMS-Billing-Jun17-ProdSupport.xlsx','RMS-Billing-Jun17-MBOD.xlsx','RMS-Billing-Jun17-RMS.xlsx','C:/RMS Laptop/Billing/RMS/2017/31-06-2017')
billingdataJul2017 <- GetMonthlyBillingData ("17-07",'RMS-Billing-Jul17-ProdSupport.xlsx','RMS-Billing-Jul17-MBOD.xlsx','RMS-Billing-Jul17-RMS.xlsx','C:/RMS Laptop/Billing/RMS/2017/31-07-2017')


#Load all the months into a single data frame
setwd("C:/RMS Laptop/Billing/RMS/PandLProgram/")

billingdata <- rbind (billingdataApr2016, 
                      billingdataMay2016, 
                      billingdataJun2016, 
                      billingdataJul2016, 
                      billingdataAug2016, 
                      billingdataSep2016, 
                      billingdataOct2016, 
                      billingdataNov2016, 
                      billingdataDec2016,
                      billingdataJan2017,
                      billingdataFeb2017,
                      billingdataMar2017,
                      billingdataApr2017,
                      billingdataMay2017,
                      billingdataJun2017,
                      billingdataJul2017)



#Remove spaces in the column names
colnames(billingdata) <- gsub(" ","",colnames(billingdata))


#Add a column in 'yy-mm' format
billingdata$MonthYear = format(as.Date(billingdata$From, origin = "1899-12-30"), "%y-%m")

#ifelse(billingdata$MonthYear != billingdata$InsertedMonYr,"Error","Ok")

#Exception handling - When resources worked in US as well in India, cost needs to be nullified in one line
#Kirthi, Senthil and Pydi spent time in US for two months
billingdata[which(billingdata$MonthYear %in% c('17-02', '17-04') & billingdata$Name == 'Kirthivasan Venkateswaran (India)'),"Pay201718"] = 0
billingdata[which(billingdata$MonthYear %in% c('17-03', '17-04') & billingdata$Name == 'Senthil Kumar Ramasamy (India)'),"Pay201718"] = 0

billingdata[which(billingdata$MonthYear == '16-08' & billingdata$Name == 'Pydi Vengamamba'),"Pay201617"] = billingdata[which(billingdata$MonthYear == '16-08' & billingdata$Name == 'Pydi Vengamamba'),"Pay201617"] / 2
billingdata[which(billingdata$MonthYear == '16-09' & billingdata$Name == 'Pydi Vengamamba'),"Pay201617"] = billingdata[which(billingdata$MonthYear == '16-09' & billingdata$Name == 'Pydi Vengamamba'),"Pay201617"] / 3


#Copy the current month record for rest of the year with hours from prev year
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "17-08")
billingdata = rbind(billingdata, CurrentMonthRecords)
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "17-09")
billingdata = rbind(billingdata, CurrentMonthRecords)
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "17-10")
billingdata = rbind(billingdata, CurrentMonthRecords)
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "17-11")
billingdata = rbind(billingdata, CurrentMonthRecords)
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "17-12")
billingdata = rbind(billingdata, CurrentMonthRecords)
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "18-01")
billingdata = rbind(billingdata, CurrentMonthRecords)
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "18-02")
billingdata = rbind(billingdata, CurrentMonthRecords)
CurrentMonthRecords = CreateRecordsFromCurrentMonth ("17-07", "18-03")
billingdata = rbind(billingdata, CurrentMonthRecords)


#Load cost of the team members
cost = read.csv("RMSctc.csv")
#load the exchange rate
exrate = read.csv("ExRate.csv")

#merge main data frame billing data with  exchange rate and team members cost 
billingdata <- merge(billingdata, cost, by.x="Name", by.y="Name", all.x = T)
billingdata <- merge(billingdata, exrate, by.x="MonthYear", by.y="MonthYear", all.x = T)

#Convert CTC from INR to USD
billingdata[which(billingdata$MonthYear %in% finyear201718),
            "CTCUSD"] = billingdata[(billingdata$MonthYear %in% finyear201718),"Pay201718"]/
  billingdata[(billingdata$MonthYear %in% finyear201718),"ExRate"]

billingdata[which(billingdata$MonthYear %in% finyear201617),
            "CTCUSD"] = billingdata[(billingdata$MonthYear %in% finyear201617),"Pay201617"]/
  billingdata[(billingdata$MonthYear %in% finyear201617),"ExRate"]

#Hard coded the pay for US employees
billingdata[which(billingdata$Name=="Veerappan Palaniappan" & billingdata$MonthYear 
                  %in% finyear201718),
            "CTCUSD"] = 12001
billingdata[which(billingdata$Name=="Veerappan Palaniappan" & billingdata$MonthYear 
                  %in% finyear201617),
            "CTCUSD"] = 11521
billingdata[which(billingdata$Name=="Sabarish Thirumoorthy" & billingdata$MonthYear 
                  %in% finyear201718),
            "CTCUSD"] = 9293
billingdata[which(billingdata$Name=="Sabarish Thirumoorthy" & billingdata$MonthYear 
                  %in% finyear201617),
            "CTCUSD"] = 8804

billingdata$Margin = billingdata$BillingAmount - billingdata$CTCUSD

write.csv(billingdata,"BillingData2016-17.csv")

write.csv(billingdata,paste0("PandL-RMSData",Sys.Date(), ".csv"))


findaverage = subset ( billingdata, !(billingdata$Name %in% c("Kirthivasan Venkateswaran","Kirthivasan Venkateswaran (India)","Kirthivasan Venkateswaran (Atlanta - US)",
                                                           "Senthil Kumar Ramasamy", "Senthil Kumar Ramasamy (Atlanta - US)","Senthil Kumar Ramasamy (India)",
                                                           "Pydi Vengamamba")) & billingdata$BillableHours > 100  )


tapply(findaverage$BillableHours,findaverage$MonthYear, FUN=mean)

Margin = aggregate ((BillingAmount-CTCUSD)~MonthYear, data=billingdata,FUN = sum)
colnames(Margin)[2] = "Margin"

Revenue = aggregate (BillingAmount~MonthYear, data=billingdata,FUN = sum)
PandL = merge(Margin, Revenue, by.x = "MonthYear", by.y = "MonthYear")
PandL$GrossMargin = PandL[,"Margin"]/PandL[,"BillingAmount"]*100
PandL

jpeg(paste0("PandL-RMS",Sys.Date(), ".jpg"))
ggplot(PandL, aes(x=MonthYear, y=GrossMargin))+geom_bar(stat="identity",fill="orange")+
geom_text (aes(label=round(as.numeric(GrossMargin),2)),vjust=-0.4) +
  ylab("RMS P and L") +
  theme(axis.title.x= element_blank(),axis.text.x = element_text(angle=45,hjust=1))
dev.off()

#load required libraries
library("lubridate")
library("ggplot2")


#set working directory
setwd("C:/Veera/R/SmartParking/From Australia/SmartParking")
# F010116_310116 = read.csv('010116_310116.csv')
# F010117_310117 = read.csv('010117_310117.csv')
# F010216_290216 = read.csv('010216_290216.csv')
# F010217_280217 = read.csv('010217_280217.csv')
# F010316_310316 = read.csv('010316_310316.csv')
# F010317_310317 = read.csv('010317_310317.csv')
# F010416_300416 = read.csv('010416_300416.csv')
# F010417_300417 = read.csv('010417_300417.csv')
# F010516_310516 = read.csv('010516_310516.csv')
# F010517_310517 = read.csv('010517_310517.csv')
# F010616_300616 = read.csv('010616_300616.csv')
# F010617_300617 = read.csv('010617_300617.csv')
# F010716_310716 = read.csv('010716_310716.csv')
# F010717_100717 = read.csv('010717_100717.csv')
# F010816_310816 = read.csv('010816_310816.csv')
# F010916_300916 = read.csv('010916_300916.csv')
# F011016_311016 = read.csv('011016_311016.csv')
# F011116_301116 = read.csv('011116_301116.csv')
# F011216_311216 = read.csv('011216_311216.csv')
# F121115_301115 = read.csv('121115_301115.csv')
# F2017_07_12_20_00_17 = read.csv('AU(ACT)_StayReport2017-07-12_20-00-17.csv')
# F2017_07_12_7_04_31 = read.csv('AU(ACT)_StayReport2017-07-12_7-04-31.csv')
# F2017_07_13_20_01_15 = read.csv('AU(ACT)_StayReport2017-07-13_20-01-15.csv')
# F2017_07_14_20_01_39 = read.csv('AU(ACT)_StayReport2017-07-14_20-01-39.csv')
# F2017_07_15_20_01_40 = read.csv('AU(ACT)_StayReport2017-07-15_20-01-40.csv')
# F2017_07_16_20_01_41 = read.csv('AU(ACT)_StayReport2017-07-16_20-01-41.csv')
# F2017_07_17_20_01_40 = read.csv('AU(ACT)_StayReport2017-07-17_20-01-40.csv')
# F2017_07_18_20_00_17 = read.csv('AU(ACT)_StayReport2017-07-18_20-00-17.csv')
# F2017_07_19_20_00_18 = read.csv('AU(ACT)_StayReport2017-07-19_20-00-18.csv')
# F2017_07_20_20_01_17 = read.csv('AU(ACT)_StayReport2017-07-20_20-01-17.csv')
# F2017_07_21_20_00_17 = read.csv('AU(ACT)_StayReport2017-07-21_20-00-17.csv')
# F2017_07_22_20_01_14 = read.csv('AU(ACT)_StayReport2017-07-22_20-01-14.csv')
# F2017_07_23_20_01_15 = read.csv('AU(ACT)_StayReport2017-07-23_20-01-15.csv')
# F2017_07_24_20_01_16 = read.csv('AU(ACT)_StayReport2017-07-24_20-01-16.csv')
# F2017_07_25_20_00_17 = read.csv('AU(ACT)_StayReport2017-07-25_20-00-17.csv')
# F2017_07_26_20_01_43 = read.csv('AU(ACT)_StayReport2017-07-26_20-01-43.csv')
# F2017_07_27_20_00_17 = read.csv('AU(ACT)_StayReport2017-07-27_20-00-17.csv')
# F2017_07_28_20_00_18 = read.csv('AU(ACT)_StayReport2017-07-28_20-00-18.csv')
# F2017_07_29_20_01_41 = read.csv('AU(ACT)_StayReport2017-07-29_20-01-41.csv')
# F2017_07_30_20_01_40 = read.csv('AU(ACT)_StayReport2017-07-30_20-01-40.csv')
# F2017_07_31_20_01_15 = read.csv('AU(ACT)_StayReport2017-07-31_20-01-15.csv')
# F2017_08_01_20_00_18 = read.csv('AU(ACT)_StayReport2017-08-01_20-00-18.csv')
# F2017_08_02_20_00_17 = read.csv('AU(ACT)_StayReport2017-08-02_20-00-17.csv')
# F2017_08_03_20_00_17 = read.csv('AU(ACT)_StayReport2017-08-03_20-00-17.csv')
# F2017_08_04_20_01_41 = read.csv('AU(ACT)_StayReport2017-08-04_20-01-41.csv')
# F2017_08_05_20_01_15 = read.csv('AU(ACT)_StayReport2017-08-05_20-01-15.csv')
# F2017_08_06_20_00_18 = read.csv('AU(ACT)_StayReport2017-08-06_20-00-18.csv')
# F2017_08_07_20_01_15 = read.csv('AU(ACT)_StayReport2017-08-07_20-01-15.csv')
# F2017_08_08_20_01_44 = read.csv('AU(ACT)_StayReport2017-08-08_20-01-44.csv')
# F2017_08_09_20_00_17 = read.csv('AU(ACT)_StayReport2017-08-09_20-00-17.csv')
# F2017_08_10_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-10_20-00-16.csv')
# F2017_08_11_20_00_18 = read.csv('AU(ACT)_StayReport2017-08-11_20-00-18.csv')
# F2017_08_12_20_01_15 = read.csv('AU(ACT)_StayReport2017-08-12_20-01-15.csv')
# F2017_08_13_20_01_16 = read.csv('AU(ACT)_StayReport2017-08-13_20-01-16.csv')
# F2017_08_14_20_00_17 = read.csv('AU(ACT)_StayReport2017-08-14_20-00-17.csv')
# F2017_08_15_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-15_20-00-16.csv')
# F2017_08_16_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-16_20-00-16.csv')
# F2017_08_17_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-17_20-00-16.csv')
# F2017_08_18_20_01_41 = read.csv('AU(ACT)_StayReport2017-08-18_20-01-41.csv')
# F2017_08_19_20_01_42 = read.csv('AU(ACT)_StayReport2017-08-19_20-01-42.csv')
# F2017_08_20_20_00_17 = read.csv('AU(ACT)_StayReport2017-08-20_20-00-17.csv')
# F2017_08_21_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-21_20-00-16.csv')
# F2017_08_22_20_00_17 = read.csv('AU(ACT)_StayReport2017-08-22_20-00-17.csv')
# F2017_08_23_20_01_16 = read.csv('AU(ACT)_StayReport2017-08-23_20-01-16.csv')
# F2017_08_24_20_00_19 = read.csv('AU(ACT)_StayReport2017-08-24_20-00-19.csv')
# F2017_08_25_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-25_20-00-16.csv')
# F2017_08_26_20_00_18 = read.csv('AU(ACT)_StayReport2017-08-26_20-00-18.csv')
# F2017_08_27_20_01_42 = read.csv('AU(ACT)_StayReport2017-08-27_20-01-42.csv')
# F2017_08_28_20_01_42 = read.csv('AU(ACT)_StayReport2017-08-28_20-01-42.csv')
# F2017_08_29_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-29_20-00-16.csv')
# F2017_08_30_20_01_43 = read.csv('AU(ACT)_StayReport2017-08-30_20-01-43.csv')
# F2017_08_31_20_01_45 = read.csv('AU(ACT)_StayReport2017-08-31_20-01-45.csv')
# F2017_09_01_20_01_16 = read.csv('AU(ACT)_StayReport2017-09-01_20-01-16.csv')
# F2017_09_02_20_01_15 = read.csv('AU(ACT)_StayReport2017-09-02_20-01-15.csv')
# F2017_09_03_20_00_17 = read.csv('AU(ACT)_StayReport2017-09-03_20-00-17.csv')
# F2017_09_04_20_01_16 = read.csv('AU(ACT)_StayReport2017-09-04_20-01-16.csv')
# F2017_09_05_20_00_17 = read.csv('AU(ACT)_StayReport2017-09-05_20-00-17.csv')
# F2017_09_06_20_01_42 = read.csv('AU(ACT)_StayReport2017-09-06_20-01-42.csv')
# F2017_09_07_20_01_18 = read.csv('AU(ACT)_StayReport2017-09-07_20-01-18.csv')
# F2017_09_08_20_00_17 = read.csv('AU(ACT)_StayReport2017-09-08_20-00-17.csv')
# F2017_09_09_20_00_18 = read.csv('AU(ACT)_StayReport2017-09-09_20-00-18.csv')
# F2017_09_10_20_01_16 = read.csv('AU(ACT)_StayReport2017-09-10_20-01-16.csv')
# F2017_09_11_20_01_40 = read.csv('AU(ACT)_StayReport2017-09-11_20-01-40.csv')


# 
# Stays=rbind(
#   F010116_310116,
#   F010117_310117,
#   F010216_290216,
#   F010217_280217,
#   F010316_310316,
#   F010317_310317,
#   F010416_300416,
#   F010417_300417,
#   F010516_310516,
#   F010517_310517,
#   F010616_300616,
#   F010617_300617,
#   F010716_310716,
#   F010717_100717,
#   F010816_310816,
#   F010916_300916,
#   F011016_311016,
#   F011116_301116,
#   F011216_311216,
#   F121115_301115,
#   F2017_07_12_20_00_17,
#   F2017_07_12_7_04_31,
#   F2017_07_13_20_01_15,
#   F2017_07_14_20_01_39,
#   F2017_07_15_20_01_40,
#   F2017_07_16_20_01_41,
#   F2017_07_17_20_01_40,
#   F2017_07_18_20_00_17,
#   F2017_07_19_20_00_18,
#   F2017_07_20_20_01_17,
#   F2017_07_21_20_00_17,
#   F2017_07_22_20_01_14,
#   F2017_07_23_20_01_15,
#   F2017_07_24_20_01_16,
#   F2017_07_25_20_00_17,
#   F2017_07_26_20_01_43,
#   F2017_07_27_20_00_17,
#   F2017_07_28_20_00_18,
#   F2017_07_29_20_01_41,
#   F2017_07_30_20_01_40,
#   F2017_07_31_20_01_15,
#   F2017_08_01_20_00_18,
#   F2017_08_02_20_00_17,
#   F2017_08_03_20_00_17,
#   F2017_08_04_20_01_41,
#   F2017_08_05_20_01_15,
#   F2017_08_06_20_00_18,
#   F2017_08_07_20_01_15,
#   F2017_08_08_20_01_44,
#   F2017_08_09_20_00_17,
#   F2017_08_10_20_00_16,
#   F2017_08_11_20_00_18,
#   F2017_08_12_20_01_15,
#   F2017_08_13_20_01_16,
#   F2017_08_14_20_00_17,
#   F2017_08_15_20_00_16,
#   F2017_08_16_20_00_16,
#   F2017_08_17_20_00_16,
#   F2017_08_18_20_01_41,
#   F2017_08_19_20_01_42,
#   F2017_08_20_20_00_17,
#   F2017_08_21_20_00_16,
#   F2017_08_22_20_00_17,
#   F2017_08_23_20_01_16,
#   F2017_08_24_20_00_19,
#   F2017_08_25_20_00_16,
#   F2017_08_26_20_00_18,
#   F2017_08_27_20_01_42,
#   F2017_08_28_20_01_42,
#   F2017_08_29_20_00_16,
#   F2017_08_30_20_01_43,
#   F2017_08_31_20_01_45,
#   F2017_09_01_20_01_16,
#   F2017_09_02_20_01_15,
#   F2017_09_03_20_00_17,
#   F2017_09_04_20_01_16,
#   F2017_09_05_20_00_17,
#   F2017_09_06_20_01_42,
#   F2017_09_07_20_01_18,
#   F2017_09_08_20_00_17,
#   F2017_09_09_20_00_18,
#   F2017_09_10_20_01_16,
#   F2017_09_11_20_01_40
# )

F2017_08_29_20_00_16 = read.csv('AU(ACT)_StayReport2017-08-29_20-00-16.csv')
F2017_08_30_20_01_43 = read.csv('AU(ACT)_StayReport2017-08-30_20-01-43.csv')
F2017_08_31_20_01_45 = read.csv('AU(ACT)_StayReport2017-08-31_20-01-45.csv')
F2017_09_01_20_01_16 = read.csv('AU(ACT)_StayReport2017-09-01_20-01-16.csv')
F2017_09_02_20_01_15 = read.csv('AU(ACT)_StayReport2017-09-02_20-01-15.csv')
F2017_09_03_20_00_17 = read.csv('AU(ACT)_StayReport2017-09-03_20-00-17.csv')
F2017_09_04_20_01_16 = read.csv('AU(ACT)_StayReport2017-09-04_20-01-16.csv')
F2017_09_05_20_00_17 = read.csv('AU(ACT)_StayReport2017-09-05_20-00-17.csv')
F2017_09_06_20_01_42 = read.csv('AU(ACT)_StayReport2017-09-06_20-01-42.csv')
F2017_09_07_20_01_18 = read.csv('AU(ACT)_StayReport2017-09-07_20-01-18.csv')
F2017_09_08_20_00_17 = read.csv('AU(ACT)_StayReport2017-09-08_20-00-17.csv')
F2017_09_09_20_00_18 = read.csv('AU(ACT)_StayReport2017-09-09_20-00-18.csv')
F2017_09_10_20_01_16 = read.csv('AU(ACT)_StayReport2017-09-10_20-01-16.csv')
F2017_09_11_20_01_40 = read.csv('AU(ACT)_StayReport2017-09-11_20-01-40.csv')

Stays=rbind(
  F2017_08_29_20_00_16,
  F2017_08_30_20_01_43,
  F2017_08_31_20_01_45,
  F2017_09_01_20_01_16,
  F2017_09_02_20_01_15,
  F2017_09_03_20_00_17,
  F2017_09_04_20_01_16,
  F2017_09_05_20_00_17,
  F2017_09_06_20_01_42,
  F2017_09_07_20_01_18,
  F2017_09_08_20_00_17,
  F2017_09_09_20_00_18,
  F2017_09_10_20_01_16,
  F2017_09_11_20_01_40
)

str(Stays)


#set working directory
setwd("C:/Veera/R/SmartParking")

#load smart parking lots data
Lots = read.csv('Smart_Parking_Lots.csv')

#load smart parking restrictions data
Restrictions = read.csv("Smart_Parking_Restrictions.csv")

#load smart parking stays data - this file is from website - does not have all days
#StaysSample = read.csv("Smart_Parking_Stays.csv")

#table(StaysSample$SectorName, StaysSample$Street)

str(Lots)
str(Restrictions)
str(StaysSample)
table(Restrictions$Street)

nrow (Restrictions)
nrow(Lots)

LotRestrictions = merge ( Lots, Restrictions, by.x = 'LotCode', by.y = 'LotCode', all = T)
write.csv(LotRestrictions, file = "LotRestrictions.csv")
temp = Stays
#temp = Stays [1:20000,]

Stays$end = ymd_hms(strptime(Stays$Departed,"%Y/%m/%d %H:%M:%S"))
Stays$begin = ymd_hms(strptime(Stays$Arrived,"%Y/%m/%d %H:%M:%S"))
Stays$parkedtime = as.numeric(as.duration(as.interval(Stays$begin, Stays$end)),"minutes")

temp$Month = month(temp$Departed)

#add the hour column, this is required to create bins
temp$hour = hour(temp$Arrived)
#create 6 bins for the bar chart
temp = data.frame(temp, bins=cut(temp$hour  , breaks=6, labels=FALSE))

#To have the labels meaning, create meaning labels for each bin
temp$label[temp$bins == 1] = "0:00-3:00"
temp$label[temp$bins == 2] = "4:00-7:00"
temp$label[temp$bins == 3] = "8:00-11:00"
temp$label[temp$bins == 4] = "12:00-15:00"
temp$label[temp$bins == 5] = "16:00-19:00"
temp$label[temp$bins == 6] = "20:00-23:00"

#Order the bar based on the label
temp$label <- factor(temp$label, levels = temp$label[order(temp$bins)])
write.csv(temp, file="test.csv")

###############  1 - Stays by hour  ###################
jpeg(paste0("Stays By Hours Of The Day.jpg"))
ggplot(temp, aes(x=label)) +
  geom_bar(stat='count', position='dodge', fill='light green') +
  labs(x="Hours of the day",y="No. of Stays") + 
  #geom_text(aes(label=label)) +
  theme_classic()
dev.off()



#extract Lot no from Lotname field 
temp$LotNo = gsub(' .*','',temp$LotName)

#merge Stays with Restrictions dataset
full = merge(temp, LotRestrictions, by.x = 'LotNo', by.y='LotCode', all.x=T)
write.csv(full,"test.csv")
# Q12
table(full$BayType.x,full$BayCount)
aggregate(full$BayCount~(full$Month+full$BayType.x), data= full, FUN=sum)
aggregate(full$BayCount~(full$BayType.x), data= full, FUN=sum)

q12 = subset (full,full$LotNo == "600" & full$BayType.x == "EV")
summary(q12)
write.csv(q12, file="q12.csv")

#Q11
q11 = subset(full, full$LotNo == "102" | full$LotNo == "201" | full$LotNo == "204" | full$LotNo == "207" | full$LotNo == "301")
write.csv(q11, file="q11.csv")
q11 =read.csv('q11.csv')
q11 = subset (q11, q11$Minutes>2)
write.csv(q11, file="q11Updated.csv")
table(q11$LotNo)

# Q1
aggregate(cbind(Arrived,BayType.x)~ Month+BayType.x, data = full, length)


#Extract price for 30 mins
full$price30min = ifelse((grepl("30 Mins", full$Prices, perl = T)), gsub('(/.*)', '', full$Prices), '')
#Extract price for 1 hour
full$price1hour = ifelse((grepl("1 Hour", full$Prices, perl = T)), gsub('(/1.*)|(.*Mins,)', '', full$Prices), '')
#Extract price for 2 hour
full$price2hours = ifelse((grepl("2 Hour", full$Prices, perl = T)), gsub('(/2.*)|(.*Hour,)', '', full$Prices), '')
#Extract price for 3 hour
#full$price3hours = gsub('(/3.*)|(.*2 Hours,)', '',full$Prices )
full$price3hours = ifelse((grepl("3 Hour", full$Prices, perl = T)), gsub('(/3.*)|(.*2 Hours,)', '', full$Prices), '')
#Extract price for 4 hour
#full$price4hours = gsub('(/4.*)|(.*3 Hours,)', '',full$Prices )
full$price4hours = ifelse((grepl("4 Hour", full$Prices, perl = T)), gsub('(/4.*)|(.*3 Hours,)', '', full$Prices), '')
#Extract price for 8 hour
#full$priceflatrate = gsub('(/Flat.*)|(.*4 Hours,)', '',full$Prices )
full$priceflatrate = ifelse((grepl("Flat", full$Prices, perl = T)), gsub('(/Flat.*)|(.*4 Hours,)', '', full$Prices), '')

write.csv(full, file="test.csv")
str(full)
UtilizationByLot = aggregate(parkedtime~LotName, data = full, FUN = sum)
UtilizationByLot=head(UtilizationByLot[order(-UtilizationByLot$parkedtime),],n=46)

#Occupancy Rate by Lot Name
jpeg(paste0("Occupancy Rate by LotName.jpg"),width = 10, height = 4, units = 'in', res = 360)
ggplot(UtilizationByLot, aes(reorder(LotName,parkedtime), y=parkedtime,fill=parkedtime)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  xlab("Lot Name") +
  ylab("Occupancy Rate")
dev.off()

#Parking Utilization by Street
jpeg(paste0("Occupancy Rate by Street Stacked.jpg"),width = 10, height = 4, units = 'in', res = 360)
ggplot(full, aes(x=hour,y=parkedtime,fill=Street.x)) +
  geom_bar(stat='identity', position="stack") +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  xlab("Street") +
  ylab("Occupancy Rate")
dev.off()


jpeg(paste0("Occupancy Rate by Street and Hour of the day - Grid.jpg"),width = 10, height = 8, units = 'in', res = 360)
ggplot(full, aes(x=hour,y=parkedtime/60, fill=Ward)) +
  geom_bar(stat='identity', position="stack") +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  facet_grid(Ward~.) +
  xlab ("Total hours parked") +
  ylab("Occupancy Rate") +
  theme(strip.text.y = element_blank())
dev.off()


jpeg(paste0("Parking Occupancy by Parking Type and Hour of the day- Grid.jpg"),width = 10, height = 8, units = 'in', res = 360)
ggplot(full, aes(x=hour,y=parkedtime/60, fill=OperatingHourCode)) +
  geom_bar(stat='identity', position="stack") +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  facet_grid(OperatingHourCode~.) +
  xlab ("Total hours parked") +
  ylab("Occupancy Rate") +
  theme(strip.text.y = element_blank())
dev.off()

#Q1
jpeg(paste0("Parking Occupancy by Parking Type - Grid.jpg"),width = 6, height = 6, units = 'in', res = 360)
ggplot(full, aes(x=OperatingHourCode,y=parkedtime/60)) +
  geom_bar(stat='identity', fill="orange") +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  xlab ("Total hours parked") +
  ylab("Occupancy Rate") 
dev.off()

#Q2 - To generate daily parking turnover chart
ParkedDateByLots = as.data.frame(table(full$LotNo, full$ParkedDate))
colnames(ParkedDateByLots) = c("LotNo", "ParkedDate", "VehiclesCount")
ParkedDateByLots = merge(ParkedDateByLots,Lots,by.x='LotNo', by.y='LotCode')
write.csv(ParkedDateByLots, file='ParkedDateByLots.csv')


jpeg(paste0("Daily Parking TurnoverByLot.jpg"),width = 16, height = 40, units = 'in', res = 360)
ggplot(ParkedDateByLots, aes(x=ParkedDate,y=VehiclesCount/BayCount)) +
  geom_line(aes(group=2)) +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  facet_grid(LotNo~.) +
  xlab ("Date") +
  ylab("Total Vehicles Parked") +
  scale_x_discrete(breaks= scales::pretty_breaks(n = 20))
dev.off()
str(full)

#What are the peaks for Furneaux street off-street parking during lunch and dinner periods?
Q3 = subset (full, full$SectorName == "Off Street" | full$Street == "Furneaux St")
Q3 = subset ( Q3, (Q3$bins == "4" | Q3$bins == "6"))
write.csv(Q3 , file="q3.csv")
jpeg(paste0("Q3- Peaks Furneaux street off-street Lunch Dinner - Grid.jpg"),width = 12, height = 18, units = 'in', res = 360)
ggplot(Q3, aes(x=as.factor(ParkedDate),y=parkedtime/60)) +
  geom_bar(stat='identity', position="stack") +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  facet_grid(LotName~.) +
  xlab ("Date") +
  ylab("Occupancy Rate during Lunch/Dinner") 
dev.off()

#Q5 - What are the Red overstay 'Hotspots' located in Free/ Timed bays, 
#i.e. where are the most overstayed bays located?
Q5 = subset (full, (full$OperatingHourCode == "All_Day" | full$OperatingHourCode == "Free") & full$MaxStayPeriod > 0 & full$BayType.x != 'Disabled')
Q5$OverStayFlag = 0
Q5[Q5$parkedtime - Q5$MaxStayPeriod  > 10, "OverStayFlag"] = 1
Q5 = subset (Q5, OverStayFlag == 1)
write.csv(Q5,file="q5.csv")
jpeg(paste0("Q5- Overstays in free and timed bays.jpg"),width = 6, height = 10, units = 'in', res = 360)
ggplot(Q5, aes(x=LotName)) +
  geom_bar(stat='count', position="stack", fill="light green") +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  xlab("Lots") +
  ylab("Overstays") 
dev.off()

#Q7 - Identify the Overstays by Bay type - The total number of overstays of 10 minutes or more?
Q7 = full
Q7$OverStayFlag = 0
Q7[Q7$parkedtime - Q7$MaxStayPeriod  > 10, "OverStayFlag"] = 1
Q7 = subset (Q7, OverStayFlag == 1)
write.csv(Q7,file="Q7.csv")
jpeg(paste0("Q7- Overstays by Baytype.jpg"),width = 6, height = 10, units = 'in', res = 360)
ggplot(Q7, aes(x=BayType.x)) +
  geom_bar(stat='count', position="stack", fill="light green") +
  theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  xlab("BayType") +
  ylab("Overstays") 
dev.off()


#Q8 - Percentage Overstays by Bay type - The total number of overstays of 10 minutes or more?
Q7 = full
Q7$OverStayFlag = 0
Q7[Q7$parkedtime - Q7$MaxStayPeriod  > 10, "OverStayFlag"] = 1
Q7 = subset (Q7, OverStayFlag == 1)
write.csv(Q7,file="Q7.csv")
jpeg(paste0("Q7- Overstays by Baytype.jpg"),width = 6, height = 10, units = 'in', res = 360)
ggplot(Q7, aes(x=factor(1),fill=BayType.x)) +
  geom_bar(width=1) +
  #theme(axis.text.x = element_text(vjust=0.6,angle=90))  +
  xlab("BayType") +
  ylab("Overstays") +
  coord_polar(theta='y') +
  scale_y_continuous (labels=scales::percent)
dev.off()

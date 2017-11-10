library("lubridate")
library("ggplot2")
setwd("C:/Veera/R/SmartParking/From Australia/SmartParking")

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

setwd("C:/Veera/R/SmartParking")
#load smart parking lots data
Lots = read.csv('Smart_Parking_Lots.csv')

#load smart parking restrictions data
Restrictions = read.csv("Smart_Parking_Restrictions.csv")

LotRestrictions = merge ( Lots, Restrictions, by.x = 'LotCode', by.y = 'LotCode', all = T)

Stays$ParkedDate = as.Date(Stays$Departed)

Stays$Month = month(Stays$Departed)
nrow(Stays) #29748

Stays$end = ymd_hms(strptime(Stays$Departed,"%Y/%m/%d %H:%M:%S"))
Stays$begin = ymd_hms(strptime(Stays$Arrived,"%Y/%m/%d %H:%M:%S"))
Stays$parkedtime = as.numeric(as.duration(as.interval(Stays$begin, Stays$end)),"minutes")

#extract Lot no from Lotname field 
Stays$LotNo = gsub(' .*','',Stays$LotName)

#merge Stays with Restrictions dataset
full = merge(Stays, LotRestrictions, by.x = 'LotNo', by.y='LotCode', all.x=T)
setwd("C:/Veera/R/SmartParking/RShiny")

write.csv(full,"Q1.csv")


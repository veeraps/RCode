setwd ("c:/veera/R/P")
library(ggplot2)
library(stringr)


v2015q3 = read.csv( 'I485_performancedata_fy2015_qtr3.csv')
v2015q4 = read.csv( 'I485_performancedata_fy2015_qtr4.csv')
v2016q1 = read.csv( 'I485_performancedata_fy2016_qtr1.csv')
v2016q2 = read.csv( 'I485_performancedata_fy2016_qtr2.csv')
v2016q3 = read.csv( 'I485_performancedata_fy2016_qtr3.csv')
v2016q4 = read.csv( 'I485_performancedata_fy2016_qtr4.csv')
v2017q1 = read.csv( 'I485_performancedata_fy2017_qtr1.csv')
v2017q2 = read.csv( 'I485_performancedata_fy2017_qtr2.csv')
v2017q3 = read.csv( 'I485_performancedata_fy2017_qtr3.csv')

v2015q3$Year = '2015'
v2015q4$Year = '2015'
v2016q1$Year = '2016'
v2016q2$Year = '2016'
v2016q3$Year = '2016'
v2016q4$Year = '2016'
v2017q1$Year = '2017'
v2017q2$Year = '2017'
v2017q3$Year = '2017'


v2015q3$key = '2015q3Apr-Jun'
v2015q4$key = '2015q4Jul-Sep'
v2016q1$key = '2016q1Oct-Dec'
v2016q2$key = '2016q2Jan-Mar'
v2016q3$key = '2016q3Apr-Jun'
v2016q4$key = '2016q4Jul-Sep'
v2017q1$key = '2017q1Oct-Dec'
v2017q2$key = '2017q2Jan-Mar'
v2017q3$key = '2017q3Apr-Jun'


i485data = rbind ( v2015q3,
                   v2015q4,
                   v2016q1,
                   v2016q2,
                   v2016q3,
                   v2016q4,
                   v2017q1,
                   v2017q2,
                   v2017q3)

i485data$State = str_trim(i485data$State)
#remove commas 
i485data$Applications.Received2 = gsub(",","",i485data$Applications.Received2)
#convert to numeric
i485data$Applications.Received2 = as.numeric(as.character(i485data$Applications.Received2))

#remove commas 
i485data$Approved3 = gsub(",","",i485data$Approved3)
#convert to numeric
i485data$Approved3 = as.numeric(as.character(i485data$Approved3))

#remove commas 
i485data$Denied4 = gsub(",","",i485data$Denied4)
#convert to numeric
i485data$Denied4 = as.numeric(as.character(i485data$Denied4))

#remove commas 
i485data$Pending5 = gsub(",","",i485data$Pending5)
#convert to numeric
i485data$Pending5 = as.numeric(as.character(i485data$Pending5))


aggregate(Applications.Received2~State, data = i485data , FUN=sum)
i485dataTexas = subset(i485data, i485data$State=="Texas")

aggregate(Applications.Received2~Year, data = i485dataTexas , FUN=sum)

library(reshape2)
myvars = c("Applications.Received2","Denied4","Approved3","Pending5","Year")
i485 = i485dataTexas[myvars]

melteddata = melt(i485, id.vars = 'Year')

melteddata = aggregate(value~(Year+variable),melteddata, FUN=sum )

ggplot(melteddata, aes(x=Year, y=value, fill=variable)) + geom_bar(stat='identity') +
  geom_text(aes(label=value), size=3,position=position_stack(vjust=0.5)) 
            
##############  Nebraska
i485dataNebraska = subset(i485data, i485data$State=="Nebraska")

aggregate(Applications.Received2~Year, data = i485dataNebraska , FUN=sum)

library(reshape2)
myvars = c("Applications.Received2","Denied4","Approved3","Pending5","Year")
i485 = i485dataNebraska[myvars]

melteddata = melt(i485, id.vars = 'Year')

melteddata = aggregate(value~(Year+variable),melteddata, FUN=sum )

ggplot(melteddata, aes(x=Year, y=value, fill=variable)) + geom_bar(stat='identity') +
  geom_text(aes(label=value), size=3,position=position_stack(vjust=0.5)) 

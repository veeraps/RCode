setwd("C:/RMS Laptop/Billing/RMS")
rms = read.csv("rmsmodel.csv")
str(rms)

#rms$RDate = as.Date(rms$Date,"%d/%m/%y")


#rms$Date = NULL


rmstest = read.csv("rmsmodeltest.csv")
str(rmstest)

RMSModel = lm(Revenue ~ Team.Size, data = rms)
summary(RMSModel)
sse = sum(RMSModel$residuals^2) 
sse

RMSModel2 = lm(Revenue ~ Team.Size + Long.Term.Onsite , data = rms)
summary(RMSModel2)
sse2 = sum(RMSModel2$residuals^2) 
sse2

RMSModel3 = lm(Revenue ~ Team.Size + Long.Term.Onsite + Short.Term.Onsite + Offshore.Buffer + Onsite.Buffer, data = rms)
summary(RMSModel3)
sse3 = sum(RMSModel3$residuals^2) 
sse3



pred = predict ( RMSModel3, newdata = rmstest )
pred

rmstest$Revenue

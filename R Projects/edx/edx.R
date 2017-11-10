setwd ("c:/veera/R/edx course")
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(ROCR)
library("mice")
library("caret")
library("e1071")
library (tm)library(flexclust)
library(ggplot2)
library(maps)  
library(ggmap)
library(reshape2)
library(igraph)
library(wordcloud)


WHO = read.csv("WHO.csv")
mean(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
which.max(WHO$LiteracyRate)
WHO$Country[44]
t = tapply(WHO$ChildMortality, WHO$Region, mean)
as.data.frame(t)
USDA = read.csv ("USDA.csv")
hist(USDA$Potassium, main = "Histogram of Potassium Levels", xlab="Potassium Levels", xlim = c(0,400), breaks=1000)

mvt = read.csv("mvtWeek1.csv")
nrow(mvt)
names(mvt)
which.max(mvt$ID)
mvt$ID[18134]
which.min(mvt$Beat)
mvt$Beat[4756]
Arrested = subset(mvt, Arrest==TRUE)
nrow(Arrested)
LocationAlley = subset(mvt, LocationDescription== "ALLEY")
nrow(LocationAlley)
str(mvt)

mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
DateConvert

mvt$Month = months(DateConvert)
mvt$WeekDay = weekdays(DateConvert)
mvt$Date = DateConvert

names(mvt)
str(mvt)
YA = table (mvt$Year, mvt$Arrest)

jpeg('mvtHistogram.jpg')
hist(mvt$Date, breaks=100)
dev.off()

jpeg('mvtboxplot.jpg')
boxplot(Date ~ Arrest, data = mvt)
dev.off()

table (mvt$Year, mvt$Arrest)
2152+18517
2152/20669
13068 +1212
1212/14280
13542 + 550
550/14092
sort(table(mvt$LocationDescription))

Top5 = subset(mvt, LocationDescription == 'STREET' | LocationDescription == 'GAS STATION' | LocationDescription == 'ALLEY' | LocationDescription == 'DRIVEWAY - RESIDENTIAL' | LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)')
nrow(Top5)

Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$WeekDay, Top5$Arrest, Top5$LocationDescription == 'GAS STATION')

table (mvt$WeekDay)
DriveWay = subset(mvt, mvt$LocationDescription=="DRIVEWAY - RESIDENTIAL")
table(DriveWay$WeekDay)

prop.table (YA, 1)
#Stock Dynamics
IBM = read.csv("IBMStock.csv")
Boeing = read.csv("BoeingStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
GE = read.csv("GEStock.csv")
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

Stock <- rbind(GE,CocaCola,IBM,ProcterGamble,Boeing)

str(Stock)
Stock$Year = as.integer(format(Stock$Date, "%Y"))

table(Stock$Year)

summary(GE)
summary(CocaCola)
summary((Boeing))
sd(ProcterGamble$StockPrice)

?plot(CocaCola$Date, CocaCola$StockPrice, type = 'l',col='red')
lines(ProcterGamble$Date, ProcterGamble$StockPrice,col='blue')
abline(v=as.Date(c("1983-03-01")), lwd=2)

#3
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col='blue')
lines(GE$Date[301:432], GE$StockPrice[301:432],col='Purple')
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],col='orange')
lines(IBM$Date[301:432], IBM$StockPrice[301:432],col='black')
abline(v=as.Date(c("2004-01-01")), lwd=1)
abline(v=as.Date(c("2005-12-31")), lwd=1)
legend(IBM$Date[400:400], 200,legend=c("Coca Cola", "PG", "GE","Boeing","IBM"), col=c("red", "blue","Purple","Orange","Black"), lty=1:2, cex=0.8)

#4.1
tapply(IBM$StockPrice, months(IBM$Date),mean)
tapply(GE$StockPrice, months(GE$Date),mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date),mean)
tapply(Boeing$StockPrice, months(Boeing$Date),mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date),mean)

#Demographics and Employment in the United States
CPS = read.csv ("cpsdata.csv")
str(CPS)
nrow((CPS))
sort(table(CPS$State))

sort(table(CPS$Citizenship))
nrow(CPS)

str(CPS)
table(CPS$Race, CPS$Hispanic)

apply(CPS,2,function(x) any(is.na(x)))

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

str(CPS)
table(CPS$State, is.na(CPS$MetroAreaCode))

RM = table(CPS$State, is.na(CPS$MetroAreaCode))
prop.table(RM,1)
      
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))

MetroAreaMap = read.csv ("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
nrow(MetroAreaMap)
nrow(CountryMap)
CPS = merge(CPS, MetroAreaMap, by.x = "MetroAreaCode", by.y = "Code", all.x=TRUE)
str(CPS)
MA = as.data.frame(sort(table(CPS$MetroArea)))
MA
write.csv(MA,"MA.csv")

RM = table(CPS$MetroArea, CPS$Hispanic)
MA = as.data.frame(prop.table(RM,1))
write.csv(MA,"MA.csv")

RM = table(CPS$MetroArea, CPS$Race)
MA = as.data.frame(prop.table(RM,1))
write.csv(MA,"MA.csv")

sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

CPS = merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x=TRUE)
str(CPS)
sort(table(CPS$Country))

RM = table(CPS$Country ,CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
MA = as.data.frame(RM)
write.csv(MA,"MA.csv")

table(CPS$MetroArea, CPS$Country)
write.csv(MA,"MA.csv")
nrow(MA)/nrow(CPS)

sort(tapply (CPS$Country=="Somalia",CPS$MetroArea, sum, na.rm=TRUE))
RM = table (CPS$MetroArea,CPS$Country=="India")
MA = as.data.frame(RM)
write.csv(MA,"MA.csv")

poll=read.csv("AnonymityPoll.csv")
str(poll)
nrow(poll)
str(poll$Internet.Use)
nrow(subset(poll,poll$Smartphone==1))
summary(poll$Smartphone)
str(poll)
sort(tapply(poll$Region=="South",poll$State,sum))
nrow(subset(poll, poll$Region=="South" & poll$State == "Texas"))
nrow(subset(poll, poll$Internet.Use==0 & poll$Smartphone == 1))
limited = subset(poll, poll$Internet.Use==1 | poll$Smartphone == 1)
nrow(limited)

apply(limited,2,function(x) any(is.na(x)))
mean(limited$Info.On.Internet )
summary(limited)
sum(limited$Info.On.Internet==11)
RM = subset(limited, is.na(limited$Anonymity.Possible)== FALSE)
str(RM)
YA = table(RM$Worry.About.Info)
YA

str(limited)
?hist

hist(limited$Age)

table(limited$Age, limited$Info.On.Internet)
plot(limited$Age, limited$Info.On.Internet)
RM = table(limited$Info.On.Internet,limited$Age)
MA = as.data.frame(RM)
write.csv(MA,"MA.csv")

write.csv(limited,"limitedCPS.csv")
max(table(limited$Age, limited$Info.On.Internet))
jitter(c(1, 2, 5))
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
tapply(limited$Info.On.Internet,limited$Age, mean,na.rm=TRUE)

tapply(limited$Info.On.Internet,limited$Smartphone, mean,na.rm=TRUE)
tapply(limited$Tried.Masking.Identity ,limited$Smartphone, mean,na.rm=TRUE)

wine = read.csv ("wine.csv")
str(wine)
summary(wine)
model1 = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model1)

sse = sum(model1$residuals^2)
sse

cor(wine)
model4 = lm(Price ~ Age + HarvestRain + WinterRain + AGST, data = wine)
summary(model4)


cor(wine$HarvestRain, wine$WinterRain)

winetest= read.csv("wine_test.csv")
str(winetest)
predicttest = predict(model4, newdata = winetest)
predicttest

sse = sum((winetest$Price-predicttest)^2)
sst=sum((winetest$Price-mean(wine$Price))^2)
1-sse/sst

winetest

baseball = read.csv("baseball.csv")
str(baseball)
moneyball = subset (baseball , Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA

plot(moneyball$RD , moneyball$W)

WinsReg = lm (W ~ RD, data = moneyball)
WinsReg
summary(WinsReg)
(95-80.8814)/0.1058

RunsReg = lm ( RS ~ OBP + SLG , data = moneyball)
summary(RunsReg)

RunsReg$coef

players = read.csv ("players.csv")
str(players)
colnames(players)

Sal = predict(RunsReg, data = players)
summary(Sal)

playerStats<-data.frame("OBP"=c(0.338,.391,.369,.313,.361),"SLG"=c(0.54,0.45,.374,.447,.500),"Price"=c(1.4,1.065,.295,.8,.3))
playerStats

RunsReg$coef[1]+RunsReg$coef[2]*playerStats[1,1]+RunsReg$coef[3]*playerStats[1,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[2,1]+RunsReg$coef[3]*playerStats[2,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[3,1]+RunsReg$coef[3]*playerStats[3,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[4,1]+RunsReg$coef[3]*playerStats[4,2]
RunsReg$coef[1]+RunsReg$coef[2]*playerStats[5,1]+RunsReg$coef[3]*playerStats[5,2]

teamrank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamrank, wins2013)

nba = read.csv("NBA_train.csv")
str(nba)

nba$PD = nba$PTS - nba$oppPTS
plot(nba$PD , nba$W)

winReg = lm(W ~ PD, data = nba)
summary(winReg)

W = 41 + 0.0326 * PD
PD = (42-41)/0.0326 = 30.67485

PointsReg = lm ( PTS ~ X2PA + X3PA + FTA + AST + ORB + TOV + STL + BLK, data = nba )
summary(PointsReg)

PointsReg2 = lm ( PTS ~ X2PA + X3PA + FTA + AST + ORB + TOV + STL, data = nba )
summary(PointsReg2)

PointsReg3 = lm ( PTS ~ X2PA + X3PA + FTA + AST + ORB + TOV, data = nba )
summary(PointsReg3)

PointsReg4 = lm ( PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = nba )
summary(PointsReg4)

SSE4 = sum(PointsReg4$residuals^2)
RMSE4 = sqrt(SSE4/nrow(nba))
RMSE4

nbatest = read.csv("nba_test.csv")
PointsPredictions = predict(PointsReg4,newdata = nbatest)
PointsPredictions
SSE = sum((PointsPredictions - nbatest$PTS)^2)
SST = sum((mean(nba$PTS)-nbatest$PTS)^2)
SST
R2 = 1 - SSE/SST
R2
RMSE = sqrt(SSE/nrow(nbatest))
RMSE

Climate = read.csv("climate_change.csv")
str(Climate)
ClimateReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = Climate )

summary(ClimateReg)


cor ( Climate$MEI, Climate$N2O + Climate$CFC.11)
train = subset(Climate, Year <= 2006)

cor ( train$MEI, train$N2O )
cor ( train$CO2, train$N2O )
cor ( train$CH4, train$N2O )
cor ( train$CFC.11, train$N2O )
cor ( train$CFC.12, train$N2O )
cor ( train$Aerosols, train$N2O )
cor ( train$TSI, train$N2O )

cor ( train$MEI, train$CFC.11 )
cor ( train$CO2, train$CFC.11 )
cor ( train$CH4, train$CFC.11 )
cor ( train$N2O, train$CFC.11 )
cor ( train$CFC.12, train$CFC.11 )
cor ( train$Aerosols, train$CFC.11 )
cor ( train$TSI, train$CFC.11 )

climateReg2 = lm ( Temp ~ MEI + TSI + Aerosols + N2O, data = train)
summary (climateReg2)
cor(train)

ClimateStep = step(climateReg2)
summary(ClimateStep)

-1.5+ 3+ 5*(-0.5)

exp(-1)

1/(1+(1/exp(-1)))

quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
#install.packages("caTools")
set.seed(88)
split=sample.split(quality$PoorCare,SplitRatio = 0.75)
  
qualityTrain = subset (quality , split==TRUE)
qualityTest = subset ( quality, split == FALSE)

Qualitylog = glm ( PoorCare ~ Narcotics + OfficeVisits, data = qualityTrain, family = binomial)
summary(Qualitylog)
str(Qualitylog)
predictTest = predict(Qualitylog, type="response", newdata=qualityTest)

predictTest
tapply(predictTest, qualityTest$PoorCare, mean)


#Qualitylog = glm ( PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTest, family = binomial)

table ( qualityTest$PoorCare , predictTest > 0.3)


install.packages ("ROCR")
ROCRPred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize="TRUE",print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

ROCRPred = prediction(predictTest, qualityTest$PoorCare)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
auc = as.numeric(performance(ROCRPred, "auc")@y.values)
plot(ROCRPerf, colorize="TRUE")
auc

framingham = read.csv("framingham.csv")
str(framingham)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
Train = subset(framingham, split==TRUE)
Test = subset(framingham, split==FALSE)
framinghamLog = glm(TenYearCHD ~ ., data = Train, family = binomial )
summary(framinghamLog)
predictTest = predict ( framinghamLog, type="response", newdat= Test)
table(Test$TenYearCHD, predictTest > 0.5)

ROCRPred = prediction(predictTest, Test$TenYearCHD)
as.numeric(performance(ROCRPred, "auc")@y.values)

Poll = read.csv ("PollingData.csv")
summary(Poll)
table(Poll$Year)
install.packages("mice")

simple = Poll[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
simple$Rasmussen
summary(imputed)
Poll$Rasmussen = imputed$Rasmussen
Poll$SurveyUSA = imputed$SurveyUSA

Train = subset(Poll, Year==2004 | Year == 2008)
Test = subset (Poll, Year == 2012)
table(Train$Republican)

# Smart Baseline
table(Train$Republican)
sign(20)
sign(-10)
?sign(0)
table(sign(Train$Rasmussen))
table(Train$Republican, sign(Train$Rasmussen))


cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])
mod1 = glm(Republican ~ PropR, data = Train, family = binomial)
summary(mod1)
pred1 = predict(mod1, type = "response")
table(Train$Republican, pred1 > 0.5)
mod2 = glm(Republican ~ SurveyUSA+DiffCount, data = Train, family = binomial)
summary(mod2)
pred2 = predict(mod2, type = "response")

table(Train$Republican, pred2 > 0.5)

table(Test$Republican, sign(Train$Rasmussen))
pred3 = predict(mod2, newdata = Test, type = "response")

table(Test$Republican, pred3 >= 0.5)


subset(Test, pred3 >= 0.5 & Test$Republican == 0)

#Unit 2

#popularity of music recording
songs = read.csv ("songs.csv")
str(songs)
MicJack = subset(songs, songs$artistname == "Michael Jackson" & songs$Top10 == 1)
write.csv(MicJack, file="micjack.csv")
table(songs$timesignature)
sort(table(songs$tempo))
TopTempo = subset (songs, songs$tempo >= 244.30)
write.csv(TopTempo, file="micjack.csv")
summary(songs$tempo)

SongsTrain = subset (songs, songs$year<=2009)
SongsTest = subset (songs, songs$year==2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[,!names(SongsTrain) %in% nonvars ]
SongsTest = SongsTest[,!names(SongsTrain) %in% nonvars ]
SongsLog1 = glm(Top10 ~., data=SongsTrain, family = binomial)
summary(SongsLog1)
cor(songs$loudness,songs$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

Pred1 = predict ( SongsLog3, newdata = SongsTest, type = "response")

table (SongsTest$Top10, Pred1 > 0.45)
(309+19)/(309+19+5+40)

table (SongsTest$Top10)
19/59

parole = read.csv("parole.csv")
str(parole)
table(parole$violator)
summary(parole$crime)
parole$crime = as.factor(parole$crime)
parole$state   = as.factor(parole$state)
set.seed(144)


split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
summary(train)
str(train)
str(test)

paroleTrain = glm (violator~., data=train, family = binomial)
summary(paroleTrain)

predictTest = predict(paroleTrain, newdata = test, type = "response")
summary(predictTest)

table(test$violator, predictTest>0.3)
  12/(12+11)
167/(167+12)
(167+12)/(167+12+12+11)
table(test$violator)


ROCRPred = prediction(predictTest, test$violator)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize="TRUE",print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc = as.numeric(performance(ROCRPred, "auc")@y.values)
plot(ROCRPerf, colorize="TRUE")
auc


loans = read.csv("loans.csv")
str(loans)
summary(loans)
table(loans$not.fully.paid)
1533/(1544+8045)
is.na(loans$pub.rec)

missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
str(missing)
table(missing$not.fully.paid)
12/62

vars.for.imputation = setdiff(names(loans), "not.fully.paid")
vars.for.imputation

loans = read.csv("loans_imputed.csv")
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7 )

train = subset (loans, split == TRUE)
test = subset(loans, split==FALSE)
str(test)

mod = glm(not.fully.paid~., data = train, family = binomial )
summary(mod)
pred = predict(mod, newdata = test, type = "response")
summary(pred)

pred
test$predicted.risk = pred

summary(test)

table(test$not.fully.paid, pred > 0.5)
(2400+3)/(2403+13+457)
table(test$not.fully.paid)

ROCRPred = prediction(pred, test$not.fully.paid)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize="TRUE",print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc = as.numeric(performance(ROCRPred, "auc")@y.values)
plot(ROCRPerf, colorize="TRUE")
auc

cor(train$int.rate, train$fico)

bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)

pred = predict(bivariate, newdata = test, type = "response")
table(test$not.fully.paid, pred > 0.5)
summary(pred)


test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
summary(test)
str(test)
sum(test$profit)
table(test$int.rate)
highinterest = subset(test, test$int.rate >= .15)
summary(highinterest)

table(highinterest$not.fully.paid)
110/(437)
highinterest

cutoff = sort(highinterest$predicted.risk, decreasing=FALSE)[100]
cutoff

selectedLoans = subset (highinterest, highinterest$predicted.risk <= cutoff)
str(selectedLoans)

sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)

#Unit 4

stevens = read.csv("stevens.csv")
str(stevens)

set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)

train = subset(stevens, spl == TRUE)

test = subset(stevens, spl == FALSE)

stevenstree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 25)
prp(stevenstree)
pred = predict(stevenstree, newdata = test, type = "class")
table(test$Reverse, pred)

(41+71)/(41+71+36+22)

PredictROC = predict(stevenstree, newdata = test )
PredictROC

pred1 = prediction(PredictROC[,2], test$Reverse)
perf = performance (pred1, "tpr","fpr")
plot(perf)
as.numeric(performance(pred1, "auc")@y.values)


stevenstree1 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
prp(stevenstree1)

set.seed(200)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
stevensforest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 200)
predictforest = predict(stevensforest, newdata = test)
table(test$Reverse, predictforest)

(40+74)/(18+37+40+74)

(43+74)/(19+34+43+74)

(44+76)/(17+33+44+76)

install.packages( "caret")
install.packages("e1071")


numFolds = trainControl ( method="cv", number = 10)

cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

stevenstreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp=0.18)
prp(stevenstreeCV)
PredictCV = predict (stevenstreeCV, newdata = test, type = "class")
table(test$Reverse, PredictCV)

(59+64)/(59+64+18+29)


claims = read.csv("claimsdata.csv")
str(claims)

table(claims$bucket2008)/nrow(claims)

set.seed (88)
split = sample.split(claims$bucket2009, SplitRatio = 0.6)
claimstrain = subset(claims, split == TRUE)
claimstest = subset (claims, split == FALSE)

summary(claimstrain)
table(claimstrain$diabetes)
104672/(104672+170131)

table ( claimstest$bucket2009, claimstest$bucket2008)
accuracy = (110138+10721+2774+1539+104)/nrow(claimstest)
sum(diag(a))/nrow(claimstest)
accuracy
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE, nrow=5)
PenaltyMatrix

sum(as.matrix(table ( claimstest$bucket2009, claimstest$bucket2008))*PenaltyMatrix)/nrow(claimstest)


ClaimsTree = rpart(bucket2009 ~ age	+ alzheimers + arthritis + cancer +	copd + depression +	diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +	reimbursement2008 +	bucket2008, method = "class", data = claimstrain, cp=0.00005) 
str(claims)
prp(ClaimsTree)
PredictTest = predict ( ClaimsTree, newdata = claimstest, type = "class")
a = table (claimstest$bucket2009, PredictTest)
a
colSums(a)/nrow(claimstest)
sum(diag(a))/nrow(claimstest)

as.matrix(table (claimstest$bucket2009, PredictTest))*PenaltyMatrix


ClaimsTree = rpart(bucket2009 ~ age	+ alzheimers + arthritis + cancer +	copd + depression +	diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +	reimbursement2008 +	bucket2008, method = "class", data = claimstrain, cp=0.00005, parms = list(loss=PenaltyMatrix)) 
str(claims)
prp(ClaimsTree)
PredictTest = predict ( ClaimsTree, newdata = claimstest, type = "class")
a = table (claimstest$bucket2009, PredictTest)
a
sum(diag(a))/nrow(claimstest)

as.matrix(table (claimstest$bucket2009, PredictTest))*PenaltyMatrix

colSums(a)/nrow(claimstest)

boston=read.csv("boston.csv")

plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="red", pch=19)
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="blue", pch=19)



latlontree = rpart(MEDV ~ LAT + LON, data = boston)
plot(latlontree)
text(latlontree)
plot(boston$LON, boston$LAT)
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2], col="blue", pch = "$")

latlontree = rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(latlontree)
text(latlontree)

plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2], col="red", pch = 19)

set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)

linreg = lm(MEDV~LAT + LON + CRIM + ZN + INDUS+ CHAS + NOX + RM + AGE + DIS+ RAD+ TAX+ PTRATIO, data = train)
summary(linreg)
linreg.pred = predict(linreg, newdata = test)
sse=sum((linreg.pred-test$MEDV)^2)
sse


numFolds = trainControl ( method="cv", number = 10)

cpGrid = expand.grid(.cp=seq(0.001,0.01,0.001))

train(MEDV~LAT + LON + CRIM + ZN + INDUS+ CHAS + NOX + RM + AGE + DIS+ RAD+ TAX+ PTRATIO, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)


gerber = read.csv("gerber.csv")
str(gerber)
table(gerber$voting)
108696/(108696+235388)
str(gerber)
table (gerber$hawthorne, gerber$voting==1)
12316/(12316+25888)
table (gerber$civicduty, gerber$voting)
12021/(12021+26197)
table (gerber$self, gerber$voting==1)
13191/(13191+25027)
table (gerber$neighbors, gerber$voting==1)
14438/(14438+23763)

tapply(gerber$voting, gerber$civicduty, mean)
                                                                                    tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

linreg.gerber = glm(voting ~ civicduty + self + neighbors + hawthorne, data = gerber, family = binomial)
summary(linreg.gerber)
pred = predict(linreg.gerber, type = "response")
pred
table ( gerber$voting, pred > 0.5)
(51966+134513)/(51966+134513+100875+56730)
235388/(235388+108696)

ROCRPred = prediction(pred, gerber$voting)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
as.numeric(performance(ROCRPred, "auc")@y.values)


CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
CARTmodel2 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
CARTcontrol= rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTcontrol,digits = 6)

abs(0.34 - 0.296638)


CARTcontrol2 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontrol2,digits = 6)

abs(0.345818 - 0.334176)

logModelSex = glm (voting ~ control + sex, data = gerber, family = binomial)
summary(logModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
Possibilities
predict(logModelSex, newdata=Possibilities, type="response")
abs(0.290456- 0.2908065 )
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.290456- 0.2904558)
2/10000000
2/10
0.0000002


letters = read.csv("letters_ABPR.csv")
table(letters$letter)
str(letters)
letters$isB = as.factor(letters$letter == 'B')
set.seed(1000)

split = sample.split(letters$isB, SplitRatio = 0.5)
trainLetters = subset (letters, split == TRUE)
testLetters = subset (letters, split == FALSE)

table(testLetters$isB)
1175/(1175+383)

CARTb = rpart(isB ~ . - letter, data=trainLetters, method="class")
prp(CARTb)
pred = predict(CARTb, newdata = testLetters, type = "class")

table ( testLetters$isB, pred)

(340+1118)/(340+1118+57+43)

set.seed(1000)
LetterForest = randomForest(isB ~ xbox+ybox+width+height+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybar+xy2bar+xedge+xedgeycor+yedge+yedgexcor, data = trainLetters)
predictforest = predict(LetterForest, newdata = testLetters)
table(testLetters$isB, predictforest)
(374+1165)/(374+1165+10+9)

letters$letter = as.factor( letters$letter )

set.seed(2000)

split = sample.split(letters$letter, SplitRatio = 0.5)
train = subset (letters, split == TRUE)
test = subset (letters, split == FALSE)

table(train$letter)

rfb = randomForest(letter ~ ., data = train)
pred = predict(rfb, newdata = test)
table(test$letter, pred)
401/nrow(test)

CARTLetter = rpart(letter ~ . - isB, data=train, method="class")
pred = predict(CARTLetter, newdata = test, type = "class")

a=table ( test$letter, pred)
sum(diag(a))/nrow(test)

set.seed(1000)
rfb = randomForest(letter ~ . - isB, data = train)
pred = predict(rfb, newdata = test)
a=table(test$letter, pred)
a
sum(diag(a))/nrow(test)

census = read.csv("census.csv")

set.seed(2000)
split = sample.split (census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

LogCensus = glm(over50k~.,family = binomial, data = train)
summary(LogCensus)

pred = predict(LogCensus, type = "response", newdata = test)

table(test$over50k, pred > 0.5)
(1888+9051)/(1888+9051+662+1190)

table(test$over50k)
3078/(3078+9713)

ROCRPred = prediction(pred, test$over50k)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
auc = as.numeric(performance(ROCRPred, "auc")@y.values)
auc

censusCART = rpart(over50k ~ ., data=train, method = "class") 
prp(censusCART)

pred = predict ( censusCART, type = "class", newdata = test)
table ( test$over50k, pred)

(9243+1596)/(9243+1596+470+1482)
predROC = predict(censusCART, newdata = test)
head(predROC)

pred = prediction(predROC[,2], test$over50k)
perf=performance(pred, "tpr","fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
smallRF = randomForest(over50k ~ ., data = trainSmall)
pred = predict(smallRF, newdata=test)
table(test$over50k, pred )
(9586+1093)/(9586+1093+127+1985)

vu = varUsed(smallRF, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(smallRF$forest$xlevels[vusorted$ix]))
varImpPlot(smallRF)


set.seed(2)
numFolds = trainControl ( method="cv", number = 10)

cpGrid = expand.grid(.cp=seq(0.002,0.1,0.002))

train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

censusCV = rpart(over50k ~ ., data=train, method = "class", cp=0.002) 
prp(censusCV)
predictCV = predict(censusCV, newdata = test, type = "class")
table(test$over50k, predictCV)
0.8612306


#Unit 5

tweets = read.csv ( "tweets.csv", stringsAsFactors = FALSE)
tweets$negative = as.factor(tweets$Avg <=-1)
table (tweets$negative)


corpus= Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus[[1]]$content
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

frequencies = DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005,500:515])
findFreqTerms(frequencies,lowfreq = 20)
sparse = removeSparseTerms(frequencies,0.995)
tweetSparse = as.data.frame(as.matrix(sparse))
colnames(tweetSparse) = make.names(colnames(tweetSparse))
tweetSparse$negative = tweets$negative

set.seed(123)
split = sample.split(tweetSparse, SplitRatio = 0.7)

trainSparse = subset (tweetSparse, split == TRUE)

testSparse = subset (tweetSparse, split == FALSE)

findFreqTerms(frequencies,lowfreq = 100)


tweetCART = rpart(negative ~ ., method= "class", data = trainSparse)

prp(tweetCART)

pred = predict(tweetCART, newdata = testSparse, type = "class")
table ( testSparse$negative, pred)
(301+20)/(321+33)

table(testSparse$negative)

304/354



set.seed(123)

tweetRF = randomForest(negative~., data = trainSparse)
pred = predict(tweetRF, newdata = testSparse)

table(testSparse$negative, pred)

(296+24)/(296+24+8+24)

tweetLog = glm(negative~., data = trainSparse, family = binomial)
pred = predict(tweetLog, newdata=testSparse, type= "response")

table(testSparse$negative, pred > 0.5)
  
(253+34)/(253+34+51+16)  
  
  
emails = read.csv("energy_bids.csv")
str(emails)
emails$email[1]
strwrap(emails$email[1])
corpus= Corpus(VectorSource(emails$email))
strwrap(corpus[[1]]$content)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map (corpus, removeWords, stopwords("english"))
corpus = tm_map (corpus, stemDocument)
strwrap(corpus[[1]]$content)
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.97)

labeledTerms = as.data.frame(as.matrix(dtm))
str(labeledTerms)
labeledTerms$responsive = emails$responsive
str(labeledTerms)

set.seed(144)

split = sample.split (labeledTerms$responsive, SplitRatio = )

trainEmails = subset ( labeledTerms, split == TRUE)
testEmails = subset (labeledTerms, split == FALSE)

EmailsCART = rpart(responsive ~ ., data = trainEmails, method = "class")
pred = predict (EmailsCART, newdata = testEmails)
pred[,2]
table (testEmails$responsive, pred[,2]>=0.5)

prp(EmailsCART)


wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)

wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal)


corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map (corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map (corpusAdded,stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
length(stopwords("english"))
dtmAdded

sparseAdded= removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
colnames(wordsAdded) 
str(wordsAdded)



corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map (corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map (corpusRemoved,stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
length(stopwords("english"))
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved


wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
colnames(wordsRemoved) 
str(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

set.seed(123)

spl = sample.split(wikiWords, SplitRatio = 0.7)
trainWords = subset(wikiWords, spl == TRUE)
testWords = subset (wikiWords, spl==FALSE)
table(testWords$Vandal)
(622/(622+542))


wikiCART = rpart(Vandal ~ ., data = trainWords, method = "class")
pred = predict(wikiCART, newdata = testWords, type = "class")

table(testWords$Vandal, pred)
(622+14)/(622+14+528)


prp(wikiCART)

grepl("cat","dogs and cats",fixed=TRUE) # TRUE

grepl("cat","dogs and rats",fixed=TRUE) # FALSE

wikiWords2 = wikiWords

wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table (wikiWords2$HTTP)


set.seed(123)

split3 = sample.split(wikiWords2, SplitRatio = 0.7)
trainWords2 = subset(wikiWords2, split3 == TRUE)
testWords2 = subset (wikiWords2, split3==FALSE)
table(testWords2$Vandal)


wikiCART2 = rpart(Vandal ~ ., data = trainWords2, method = "class")
pred2 = predict(wikiCART2, newdata = testWords2, type = "class")

table(testWords2$Vandal, pred2)

620/(620+539)
(613+67)/(613+67+472+7)


wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))

wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin


mean(wikiWords2$NumWordsAdded)


trainWords3 = subset(wikiWords3, spl == TRUE)
testWords3 = subset (wikiWords3, spl==FALSE)
table(testWords3$Vandal)


wikiCART3 = rpart(Vandal ~ ., data = trainWords3, method = "class")
pred3 = predict(wikiCART2, newdata = testWords3, type = "class")

table(testWords3$Vandal, pred3)


clinical = read.csv("clinical_trial.csv", stringsAsFactors =  FALSE)
str(clinical)
summary(clinical)
max(nchar(clinical$abstract))
sum(nchar(clinical$abstract)==0)

which(nchar(clinical$title)==28)
clinical$title[1258]


corpusTitle = Corpus(VectorSource(clinical$title))
corpusabstract = Corpus(VectorSource(clinical$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusabstract = tm_map(corpusabstract, tolower)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusabstract = tm_map(corpusabstract,removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("English"))
corpusabstract = tm_map(corpusabstract,removeWords, stopwords("English"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusabstract = tm_map(corpusabstract,stemDocument)

Corpus = tm_map(corpusTitle, PlainTextDocument)
Corpus = tm_map(corpusabstract, PlainTextDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusabstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract)) 

str(dtmTitle)
str(dtmAbstract)

which.max(colSums(dtmAbstract))
dim(dtmAbstract)

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = clinical$trial
ncol(dtm)

spl = sample.split(dtm$trial, SplitRatio = 0.7)
trainingTrial = subset ( dtm, spl == TRUE)
testingTrial = subset ( dtm, spl == FALSE)

table(trainingTrial$trial)
733/(733+566)


trialCART= rpart(trial~., data=trainingTrial, method="class") #the method="class" argument as this is a classification problem


prp(trialCART)

predTrain = predict(trialCART)
sort (predTrain[,2], decreasing = TRUE)
max(predTrain[,2])


table(trainingTrial$trial, predTrain[,2] >= 0.5)

predTest = predict (trialCART, newdata = testingTrial)

table ( testingTrial$trial, predTest[,2] >= 0.5)  

ROCRPred = prediction(predTest[,2], testingTrial$trial)
as.numeric(performance(ROCRPred, "auc")@y.values)

emails = read.csv ("emails.csv", stringsAsFactors = FALSE)
summary(emails)

table(emails$spam)
head(emails)

sort(nchar(emails$text))

which(nchar(emails$text)==13)



corpus = VCorpus(VectorSource(emails$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm,0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
sort(colSums(emailsSparse))

emailsSparse$spam = as.factor(emails$spam)

sum(colSums(subset(emailsSparse, emailsSparse$spam==0)) >= 1000)

set.seed(123)

split = sample.split(emails$spam, SplitRatio = 0.7)
train = subset(emails, split == TRUE)
test = subset(emails, split == FALSE)

spamLog = glm(spam~., data = train, family = binomial)
spamCART = rpart(spam~., data = train, method = "class")
set.seed(123)
spamRF = randomForest ( spam~., data = train)

pred = predict(spamLog, type = "response")
predTrainCART<- predict(spamCART)[,2]

sum(pred <   0.99999 & pred > 0.00001)

summary(spamLog)
prp(spamCART)

table(train$spam, pred>0.5)

ROCRPred = prediction(pred, train$spam)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
auc = as.numeric(performance(ROCRPred, "auc")@y.values)
auc


movies = read.table("movielens.txt", header = FALSE, sep = "|", quote = "\"")
str(movies)
colnames (movies) = c( "ID", "Title", "ReleaseDate", "VideoReleaseDate","IMDB","Unknown","Action", "Adventure","Animation","Childrens","Comedy","Crime","Documentary", "Drama", "Fantasy", "FilmNoir","Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller","War","Western")

movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB=NULL
movies=unique(movies)
str(movies)
head(movies)
sum(movies$Romance==1 & movies$Drama==1)
distances = dist(movies[2:20],method = "euclidean")
distances
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k=10)
clusterGroups
tapply(movies$Action, clusterGroups, FUN=mean)

subset(movies, movies$Title == "Men in Black (1997)")
clusterGroups[257]


clusterGroups2 = cutree(clusterMovies, k=2)

tapply(movies$Action, clusterGroups2, FUN=mean)
tapply(movies$ Adventure, clusterGroups2, FUN=mean)
tapply(movies$Animation, clusterGroups2, FUN=mean)
tapply(movies$Childrens, clusterGroups2, FUN=mean)
tapply(movies$Comedy, clusterGroups2, FUN=mean)
tapply(movies$Crime, clusterGroups2, FUN=mean)
tapply(movies$Documentary, clusterGroups2, FUN=mean)
tapply(movies$ Drama, clusterGroups2, FUN=mean)
tapply(movies$ Fantasy, clusterGroups2, FUN=mean)
tapply(movies$ FilmNoir, clusterGroups2, FUN=mean)
tapply(movies$Horror, clusterGroups2, FUN=mean)
tapply(movies$ Musical, clusterGroups2, FUN=mean)
tapply(movies$ Mystery, clusterGroups2, FUN=mean)
tapply(movies$ Romance, clusterGroups2, FUN=mean)
tapply(movies$ SciFi, clusterGroups2, FUN=mean)
tapply(movies$ Thriller, clusterGroups2, FUN=mean)
tapply(movies$War, clusterGroups2, FUN=mean)
tapply(movies$Western, clusterGroups2, FUN=mean)

flower = read.csv("flower.csv", header = FALSE)
str(flower)
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
flowerVector = as.vector(flowerMatrix)
str(flowerVector)
distances = dist(flowerVector,method = "euclidean")

clusterIntensity = hclust(distances, method = "ward.D")
plot(clusterIntensity)

rect.hclust(clusterIntensity, k=3, border = "red")
flowerclusters = cutree(clusterIntensity, k=3)
flowerclusters

tapply(flowerVector, flowerclusters, mean)
dim(flowerclusters)=c(50,50)

image(flowerclusters, axes=FALSE)
image(flowerMatrix, axes=FALSE, col = grey(seq(0,1,length=256)))

healthy = read.csv("healthy.csv", header=FALSE)
str(healthy)
healthymatrix = as.matrix(healthy)
#image(healthymatrix, axes = FALSE, col=grey(seq(0,1,length=256))

healthyvector = as.vector(healthymatrix)
str(healthyvector)
distance = dist(healthyvector, method="euclidean")
k=5
set.seed(1)
KMC = kmeans(healthyvector,centers = k, iter.max=1000)
str(KMC)
healthyCluster = KMC$cluster


dim(healthyCluster) = c(nrow(healthymatrix),ncol(healthymatrix))
image(healthyCluster, axes = FALSE, col = rainbow(k))

tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector=as.vector(tumorMatrix)

KMC.kcca = as.kcca(KMC, healthyvector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix),ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col = rainbow(k))

dailykos = read.csv("dailykos.csv")
str(dailykos)
# dailykosmatrix = as.matrix(dailykos)
# str(dailykosmatrix)
# dailykosvector = as.vector(dailykosmatrix)
# str(dailykosvector)
distances=dist(dailykos, method = "euclidean")
dailykoscluster = hclust(distances, method = "ward.D")
plot(dailykoscluster)
rect.hclust(dailykoscluster, k=7, border = "red")

str(dailykos)

clustergroups = cutree(dailykoscluster, k=7)
str(clustergroups)
dailykosCluster1 = subset(dailykos, clustergroups == 1)
dailykosCluster2 = subset(dailykos, clustergroups == 2)
dailykosCluster3 = subset(dailykos, clustergroups == 3)
dailykosCluster4 = subset(dailykos, clustergroups == 4)
dailykosCluster5 = subset(dailykos, clustergroups == 5)
dailykosCluster6 = subset(dailykos, clustergroups == 6)
dailykosCluster7 = subset(dailykos, clustergroups == 7)

nrow(dailykosCluster1)
nrow(dailykosCluster2)
nrow(dailykosCluster3)
nrow(dailykosCluster4)
nrow(dailykosCluster5)
nrow(dailykosCluster6)
nrow(dailykosCluster7)

tail(sort(colMeans(dailykosCluster1)))
tail(sort(colMeans(dailykosCluster2)))
tail(sort(colMeans(dailykosCluster3)))
tail(sort(colMeans(dailykosCluster4)))
tail(sort(colMeans(dailykosCluster5)))
tail(sort(colMeans(dailykosCluster6)))
tail(sort(colMeans(dailykosCluster7)))

set.seed(1000)
k=7
KMC = kmeans(dailykos,centers=k)
kmeanscluster = KMC$cluster
dailykosCluster1 = subset(dailykos, kmeanscluster == 1)
dailykosCluster2 = subset(dailykos, kmeanscluster == 2)
dailykosCluster3 = subset(dailykos, kmeanscluster == 3)
dailykosCluster4 = subset(dailykos, kmeanscluster == 4)
dailykosCluster5 = subset(dailykos, kmeanscluster == 5)
dailykosCluster6 = subset(dailykos, kmeanscluster == 6)
dailykosCluster7 = subset(dailykos, kmeanscluster == 7)
  
nrow(dailykosCluster1)
nrow(dailykosCluster2)
nrow(dailykosCluster3)
nrow(dailykosCluster4)
nrow(dailykosCluster5)
nrow(dailykosCluster6)
nrow(dailykosCluster7)

tail(sort(colMeans(dailykosCluster1)))
tail(sort(colMeans(dailykosCluster2)))
tail(sort(colMeans(dailykosCluster3)))
tail(sort(colMeans(dailykosCluster4)))
tail(sort(colMeans(dailykosCluster5)))
tail(sort(colMeans(dailykosCluster6)))
tail(sort(colMeans(dailykosCluster7)))

table(clustergroups, kmeanscluster)

KmeansClusterspl=split(dailykos,KMC$cluster)
str(KmeansClusterspl)

sapply(KmeansClusterspl, function(c) {tail(sort(colMeans(c)))})

airlines = read.csv("airlinescluster.csv")
summary(airlines)

preproc = preProcess(airlines)

airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

airlinedistances = dist(airlinesNorm, method = "euclidean")
airlinesclust = hclust(airlinedistances, method = "ward.D" )
plot(airlinesclust)
rect.hclust(airlinesclust, k=2, border = "red")

airlinegroups = cutree(airlinesclust, k=5)
table(airlinegroups)

tapply(airlines$Balance, airlinegroups, mean)
tapply(airlines$QualMiles, airlinegroups, mean)
tapply(airlines$BonusMiles, airlinegroups, mean)
tapply(airlines$BonusTrans, airlinegroups, mean)
tapply(airlines$FlightMiles, airlinegroups, mean)
tapply(airlines$FlightTrans, airlinegroups, mean)
tapply(airlines$DaysSinceEnroll, airlinegroups, mean)

airlinesUnnormClusters<-split(airlines,airlinegroups)
round(sapply(airlinesUnnormClusters,colMeans),4)

set.seed(88)
airlinesKMC = kmeans(airlinesNorm, centers = 5, iter.max =100)
kmccluster = airlinesKMC$cluster
table(kmccluster)

airlinesKMC$centers


airlinesUnnormClusters<-split(airlinesNorm,clusterGroups)
round(sapply(airlinesUnnormClusters,colMeans),4)
    

stocks = read.csv("StocksCluster.csv")

table(stocks$PositiveDec)

6324/11580
cor(stocks)

sort(colMeans(stocks))

set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset (stocks, spl==TRUE)
stocksTest = subset(stocks, spl==FALSE)
stocksLOG = glm(PositiveDec~., data = stocksTrain, family = binomial)

pred = predict(stocksLOG, type="response")

cmat_LR <-table(stocksTrain$PositiveDec, pred> 0.5)
cmat_LR 

accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR



pred = predict(stocksLOG, type="response", newdata=stocksTest )

cmat_LR <-table(stocksTest$PositiveDec, pred> 0.5)
cmat_LR 

accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR


table(stocksTest$PositiveDec)


limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTest)
mean(normTrain$ReturnJan)


set.seed(144)
km = kmeans(normTrain, centers = 3, iter.max =1000)
table(km$cluster)

km.kcca = as.kcca(km,normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata = normTest)
table(clusterTest)

stocktrain1 = subset (stocksTrain, clusterTrain ==1)
stocktrain2 = subset (stocksTrain, clusterTrain ==2)
stocktrain3 = subset (stocksTrain, clusterTrain ==3)

stockTest1 = subset (stocksTest, clusterTest==1)
stockTest2 = subset (stocksTest, clusterTest==2)
stockTest3 = subset (stocksTest, clusterTest==3)

mean(stocktrain1$PositiveDec)
mean(stocktrain2$PositiveDec)
mean(stocktrain3$PositiveDec)

mean(stockTest11$PositiveDec)
mean(stockTest12$PositiveDec)
mean(stockTest13$PositiveDec)

StocksModel1 = glm(PositiveDec~., data = stocktrain1, family = binomial)
StocksModel2 = glm(PositiveDec~., data = stocktrain2, family = binomial)
StocksModel3 = glm(PositiveDec~., data = stocktrain3, family = binomial)

str(StocksModel1)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredTest1 = predict (StocksModel1, newdata = stockTest1, type ="response")
PredTest2 = predict (StocksModel2, newdata = stockTest2, type ="response")
PredTest3 = predict (StocksModel3, newdata = stockTest3, type ="response")

cmat_LR = table(stockTest1$PositiveDec, PredTest1 > 0.5)
accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR
cmat_LR = table(stockTest2$PositiveDec, PredTest2 > 0.5)
accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR
cmat_LR = table(stockTest3$PositiveDec, PredTest3 > 0.5)
accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR
 
AllPredictions = c(PredTest1, PredTest2, PredTest3)

AllOutcomes = c(stockTest1$PositiveDec, stockTest2$PositiveDec, stockTest3$PositiveDec)
AllOutcomes
cmat_LR = table(AllOutcomes, AllPredictions > 0.5)
accu_LR <-(cmat_LR[1,1] + cmat_LR[2,2])/sum(cmat_LR)
accu_LR

who = read.csv("who.csv")
str(who)

plot(who$GNI, who$FertilityRate)
scatterplot = ggplot(who, aes(x=GNI, y=FertilityRate))
fertilityGNIplot = scatterplot + geom_point(color="dark red", size=3, shape=15) + ggtitle("Veera's first plot")
pdf("myplot.pdf")
print(fertilityGNIplot)
dev.off()

ggplot(who, aes(x=GNI, y=FertilityRate, color = Region)) + geom_point()

ggplot(who, aes(x=GNI, y=FertilityRate, color = LifeExpectancy)) + geom_point()
ggplot(who, aes(x=log(FertilityRate), y=Under15, color = Region)) + geom_point()
ggplot(who, aes(x=log(FertilityRate), y=Under15, color = Region)) + geom_point()  + scale_color_brewer(palette="Dark2") 
model = lm(Under15 ~ log(FertilityRate), data = who)
summary(model)

ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point() + stat_smooth(method = "lm",se = FALSE, color="red")

mvt = read.csv("mvt.csv", stringsAsFactors =  FALSE)
str(mvt)
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
mvt$Weekday = weekdays((mvt$Date))
mvt$Hour = mvt$Date$hour
str(mvt)

WeekDayCounts = as.data.frame(table(mvt$Weekday))
str(WeekDayCounts)

ggplot(WeekDayCounts, aes(x=Var1 , y=Freq)) + geom_line(aes(group=1))
WeekDayCounts$Var1 = factor(WeekDayCounts$Var1, ordered = TRUE, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday"))
ggplot(WeekDayCounts, aes(x=Var1 , y=Freq)) + geom_line(aes(group=1), alpha=0.9) + xlab("Day of the Week") + ylab("Total Motor Vehicle Theft")
WeekDayHour = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(WeekDayHour)
WeekDayHour$Hour = as.numeric(as.character(WeekDayHour$Var2))

ggplot(WeekDayHour, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1,color=Var1), size=1)

WeekDayHour$Var1 = factor(WeekDayHour$Var1, ordered = TRUE, levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday"))
ggplot(WeekDayHour, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq), size=2) + scale_fill_gradient(name = "Total Vehicle Thefts", low="white", high="red") + theme(axis.title.y = element_blank())

chicago = get_map(location="chicago",zoom=11)
ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude, y=Latitude))
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(LatLonCounts)

LatLonCounts2 = subset (LatLonCounts, Freq>0)
str(LatLonCounts2)
LatLonCounts2$Long = as.numeric(as.character(LatLonCounts2$Var1))
LatLonCounts2$Lat = as.numeric(as.character(LatLonCounts2$Var2))
ggmap(chicago) + geom_point(data=LatLonCounts2,aes(x=Long, y=Lat, color = Freq, size=Freq)) +scale_color_gradient(low = "yellow", high="red")
ggmap(chicago) + geom_tile(data=LatLonCounts2,aes(x=Long, y=Lat, alpha = Freq, size=Freq), fill="red")
1638-686

murders=read.csv("murders.csv")
str(murders)

statesMap = map_data("state")
str(statesMap)

ggplot(statesMap, aes(x=long, y=lat, group = group)) + geom_polygon(fill="white" , color = "brown")
murders$region = tolower(murders$State)

murderMap = merge(murders, statesMap,by="region" )
str(murderMap)
murderMap$Population
ggplot(murderMap, aes(x=long, y=lat, group = group, fill=Murders)) + geom_polygon(color = "brown") + scale_fill_gradient(low="grey", high="red", guide="legend")
murderMap$Rate = murderMap$Murders/murderMap$Population*100000
max(murderMap$Rate)
ggplot(murderMap, aes(x=long, y=lat,group = group, fill=GunOwnership)) + 
  geom_polygon(color = "brown") +
  scale_fill_gradient(low="grey", high="red", guide="legend") 

intl = read.csv("intl.csv")
str(intl)

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text (aes(label=PercentOfIntl))

intl = transform(intl, Region = reorder(Region, - PercentOfIntl))
str(intl)
intl$PercentOfIntl = intl$PercentOfIntl*100

ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat="identity", fill ="orange") +
  geom_text (aes(label=PercentOfIntl),vjust=-0.4) +
  ylab("Percent of International Students") +
  theme(axis.title.x= element_blank(),axis.text.x = element_text(angle=45,hjust=1))

intlall = read.csv("intlall.csv", stringsAsFactors = FALSE)
str(intlall)
intlall[is.na(intlall)] = 0

world_map = map_data("world")
str(world_map)
world_map = merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)
ggplot(world_map, aes(x=long, y=lat, group = group))+geom_polygon(fill="white",color="black") + coord_map ("mercator")
world_map = world_map [order(world_map$group,world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group = group))+geom_polygon(aes(fill=Total),color="black") + coord_map ("mercator",xlim=c(-180,180), ylim=c(-60, 90))

intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"
  
ggplot(world_map, aes(x=long, y=lat, group = group))+geom_polygon(aes(fill=Total),color="black") + coord_map ("ortho",orientation=c(-37,175,0),xlim=c(-180,180), ylim=c(-60, 90))

households = read.csv ("households.csv")

melt(households,id="Year")[1:20,]

ggplot(melt(households,id="Year"),aes(x=Year, y=value, color=variable)) + geom_line(size=2) + geom_point(size=3) + ylab("% of hourseholds")

statesMap = map_data("state")
str(statesMap)
length(unique(statesMap$group))
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="red")

polling = read.csv("pollingimputed.csv")

Train = subset(polling, Year==2004 | Year == 2008)
Test = subset (polling, Year == 2012)

mod2 = glm( Republican ~ SurveyUSA + DiffCount , data = Train, family = binomial)
TestPrediction = predict(mod2, newdata = Test, type = "response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(predictionDataFrame$TestPredictionBinary)
colors()
summary(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(predictionDataFrame, statesMap, by="region")
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap )

ggplot(predictionMap, aes(x=long, y=lat, group=group, fill = TestPredictionBinary))+ geom_polygon(color="red")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype = 1, alpha=0.3,size=1) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(users)
str(edges)

nrow(edges)*2 / nrow(users)
table(users$locale, users$school)
table(users$gender, users$school)

?graph.data.frame

g <- graph.data.frame(edges, FALSE, users)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
sort(degree(g))
sort(V(g)$size)

V(g)$color = "black"
V(g)$color[V(g)$gender=="A"]="red"
V(g)$color[V(g)$gender=="B"]="gray"
V(g)$gender[V(g)$size==11]

V(g)$color[V(g)$school=="A"]="orange"
V(g)$color[V(g)$school=="B"]="white"
V(g)$color[V(g)$school=="AB"]="red"
plot(g, vertex.label=NA)


V(g)$color[V(g)$locale=="A"]="orange"
V(g)$color[V(g)$locale=="B"]="white"
plot(g, vertex.label=NA)

tweets = read.csv("tweets.csv")

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english"))  )
freq = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(freq))

colnames(allTweets)
colSums(allTweets)
str(allTweets)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),rot.per=0.5)
display.brewer.all() 

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),min.freq=10,random.order=FALSE,random.color=TRUE,colors=brewer.pal(9,"Blues")[c(5,6,7,8,9)])

#Final Exam

visits = read.csv("park_visits.csv")

str(visits)

visits2016jul = subset (visits, Year==2016 & Month==7 )

sort(table(visits2016jul$ParkType))

head(visits2016jul[order(visits2016jul$NumberOfVisits,decreasing = TRUE),],n=5)

aggregate(visits2016jul$logVisits~visits2016jul$Region,data = visits2016jul, FUN=mean)

cor(visits2016jul$cost, visits2016jul$logVisits)

ggplot(visits2016jul,aes(y=cost, x=logVisits)) + geom_point(color="orange", size=1, shape=15)


ys = subset(visits, ParkName == "Yellowstone NP")

ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)

plot(ys_ts)

colSums(is.na(visits)) 

visits = visits[rowSums(is.na(visits)) == 0, ]
str(visits)

visits$Month = as.factor(visits$Month)

train = subset(visits, visits$Year >=2010 & visits$Year <= 2014)
test = subset(visits, visits$Year >=2015 & visits$Year <= 2016)
str(train)
summary(train)
mod = lm(logVisits ~ laglogVisits, data = train)
summary(mod)

pred = predict(mod, newdata = test)
summary(pred)
str(pred)

sse = sum((pred-test$logVisits)^2)
sst = sum((mean(visits$logVisits)-test$logVisits)^2)
R2 = 1-sse/sst
R2
str(visits)
mod2 = lm(logVisits ~ ParkType+Region+Year+Month+cost+laglogVisits+laglogVisitsYear , data=train)
summary(mod2)$r.squared

pred2 = predict(mod2, newdata = test)

sse = sum((pred2-test$logVisits)^2)
sst = sum((mean(visits$logVisits)-test$logVisits)^2)
R2 = 1-sse/sst
R2



CARTvisit = rpart(logVisits ~ ParkType+Region+Year+Month+cost+laglogVisits+laglogVisitsYear , data=train, method = "class", cp=0.05)
summary(CARTvisit)
prp(CARTvisit)

pred=predict(CARTvisit, newdata=test, type="class")
pred



Cvisit = rpart(logVisits ~ ParkType+Region+Year+Month+cost+laglogVisits+laglogVisitsYear , data=train, cp=0.05)
prp(Cvisit)
pred=predict(Cvisit, newdata=test)
sse = sum((pred-test$logVisits)^2)
sst = sum((mean(visits$logVisits)-test$logVisits)^2)
R2 = 1-sse/sst
R2



set.seed(201)
VisitRF = randomForest(logVisits ~ ParkType+Region+Year+Month+cost+laglogVisits+laglogVisitsYear , data=train)
pred=predict(VisitRF, newdata=test)
sse = sum((pred-test$logVisits)^2)
sst = sum((mean(visits$logVisits)-test$logVisits)^2)
R2 = 1-sse/sst
R2
str(VisitRF)



#Final Second set of questions

bank = read.csv("bank.csv")

str(bank)    
mean(bank$age)

sort(tapply(bank$duration, bank$job, FUN=mean))

cor(bank[c("emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")])

set.seed(201)

library(caTools)

spl = sample.split(bank$y, SplitRatio =  0.7)

training = subset (bank, spl==TRUE)
testing = subset (bank, spl==FALSE)
str(testing)
str(training)
mod = glm(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx, data = training, family="binomial")
str(mod)
summary(mod)

pred = predict(mod, newdata = testing, type = "response")

table (testing$y, pred > 0.5)

ROCRPred = prediction(pred, testing$y)
ROCRPerf = performance(ROCRPred, "tpr", "fpr")
auc = as.numeric(performance(ROCRPred, "auc")@y.values)
auc
plot(ROCRPerf, colorize="TRUE")

plot(ROCRPerf, colorize="TRUE",print.cutoffs.at=seq(0,1,0.11), text.adj=c(-0.2,1.7))

orders = read.csv("orders.csv")
str(orders)
# table(orders$order_hour_of_day)
# mean(orders$days_since_prior_order)
# cor(orders$fresh.fruits, orders$fresh.vegetables)
# table(orders$frozen.pizza)
# (211+44+4+1+1)
# 261/(261+4739)


orders.aisle = orders[, 5:ncol(orders)]
library(caret)
str(orders.aisle)
#orders.aisle[1:134] = lapply(orders.aisle[1:134], as.numeric)
preproc = preProcess(orders.aisle)

ordersNorm = predict(preproc, orders.aisle)
str(ordersNorm)
set.seed(200)
distances <- dist(ordersNorm, method = "euclidean")

ClusterProducts <- hclust(distances, method = "ward.D")

plot(ClusterProducts, labels = FALSE)
summary(ClusterProducts)

set.seed(200)

KMC = kmeans(ordersNorm, centers = 4)
clustergroups = KMC$cluster
table(clustergroups)
ordercenters = KMC$centers

clustergroups

ordercenters = as.data.frame(ordercenters)
write.csv(ordercenters, file="ordercenters-veera.csv")

Cluster1 = subset(orders, clustergroups == 1)
Cluster2 = subset(orders, clustergroups == 2)
Cluster3 = subset(orders, clustergroups == 3)
Cluster4 = subset(orders, clustergroups == 4)

sort(rowMeans(Cluster4))

summary(Cluster1$days_since_prior_order)
summary(Cluster2$days_since_prior_order)
summary(Cluster3$days_since_prior_order)
summary(Cluster4$days_since_prior_order)

apply(Cluster1, 1, FUN=mean)

Cluster1 = Cluster1[,5:ncol(Cluster1)]
wordcloud(colnames(Cluster1), colSums(Cluster1), scale=c(2, 0.25),rot.per=0.5,min.freq=100,random.order=FALSE,random.color=TRUE,colors=brewer.pal(9,"Blues"))

Cluster2 = Cluster2[,5:ncol(Cluster2)]
wordcloud(colnames(Cluster2), colSums(Cluster2), scale=c(2, 0.25),rot.per=0.5,min.freq=100,random.order=FALSE,random.color=TRUE,colors=brewer.pal(9,"Blues"))

Cluster3 = Cluster3[,5:ncol(Cluster3)]
wordcloud(colnames(Cluster3), colSums(Cluster3), scale=c(2, 0.25),rot.per=0.5,min.freq=100,random.order=FALSE,random.color=TRUE,colors=brewer.pal(9,"Blues"))

Cluster4 = Cluster4[,5:ncol(Cluster4)]
wordcloud(colnames(Cluster4), colSums(Cluster4), scale=c(2, 0.25),rot.per=0.5,min.freq=5,random.order=FALSE,random.color=TRUE,colors=brewer.pal(9,"Blues"))

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),min.freq=10,random.order=FALSE,random.color=TRUE,colors=brewer.pal(9,"Blues")[c(5,6,7,8,9)])


boxplot (orders$days_since_prior_order~KMC$cluster)

ggplot(orders, aes(y=orders$days_since_prior_order, x=KMC$cluster, group=KMC$cluster)) + geom_boxplot()

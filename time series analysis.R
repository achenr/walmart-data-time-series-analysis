### CASE 1 ###

library(forecast)
library(readr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)

train <- read_csv("C:/Users/Robert DiFazio/Desktop/IDS 552/Case 1/train.csv")
features <- read_csv("C:/Users/Robert DiFazio/Desktop/IDS 552/Case 1/features.csv")
stores <- read_csv("C:/Users/Robert DiFazio/Desktop/IDS 552/Case 1/stores.csv")

trainingData <- train

dataFreq= 52 # 52 weeks per year

startEntry= c(2010,6)  #the first period is the week of Feb 5 2010 (the sixth week of the year)
endEntry= c(2012,44)  # the last period is the week if Oct 25 2012 (the 44th week)

demandTS <- ts(trainingData$Weekly_Sales, frequency=dataFreq, 
               start=startEntry, end = endEntry ) #create a time series

plot(demandTS,main = "Sales at Store 1 Dept 1 \n Feb 5, 2010 thru Oct 25 2012", xlab="Year",ylab="Sales") #plot 

######### GOAL: Compare TS of Dept 1 sales for four random stores  ########

# SELECT 4 random numbers between 1 and 45 for Store 

# https://www.random.org/integers/

# 15
# 34
# 17
# 2

# Create subsets of trainingData based on Store Number 

### STORE 2 All Dept ###

Store2AllDept <- trainingData[trainingData$Store == 2,] 
Str2Dept1TS <- ts(Store2AllDept$Weekly_Sales, frequency=dataFreq, 
                     start=startEntry, end = endEntry ) #create a time series
plot(Str2Dept1TS, main = "Sales at Store 2 Dept 1 \n Feb 5, 2010 thru Oct 25 2012", 
                  xlab="Year",ylab="Sales") 

### STORE 15 All Dept ###

Store15AllDept <- trainingData[trainingData$Store == 15,] 
Str15Dept1TS <- ts(Store15AllDept$Weekly_Sales, frequency=dataFreq, 
                  start=startEntry, end = endEntry ) #create a time series
plot(Str15Dept1TS, main = "Sales at Store 15 Dept 1 \n Feb 5, 2010 thru Oct 25 2012", 
                  xlab="Year",ylab="Sales")  

### STORE 34 All Dept ###

Store34AllDept <- trainingData[trainingData$Store == 34,] 
Str34Dept1TS <- ts(Store34AllDept$Weekly_Sales, frequency=dataFreq, 
                   start=startEntry, end = endEntry ) #create a time series
plot(Str34Dept1TS, main = "Sales at Store 34 Dept 1 \n Feb 5, 2010 thru Oct 25 2012", 
     xlab="Year",ylab="Sales")  

### STORE 17 All Dept ###

Store17AllDept <- trainingData[trainingData$Store == 17,] 
Str17Dept1TS <- ts(Store17AllDept$Weekly_Sales, frequency=dataFreq, 
                   start=startEntry, end = endEntry ) #create a time series
plot(Str17Dept1TS, main = "Sales at Store 17 Dept 1 \n Feb 5, 2010 thru Oct 25 2012", 
     xlab="Year",ylab="Sales") 

### Create a Display of Four TS's ###

par(mar=c(3,3,3,3))
par(mfrow=c(2,2))
plot(Str17Dept1TS, main = "Sales at Store 17 Dept 1", 
     xlab="Year",ylab="Sales")
plot(Str34Dept1TS, main = "Sales at Store 34 Dept 1", 
     xlab="Year",ylab="Sales") 
plot(Str15Dept1TS, main = "Sales at Store 15 Dept 1", 
     xlab="Year",ylab="Sales") 
plot(Str2Dept1TS, main = "Sales at Store 2 Dept 1", 
     xlab="Year",ylab="Sales")

######### GOAL: Compare TS of Random Deptartments at Random Stores  ########

# SELECT 4 random numbers for Store between 1 and 45 

# https://www.random.org/integers/

# 22
# 15
# 36
# 26

# SELECT 4 random numbers for Department between 1 and 98 

# https://www.random.org/integers/

# 68
# 44
# 21
# 13

# Create subsets of trainingData based on Store Number and Department number

### STORE 22 Dept 68 ###

Store22Dept68 <- trainingData[trainingData$Store == 22 & trainingData$Dept == 98,] 
Store22Dept68TS <- ts(Store22Dept68$Weekly_Sales, frequency=dataFreq, 
                  start=startEntry, end = endEntry ) #create a time series
plot(Store22Dept68TS, main = "Sales at Store 22 Dept 68 \n Feb 5, 2010 thru Oct 25 2012", 
     xlab="Year",ylab="Sales") 

### STORE 15 Dept 44 ###

Store15Dept44 <- trainingData[trainingData$Store == 15 & trainingData$Dept == 44,] 
Store15Dept44TS <- ts(Store15Dept44$Weekly_Sales, frequency=dataFreq, 
                      start=startEntry, end = endEntry ) #create a time series
plot(Store15Dept44TS, main = "Sales at Store 15 Dept 44 \n Feb 5, 2010 thru Oct 25 2012", 
     xlab="Year",ylab="Sales") 

### STORE 36 Dept 21 ###

Store36Dept21 <- trainingData[trainingData$Store == 36 & trainingData$Dept == 21,] 

Store36Dept21TS <- ts(Store36Dept21$Weekly_Sales, frequency=dataFreq, 
                      start=startEntry, end = endEntry ) #create a time series
plot(Store36Dept21TS, main = "Sales at Store 36 Dept 21 \n Feb 5, 2010 thru Oct 25 2012", 
     xlab="Year",ylab="Sales") 

### STORE 26 Dept 13 ###

Store26Dept13 <- trainingData[trainingData$Store == 26 & trainingData$Dept == 13,] 
Store26Dept13TS <- ts(Store26Dept13$Weekly_Sales, frequency=dataFreq, 
                      start=startEntry, end = endEntry ) #create a time series
plot(Store26Dept13TS, main = "Sales at Store 26 Dept 13 \n Feb 5, 2010 thru Oct 25 2012", 
     xlab="Year",ylab="Sales") 

### Create a Display of Four TS's ###

par(mar=c(3,3,3,3))
par(mfrow=c(2,2))
plot(Store26Dept13TS, main = "Sales at Store 26 Dept 13", 
     xlab="Year",ylab="Sales")
plot(Store36Dept21TS, main = "Sales at Store 36 Dept 21", 
     xlab="Year",ylab="Sales") 
plot(Store15Dept44TS, main = "Sales at Store 15 Dept 44", 
     xlab="Year",ylab="Sales") 
plot(Store22Dept68TS, main = "Sales at Store 22 Dept 68", 
     xlab="Year",ylab="Sales") 

######### GOAL: Compare TS of sales at four random stores  ########

### Aggregate Data ###

StoreLevelSales <- aggregate(trainingData$Weekly_Sales, 
                             by=list(trainingData$Store, 
                                     trainingData$Date), FUN=mean)  #mean instead of sum  
# Aggregating sales of each store 
# by department then date

str(StoreLevelSales$Group.2)                                # check data type, its a chr

StoreLevelSales$Group.3 <- as.Date(StoreLevelSales$Group.2, "%m/%d/%Y") # create new column with
# data type = date

StoreLevelSales$Group.3[1:5]                                            # print a few rows

OrderedSales <- order(StoreLevelSales$Group.1, StoreLevelSales$Group.3) # order by store by date

OrderedSalesTable <- StoreLevelSales[OrderedSales,]                     # create ordered sales object

FinalStoreSales <- OrderedSalesTable [,-2]                              # drop second column / new object

colnames(FinalStoreSales) <- c("Store", "Weekly_Sales", "Date")         # name columns


# SELECT 4 random numbers between 1 and 45 for Store 

# https://www.random.org/integers/

# 32
# 18
# 5
# 35

# Create subsets of trainingData based on Store Number 

Store32 <- FinalStoreSales[FinalStoreSales$Store == 32,] 
Store18 <- FinalStoreSales[FinalStoreSales$Store == 18,] 
Store5  <- FinalStoreSales[FinalStoreSales$Store == 5,] 
Store35 <- FinalStoreSales[FinalStoreSales$Store == 35,] 

# Create TS's of subsets

Store32TS <- ts(Store32$Weekly_Sales, frequency=dataFreq, 
                start=startEntry, end = endEntry ) 

Store18TS <- ts(Store18$Weekly_Sales, frequency=dataFreq, 
                start=startEntry, end = endEntry ) 

Store5TS <- ts(Store5$Weekly_Sales, frequency=dataFreq, 
                start=startEntry, end = endEntry ) 

Store35TS <- ts(Store35$Weekly_Sales, frequency=dataFreq, 
                start=startEntry, end = endEntry ) 

### Create a Display of Four TS's ###

par(mar=c(3,3,3,3))
par(mfrow=c(2,2))
plot(Store32TS, main = "Sales at Store 87", xlab="Year",ylab="Sales") 
plot(Store18TS, main = "Sales at Store 74", xlab="Year",ylab="Sales") 
plot(Store5TS, main = "Sales at Store 55", xlab="Year",ylab="Sales") 
plot(Store35TS, main = "Sales at Store 40", xlab="Year",ylab="Sales") 


### Aggregate Time Series ###

dataFreq= 52 # 52 weeks per year

startEntry= c(2010,6)  #the first period is the week of Feb 5 2010 (the sixth week of the year)
endEntry= c(2012,44)   # the last period is the week if Oct 25 2012 (the 44th week)

AggTS <- ts(FinalStoreSales$Weekly_Sales, frequency=dataFreq, 
            start=startEntry, end = endEntry ) #create a time series


### Create Train and Test for Agg Data ###


trainSetStart= c(2010,6)    # training set start location in time series 
# (typically the first entry)
trainSetEnd= c(2012,6)      # training set end location in time series 
# (typically covers 70% of time series)
# there are 143 obs in series so 100 in train and 43 in test
testSetStart= c(2012,7)     # test set start location in time series 
# (typically location of entry after training set ends)
testSetEnd= c(2012,44)      # test set end location in time series 
# (typically end of time series)

AggTrain <- window(AggTS,start=trainSetStart,end=trainSetEnd)   #extract training set
AggTest <- window(AggTS,start=testSetStart,end=testSetEnd)      #extract test set

plot(AggTS, main = "Sales at Store 1 \n Feb 5, 2010 thru Oct 25 2012", 
     xlab="Year",ylab="Sales") 

#### NAive Methods ####
# 
# rwf(y, h = 10, drift = FALSE, level = c(80, 95), fan = FALSE,
#     lambda = NULL, biasadj = FALSE, x = y)
# naive(y, h = 10, level = c(80, 95), fan = FALSE, lambda = NULL,
#       biasadj = FALSE, x = y)
# snaive(y, h = 2 * frequency(x), level = c(80, 95), fan = FALSE,
#        lambda = NULL, biasadj = FALSE, x = y)
# 
# Arguments
# y a numeric vector or time series of class ts
# h Number of periods for forecasting
# drift Logical flag. If TRUE, fits a random walk with drift model.
# level Confidence levels for prediction intervals.
# fan If TRUE, level is set to seq(51,99,by=3). This is suitable for fan plots.
# lambda Box-Cox transformation parameter. Ignored if NULL. Otherwise, forecasts
# back-transformed via an inverse Box-Cox transformation.
# biasadj Use adjusted back-transformed mean for Box-Cox transformations. If TRUE,
# point forecasts and fitted values are mean forecast. Otherwise, these points can
# be considered the median of the forecast densities.
# x Deprecated. Included for backwards compatibility
# 
# gold.fcast <- rwf(gold[1:60], h=50)
# plot(gold.fcast)
# plot(naive(gold,h=50),include=200)
# plot(snaive(wineind))

# 3 naive models - last actual - seasonal last actual - moving average

# Naive

LastActual <- naive(AggTrain, h = 38, level = c(80, 95), fan = FALSE, lambda = NULL,
      biasadj = FALSE)

SNaive <- snaive(AggTrain, h = 38 * frequency(52), level = c(80, 95), fan = FALSE,
       lambda = NULL, biasadj = FALSE)

MA <- ma(AggTrain, order=1,centre = TRUE)
MAForecast <- predict(MA, h = 38)

ForecastFrame <- data.frame(lastActual = LastActual$mean, Naive = SNaive$mean, MA = MAForecast$mean)

#### plot RWForecast ###

plot(LastActual, main="Plot of training demand, 
     testing demand, and RWforecast with 80% and 95% 
     prediction intervals",xlab="Time", 
     ylab="Sales")                                # plot the training demand, and forecast 

lines(AggTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), cex=.6,
       legend=c("Training Demand","Forecast","Testing Demand"))  # create plot legend

### plot SNaive Forecast ###

plot(SNaive, main="Plot of training demand, 
     testing demand, and Seasonal Naive Forecast with 80% and 95% 
     prediction intervals",xlab="Time", 
     ylab="Sales")                                # plot the training demand, and forecast 

lines(AggTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), cex=.6,
       legend=c("Training Demand","Forecast","Testing Demand"))  # create plot legend

### MA... ###

plot(MAForecast, main="Plot of training demand, 
     testing demand, and Moving Average Forecast with 80% and 95% 
     prediction intervals",xlab="Time", 
     ylab="Sales")                                # plot the training demand, and forecast 

lines(AggTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), cex=.6,
       legend=c("Training Demand","Forecast","Testing Demand"))  # create plot legend



# ma(x, order, centre = TRUE)
# Arguments
# x Univariate time series
# order Order of moving average smoother
# centre If TRUE, then the moving average is centred for even orders


#### HOLT WINTERS MODEL ####

HWForcModel <- HoltWinters(AggTrain,seasonal="multiplicative")           #Train Holt-Winters forecasting model. Can be additive or multiplicative
HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = HoltWinters)  #Foreacst using Holt-Winters model trained in previous step

plot(HWForecast, main="Plot of training demand, 
     testing demand, and forecast with 80% and 95% 
     prediction intervals",xlab="Time", 
     ylab="Sales")                                # plot the training demand, and forecast 
# with prediction intervals

lines(AggTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2),cex=.6,
       legend=c("Training Demand","Forecast","Testing Demand"))  # create plot legend


######## Decompose #########

tsDecomp <- decompose(AggTS, type="multiplicative") # classical decomposition. 
# Can be set to multiplicative or additive


names(tsDecomp)

plot(tsDecomp)                                 #plot decomposed time series

trainSetStart= c(2010,6)    # training set start location in time series 
                            # (typically the first entry)
trainSetEnd= c(2012,6)      # training set end location in time series 
                            # (typically covers 70% of time series)
                            # there are 143 obs in series so 100 in train and 43 in test
testSetStart= c(2012,7)     # test set start location in time series 
                            # (typically location of entry after training set ends)
testSetEnd= c(2012,44)      # test set end location in time series 
                            # (typically end of time series)

demandTrain <- window(demandTS,start=trainSetStart,end=trainSetEnd)   #extract training set
demandTest <- window(demandTS,start=testSetStart,end=testSetEnd)      #extract test set

numForcPeriods = 38         # number of periods to forecast in the future (Note: If you are computing errors with respect to testing data then this value 
                            # should be equal to the duration of the testing data)

plot(demandTrain)
plot(demandTest)

HWForcModel <- HoltWinters(demandTrain,seasonal="multiplicative")           #Train Holt-Winters forecasting model. Can be additive or multiplicative
HWForecast <- forecast(HWForcModel, h=numForcPeriods, model = HoltWinters)  #Foreacst using Holt-Winters model trained in previous step

plot(HWForecast, main="Plot of training demand, 
     testing demand, and forecast with 80% and 95% 
     prediction intervals",xlab="Time", 
     ylab="Sales")                                # plot the training demand, and forecast 
                                                  # with prediction intervals

lines(demandTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), 
legend=c("Training Demand","Forecast","Testing Demand"))  # create plot legend

###########Analyze forecasting error#########

error = HWForecast$mean - demandTest #difference between forecast and actual demand
AD=abs(error) #absolute value of error

#Create empty vectors to store errors
MSE <- matrix(, nrow = numForcPeriods, ncol = 1)
MAD <- matrix(, nrow = numForcPeriods, ncol = 1)
MAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
bias <- matrix(, nrow = numForcPeriods, ncol = 1)
TS <- matrix(, nrow = numForcPeriods, ncol = 1)

#Label columns of matrices using name of error
colnames(MSE) <- "MSE"
colnames(MAD) <- "MAD"
colnames(MAPE) <- "MAPE"
colnames(bias) <- "bias"
colnames(TS) <- "TS"

#compute errors 
for(t in 1:numForcPeriods){
  MSE[t] <- mean(error[1:t]*error[1:t])
  MAD[t] <- mean(AD[1:t])
  MAPE[t] <- mean(100*abs(error[1:t]/demandTest[1:t]))
  bias[t] <- sum(error[1:t])
  TS[t]= bias[t]/MAD[t]
}

#combined vectors into a dataframe, also appending year and quarter information in the first two columns
error_Meas <- data.frame(floor(time(error)),cycle(error),demandTest,HWForecast$mean,error,AD,MSE,MAD,MAPE,bias,TS)
colnames(error_Meas)[1] <- "Year"
colnames(error_Meas)[2] <- "Qtr"
colnames(error_Meas)[3] <- "Actual demand"
colnames(error_Meas)[4] <- "Forecast"



### GG TS Plots ###

library(ggplot2)
library(plotly)

p = ggplot(aes(x='date', y='beef'), data=meat)

S <- ggplot2::aes(X='Date', y = 'Sales', data=FinalStoreSales)
S

plot(S)

#  w_dat1 <- subset(train_j, Store == 1)

#  w_dat1$Dept <- factor(w_dat1$Dept)       # by dep

refLines_w1 <- FinalStoreSales              # new frame for computed means

pw1 <- ggplot(FinalStoreSales, aes(x = Date, y = Weekly_Sales)) # assign data for graph

pw1 <- pw1 + geom_line(data = refLines_w1,                      # Plotting the "underlayer"
                       aes(x = Date, y = Weekly_Sales),
                       colour = "GRAY", alpha = 1/2, size = 1/2)
pw1 <- pw1 + stat_summary(fun.y=mean, geom="line", colour="green", size = 2)

ggplotly(pw1)

Store15AllDept$Date <- as.Date(Store15AllDept$Date, "%m/%d/%Y") # create new column with
Store15AllDept <- Store15AllDept [,-6]                              # drop second column / new object

p1 <- ggplot(aes(x=Date, y=Weekly_Sales.x, color = Store), data = total)
p1 <- p1 + geom_line() + stat_smooth(color='blue')
ggplotly(p1)
p1

str(Store15AllDept$Date)

# merge two data frames by ID and Country
total <- merge(data frameA,data frameB,by=c("ID","Country"))
newdata<- merge(data1[,1:2],data2[,1:2],by=1,all=TRUE)


Store55$Date <- as.character(Store55$Date)
Store74$Date <- as.character(Store74$Date)
Store32$Date <- as.character(Store32$Date)
Store18$Date <- as.character(Store18$Date)

total <- merge(Store18, Store55, by = "Date")

str(Store55$Date)

frame2$age = frame1[match(frame2$ID, frame1$ID),"age"] 

####



###

# #### Cool example, could do by Month since too many stores
# # plotting reference lines across each facet:
# # Air passenger data. ts converted to long matrix:
# myData <- data.frame(Year = c(floor(time(AirPassengers) + .01)),
#                      Month = c(cycle(AirPassengers)),
#                      Value = c(AirPassengers))
# # easy conversion code from: http://stackoverflow.com/a/4973859/479554
# 
# # convert month numbers to names, using a built-in constant:
# myData$Month <- factor(myData$Month)
# levels(myData$Month) <- month.abb
# referenceLines <- myData  # \/ Rename
# colnames(referenceLines)[2] <- "groupVar"
# zp <- ggplot(myData,
#               aes(x = Year, y = Value))
# zp <- zp + geom_line(data = referenceLines,  # Plotting the "underlayer"
#                        aes(x = Year, y = Value, group = groupVar),
#                        colour = "GRAY", alpha = 1/2, size = 1/2)
# zp <- zp + geom_line(size = 1)  # Drawing the "overlayer"
# zp <- zp + facet_wrap(~ Month)
# zp <- zp + theme_bw()
# 
# #also maybe if geom_line overlayer issue: stat_summary(aes(group=bucket), fun.y=mean, geom="line", colour="green")
# 
# ggplotly()







####

train_meanbystore <- train %>% group_by(Store, Date) %>%      # ASsign groups
  summarise(mWS = mean(Weekly_Sales)) %>%                     # one line per store and 
                                                              # one line per date
  mutate(Date = as.Date(Date, format = '%Y/%M/%D'))           # change to date type

tsMat_mbs <- ts(reshape2::dcast(train_meanbystore, Date~Store)) # transpose

plot(tsMat_mbs)

#forecast by auto.arima:
fc_a.a <- lapply(tsMat_mbs, function(x) forecast(auto.arima(x))) #train data with HW
                                                                  # generate predictions
                  # lapply is similar to a looping function

par(mfrow=c(1,1))
plot(fc_a.a[[2]])
plot(fc_a.a[[45]])
par(mfrow=c(1,1))

arimaout <- auto.arima(AggTrain)
arimafore <- forecast(arimaout,h=38)

accuracy(arimafore)
summary(arimafore)
plot(arimafore)
lines(AggTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), cex=.6,
       legend=c("Training Demand","Forecast","Testing Demand"))  # create plot legend

store1features <- features[1:105,]
FutureTemp <- features$Temperature[106:143]

arimamodfeatures <- auto.arima(AggTrain, xreg=store1features$Temperature)
arimaTemp <- forecast(arimamodfeatures,xreg = FutureTemp, h=38)
plot(arimaTemp)
lines(AggTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), cex=.6,
       legend=c("Training Demand","Forecast","Testing Demand"))  # create plot l
summary(arimaTemp)

### how to do this for train and test data ###

### one to train model ###

### one

##### ARIMA  #####

w <- matrix(0,nrow=27,ncol=4)   # hold the oders of the arima objects
                                  #when you fit arima you look at:
                                  # I: order of AR
                                  # II: order of the differenceing
                                  # III: order of the MA
                                  # 3 ARs * 3 differences * 3 MAs = 27 observations

jj <- 1                         # setting the row number
                                # saving the AIC (4th col) we want to minimize
                                  # looks at error and # of parameters (variance and num of parameters)
for(i in 0:2) {                 # start loop which will fill the matrix with i, j, k and AIC
  for(j in 0:2) { 
    for(k in 0:2) {
      xx <- arima(AggTrain,order=c(i,j,k))
      w[jj,1] <- i
      w[jj,2] <- j
      w[jj,3] <- k
      w[jj,4] <- xx$aic
      jj <- jj +1
    }
  }
}
minrow1 <- which(w[,4]==min(w[,4])) # choose which row has the min AIC

fin.arma <- arima(AggTrain,order=c(w[minrow1,1],
                                   w[minrow1,2],w[minrow1,3])) # we saved order in W matrix      
# but not cooificients
# in fin model we are using the model
# with min AIC and store values

pred <- predict(fin.arma,n.ahead=38) #predicted model, I'll use this final model,
pred
fin.arma

xplower	<- pred$pred - 1.96*pred$se/sqrt(length(pred$pred)) # 95% Interval low
xpupper	<- pred$pred + 1.96*pred$se/sqrt(length(pred$pred)) # 95% upper
pred$lower <- xplower 
pred$upper <- xpupper

####

plot(pred$lower, main="Plot of training demand, 
     testing demand, and ARIMA Forecast with 80% and 95% 
     prediction intervals",xlab="Time", 
     ylab="Sales")                                # plot the training demand, and forecast 

lines(AggTest,col=2)                                   # add the testing demand line to plot
legend("topleft", lty=1, col=c(1,4,2), cex=.6,
       legend=c("Training Demand","Forecast","Testing Demand"))  # create plot legend

#### Evaluate the model on the test data ####

# in the pred object we have attribute pred

error = pred$pred - AggTest #difference between forecast and actual demand
AD=abs(error) #absolute value of error

#Create empty vectors to store errors
MSE <- matrix(, nrow = numForcPeriods, ncol = 1)
MAD <- matrix(, nrow = numForcPeriods, ncol = 1)
MAPE <- matrix(, nrow = numForcPeriods, ncol = 1)
bias <- matrix(, nrow = numForcPeriods, ncol = 1)
TS <- matrix(, nrow = numForcPeriods, ncol = 1)

#Label columns of matrices using name of error
colnames(MSE) <- "MSE"
colnames(MAD) <- "MAD"
colnames(MAPE) <- "MAPE"
colnames(bias) <- "bias"
colnames(TS) <- "TS"

#compute errors 
for(t in 1:numForcPeriods){
  MSE[t] <- mean(error[1:t]*error[1:t])
  MAD[t] <- mean(AD[1:t])
  MAPE[t] <- mean(100*abs(error[1:t]/AggTest[1:t]))
  bias[t] <- sum(error[1:t])
  TS[t]= bias[t]/MAD[t]
}

#combined vectors into a dataframe, also appending year and quarter information in the first two columns
error_Meas <- data.frame(floor(time(error)),cycle(error),AggTest,pred$pred
                         ,error,AD,MSE,MAD,MAPE,bias,TS)
colnames(error_Meas)[1] <- "Year"
colnames(error_Meas)[2] <- "Week"
colnames(error_Meas)[3] <- "Actual demand"
colnames(error_Meas)[4] <- "Forecast"

# want to compare ARIMA with MSE or MAPE with HW model #

# take two forecasts, average them, get another error


train_meanbystore_wyear <- train_meanbystore %>% 
  mutate(Year = lubridate::year(Date), Week = lubridate::week(Date), 
         Yday = lubridate::yday(Date))

train_meanbystore_wyear$Store <- factor(train_meanbystore_wyear$Store)

p1 <- ggplot(train_meanbystore_wyear, aes(x=Yday, y=mWS, color = Store))
p1 <- p1+geom_line() + facet_wrap("Year", nrow = 3)
p1





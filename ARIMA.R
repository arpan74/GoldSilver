#install.packages("tseries")
library(tseries)

plot(DFCom$GSRatio)
# Can see a clear trend line - want to eliminate trends

#Perform Augmented Dickey Fuller Test w/ Null Hypothesis that series is not stationary
#Alternative Hypothesis is that series is stationary
#Alpha value of 0.05
adf.test(DFCom$GSRatio,alternative="stationary",k=0)
# p-value of 0.08962

# Manipulate data set to get iterated differences
diffRatio <- diff(DFCom$GSRatio)
diffRatio <- na.omit(diffRatio)
# diffRatio(Today) = GS Ratio(Today) - GSRatio(Yesterday)

#Perform Augmented Dickey Fuller Test w/ Null Hypothesis that series is not stationary
#Alternative Hypothesis is that series is stationary
#Alpha value of 0.05
adf.test(diffRatio,alternative="stationary",k=0)
# p-value of 0.01 - satisfies alpha paramter

#Compute autocorrelation
#Autocorrelation - Correlation of a series with itself at different points of time
acf(diffRatio)
#Correlation of function with itself for no lag is significant
#Autocorrelation function does not control for the value of the time series for shorter lags
#It indicates that the MA  component for ARIMA should be kept at 0

#Compute partial autocorrelation
#controls for the values of the time series at all shorter lags
pacf(diffRatio)
# One lag period is significant
#indicator for autoregression - AR component in ARIMA

rsquared <- c()
resids <- c() # residual between actual values and predicted
start <- 0 # start date for values of rolling regression
rollDays <- 252 # number of days to roll
end <- start + rollDays # end date for values for rolling regression
actualVal <- c()
Rpred <- c()
Rse <- c()

for(i in index(btDF)){
  start <- start + 1
  end <- start + rollDays
  if(start < rollDays){next}
  if(end == length(index(btDF))){break}
  model <- arima(btDF$GSRatio[start:end], c(1,1,0))
  Rpred <- c(Rpred,predict(model, n.ahead = 1)$pred)
  Rse <- c(Rse, predict(model, n.ahead = 1)$se)
  actualVal <- c(actualVal, predict(model, btDF[end + 1]))
}

resids <- Rpred - actualVal
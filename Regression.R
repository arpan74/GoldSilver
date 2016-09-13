rsquared <- c()
resids <- c() # residual between actual values and predicted
start <- 0 # start date for values of rolling regression
rollDays <- 252 # number of days to roll
end <- start + rollDays # end date for values for rolling regression
actualVal <- c()
predictedVal <- c()
dates <- c()


for(i in index(btDF)){
  start <- start + 1
  end <- start + rollDays
  if(start < rollDays){next}
  if(end == length(index(btDF))){break}
  model <- lm(GSRatio ~ GoldClose + SilvClose, data = btDF[start:end])
  rsquared <- c(rsquared, summary(model)$adj.r.squared)
  predictedVal <- c(predictedVal, predict(model, btDF[end + 1]))
  actualVal <- c(actualVal, btDF[end+1]$GSRatio)
  dates <- c(dates, i)
}
dates <- as.Date(dates)
resids <- predictedVal - actualVal
plot(dates, resids, type = "l")

resids <- as.xts(resids)
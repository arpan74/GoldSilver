rsquared <- c()
resids <- c() # residual between actual values and predicted
start <- 1 # start date for values of rolling regression
end <- start + 252 # end date for values for rolling regression
actualVal <- c()
predictedVal <- c()
money <- 100000
portdelta <- 0
portfolio <- c(money)

# USED TO TEST REGRESSION
# for(i in DFCom$GSRatio){
#   if(end == length(DFCom$GSRatio)){break}
#   model <- lm(GSRatio ~ GoldClose + SilvClose + PlatClose, data = DFCom[start:end])
#   rsquared <- c(rsquared,summary(model1)$adj.r.squared)
#   predictedVal <- c(predictedVal,predict(model1,DFCom[end+1]))
#   actualVal <- c(actualVal,DFCom$GSRatio[end+1])
#   start <- start + 1
#   end <- start + 252
#   print(end)
# }
start <- 1
end <- start + 252

for(i in btDF$IAUClose){
  if(end == length(btDF$IAUClose)){break}
  model <- lm(GSRatio ~ GoldClose + SilvClose + PlatClose, data = btDF[start:end])
  predictedVal <- predict(model,btDF[end+1])
  std <- sd(btDF$GSRatio[start:end])
    
  portfolio <- c(portfolio,portfolio + portdelta)
}

predictedVal <- as.xts(predictedVal)
resids <- as.xts(predictedVal - actualVal)
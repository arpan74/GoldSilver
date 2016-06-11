rsquared <- c() 
resids <- c() # residual between actual values and predicted
start <- 1 # start date for values of rolling regression
end <- start + 252 # end date for values for rolling regression - set to 252 to approximate number of trading days in one year (regression period is one year)
actualVal <- c()
predictedVal <- c()
money <- 100000
portdelta <- 0
portfolio <- c(money)
 
#USED TO TEST REGRESSION
for(i in DFCom$GSRatio){
 if(end == length(DFCom$GSRatio)){break} # break the loop when the end of the regression reaches the end of the data set
 model <- lm(GSRatio ~ GoldClose + SilvClose + PlatClose, data = DFCom[start:end]) # fit a model to the Gold Close, Silver Close, Platinum Close and GS Ratio from the start: end date prices
 rsquared <- c(rsquared,summary(model)$adj.r.squared) # add R^2 value to vector
 predictedVal <- c(predictedVal,predict(model,DFCom[end+1])) # predict the GS Ratio for the next day based on the model created
 actualVal <- c(actualVal,DFCom$GSRatio[end+1])
 start <- start + 1
 end <- start + 252
 print(end)
}




# 
# for(i in btDF$IAUClose){
#   if(end == length(btDF$IAUClose)){break}
#   model <- lm(GSRatio ~ GoldClose + SilvClose + PlatClose, data = btDF[start:end])
#   predictedVal <- c(predictedVal,predict(model,btDF[end+1]))
#   std <- sd(btDF$GSRatio[start:end])
#   start <- start + 1
#   end <- end + 1
# 
# }
# actualVal <- as.xts(btDF$GSRatio)
# predictedVal <- as.xts(predictedVal)
# resids <- as.vector(actualVal["2007-05-17/"]) - as.vector(predictedVal)

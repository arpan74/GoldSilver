#install.packages("tseries")
library(tseries)

plot(DFCom$GSRatio)
# 
adf.test(DFCom$GSRatio,alternative="stationary",k=0)
adf.test(DFCom$GSRatio,alternative="explosive",k=0)
# 
# 
diffRatio <- diff(DFCom$GSRatio)
diffRatio <- na.omit(diffRatio)
adf.test(diffRatio,alternative="stationary",k=0)
adf.test(diffRatio,alternative="explosive",k=0)
# 
acf(diffRatio)

#x <- arima(DFCom$GSRatio[1:1000],c(1,1,0))
#LOOK INTO CROSS VALIDATION TO OPTIMIZE - MACHINE LEARNING CONCEPT
#predict(x,n.ahead = 1)
start <- 1
end <- start + 252
residuals <- c()
predictedVals <- c()
predictedSEs <- c()
dates123 <- c()
# for(i in index(btDF)){
#   x <- arima(DFCom$GSRatio[start:end],c(1,1,0))
#   predicted <- predict(x,n.ahead = 1)
#   predictedVals <- c(predictedVals,predicted$pred)
#   predictedSEs <- c(predictedSEs,predicted$se)
#   dates123 <- c(dates123, i)
#   start <- start + 1
#   end <- start + 252
#   
#   if(end == length(index(btDF))){break;}
# }

dates123 <- as.Date(dates123)

predVals <- data.frame(predictedVals, predictedSEs, row.names = dates123)

colnames(predVals) <- c("pred", "se")
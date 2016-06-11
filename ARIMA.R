#install.packages("tseries")
library(tseries)

# plot(DFCom$GSRatio)
# 
# adf.test(DFCom$GSRatio,alternative="stationary",k=0)
# adf.test(DFCom$GSRatio,alternative="explosive",k=0)
# 
# 
# diffRatio <- diff(DFCom$GSRatio)
# diffRatio <- na.omit(diffRatio)
# adf.test(diffRatio,alternative="stationary",k=0)
# adf.test(diffRatio,alternative="explosive",k=0)
# 
# acf(diffRatio)

#x <- arima(DFCom$GSRatio[1:1000],c(1,1,0))
#SHAUN MENTIONED CROSS VALIDATION MACHINE LEARNING
#predict(x,n.ahead = 1)
Rpred <- data.frame(row.names = c(20:252))
Rse <- data.frame(row.names = c(20:252))
for(p in 20:252){
  start <- 1
  end <- start + p
  residuals <- c()
  predictedVals <- c()
  predictedSEs <- c()
  dates123 <- c()
  for(i in index(btDF)){
    x <- arima(DFCom$GSRatio[start:end],c(1,1,0))
    predicted <- predict(x,n.ahead = 1)
    predictedVals <- c(predictedVals,predicted$pred)
    predictedSEs <- c(predictedSEs,predicted$se)
    dates123 <- c(dates123, i)
    start <- start + 1
    end <- start + p
    
    if(end == length(index(btDF))){break;}
  }
  
  dates123 <- as.Date(dates123)
  
  predictedVals <- c(rep(NA,(2388  - length(predictedVals) )),predictedVals)
  predictedSEs <- c(rep(NA,(2388  - length(predictedSEs) )),predictedSEs)
  Rpred <- rbind(Rpred, predictedVals)
  Rse <- rbind(Rse, predictedSEs)
  predVals <- data.frame(predictedVals, predictedSEs)
  
  colnames(predVals) <- c("pred", "se")
  print(p)
}
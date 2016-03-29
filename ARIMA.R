#install.packages("tseries")
library(tseries)

plot(DFCom$GSRatio)

adf.test(DFCom$GSRatio,alternative="stationary",k=0)
adf.test(DFCom$GSRatio,alternative="explosive",k=0)


diffRatio <- diff(DFCom$GSRatio)
diffRatio <- na.omit(diffRatio)
adf.test(diffRatio,alternative="stationary",k=0)
adf.test(diffRatio,alternative="explosive",k=0)

acf(diffRatio)

x <- arima(DFCom$GSRatio[1:1000],c(1,1,0))

predict(x,n.ahead = 1)
start <- 1
end <- start + 252
residuals <- c()
for(i in DFCom$GSRatio){
  x <- arima(DFCom$GSRatio[start:end],c(1,1,0))
  predicted <- predict(x,n.ahead = 1)
  residuals <- c(residuals,DFCom$GSRatio[end+1] - predicted$pred)
  print(DFCom$GSRatio[end+1] - predicted$pred)
  start <- start + 1
  end <- start + 252
}





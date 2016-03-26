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


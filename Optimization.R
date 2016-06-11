start <- 1
length <- 252
end <- start + length
signal <- 0.5 # Current signal to enter a trade
Esignal <- 0.10 #Current signal to exit a trade
position <- 0 
inTrade <- FALSE
gLong <- FALSE
returns <- c(0)
preturns <- c() #unrealized and realized returns
pdates <- c() #dates for each of the returns for preturns vector
rdates <- c() #vector of dates for returns
diff <- 0


PArima <- function(DFtest){
  x <- arima(DFtest$GSRatio,c(1,1,0))
  return(c(predict(x,n.ahead = 1)$pred,predict(x,n.ahead = 1)$se))
}


start <- 1
end <- start + 20
for(i in btDF$IAUClose){
  prediction <- PArima(btDF[start:end])
  predictedVal <- prediction[1]
  se <- prediction[2]
  print(predVals$pred[start] - predictedVal)
  print(predVals$se[start] - se)
  start <- start + 1
  end <- start + 20
  if( end == length(btDF$IAUClose) ){break}
}
  


for(i in btDF$IAUClose){
  
  if(inTrade == TRUE){     # Calculate unrealized gains/losses
    
    if(gLong == TRUE){
      preturns <- c(preturns, log((btDF$IAUClose[end+1])/pGold) + log(pSilv/(btDF$SLVClose[end+1])))
      pdates <- c(pdates, index(btDF)[end+1])
      if(length(pdates)-length(preturns)  > diff){
        cat("\n")
        print(index(btDF)[end+1])
        print("inTrade glong")
        cat("differnce: ",length(preturns)-length(pdates) )
        cat("\n")
        cat("\n")
        diff <- length(pdates)-length(preturns) 
      }
    }
    
    if(gLong == FALSE){
      preturns <- c(preturns, log(pGold/(btDF$IAUClose[end+1])) + log((btDF$SLVClose[end+1])/pSilv))
      pdates <- c(pdates, index(btDF)[end+1])
      if(length(pdates)-length(preturns)  > diff){
        cat("\n")
        print(index(btDF)[end+1])
        print("inTrade slong GLONG IS FALSE")
        cat("differnce: ",length(preturns)-length(pdates) )
        cat("\n")
        cat("\n")
        diff <- length(pdates)-length(preturns) 
      }
    }
  }
  
  if(inTrade == FALSE){
    preturns <- c(preturns,0)
    pdates <- c(pdates, index(btDF)[end+1])
    if(length(pdates)-length(preturns)  > diff){
      cat("\n")
      print(index(btDF)[end+1])
      print("inTrade is FALSE")
      cat("differnce: ",length(preturns)-length(pdates) )
      cat("\n")
      cat("\n")
      diff <- length(pdates)-length(preturns) 
    }
  }
  
  predictedVal <- predVals$pred[end]
  se <- prediction[2]
  
  if(inTrade == FALSE){
    if( ( abs(predictedVal - btDF$GSRatio[end+1]) - se ) > signal*sd(btDF$GSRatio[start:end]) ){
      if(predictedVal < btDF$GSRatio[end+1]){ # SHORT GOLD LONG SILVER
        pGold <- as.numeric(btDF$IAUClose[end+1]) # price of gold ETF when position was entered
        pSilv <- as.numeric(btDF$SLVClose[end+1]) # price of silv ETF when position was entered
        inTrade <- TRUE
        gLong <- FALSE
        cat("Entering Trade at ")
        print(btDF[end+1,0])
        cat("Short Gold at ",pGold,"\n")
        cat("Long Silver at ",pSilv,"\n")
      }
      if(predictedVal > btDF$GSRatio[end+1]){ # LONG GOLD SHORT SILVER
        pGold <- as.numeric(btDF$IAUClose[end+1]) # price of gold ETF when position was entered
        pSilv <- as.numeric(btDF$SLVClose[end+1]) # price of silv ETF when position was entered
        inTrade <- TRUE
        gLong <- TRUE
        cat("Entering Trade at ")
        print(btDF[end+1,0])
        cat("Long Gold at ", pGold,"\n")
        cat("Short Silver at ",pSilv,"\n")
      }
      
    }
  }
  #Exit Trade
  if(inTrade == TRUE){
    if( ( abs(predictedVal - btDF$GSRatio[end+1]) - se ) < Esignal*sd(btDF$GSRatio[start:end]) ){ # Exit Signal from Trade
      
      if(gLong == TRUE){
        return <- log((btDF$IAUCLose[end+1])/pGold) + log(pSilv/(btDF$SLVClose[end+1]))
        returns <- c(returns,return)
        cat("Gold sold at ",btDF$IAUClose[end+1],"\n")
        cat("Silver bought back at ", btDF$SLVClose[end+1],"\n")
        cat("Trade exited on ")
        print(btDF[end+1,0])
      }
      
      if(gLong == FALSE){
        return <- log(pGold/(btDF$IAUClose[end+1])) + log((btDF$SLVClose[end+1])/pSilv)
        returns <- c(returns,return)
        cat("Gold bought back at ",btDF$IAUClose[end+1],"\n")
        cat("Silver sold at ", btDF$SLVClose[end+1],"\n")
        cat("Trade exited on ")
        print(btDF[end+1,0])
      }
      
      inTrade <- FALSE
      print(as.integer(return))
      rdates <- c(rdates,index(btDF)[end+1])
      cat("\n")
    }
  }
  
  start <- start + 1
  end <- start + length
  if( end == length(btDF$IAUClose) ){break}
}
sharpe <- (mean(preturns)*252)/(sqrt(252)*sd(preturns))

pdates <- as.Date(pdates)



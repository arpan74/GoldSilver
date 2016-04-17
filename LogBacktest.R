start <- 1
length <- 252
end <- start + length
signal <- 0.5
Esignal <- 0.10
portfolio <- c(10000)
position <- 0
inTrade <- FALSE
gLong <- FALSE
returns <- c(0)
PArima <- function(DFtest){
  x <- arima(DFtest$GSRatio,c(1,1,0))
  return(predict(x,n.ahead = 1)$pred)
}
PRegression <- function(DFtest){
  model <- lm(GSRatio ~ GoldClose + SilvClose + PlatClose, data = DFtest)
  return(predict(model, btDF[end+1]))
}


for(i in btDF$IAUClose){
  
  predictedVal <- PArima(btDF[start:end])
  if(inTrade == FALSE){
    if( abs(predictedVal - btDF$GSRatio[end+1]) > signal*sd(btDF$GSRatio[start:end]) ){
      if(predictedVal < btDF$GSRatio[end+1]){ # SHORT GOLD LONG SILVER
        position <- portfolio[length(portfolio)]
        pGold <- as.numeric(btDF$IAUClose[end+1]) # price of gold ETF when position was entered
        pSilv <- as.numeric(btDF$SLVClose[end+1]) # price of silv ETF when position was entered
        inTrade <- TRUE
        gLong <- FALSE
        print("SHORT GOLD LONG SILVER")
      }
      if(predictedVal > btDF$GSRatio[end+1]){ # LONG GOLD SHORT SILVER
        position <- portfolio[length(portfolio)]
        pGold <- as.numeric(btDF$IAUClose[end+1]) # price of gold ETF when position was entered
        pSilv <- as.numeric(btDF$SLVClose[end+1]) # price of silv ETF when position was entered
        inTrade <- TRUE
        gLong <- TRUE
        print("LONG GOLD SHORT SILVER")
      }
      print("Gold Bought at ")
      print(pGold)
      print("Silver bought at ")
      print(pSilv)
    }
  }
  if(inTrade == TRUE){
    if( abs(predictedVal - btDF$GSRatio[end+1]) < Esignal*sd(btDF$GSRatio[start:end]) ){ # Exit Signal from Trade
      inTrade <- FALSE
      if(gLong == TRUE){
        return <- log((btDF$IAUCLose[end+1])/pGold) + log(pSilv/(btDF$SLVClose[end+1]))
        returns <- c(returns,return)
      }
      if(gLong == FALSE){
        return <- log(pGold/(btDF$IAUClose[end+1])) + log((btDF$SLVClose[end+1])/pSilv)
        returns <- c(returns,return)
      }
      inTrade <- FALSE
      print("Gold exited at ")
      print(btDF$IAUClose[end+1])
      print("Silver exted at ")
      print(btDF$SLVClose[end+1])
      print(as.integer(return))
    }
    # End of code to exit from trade
    
    # #Code below is to calculate portfolio value
    # if(gLong == TRUE & inTrade == TRUE){
    #   return <- as.numeric(btDF$IAUCLose[end+1] - pGold)*aGold + as.numeric(pSilv - btDF$SLVClose[end+1])*aSilv
    #   portfolio <- c(portfolio, portfolio[length(portfolio)] + return)
    # }
    # if(gLong == FALSE & inTrade == TRUE){
    #   return <- as.numeric(pGold - btDF$IAUClose[end+1])*aGold + as.numeric(btDF$SLVClose[end+1] - pSilv)*aSilv
    #   portfolio <- c(portfolio, portfolio[length(portfolio)] + return)
    #   
    # }
  }
  
  start <- start + 1
  end <- start + length
  if( end == length(btDF$IAUClose) ){break}
}
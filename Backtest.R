rolling_Days = 150
EnterSig = 0.25
ExitSig = 0.10
inTrade = FALSE
paperProfit <- c(0)
LastGoldPriceBought = 0 # Last price Gold was traded @ - used to calc returns
LastSilvPriceBought = 0 # Last price Silv was traded @ - used to calc returns
tradeCount = 0 # Number of trades being made
returns <- c(0) # vector of realized log returns
gLong = FALSE # if(inTrade) --> this will be True when Long Gold and Short Silv, False if vice versa
predictedRatio = 0
start <- 1
end <- start + rolling_Days
counter <- 0

PArima <- function(DF1){
  x <- arima(DF1$GSRatio,c(1,1,0))
  return(predict(x,n.ahead = 1)$pred)
}

trade <- function(date){
  pGold = as.numeric(btDF[as.Date(date)]$IAUClose)
  pSilv = as.numeric(btDF[as.Date(date)]$SLVClose)
  print(pGold)
  if(inTrade){ # Exiting Trade now
    cat("Trade exited on ", as.Date(date), "\n")
    tempReturn <- CalcReturns(date)
    returns <<- append(returns, tempReturn)
    cat("Returns are ",  tempReturn, "\n")
  }
  else if (!inTrade){
    tradeCount <<- tradeCount + 1
    LastGoldPriceBought <<- pGold
    LastSilvPriceBought <<- pSilv
    cat("Trade entered on ", as.Date(date), "\n")
  }
}

CalcReturns <- function(date){
  if(!inTrade){
    return(0)
  }
  pGold = as.numeric(btDF[as.Date(date)]$IAUClose)
  pSilv = as.numeric(btDF[as.Date(date)]$SLVClose)
  print(pGold)
  if(gLong){
    return(log(pGold/LastGoldPriceBought) + log(LastSilvPriceBought/pSilv))
  }
  return(log(LastGoldPriceBought/pGold) + log(pSilv/LastSilvPriceBought)) 
}


for (i in index(btDF)){
  predictedRatio <- PArima(btDF[start:end])
  if(inTrade){
    counter <- counter + 1
    if( abs(predictedRatio - btDF[as.Date(i)]$GSRatio)  < ExitSig*sd(btDF[start:end]$GSRatio)){
      trade(i)
      inTrade <- FALSE
    }
    paperProfit <- append(paperProfit, CalcReturns(i))
  }
  
  if(!inTrade){
    if( abs(predictedRatio - btDF[as.Date(i)]$GSRatio)  > EnterSig*sd(btDF[start:end]$GSRatio)){
      trade(i)
      inTrade <- TRUE
    }
  }
}
rolling_Days = 252
EnterSig = 0.3
ExitSig = 0.05
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
  if(inTrade){ # Exiting Trade now
    dateString <- toString(as.Date(date))
    cat("Trade exited on ", dateString, "\n")
    tempReturn <- CalcReturns(date)
    returns <<- append(returns, tempReturn)
  }
  else if (!inTrade){
    tradeCount <<- tradeCount + 1
    LastGoldPriceBought <<- pGold
    LastSilvPriceBought <<- pSilv
    dateString <- toString(as.Date(date))
    cat("Trade entered on ", dateString, "\n")
  }
}

CalcReturns <- function(date){
  if(!inTrade){
    return(0)
  }
  pGold = as.numeric(btDF[as.Date(date)]$IAUClose)
  pSilv = as.numeric(btDF[as.Date(date)]$SLVClose)
  if(gLong){
    return(log(pGold/LastGoldPriceBought) + log(LastSilvPriceBought/pSilv))
  }
  return(log(LastGoldPriceBought/pGold) + log(pSilv/LastSilvPriceBought)) 
}


for (i in index(btDF)){
  predictedRatio <- PArima(btDF[start:end])
  if(inTrade){
    counter <- counter + 1
    if( abs(predictedRatio - btDF[end + 1]$GSRatio)  < ExitSig*sd(btDF[start:end]$GSRatio)){
      trade(index(btDF[end + 1]))
      inTrade <- FALSE
    }
    paperProfit <- append(paperProfit, CalcReturns(index(btDF[end + 1])))
  }
  
  if(!inTrade){
    if( abs(predictedRatio - btDF[end + 1]$GSRatio)  > EnterSig*sd(btDF[start:end]$GSRatio)){
      trade(index(btDF[end + 1]))
      inTrade <- TRUE
    }
  }
  
  start <- start + 1
  end <- start + rolling_Days
  if(end == length(btDF) - 1){break}
}
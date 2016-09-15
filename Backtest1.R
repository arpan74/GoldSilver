# This is the backtest where the predictedRatio is kept constant during a trade
# When a trade is happening, the model assumes the Gold/Silver Ratio is mispriced, and any future data on
# the ratio is spurious

# due to the previous assumption, a stop-loss will have to be built in

rolling_Days = 40
rolling_DaysSD = 40
EnterSig = 1.5
ExitSig = 0.10
inTrade = FALSE
paperProfit <- c()
LastGoldPriceBought = 0 # Last price Gold was traded @ - used to calc returns
LastSilvPriceBought = 0 # Last price Silv was traded @ - used to calc returns
tradeCount = 0 # Number of trades being made
returns <- c() # vector of realized log returns
gLong = FALSE # if(inTrade) --> this will be True when Long Gold and Short Silv, False if vice versa
predictedRatio = 0
start <- 1
end <- start + rolling_Days
datesinTrade <- c()

PArima <- function(DF1){
  x <- arima(DF1$GSRatio,c(1,1,0))
  return(predict(x,n.ahead = 1)$pred)
}

trade <- function(date){
  pGold = as.numeric(btDF[as.Date(date)]$IAUClose)
  pSilv = as.numeric(btDF[as.Date(date)]$SLVClose)
  if(inTrade){ # Exiting Trade now
    dateString <- toString(as.Date(date))
    returns <<- c(returns, CalcReturns(date))
    if(gLong){
      cat("Long Gold Short Silver\n")
      cat("Gold bought at ", LastGoldPriceBought, " and sold at ", pGold, "\n")
      cat("Silv bought at ", pSilv, " and sold at ", LastSilvPriceBought, "\n")
    }
    else if (!gLong){
      cat("Short Silver Long Gold\n")
      cat("Gold bought at ", pGold, " and sold at ", LastGoldPriceBought, "\n")
      cat("Silv bought at ", LastSilvPriceBought, " and sold at ", pSilv, "\n")
    }
    cat("Trade exited on ", dateString, "\n")
  }
  else if (!inTrade){
    if( as.numeric(predictedRatio) >  as.numeric(btDF[end + 1]$GSRatio) ){
      gLong <<- TRUE
    }
    else if ( as.numeric(predictedRatio) <  as.numeric(btDF[end + 1]$GSRatio) ){
      gLong <<- FALSE
    }
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
  if(gLong){ # Calculate Return if Long Gold and Short Silver
    cat("The return is ", toString(log(pGold/LastGoldPriceBought) + log(LastSilvPriceBought/pSilv )) , "\n")
    print(log(pGold/LastGoldPriceBought) + log(LastSilvPriceBought/pSilv ))
    cat(pGold, " ", LastGoldPriceBought, " ", pSilv, " ", LastSilvPriceBought, "\n")
    return(log(pGold/LastGoldPriceBought) + log(LastSilvPriceBought/pSilv))
  } # Calculate Return if Short Gold and Long Silver
  cat("The return is ", toString(log(LastGoldPriceBought/pGold) + log(pSilv/LastSilvPriceBought)) , "\n")
  return(log(LastGoldPriceBought/pGold) + log(pSilv/LastSilvPriceBought))
}

# Loop to go through all the data points in the dataset and test strategy
for (i in index(btDF)){ 
  if(!inTrade){ 
    predictedRatio <- PArima(btDF[start:end]) # The predictedRatio is the predicted Gold/Silver Ratio for tomorrow.
  }
  if(inTrade){
    datesinTrade <- c(datesinTrade, index(btDF[end+1]))
    if( abs(as.numeric(predictedRatio) - as.numeric(btDF[end + 1]$GSRatio))  < ExitSig*sd(btDF[start:end]$GSRatio)){
      trade(index(btDF[end + 1]))
      cat("Actual Ratio is ", toString(as.numeric(btDF[end + 1]$GSRatio)), "\n")
      cat("Gold spot is ", toString(btDF[end+1]$GoldClose), "Silv spot is ", toString(btDF[end+1]$SilvClose), "\n\n")
      inTrade <- FALSE
    }
   paperProfit <- append(paperProfit, CalcReturns(index(btDF[end + 1])))
  }
  else if(!inTrade){
    if( abs(as.numeric(predictedRatio) - as.numeric(btDF[end + 1]$GSRatio))  > EnterSig*sd(btDF[start:end]$GSRatio) ){
      trade(index(btDF[end + 1]))
      cat("Actual Ratio is ", toString(as.numeric(btDF[end + 1]$GSRatio)), "\n")
      cat("Predicted Ratio is ", toString(as.numeric(predictedRatio)), "\n")
      cat("Gold spot is ", toString(btDF[end+1]$GoldClose), "Silv spot is ", toString(btDF[end+1]$SilvClose), "\n")
      inTrade <- TRUE
    }
  }
  start <- start + 1
  end <- start + rolling_Days
  
  if(end == length(index(btDF)) - 1){break}
}

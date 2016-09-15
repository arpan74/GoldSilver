rolling_Days = 252
EnterSig = 0.5
ExitSig = 0.10
inTrade = FALSE
paperProfit <- c()
LastGoldPriceBought = 0 # Last price Gold was traded @ - used to calc returns
LastSilvPriceBought = 0 # Last price Silv was traded @ - used to calc returns
tradeCount = 0 # Number of trades being made
returns <- c() # vector of realized log returns
gLong = FALSE # if(inTrade) --> this will be True when Long Gold and Short Silv, False if vice versa
predictedRatio = 0
start <- 0
end <- start + rolling_Days
datesinTrade <- c()
SharpeDF <- data.frame(matrix(sample(c(NA), 14097, replace = TRUE), 381))

PArima <- function(DF1){
  x <- arima(DF1$GSRatio,c(1,1,0))
  return(predict(x,n.ahead = 1)$pred)
}

PArimaDF <- function(date, rolling_Days){
  roller <- toString(rolling_Days)
  return(DFArima[as.Date(date), roller])
}

trade <- function(date){
  pGold = as.numeric(btDF[as.Date(date)]$IAUClose) # Price of Gold using iShares ETF
  pSilv = as.numeric(btDF[as.Date(date)]$SLVClose) # Price of Silv using iShares ETF
  if(inTrade){ # Exiting Trade now
    dateString <- toString(as.Date(date))
    returns <<- c(returns, CalcReturns(date))
  }
  else if (!inTrade){ # Entering Trade now
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
  }
}


CalcReturns <- function(date){
  if(!inTrade){
    return(0)
  }
  pGold = as.numeric(btDF[as.Date(date)]$IAUClose)
  pSilv = as.numeric(btDF[as.Date(date)]$SLVClose)
  if(gLong){ # Calculate Return if Long Gold and Short Silver
    return(log(pGold/LastGoldPriceBought) + log(LastSilvPriceBought/pSilv))
  } # Calculate Return if Short Gold and Long Silver
  return(log(LastGoldPriceBought/pGold) + log(pSilv/LastSilvPriceBought))
}

enterSigNum <- 1
rollingDaysNum <- 1

for( EnterSig in seq( 0.2, 2.0 , 0.05 ) ){
  
  for ( rolling_Days in 20:400 ){
    
    # Loop to go through all the data points in the dataset and test strategy
    for (i in index(btDF)){ 
      
      # These manage the rolling dates for the ARIMA model --> The ARIMA model uses start and end to select
      # part of the btDF to optimize
      start <- start + 1
      end <- start + rolling_Days
      
      if(end == length(index(btDF)) - 1){break}
      
      predictedRatio <- PArimaDF(index(btDF)[end+1], rolling_Days) # The predictedRatio is the predicted Gold/Silver Ratio for tomorrow.
      
      
      
      if(inTrade){ # If in a trade, check if we can exit the trade
        datesinTrade <- c(datesinTrade, index(btDF[end+1])) # Add the current date to a vector containing all the dates
        # that the program is in a trade
        
        #If statement below checks if we are within the confines to exit the trade - the confines are set by the
        #standard deviation of the ratio and an ExitSignal
        if( abs(as.numeric(predictedRatio) - as.numeric(btDF[end + 1]$GSRatio))  < ExitSig*sd(btDF[start:end]$GSRatio)){
          trade(index(btDF[end + 1]))
          paperProfit <- append(paperProfit, CalcReturns(index(btDF[end + 1])))
          inTrade <- FALSE
        }
        else {  
          paperProfit <- append(paperProfit, CalcReturns(index(btDF[end + 1])))
        }
      }
      else if(!inTrade){
        if( abs(as.numeric(predictedRatio) - as.numeric(btDF[end + 1]$GSRatio))  > EnterSig*sd(btDF[start:end]$GSRatio) ){
          trade(index(btDF[end + 1]))
          inTrade <- TRUE
        }
        paperProfit <- append(paperProfit, CalcReturns(index(btDF[end + 1])))
      }
      
    }
    cat("Currently using ", rolling_Days, " for # of roll days and ", EnterSig, "for Enter signal\n")
    sharpe <- ( mean(paperProfit) / sd(paperProfit) ) * sqrt(252)
    SharpeDF[rollingDaysNum, enterSigNum] <- sharpe
    start <- 0
    gLong = FALSE
    inTrade = FALSE
    paperProfit <- c()
    rollingDaysNum <- rollingDaysNum + 1
    if(rollingDaysNum == 382){rollingDaysNum = 1}
    
  }
  enterSigNum <- enterSigNum + 1
}

colnames(SharpeDF) <- c(seq(0.2,2.0, 0.05))
rownames(ShapeDF) <- c(20:400)

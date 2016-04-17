start <- 1
length <- 252
end <- start + length
signal <- 0.25
Esignal <- 0.10
portfolio <- c(10000)
position <- 0
inTrade <- FALSE
gLong <- FALSE
returns <- c(0)
# PArima <- function(DFtest){
# }
PRegression <- function(DFtest){
  print(length(DFtest$GoldClose))
  print(length(DFtest$SilvClose))
  print(length(DFtest$GSRatio))
  model <- lm(GSRatio ~ GoldClose + SilvClose, data = DFtest)
  predictedVal <- return(predict(model, btDF[end+1]))
}


for(i in btDF$IAUClose){
  predictedVal <- PRegression(btDF[start:end])
  if(inTrade == FALSE){
    if((predictedVal - btDF$GSRatio[end+1]) > signal*sd(btDF$GSRatio[start:end]) ){
      if(predictedVal > btDF$IAUClose[end+1]){ # SHORT GOLD LONG SILVER
        position <- portfolio[length(portfolio)]
        pGold <- btDF$IAUClose[end+1] # price of gold ETF when position was entered
        aGold <- floor(portfolio[length(portfolio)]/2)*pGold # amount of gold ETF shares bought 
        pSilv <- btDF$SLVClose[end+1] # price of silv ETF when position was entered
        aSilv <- floor(portfolio[length(portfolio)]/2)*pSilv # amount of silv ETF shares bought
        inTrade <- TRUE
        gLong <- FALSE
      }
      if(predictedVal < btDF$IAUClose[end+1]){ # LONG GOLD SHORT SILVER
        position <- portfolio[length(portfolio)]
        pGold <- btDF$IAUClose[end+1] # price of gold ETF when position was entered
        aGold <- floor(portfolio[length(portfolio)]/2)*pGold # amount of gold ETF shares bought 
        pSilv <- btDF$SLVClose[end+1] # price of silv ETF when position was entered
        aSilv <- floor(portfolio[length(portfolio)]/2)*pSilv # amount of silv ETF shares bought
        inTrade <- TRUE
        gLong <- TRUE
      }
    }
  }
  if(inTrade == TRUE){
    if((predictedVal - btDF$GSRatio[end+1]) < Esignal*sd(btDF$GSRatio[start:end]) ){ # Exit Signal from Trade
      inTrade <- FALSE
      if(gLong == TRUE){
        return <- (btDF$IAUCLose[end+1] - pGold)*aGold + (pSilv - btDF$SLVClose[end+1])*aSilv
        returns <- c(returns,return)
      }
      if(gLong == FALSE){
        return <- (pGold - btDF$IAUClose[end+1])*aGold + (btDF$SLVClose[end+1] - pSilv)*aSilv
        returns <- c(returns,return)
      }
      inTrade <- FALSE
    }
    # End of code to exit from trade
    
    #Code below is to calculate portfolio value
    if(gLong == TRUE & inTrade <- TRUE){
      return <- (btDF$IAUCLose[end+1] - pGold)*aGold + (pSilv - btDF$SLVClose[end+1])*aSilv
      portfolio <- c(portfolio, portfolio[length(portfolio)] + return)
    }
    if(gLong == FALSE & inTrade <- TRUE){
      return <- (pGold - btDF$IAUClose[end+1])*aGold + (btDF$SLVClose[end+1] - pSilv)*aSilv
      portfolio <- c(portfolio, portfolio[length(portfolio)] + return)
      
    }
  }
  
  start <- start + 1
  end <- start + length
  if(end == length(btDF[IAUClose])){break}
}
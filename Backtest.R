start <- 1
length <- 252
end <- start + length
signal <- 0.25
Esignal <- 0.10
portfolio <- c(10000)
position <- 0
inTrade <- FALSE
gLong <- FALSE
# PArima <- function(DFtest){
# }
PRegression <- function(DFtest){
  model <- lm(GSRatio ~ GoldClose + SilvClose, data = DFtest)
  predictedVal <- return(predict(model, btDF[end+1]))
}


for(i in btDF$IAUClose){
  predictedVal <- PRegression(btDF[start:end])
  if(inTrade == FALSE){
    if((predictedVal - btDF$IAUClose[end+1]) > signal*sd(btDF$GSRatio[start:end]) ){
      if(predictedVal > btDF$IAUClose[end+1]){
        position <- portfolio[length(portfolio)]
        position <- position + (portfolio[length(portfolio)]/2)*btDF$GoldClose[end+1] # Short Gold
        position <- position - portfolio[length(portfolio)]/2*btDF$SilvClose[end+1] # Long Silver
        portfolio <- c(portfolio,position)
        inTrade <- TRUE
        gLong <- FALSE
      }
      if(predictedVal < btDF$IAUClose[end+1]){
        position <- portfolio[length(portfolio)]
        position <- position - (portfolio[length(portfolio)]/2)*btDF$GoldClose[end+1] # Long Gold
        position <- position + portfolio[length(portfolio)]/2*btDF$SilvClose[end+1] # Short Silver
        portfolio <- c(portfolio,position)
        inTrade <- TRUE
        gLong <- TRUE
      }
    }
  }
  if(inTrade == TRUE){
    if(gLong == TRUE){
      position <- portfolio[length(portfolio)]
      position <- position + (portfolio[length(portfolio)]/2)*btDF$GoldClose[end+1] # Sell Gold
      position <- position - portfolio[length(portfolio)]/2*btDF$SilvClose[end+1] # Buy to cover Silver
      portfolio <- c(portfolio,position)
    }
    if(gLong == FALSE){
      position <- portfolio[length(portfolio)]
      position <- position - (portfolio[length(portfolio)]/2)*btDF$GoldClose[end+1] # Buy to Cover Gold
      position <- position + portfolio[length(portfolio)]/2*btDF$SilvClose[end+1] # Sell Silver
      portfolio <- c(portfolio,position)
    }
    if((predictedVal - btDF$IAUClose[end+1]) > Esignal*sd(btDF$GSRatio[start:end]) ){
      inTrade <- FALSE
    }
  }
  
  start <- start + 1
  end <- start + length
  if(end = length(btDF[IAUClose])){break}
}
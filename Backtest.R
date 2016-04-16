start <- 1
length <- 252
end <- start + length
signal <- 0.25
Esignal <- 0.10
portfolio <- c(10000)
inTrade <- FALSE

# PArima <- function(DFtest){
# }
PRegression <- function(DFtest){
  model <- lm(GSRatio ~ GoldClose + SilvClose, data = DFtest)
  predictedVal <- return(predict(model, btDF[end+1]))
}


for(i in btDF$IAUClose){
  predictedVal <- PRegression
  if(inTrade == FALSE){
    if((predictedVal - btDF$IAUClose[end+1]) > signal*sd(btDF$GSRatio[start:end]) ){
      if(predictedVal > btDF$IAUClose[end+1]){
        #Short Gold
        #Long Silver
        inTrade <- TRUE
      }
      if(predictedVal < btDF$IAUClose[end+1]){
        #Long Gold
        #Short Silver
        inTrade <- TRUE
      }
    }
  }
  if(inTrade == TRUE){
    if((predictedVal - btDF$IAUClose[end+1]) > Esignal*sd(btDF$GSRatio[start:end]) ){
      #EXIT TRADE
      inTrade <- FALSE
    }
  }
  start <- start + 1
  end <- start + length
}
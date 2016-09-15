DFArima <- xts(order.by = index(btDF))
Rpred <- c()
start <- 0
rolling_Days <- 20
end <- start + rolling_Days
dates <- c()

for( rolling_Days in 20:400){
  for(i in index(btDF)){
    start <- start + 1
    end <- start + rolling_Days
    if(end == length(index(btDF))){break}
    
    model <- arima(btDF$GSRatio[start:end], c(1,1,0))
    Rpred <- c(Rpred, as.numeric(predict(model, n.ahead = 1)$pred))
    dates <- c(dates, as.Date( index(btDF[end+1]) ) )
  }
  test <- xts(Rpred, order.by = as.Date(dates))
  colnames(test) <- c(toString(rolling_Days))
  DFArima <- merge.xts(DFArima, test, fill = NA)
  print(tail(DFArima))
  print(rolling_Days)
  start <- 0
  Rpred <- c()
  dates <- c()
}

colnames(DFArima) <- c(20:400)
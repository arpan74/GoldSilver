DFArima <- c()
start <- 0
rolling_Days <- 10
end <- start + rolling_Days

for(i in index(btDF)){
  start <- start + 1
  end <- start + rolling_Days
  if(start < rollDays){next}
  if(end == length(index(btDF))){break}
  
  model <- arima(btDF$GSRatio[start:end], c(1,1,0))
  Rpred <- c(Rpred,predict(model, n.ahead = 1)$pred)
  Rse <- c(Rse, predict(model, n.ahead = 1)$se)
  
  dates <- c(dates, i)
}
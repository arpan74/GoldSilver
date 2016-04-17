library(Quandl)
GoldClose <- Quandl("LBMA/GOLD", api_key="iU2zhPffw6b_yfXcvj6v") #Gold spot closing prices
GoldDates <- as.Date(as.character(GoldClose$Date),format = "%Y-%m-%d")
GoldClose <- xts(GoldClose$`USD (AM)`,GoldDates)
rm(GoldDates)

SilvClose <- Quandl("LBMA/SILVER", api_key="iU2zhPffw6b_yfXcvj6v") #Silver spot closing prices
SilvDates <- as.Date(as.character(SilvClose$Date),format = "%Y-%m-%d")
SilvClose <- xts(SilvClose$USD,SilvDates)
rm(SilvDates)

GSRatio <- GoldClose/SilvClose

PlatClose <- Quandl("LPPM/PLAT", api_key="iU2zhPffw6b_yfXcvj6v") #Platinum spot closing prices
PlatDates <- as.Date(as.character(PlatClose$Date),format = "%Y-%m-%d")
PlatClose <- xts(PlatClose$`USD PM`,PlatDates)
rm(PlatDates)

PallClose <- Quandl("LPPM/PALL", api_key="iU2zhPffw6b_yfXcvj6v") #Palladium Spot closing prices
PallDates <- as.Date(as.character(PallClose$Date),format = "%Y-%m-%d")
PallClose <- xts(PallClose$`USD PM`,PallDates)
rm(PallDates)


#Creates xts object with Commodity Prices
DFCom <- merge(GoldClose,SilvClose,PlatClose,PallClose,GSRatio)
colnames(DFCom) <- c("GoldClose","SilvClose","PlatClose","PallClose","GSRatio")
DFCom <- na.omit(DFCom)

#xts object with ETF prices -ishares ETFs - IAU and SLV
IAUClose <- Quandl("YAHOO/TSX_IGT_TO", api_key="iU2zhPffw6b_yfXcvj6v") #Gold ETF
IAUDates <- as.Date(as.character(IAUClose$Date),format = "%Y-%m-%d")
IAUClose <- xts(IAUClose$Close,IAUDates)
IAUClose["/2010-06-16"] <- (IAUClose["/2010-06-16"])/10 #ADJUSTING FOR SPLIT
rm(IAUDates)

SLVClose <- Quandl("GOOG/NYSE_SLV", api_key="iU2zhPffw6b_yfXcvj6v") #Silver ETF
SLVDates <- as.Date(as.character(SLVClose$Date),format = "%Y-%m-%d")
SLVClose <- xts(SLVClose$Close,SLVDates)
rm(SLVDates)

#Backtest Data Frame
btDF <- merge(IAUClose,SLVClose,GoldClose,SilvClose,PlatClose,GSRatio) #Backtest Data Frame
btDF <- na.omit(btDF)
colnames(btDF) <- c(IAUClose,SLVClose,GoldClose,SilvClose,PlatClose, GSRatio)

#Return Vectors
Gold1 <- as.numeric(GoldClose[1])
GoldReturns <- GoldClose/Gold1
rm(Gold1)

Pall1 <- as.numeric(PallClose[1])
PallReturns <- PallClose/Pall1
rm(Pall1)

Plat1 <- as.numeric(PlatClose[1])
PlatReturns <- PlatClose/Plat1
rm(Plat1)

Silv1 <- as.numeric(SilvClose[1])
SilvReturns <- SilvClose/Silv1
rm(Silv1)

DFReturns <- merge(GoldReturns,SilvReturns,PallReturns,PlatReturns)
DFReturns <- na.omit(DFReturns)

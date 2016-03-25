GoldClose <- Quandl("LBMA/GOLD", api_key="iU2zhPffw6b_yfXcvj6v") #Gold spot closing prices
GoldDates <- as.Date(as.character(GoldClose$Date),format = "%Y-%m-%d")
GoldClose <- xts(GoldClose$`USD (AM)`,GoldDates)
rm(GoldDates)

SilvClose <- Quandl("LBMA/SILVER", api_key="iU2zhPffw6b_yfXcvj6v") #Silver spot closing prices
SilvDates <- as.Date(as.character(SilvClose$Date),format = "%Y-%m-%d")
SilvClose <- xts(SilvClose$USD,SilvDates)
rm(SilvDates)

PlatClose <- Quandl("LPPM/PLAT", api_key="iU2zhPffw6b_yfXcvj6v") #Platinum spot closing prices
PlatDates <- as.Date(as.character(PlatClose$Date),format = "%Y-%m-%d")
PlatClose <- xts(PlatClose$`USD PM`,PlatDates)
rm(PlatDates)

PallClose <- Quandl("LPPM/PALL", api_key="iU2zhPffw6b_yfXcvj6v") #Palladium Spot closing prices
PallDates <- as.Date(as.character(PallClose$Date),format = "%Y-%m-%d")
PallClose <- xts(PallClose$`USD PM`,PallDates)
rm(PallDates)


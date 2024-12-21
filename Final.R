library(vrtest)
library(tseries)
library(quantmod) 
library(forecast)
library(ggplot2) 
library(ggfortify)
library(dplyr)
library(gridExtra)
library(rugarch)
library(FinTS) 
library(daltoolbox) 
library(harbinger) 

eth_data <- getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE, 
                       from = "2018-01-01", to = "2023-12-30")
eth_data <- eth_data |> janitor::clean_names()

eth_returns <- data.frame(eth_data) |> 
  mutate(Return = log(eth_usd_close / lag(eth_usd_close) ),Date = as.Date(index(eth_data))) |> 
  na.omit() 

res <-list()
spec1 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(1, 1)),distribution.model = "norm")

fit1 <- ugarchfit(spec = spec1, data = eth_returns["2018-01/2018-03"]$Return)
fit2 <- ugarchfit(spec = spec1, data = eth_returns["2018-01/2018-06"]$Return)
fit3 <- ugarchfit(spec = spec1, data = eth_returns["2018"]$Return)
fit4 <- ugarchfit(spec = spec1, data = eth_returns$Return)


mean(residuals(fit1)**2)
mean(residuals(fit2)**2)
mean(residuals(fit3)**2)
mean(residuals(fit4)**2)


spec2 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(1, 1)),distribution.model = "std")
eth_returns["2018-01"]$Return
fit5 <- ugarchfit(spec = spec2, data = eth_returns["2018-01/2018-03"]$Return)
fit6 <- ugarchfit(spec = spec2, data = eth_returns["2018-01/2018-06"]$Return)
fit7 <- ugarchfit(spec = spec2, data = eth_returns["2018"]$Return)
fit8 <- ugarchfit(spec = spec2, data = eth_returns$Return)


mean(residuals(fit5)**2)
mean(residuals(fit6)**2)
mean(residuals(fit7)**2)
mean(residuals(fit8)**2)


eth_data_test <- getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE, 
                       from = "2024-01-01", to = "2024-6-30")
eth_returns_test <- data.frame(eth_data) |> 
  mutate(Return = log(eth_usd_close / lag(eth_usd_close) ),Date = as.Date(index(eth_data))) |> 
  na.omit() 


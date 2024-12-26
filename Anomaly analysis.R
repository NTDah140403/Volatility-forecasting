
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
eth_returns <- as.xts(eth_returns)
eth_returns['2019']
plot(eth_returns['2019'])

ts(eth_returns["Return"])
spec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(1, 1)),distribution.model = "norm")


fit<- ugarchfit(spec = spec, data = eth_returns["Return"])
std_residuals <- residuals(fit,standardize =T)



hist(std_residuals,breaks = 100)
outliers <- which(abs(std_residuals) > 3)  


data <- eth_returns |> mutate(outlier = ifelse(index(eth_returns) %in% outliers, "Outlier", "Normal"))


ggplot(data, aes(x = Date, y = Return)) +
  geom_line(color = "blue") +  
  geom_point(data = subset(data, outlier == "Outlier"), 
             aes(x = Date, y = Return), color = "red", size = 3) + 
  labs(title = "Chuỗi thời gian với các điểm bất thường", 
       x = "Thời gian", y = "Giá trị") +
  theme_minimal()







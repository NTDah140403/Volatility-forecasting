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
library("moments") 
library("garchx") 

# EDA ---------------------------------------------------------------------
eth_data <- getSymbols("ETH-USD", src = "yahoo", auto.assign = FALSE, 
                       from = "2018-01-01", to = "2023-12-30")
eth_data <- eth_data |> janitor::clean_names()
eth_returns <- data.frame(eth_data) |> 
  mutate(Return = log(eth_usd_close / lag(eth_usd_close) ),
         Date = as.Date(index(eth_data)),
                        month = format(Date,"%m"),
         year =format(Date,"%Y")
         ) |>   na.omit() 
  
kurtosis(eth_returns$Return)
skewness(eth_returns$Return)


ggplot(data = eth_returns,aes(x = Date, y = Return))+geom_line()
ggplot(data = eth_returns,aes(x = Date, y = Return))+geom_line()+facet_wrap(~year,scales="free_x",  ncol=2)
ggplot(data = eth_returns,aes( x = Return))+geom_histogram(bins = 100) +facet_wrap(~year,  ncol=2)

res <-list()
spec1 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(1, 1)),distribution.model = "norm")





acf_values <- acf(eth_return$Return,plot = F)
acf_df <- acf_df <- data.frame(lag = acf_values$lag, acf = acf_values$acf)

acfplot <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = c(-1.96/sqrt(length(eth_return$Return)), 1.96/sqrt(length(eth_return$Return))), 
             linetype = "dashed", color = "red") +
  labs(title = "Autocorrelation Function (ACF)", x = "Lag", y = "ACF") +
  theme_minimal()

pacf_values <- pacf(eth_return$Return**2,plot = F)
pacf_df <- pacf_df <- data.frame(lag = pacf_values$lag, acf = pacf_values$acf)

pacfplot <- ggplot(pacf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = c(-1.96/sqrt(length(eth_return$Return)), 1.96/sqrt(length(eth_return$Return))), 
             linetype = "dashed", color = "red") +
  labs(title = "Autocorrelation Function (PACF)", x = "Lag", y = "ACF") +
  theme_minimal()

grid.arrange( acfplot,pacfplot,ncol = 1)






# distribution effect -----------------------------------------------------


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


# var option price --------------------------------------------------------
spec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)), 
                    distribution.model = "norm",
                   mean.model = list(armaOrder = c(0, 0),include.mean = F))

fit <- ugarchfit(spec = spec, data = eth_returns$Return)
fit
T <- 26         
sig<-  sigma(ugarchforecast(fit, n.ahead = T))
K <- 2400
r <- uncmean(fit)    
N <- 1000  
confidence_level <- 0.95  
call <- rep(0,N)
put <- rep(0,N)
ST_list <- rep(0,N)
hist(ST_list)
ls <- c()
Z <- rnorm( N,0,1 ) 
ST_list <- S0 * exp(r+ - 0.5 * sig[1]**2  + ig [1] * Z)
S0
ST_list

call_list_old <- exp(-r * 1) * pmax(ST_list - K, 0) 
for(j in 2:T){
  Z <- rnorm( N,0,1 ) 
  ST_list <- ST_list * exp(r+ - 0.5 * sig[j]**2  + sig [j] * Z)
  call_list <- exp(-r * j) * pmax(ST_list - K, 0) 
  ls <- rbind(ls,call_list - call_list_old)
  call_list_old <-call_list
}
hist(ls, breaks = 100)
quantile(ls,.05)

library(vrtest)

library(tseries)
library(quantmod)   # Thư viện để tải dữ liệu tài chính
library(forecast)    # Thư viện cho ARIMA
library(ggplot2)     # Thư viện cho vẽ biểu đồ
library(ggfortify)   # Thư viện cho vẽ biểu đồ với dữ liệu thời gian
library(dplyr)
library(gridExtra)
library(rugarch)

# Tải dữ liệu giá Bitcoin từ Yahoo Finance
chartSeries(btc_data)
barChart(btc_data)

?quantmod

btc_data <- getSymbols("BTC-USD", src = "yahoo", auto.assign = FALSE, 
                       from = "2018-01-01", to = "2023-01-01")
btc_data <- btc_data |> janitor::clean_names()

# Tính toán lợi suất logarit
btc_returns <- data.frame(btc_data) |> 
  mutate(Return = (btc_usd_close / lag(btc_usd_close) - 1)) |> 
  na.omit() 
btc_returns <- btc_returns |> mutate(Date = as.Date(rownames(btc_returns)))

head(btc_returns)
# Visualization -----------------------------------------------------------

waveplot1 <- ggplot(data = btc_returns, aes(x = Date, y = btc_usd_close )) +
  geom_line() +
  labs(title = 'Wave plot', x = 'Time', y = 'Close price') +
  theme_minimal()
waveplot1
waveplot2 <- ggplot(data = btc_returns, aes(x =Date , y = Return)) +
  geom_line() +
  labs( x = 'Time', y = 'Log return') +
  theme_minimal()
waveplot2
histogram_close <- ggplot(data = btc_data, aes(x = btc_usd_close)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of BTC Closing Prices", x = "BTC/USD Closing Price", y = "Frequency") +
  theme_minimal()
histogram_close

histogram_return <- ggplot(data = btc_returns, aes(x = Return)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of BTC Return", x = "Return", y = "Frequency") +
  theme_minimal()
histogram_return

rownames(btc_returns)
volatilityRoll <- runSD(btc_returns$Return, n = 30) 
mean <- runMean(btc_returns$Return, n = 30)  


volatilityEst <- data.frame(
  Date = as.Date(rownames(btc_returns)),
  Volatility = volatility
)
MeanEst <- data.frame(
  Date = as.Date(rownames(btc_returns)),
  Mean = mean
)

# Create the ggplot
volatilityPlotEst <- ggplot(volatilityEst, aes(x = Date, y = Volatility)) +
  geom_line() +
  labs(
    title = "30-Day Rolling Volatility",
    y = "Volatility",
    x = "Date"
  ) +
  theme_minimal()
volatilityPlotEst
meanPlotEst <- ggplot(MeanEst, aes(x = Date, y = Mean)) +
  geom_line() +
  labs(
    title = "30-Day Rolling Volatility",
    y = "Mean",
    x = "Date"
  ) +
  theme_minimal()
meanPlotEst


acf_values <- acf(btc_returns$Return,plot = F)
acf_df <- acf_df <- data.frame(lag = acf_values$lag, acf = acf_values$acf)

acfplot <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = c(-1.96/sqrt(length(btc_returns$Return)), 1.96/sqrt(length(btc_returns$Return))), 
             linetype = "dashed", color = "red") +
  labs(title = "Autocorrelation Function (ACF)", x = "Lag", y = "ACF") +
  theme_minimal()
acfplot


pacf_values <- pacf(btc_returns$Return**2,plot = F)
pacf_df <- pacf_df <- data.frame(lag = pacf_values$lag, acf = pacf_values$acf)

pacfplot <- ggplot(pacf_df, aes(x = lag, y = acf)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = c(-1.96/sqrt(length(btc_returns$Return)), 1.96/sqrt(length(btc_returns$Return))), 
             linetype = "dashed", color = "red") +
  labs(title = "Autocorrelation Function (PACF)", x = "Lag", y = "ACF") +
  theme_minimal()

pacfplot
grid.arrange(waveplot1, acfplot,waveplot2,pacfplot,ncol = 2)





# Testing -----------------------------------------------------------------

adf_test <- adf.test(btc_returns$Return )
adf_test
# Thực hiện Variance Ratio Test
vr_test <- Auto.VR()
Lo.Mac(btc_returns$Return,c(2,3,5,10))
print(vr_test)


# Model -------------------------------------------------------------------
returnlist <-  btc_returns$Return
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)),
                         distribution.model = "std"	,
                         mean.model = list(armaOrder = c(1,1)))
?ugarchspec
# Fit the GARCH model
garch_fit <- ugarchfit(spec = garch_spec, data = btc_returns["Return"])

start_time <- system.time({
  # garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           # mean.model = list(armaOrder = c(1,1)),start.pars = as.list(coef(garch_fit)))
  garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(0,1)))
  garch_fit1 <- ugarchfit(spec = garch_spec, data = btc_returns["Return"][c(1:800),])
  
})
start_time
start_time2 <- system.time({
  # garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           # mean.model = list(armaOrder = c(1,1)),start.pars = as.list(coef(garch_fit)))
  garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(0,1)))
  garch_fit2 <- ugarchfit(spec = garch_spec, data = btc_returns["Return"][c(1:1200),],start.pars= as.list(coef(garch_fit1)))
  
})
start_time2

# Extract and plot the conditional volatility (sigma)
garch_volatility <- sigma(garch_fit)

print(garch_fit)

Aspec <-  ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), fixed.pars = as.list(coef(garch_fit)))
?ugarchspec
filter_result <- ugarchfilter(spec =spec,data =  btc_returns["Return"])
c(uncmean(garch_fit), uncmean(filter_result))
# Truy cập các giá trị đã lọc
conditional_volatility <- filter_result  # Độ biến động có điều kiện
standardized_residuals <- filter_result@residuals$stdresid 
plot(garch_volatility, main = "GARCH(1,1) Conditional Volatility", ylab = "Volatility")


garch_fit
summary(garch_fit)




# Dự đoán cho 10 bước tiếp theo
forecast <- ugarchforecast(garch_fit, n.ahead = 100)


?ugarchforecast
# Kết quả dự đoán
forecast_values <- forecast@forecast$seriesFor
forecast_variance <- forecast@forecast$sigmaFor
forecast_variance
BIC
A
garch_fit
index(predict_volatility)
predict_volatility <- data.frame("date" = index(predict_volatility), 'value' = sigma(garch_fit))

# Vẽ phương sai có điều kiện
volatilityPlot1 <- ggplot(predict_volatility, aes(x = date, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Conditional Variance from GARCH Model (Actual and Predicted)",
       x = "Date",
       y = "Conditional Variance") +
  theme_minimal()
volatilityPlot1

grid.arrange(waveplot1, volatilityPlot1,ncol = 1)
grid.arrange(volatilityPlotEst, volatilityPlot1,ncol = 1)



# evaluation ---------------------------------------------------------------


stdret <- data.frame( 'value' = residuals(garch_fit, standardize = F))
uncvariance(garch_fit)
histResidual1 <- ggplot(stdret, aes(x  = value)) +
  geom_density( fill = "blue", color = "black", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Values",
       x = "Values",
       y = "Frequency") +
  theme_minimal()
histResidual1

sumRes <- pnorm(stdret$value, mean(stdret$value),sd(stdret$value))
hist(sumRes)



# decomponent -------------------------------------------------------------
ts(btc_returns["Return"])
decomposed_ts <- decompose(ts(btc_returns["Return"]), type = "additive")  # hoặc type = "multiplicative" cho mô hình nhân
?decompose
# Vẽ các thành phần

plot(decomposed_ts)
btc_returns["Return"]
ts(btc_returns$Return,start = rownames(btc_returns)[1])
?ts
x <- c(-50, 175, 149, 214, 247, 237, 225, 329, 729, 809,
       530, 489, 540, 457, 195, 176, 337, 239, 128, 102, 232, 429, 3,
       98, 43, -141, -77, -13, 125, 361, -45, 184)
x <- ts(x, start = c(1951, 1), end = c(1958, 4), frequency = 4)
m <- decompose(x)
x
## seasonal figure: 6.25, 8.62, -8.84, -6.03
round(decompose(x)$figure / 10, 2)

plot(x)
plot(m)







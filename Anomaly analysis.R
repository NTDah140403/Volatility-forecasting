
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












library(wavelets)

# wavelets-base -----------------------------------------------------------


# Step 1: Apply Discrete Wavelet Transform (DWT)
apply_dwt <- function(residuals) {
  dwt_result <- dwt(residuals, filter = "haar", boundary = "periodic")
  A1 <- dwt_result@V[[1]]  # Approximation coefficients
  D1 <- dwt_result@W[[1]]  # Detail coefficients
  return(list(A1 = A1, D1 = D1, dwt_result = dwt_result))
}

# Step 2: Set Threshold
set_threshold <- function(D1, distribution = "normal") {
  n <- length(D1)
  if (distribution == "normal") {
    threshold <- qnorm(0.975) * sqrt(log(n))  # 95th percentile for normal distribution
  } else if (distribution == "t") {
    df <- 5  # Degrees of freedom for t-distribution
    threshold <- qt(0.975, df = df) * sqrt(log(n))
  } else {
    stop("Unsupported distribution")
  }
  return(threshold)
}
?dwt
# Step 3: Identify Largest Coefficient
find_largest_coefficient <- function(D1, threshold) {
  abs_D1 <- abs(D1)
  max_index <- which.max(abs_D1)
  if (abs_D1[max_index] > threshold) {
    return(max_index)
  } else {
    return(NULL)
  }
}

# Step 4: Nullify Outlier Influence
nullify_outlier <- function(D1, index) {
  D1[index] <- 0
  return(D1)
}

# Step 5: Recompose Series with Inverse DWT
recompose_series <- function(A1, D1, dwt_result) {
  dwt_result@V[[1]] <- A1
  dwt_result@W[[1]] <- D1
  reconstructed <- idwt(dwt_result)
  return(reconstructed)
}




# Full Procedure for Detecting ALOs
wavelet_outlier_detection <- function(residuals, distribution = "normal") {
  dwt_step <- apply_dwt(residuals)
  A1 <- dwt_step$A1
  D1 <- dwt_step$D1
  dwt_result <- dwt_step$dwt_result
  
  threshold <- set_threshold(D1, distribution)
  outlier_indices <- list()
  
  repeat {
    index <- find_largest_coefficient(D1, threshold)
    if (is.null(index)) {
      break
    }
    outlier_indices <- c(outlier_indices, index)
    D1 <- nullify_outlier(D1, index)
  }
  
  cleaned_residuals <- recompose_series(A1, D1, dwt_result)
  return(list(outlier_indices = outlier_indices, cleaned_residuals = cleaned_residuals))
}


res <-
library(wavelets)

# Step 1: Apply Discrete Wavelet Transform (DWT)
apply_dwt <- function(residuals) {
  dwt_result <- dwt(residuals, filter = "haar", boundary = "periodic")
  A1 <- dwt_result@V[[1]]  # Approximation coefficients
  D1 <- dwt_result@W[[1]]  # Detail coefficients
  return(list(A1 = A1, D1 = D1, dwt_result = dwt_result))
}

# Step 2: Set Threshold
set_threshold <- function(D1, distribution = "normal") {
  n <- length(D1)
  if (distribution == "normal") {
    threshold <- qnorm(0.975) * sqrt(log(n))  # 95th percentile for normal distribution
  } else if (distribution == "t") {
    df <- 5  # Degrees of freedom for t-distribution
    threshold <- qt(0.975, df = df) * sqrt(log(n))
  } else {
    stop("Unsupported distribution")
  }
  return(threshold)
}
set_threshold(D1)
# Step 3: Identify Largest Coefficient
find_largest_coefficient <- function(D1, threshold) {
  abs_D1 <- abs(D1)
  max_index <- which.max(abs_D1)
  if (abs_D1[max_index] > threshold) {
    return(max_index)
  } else {
    return(NULL)
  }
}

# Step 4: Nullify Outlier Influence
nullify_outlier <- function(D1, index) {
  D1[index] <- 0
  return(D1)
}

# Step 5: Recompose Series with Inverse DWT
recompose_series <- function(A1, D1, dwt_result) {
  dwt_result@V[[1]] <- A1
  dwt_result@W[[1]] <- D1
  reconstructed <- idwt(dwt_result)
  return(reconstructed)
}




eth_returns$Return
# Full Procedure for Detecting ALOs
wavelet_outlier_detection <- function(residuals, distribution = "normal") {
  dwt_step <- apply_dwt(residuals)
  A1 <- dwt_step$A1
  D1 <- dwt_step$D1
  dwt_result <- dwt_step$dwt_result
  
  threshold <- set_threshold(D1, distribution)
  outlier_indices <- list()
  
  repeat {
    index <- find_largest_coefficient(D1, threshold)
    if (is.null(index)) {
      break
    }
    outlier_indices <- c(outlier_indices, index)
    D1 <- nullify_outlier(D1, index)
  }
  
  cleaned_residuals <- recompose_series(A1, D1, dwt_result)
  return(list(outlier_indices = outlier_indices, cleaned_residuals = cleaned_residuals))
}

res <- residuals(fit)
res



# Run Procedure
result <- wavelet_outlier_detection(ts(res), distribution = "normal")
print("Outlier Indices:")
print(result$outlier_indices)
plot(res, type = "l", main = "Residuals with Detected Outliers")
plot(result$cleaned_residuals, type = "l", main = "Residuals with Detected Outliers")
points(as.numeric(result$outlier_indices), residuals[as.numeric(result$outlier_indices)], col = "red", pch = 19)

























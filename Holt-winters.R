rm(list=ls( all = TRUE )) # clears the entire R environment
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# HOLTWINTERS (TRIPLE EXPONENTIAL) - HoltWinters(data)
#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# HOLTWINTERS (TRIPLE EXPONENTIAL)
# STEP 1 - Import and visualize the time series data
# STEP 2 - Data Splitting
# STEP 3 - Data manipulation for Triple Exponential Smoothing
# STEP 4 - In-Sample Fitting of Triple Exponential Smoothing Model vs. Actual Training Data
# STEP 5 - In-Sample Fitting Error Evaluation and Residual Diagnostics of TES Model
# STEP 6 – In-sample fitting of the TES model over a 3-year period
# STEP 7 – In-sample fitting of the TES model using a rolling 3-year window
#                         FORECAST
# STEP 8 – Out-of-Sample Evaluation: TES Model Forecast vs. Actual Test Data
# STEP 9 – Out-of-Sample Evaluation: Rolling Window TES Model (100-Point Window) vs. Actual Test Data

#----------------------------------------------------------------------
# STEP 1 - Import and visualize the time series data
#----------------------------------------------------------------------
data <- read.csv("C:/Users/Melectronics/OneDrive/Documenti/Master Finance/Thesis Idea/Codes/2_Multi Regression/AAPL_prices.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
data$Date <- as.Date(data$Date, format="%d.%m.%Y")
SP <- data[, -ncol(data)] # removing the vol colum
SP <- SP[nrow(SP):1, ] # invert the series. Starting from 2010
SP.ts = ts(SP[,2], start = c(2010, 1), frequency =252) # Creation of the time series
plot(SP.ts, main = "S&P Time Series", ylab = "S&P Index", xlab = "Time")

length(SP.ts) # length of dataset

anyNA(SP.ts) # check if there is missing value. 
#result: FALSE: no missing value

# Decompose the series in trend, seasonal and the irregular part
decomp <- decompose(SP.ts) # Check if there is seasonality. decompose() -> additive model for default
decomp$seasonal
plot(decomp)
decomp2 <- stl(SP.ts, s.window = "periodic") # more modern method
plot(decomp2)

#----------------------------------------------------------------------
# STEP 2 - Data Splitting
#----------------------------------------------------------------------
n <- length(SP.ts)                 # Total number of observations
split_index <- floor(0.80 * n)      # 80% index
train.ts <- window(SP.ts, end = time(SP.ts)[split_index])
test.ts <- window(SP.ts, start = time(SP.ts)[split_index + 1])

# Step 2: Plot full data as background
plot(SP.ts, type = "l", col = "gray", lwd = 1.5,
     main = "Train/Test Split on Same Timeline", xlab = "Time", ylab = "Value")
# Step 3: Overlay training data
lines(train.ts, col = "blue", lwd = 2)
# Step 4: Overlay test data
lines(test.ts, col = "red", lwd = 2)
# Step 5: Add legend
legend("topleft", legend = c("Full Series", "Training Set", "Test Set"),
       col = c("gray", "blue", "red"), lwd = 2, lty = 1)

plot(decompose(train.ts))

#----------------------------------------------------------------------
# STEP 3 - Data manipulation for Triple Exponential Smoothing
#----------------------------------------------------------------------
train.ts_log <- log(train.ts)
test.ts_log <- log(test.ts)

# Check if the series is additive or multiplicative
tec_add_train <- decompose(train.ts_log, type = "additive")
tec_mul_train <- decompose(train.ts_log, type = "multiplicative")

var(tec_add_train$random, na.rm = TRUE) # Additive
# results: 0.005748676
var(tec_mul_train$random, na.rm = TRUE) # Multiplicative
# results: 0.0004927514

# additive result > multiplicative result -> the series is additive.

# Although the decomposition is additive, the increasing variance in the observed and residual components suggests a multiplicative structure.

#----------------------------------------------------------------------
# STEP 4 - In-Sample Fitting of Triple Exponential Smoothing Model vs. Actual Training Data
#----------------------------------------------------------------------
# MODEL FIT
train.ts.forecasts_TES <- HoltWinters(train.ts_log)
# train.ts.forecasts_TES <- HoltWinters(train.ts_log, beta = 0.1)  # you can specify a manual beta in case you are not trust at the automatic setting.

train.ts.forecasts_TES
# results:
#           Smoothing parameters:
#             alpha: 0.8892548
#             beta : 0
#             gamma: 1
#           Coefficients:
#                     [,1]
#             a     5.1427731305
#             b     0.0013476927
#             s1    0.0480457787
#             s2    0.0463688493
#             s3    0.0334023232
#             ..       ....
#             s252  0.0467333302

# If you want to start from another value and not from the first one you can use the code below:
# level[1] <- train.ts_log[1]
# trend[1] <- train.ts_log[2] - train.ts_log[1]
# HoltWinters(train.ts_log, l.start=level[1], b.start=trend[1]) # b is for the slope, while l is for the level

# plot decomposition of fitted value
plot(train.ts.forecasts_TES$fitted, main = "In-Sample Fitted Values from TES Model - Log-Transformed")

# xhat is the model’s reconstruction of the observed time series, using the estimated components:
#       xhat_t = Level_t + Trend_t + Seasonality_t      (additive model)
#                         or
#       xhat_t = Level_t * Trend_t * Seasonality_t      (multiplicative model)


# Triple Exponential Smoothing vs Train Data IN SAMPLE METHOD
plot(train.ts_log, type = "l", main = "Actual Data vs Fitted (TES) - Log-Transformed", ylab = "Values", xlab = "Time")
lines(fitted(train.ts.forecasts_TES)[,"xhat"], col = "red")

#----------------------------------------------------------------------
# STEP 5 - In-Sample Fitting Error Evaluation and Residual Diagnostics of TES Model
#----------------------------------------------------------------------
# Sum of Squared Errors (SSE)
cat("SSE (Sum of Squared Errors):", train.ts.forecasts_TES$SSE, "\n")
# results:

# Calculate in-sample forecast errors
fitted_values <- fitted(train.ts.forecasts_TES)[, "xhat"]
errors_TES <- train.ts_log - fitted_values

MSE <- mean(errors_TES^2)     # Mean Squared Error
RMSE <- sqrt(MSE)         # Root Mean Squared Error
MAE <- mean(abs(errors_TES))  # Mean Absolute Error
MAPE <- mean(abs(errors_TES) / train.ts_log) * 100  # Mean Absolute Percentage Error
MAD <- median(abs(errors_TES))  # Median Absolute Deviation

# R-squared
SSE <- sum(errors_TES^2)
SST <- sum((train.ts_log - mean(train.ts_log))^2)
R2  <- 1 - (SSE / SST)

# Shifted actual and predicted series
actual <- train.ts_log
predicted <- fitted_values
actual_diff <- diff(actual)
predicted_diff <- diff(fitted_values)
DA <- mean(sign(actual_diff) == sign(predicted_diff)) # Directional accuracy

cat("MAE:", MAE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAPE:", MAPE, "%",
    "| MAD:", MAD,
    "| DA:", round(DA * 100, 2), "%",
    "| R²:", R2, "\n")
# results: MAE: 0.01493432 | MSE: 0.0004355292 | RMSE: 0.02086934 | MAPE: 0.4286801 % | MAD: 0.0110764 | DA: 52.6 % | R²: 0.9993197 

# Compute residuals
residuals_TES <- errors_TES  # Same as train.ts_log - fitted_values

# Plot residuals over time
plot(residuals_TES,
     main = "Residuals of TES Model (In-Sample)",
     ylab = "Residuals",
     xlab = "Time",
     type = "h")

# Plot ACF of residuals
acf(residuals_TES, lag.max = 20, main = "ACF of In-Sample Residuals (TES Model)")

# Ljung-Box test for autocorrelation
lb_test <- Box.test(residuals_TES, lag = 20, type = "Ljung-Box")
print(lb_test)
# results: X-squared = 105.48, df = 20, p-value = 1.3e-13
# Interpretation: if p-value > 0.05, residuals are likely white noise
# The residuals are not white noise, meaning the TES model did not capture all the patterns in the data.

# Plot Forecast Error Distribution vs Normal
plotForecastErrors <- function(forecasterrors) {
  forecasterrors <- na.omit(forecasterrors)
  mybinsize <- IQR(forecasterrors) / 4
  if (mybinsize == 0) {  # handle flat error case
    mybinsize <- sd(forecasterrors) / 4
  }
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd * 5
  mymax <- max(forecasterrors) + mysd * 3
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins,
       main = "In-Sample Forecast Errors vs Normal Distribution (DES Model)", xlab = "Error")
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
  legend("topright", legend = c("Forecast Errors", "Normal Distribution"),
         col = c("red", "blue"), lty = c(1, 1), lwd = c(2, 2))
}

# Plot the error distribution
plotForecastErrors(residuals_TES)

#----------------------------------------------------------------------
# STEP 6 – In-sample fitting of the TES model over a 3-year period
#----------------------------------------------------------------------
# take the last 3 years
SP_3y <- ts(tail(SP.ts, 756), 
            start=c(2022, 1), 
            frequency=252)
# result: ...... - span 12 months
plot(SP_3y, type='l', 
     main='S&P from 01.2022 to 12.2024', 
     ylab='S&P Index', 
     xlab='Time')

n <- length(SP_3y)                 # Total number of observations
split_index <- floor(0.80 * n)      # 80% index
train.ts <- window(SP_3y, end = time(SP_3y)[split_index])
length(train.ts)
test.ts <- window(SP_3y, start = time(SP_3y)[split_index + 1])
length(test.ts)

train.ts_log <- log(train.ts)

# MODEL FIT
train.ts.fit_TES <- HoltWinters(train.ts_log)
train.ts.fit_TES

# Sum of Squared Errors (SSE)
cat("SSE (Sum of Squared Errors):", train.ts.fit_TES$SSE, "\n")
# results: SSE (Sum of Squared Errors): 0.2048843 

# Calculate in-sample forecast errors
fitted_values <- fitted(train.ts.fit_TES)[, "xhat"]
errors_TES <- train.ts_log - fitted_values

MSE <- mean(errors_TES^2)     # Mean Squared Error
RMSE <- sqrt(MSE)         # Root Mean Squared Error
MAE <- mean(abs(errors_TES))  # Mean Absolute Error
MAPE <- mean(abs(errors_TES) / train.ts_log) * 100  # Mean Absolute Percentage Error
MAD <- median(abs(errors_TES))  # Median Absolute Deviation

# R-squared
SSE <- sum(errors_TES^2)
SST <- sum((train.ts_log - mean(train.ts_log))^2)
R2  <- 1 - (SSE / SST)

# Shifted actual and predicted series
actual <- train.ts_log
predicted <- fitted_values
actual_diff <- diff(actual)
predicted_diff <- diff(fitted_values)
DA <- mean(sign(actual_diff) == sign(predicted_diff)) # Directional accuracy

cat("MAE:", MAE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAPE:", MAPE, "%",
    "| MAD:", MAD,
    "| DA:", round(DA * 100, 2), "%",
    "| R²:", R2, "\n")

# MAE: 0.01343426 | MSE: 0.0005820577 | RMSE: 0.02412587 | MAPE: 0.2585021 % | MAD: 0.006808302 | DA: 65.81 % | R²: 0.9707924 

# Compute residuals
residuals_TES <- errors_TES  # Same as train.ts_log - fitted_values

# Ljung-Box test for autocorrelation
lb_test <- Box.test(residuals_TES, lag = 20, type = "Ljung-Box")
print(lb_test)
# results: X-squared = 13.988, df = 20, p-value = 0.8311
# Interpretation: if p-value > 0.05, residuals are likely white noise

#----------------------------------------------------------------------
# STEP 7 – In-sample fitting of the TES model using a rolling 3-year window
#----------------------------------------------------------------------
# Parameters
train_size <- 504  # 2 years
total_obs <- length(SP.ts)

# Store all fitted values
all_fits <- ts(rep(NA, total_obs), frequency = 252, start = start(SP.ts))

# Rolling fit loop
for (start_idx in seq(1, total_obs - train_size + 1, by = 252)) {
  
  # Define training window (2 years)
  train_window <- window(SP.ts, 
                         start = time(SP.ts)[start_idx], 
                         end   = time(SP.ts)[start_idx + train_size - 1])
  
  # Log-transform
  train_log <- log(train_window)
  
  # Fit model with error handling
  model_hw <- tryCatch({
    HoltWinters(train_log)
  }, error = function(e) {
    message(sprintf("⚠️ Fitting failed at index %d: %s", start_idx, e$message))
    return(NULL)
  })
  
  if (!is.null(model_hw)) {
    # Back-transform fitted values
    fit_log <- model_hw$fitted[,1]  # 1st column = fitted values (on log scale)
    fit_original <- exp(fit_log)    # back-transform
    
    # Assign fitted values into the right location in full ts
    fit_index_start <- start_idx + 252  # fitted values start after 2nd obs
    fit_index_end <- fit_index_start + length(fit_original) - 1
    all_fits[fit_index_start:fit_index_end] <- fit_original
  }
}

# Plot the actual series and fitted values
plot(SP.ts, main = "Rolling In-Sample Fit (Holt-Winters)", ylab = "Price", col = "black")
lines(all_fits, col = "blue", lwd = 2)
legend("topleft", legend = c("Actual", "Fitted"), col = c("black", "blue"), lty = 1)

# Filter out NA values (fitted values don't cover the full range)
valid_idx <- which(!is.na(all_fits))
actual <- SP.ts[valid_idx]
predicted <- all_fits[valid_idx]

# Errors
errors <- log(actual) - log(predicted)  # since model was fit on log-scale

# SSE
SSE <- sum(errors^2)

# Error metrics
MSE <- mean(errors^2)
RMSE <- sqrt(MSE)
MAE <- mean(abs(errors))
MAD <- median(abs(errors))
MAPE <- mean(abs(errors) / abs(log(actual))) * 100  # Log-scale MAPE
SST <- sum((log(actual) - mean(log(actual)))^2)
R2  <- 1 - (SSE / SST)

# Directional Accuracy
actual_diff <- diff(log(actual))
predicted_diff <- diff(log(predicted))
DA <- mean(sign(actual_diff) == sign(predicted_diff)) * 100  # %

# Print results
cat("SSE:", SSE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAE:", MAE,
    "| MAD:", MAD,
    "| MAPE:", MAPE, "%",
    "| R²:", R2,
    "| DA:", round(DA, 2), "%\n")
# results: SSE: 1.503417 | MSE: 0.0004589184 | RMSE: 0.02142238 | MAE: 0.01044943 | MAD: 0.001363332 | MAPE: 0.2812857 % | R²: 0.9993691 | DA: 73.44 %

# Ljung-Box test for autocorrelation in residuals
lb_test <- Box.test(errors, lag = 20, type = "Ljung-Box")
print(lb_test)
# results: X-squared = 25.915, df = 20, p-value = 0.1686
# Interpretation: if p-value > 0.05, residuals are likely white noise

#----------------------------------------------------------------------
#                             FORECAST
#----------------------------------------------------------------------
# STEP 8 – Out-of-Sample Evaluation: TES Model Forecast vs. Actual Test Data
#----------------------------------------------------------------------
# take the last 3 years
SP_3y <- ts(tail(SP.ts, 756), 
            start=c(2022, 1), 
            frequency=252)

plot(SP_3y, type='l', 
     main='S&P from 01.2022 to 12.2024', 
     ylab='S&P Index', 
     xlab='Time')

# Split data in train and test
n <- length(SP_3y)                 # Total number of observations
split_index <- floor(0.80 * n) # 80% index
train.ts <- window(SP_3y, end = time(SP_3y)[split_index])
length(train.ts)

# length of train
length(train.ts)

test.ts <- window(SP_3y, start = time(SP_3y)[split_index + 1])
length(test.ts)
# length of test check: 152
length(SP_3y)-length(train.ts)

train.ts_log <- log(train.ts)
test.ts_log <- log(test.ts)

# forecast
library(forecast)

# prediction length
n <- length(test.ts_log)

# MODEL FIT
train.ts_log.forecasts_TES <- HoltWinters(train.ts_log)

# MODEL FORECAST
future_forecast_TES <- forecast(train.ts_log.forecasts_TES, h = n)
print(future_forecast_TES)

# Plot the forecast
plot(future_forecast_TES, main = "Out-of-Sample Forecast vs Actual Test Data (DES Model)",
     ylab = "Values", xlab = "Time")
forecast_time <- time(future_forecast_TES$mean)
aligned_time <- forecast_time[1:length(test.ts)]
lines(aligned_time, log(test.ts), col = "red", lwd = 1)
legend("topleft", legend = c("Forecast (DES)", "Actual Test Data"),
       col = c("red", "black"), lty = c(1, 1), lwd = c(2, 2))


# Sum of Squared Errors (SSE)
cat("In-sample SSE (from training):", train.ts_log.forecasts_TES$SSE, "\n")

# Calculate out-sample forecast errors
predicted_values <- future_forecast_TES$mean
# Calculate out-of-sample forecast errors
errors_TES_forecast <- test.ts_log - predicted_values

MSE <- mean(errors_TES_forecast^2)     # Mean Squared Error
RMSE <- sqrt(MSE)         # Root Mean Squared Error
MAE <- mean(abs(errors_TES_forecast))  # Mean Absolute Error
MAPE <- mean(abs(errors_TES_forecast) / abs(test.ts_log)) * 100  # Mean Absolute Percentage Error
MAD <- median(abs(errors_TES_forecast))  # Median Absolute Deviation

# R-squared
SSE <- sum(errors_TES_forecast^2)
SST <- sum((train.ts_log - mean(train.ts_log))^2)
R2  <- 1 - (SSE / SST)

# Shifted actual and predicted series
actual_forecast <- test.ts_log
predicted_forecast <- future_forecast_TES$mean # as before
# Compute first differences (i.e., daily/periodic changes)
actual_diff_forecast <- diff(actual_forecast)
predicted_diff_forecast <- diff(predicted_forecast)

# Directional Accuracy (percentage of times the model predicted the correct direction)
DA_forecast <- mean(sign(actual_diff_forecast) == sign(predicted_diff_forecast), na.rm = TRUE)

cat("MAE:", MAE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAPE:", MAPE, "%",
    "| MAD:", MAD,
    "| DA:", round(DA_forecast  * 100, 2), "%",
    "| R²:", R2, "\n")

# Results: MAE: 0.1749691 | MSE: 0.04233185 | RMSE: 0.2057471 | MAPE: 3.211121 % | MAD: 0.1738921 | DA: 51.66 % | R²: 0.08272728 


# Compute residuals
residuals_TES <- errors_TES_forecast  # Same as train.ts_log - fitted_values

# Ljung-Box test for autocorrelation
lb_test <- Box.test(residuals_TES, lag = 20, type = "Ljung-Box")
print(lb_test)
# results: X-squared = 778.6, df = 20, p-value < 2.2e-16
# Interpretation: if p-value > 0.05, residuals are likely white noise

#----------------------------------------------------------------------
# STEP 9 – Out-of-Sample Evaluation: Rolling Window TES Model (100-Point Window) vs. Actual Test Data
#----------------------------------------------------------------------
# take the last 3 years
SP_3y <- ts(tail(SP.ts, 756), 
            start=c(2022, 1), 
            frequency=252)

plot(SP_3y, type='l', 
     main='S&P from 01.2022 to 12.2024', 
     ylab='S&P Index', 
     xlab='Time')

# Split data in train and test
n <- length(SP_3y)            # Total number of observations
split_index <- floor(0.80 * n) # 80% index
train.ts <- window(SP_3y, end = time(SP_3y)[split_index])
length(train.ts)
test.ts <- window(SP_3y, start = time(SP_3y)[split_index + 1])
length(test.ts)

train.ts_log <- log(train.ts)
test.ts_log <- log(test.ts)

length(train.ts_log)
length(test.ts_log)

# Rolling Holt-Winters one-step-ahead forecast
# Parameters
train_length <- length(train.ts_log)  # 604
test_length <- length(test.ts_log)    # Should be 152

# Debug: Check actual lengths
cat("Train length:", train_length, "\n")
cat("Test length:", test_length, "\n")
cat("Full data length:", length(c(train.ts_log, test.ts_log)), "\n")

# Force correct test length if needed
if (test_length != length(test.ts_log)) {
  cat("Warning: test_length is", test_length, "but should be 152. Correcting...\n")
  test_length <- length(test.ts_log)
}

# Initialize vector to store predictions
predicted_values <- numeric(test_length)

# Combine train and test for rolling window
full_data_log <- c(train.ts_log, test.ts_log)

# Rolling forecast loop - CORRECTED
for (i in 1:test_length) {
  # Define the FIXED rolling window: always 604 observations
  # Window moves forward but maintains same size
  end_idx <- train_length + i - 1  # Position in full_data_log
  start_idx <- end_idx - train_length + 1  # Always 604 observations back
  
  # Extract rolling window data (always 604 observations)
  rolling_window <- full_data_log[start_idx:end_idx]
  
  # Verify window length
  if (length(rolling_window) != train_length) {
    stop("Window length error at iteration ", i)
  }
  
  # Convert to time series (maintaining same frequency)
  rolling_ts <- ts(rolling_window, frequency = frequency(train.ts_log))
  
  # Fit Holt-Winters model on rolling window with error handling
  tryCatch({
    hw_model <- HoltWinters(rolling_ts)
    
    # Forecast one step ahead
    forecast_result <- predict(hw_model, n.ahead = 1)
    
    # Store the prediction
    predicted_values[i] <- forecast_result[1]
    
  }, error = function(e) {
    # If HoltWinters fails, use simple exponential smoothing or last value
    cat("HoltWinters failed at iteration", i, "- using fallback method\n")
    predicted_values[i] <<- rolling_window[length(rolling_window)]  # Use last observed value
  })
  
  # Optional: print progress every 25 iterations
  if (i %% 25 == 0) {
    cat("Processed", i, "out of", test_length, "forecasts\n")
  }
}

# Display results
cat("Rolling forecast completed!\n")
cat("Number of predictions:", length(predicted_values), "\n")
cat("First 5 predictions:", head(predicted_values, 5), "\n")
cat("Last 5 predictions:", tail(predicted_values, 5), "\n")

# Convert predicted_values into a time series starting at test.ts_log's first time
predicted_ts <- ts(predicted_values,
                   start = time(test.ts_log)[1],
                   frequency = frequency(test.ts_log))

# Plot all series together
ts.plot(train.ts_log, test.ts_log, predicted_ts,
        col = c("black", "blue", "red"),
        lty = c(1, 1, 2),
        lwd = 2,
        main = "Train, Test, and 1-step Ahead Predictions (Log Scale)",
        ylab = "Log(S&P Index)",
        xlab = "Time")

legend("topleft",
       legend = c("Train Data", "Test Data", "Predicted"),
       col = c("black", "blue", "red"),
       lty = c(1, 1, 2),
       lwd = 2)


# Plot actual test values and predicted values
ts.plot(test.ts_log, predicted_ts,
        col = c("blue", "red"),
        lty = c(1, 2),
        lwd = 2,
        main = "Test Data vs 1-step Ahead Forecast (Log Scale)",
        ylab = "Log(S&P Index)",
        xlab = "Time")

legend("topleft",
       legend = c("Actual Test Data", "Predicted"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)

# Verify we have exactly 152 predictions
cat("Expected predictions:", test_length, "- Actual predictions:", length(predicted_values), "\n")

# Sum of Squared Errors (SSE) for out-of-sample
sse_out_of_sample <- sum(errors_TES_forecast^2)
cat("\nOut-of-sample forecast results:\n")
cat("SSE (Sum of Squared Errors):", round(sse_out_of_sample, 4), "\n")

# Optional: Convert back from log scale if needed
# predicted_values_original_scale <- exp(predicted_values)

# Store predictions (equivalent to future_forecast_TES$mean)
predicted_forecast <- predicted_values

# Calculate out-of-sample forecast errors
actual_values <- as.numeric(test.ts_log)
errors_TES_forecast <- actual_values - predicted_forecast

MSE <- mean(errors_TES_forecast^2)     # Mean Squared Error
RMSE <- sqrt(MSE)                       # Root Mean Squared Error
MAE <- mean(abs(errors_TES_forecast))  # Mean Absolute Error
MAPE <- mean(abs(errors_TES_forecast) / abs(test.ts_log)) * 100  # Mean Absolute Percentage Error
MAD <- median(abs(errors_TES_forecast))  # Median Absolute Deviation

# R-squared
SSE <- sum(errors_TES_forecast^2)
SST <- sum((train.ts_log - mean(train.ts_log))^2)
R2  <- 1 - (SSE / SST)

# Shifted actual and predicted series
actual_forecast <- as.numeric(test.ts_log)
predicted_forecast <- predicted_forecast
# Compute first differences (i.e., daily/periodic changes)
actual_diff_forecast <- diff(actual_forecast)
predicted_diff_forecast <- diff(predicted_forecast)

# Directional Accuracy (percentage of times the model predicted the correct direction)
DA_forecast <- mean(sign(actual_diff_forecast) == sign(predicted_diff_forecast))

cat("MAE:", MAE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAPE:", MAPE, "%",
    "| MAD:", MAD,
    "| DA:", round(DA_forecast * 100, 2), "%",
    "| R²:", R2, "\n")

# Results: MAE: 0.01408649 | MSE: 0.0003146174 | RMSE: 0.01773746 | MAPE: 0.2604913 % | MAD: 0.01126795 | DA: 49.67 % | R²: 0.9931827

# Compute residuals
residuals_TES <- errors_TES_forecast  # Same as train.ts_log - fitted_values

# Ljung-Box test for autocorrelation
lb_test <- Box.test(residuals_TES, lag = 20, type = "Ljung-Box")
print(lb_test)
# results: X-squared = 18.645, df = 20, p-value = 0.545
# Interpretation: if p-value > 0.05, residuals are likely white noise









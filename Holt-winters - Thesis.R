rm(list=ls( all = TRUE )) # clears the entire R environment
#_____________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________
#                                         HOLTWINTERS (TRIPLE EXPONENTIAL) - HoltWinters(data)
#_____________________________________________________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________________

# INDEX:

# STEP 1 - DATA PREPARATION
        # train/test split
        # triple exponential smoothing (tes) preparation and decomposition check

# STEP 2 - IN-SAMPLE FITTING OF THE TRIPLE EXPONENTIAL SMOOTHING (TES) MODEL
        # in-sample error evaluation and residual diagnostics of Tes model vs actual training data

# STEP 3 – IN-SAMPLE FITTING OF THE TES MODEL OVER A 3-YEAR PERIOD
        # in-sample error evaluation and residual diagnostics of the Tes model (3-year period) vs actual training data

# FORECAST PART
# STEP 4 – OUT-OF-SAMPLE EVALUATION: TES MODEL FORECAST 
        # out-of-sample error evaluation and residual diagnostics of the Tes model (3-year period) vs actual test data

# STEP 5 – OUT-OF-SAMPLE EVALUATION: ROLLING WINDOW TES MODEL (642-POINT WINDOW) VS ACTUAL TEST DATA
        # out-of-sample error evaluation and residual diagnostics of the rolling window Tes model (3-year period) vs actual test data


#_____________________________________________________________________________________________________________________________________________
#                                         STEP 1 - DATA PREPARATION
#_____________________________________________________________________________________________________________________________________________
data <- read.csv("C:/Users/Melectronics/OneDrive/Documenti/Master Finance/Thesis Idea/Codes/2_Multi Regression/AAPL_prices.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
data$Date <- as.Date(data$Date, format="%d.%m.%Y")
SP <- data[, -ncol(data)] # removing the vol colum
SP <- SP[nrow(SP):1, ] # invert the series. Starting from 2010
SP.ts = ts(SP[,2], start = c(2010, 1), frequency =252) # Creation of the time series
plot(SP.ts, main = "S&P Time Series", ylab = "S&P Index", xlab = "Time")

length(SP.ts)
# result: 3774

anyNA(SP.ts) # check if there is missing value. 
#result: FALSE: no missing value

# Decompose the series in trend, seasonal and the irregular part
decomp <- decompose(SP.ts) # Check if there is seasonality. decompose() -> additive model for default
decomp$seasonal
plot(decomp)

#----------------------------------------------------------------------
# TRAIN/TEST SPLIT
#----------------------------------------------------------------------
n <- length(SP.ts)                 # Total number of observations
split_index <- floor(0.85 * n)      # 80% index
train.ts <- window(SP.ts, end = time(SP.ts)[split_index])
test.ts <- window(SP.ts, start = time(SP.ts)[split_index + 1])

length(train.ts)
# result: 3207
length(test.ts)
# result: 567

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
# TRIPLE EXPONENTIAL SMOOTHING (TES) PREPARATION AND DECOMPOSITION CHECK
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

# comments:
# additive result > multiplicative result -> the series is additive.
# Although the decomposition is additive, the increasing variance in the observed and residual components suggests a multiplicative structure.

#_____________________________________________________________________________________________________________________________________________
#                  STEP 2 - IN-SAMPLE FITTING OF THE TRIPLE EXPONENTIAL SMOOTHING (TES) MODEL
#_____________________________________________________________________________________________________________________________________________
# MODEL FIT
train.ts.forecasts_TES <- HoltWinters(train.ts_log)
# train.ts.forecasts_TES <- HoltWinters(train.ts_log, beta = 0.1)  # you can specify a manual beta in case you are not trust at the automatic setting.

train.ts.forecasts_TES
# results:
#           Smoothing parameters:
#             alpha: 0.892884
#             beta : 0
#             gamma: 1
#           Coefficients:
#                     [,1]
#             a     5.0163414579
#             b     0.0013476927
#             s1    0.0064723183
#             s2    0.0064835430
#             s3    0.0012484402
#             ..       ....
#             s252 -0.0067733998

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
# IN-SAMPLE ERROR EVALUATION AND RESIDUAL DIAGNOSTICS OF TES MODEL VS ACTUAL TRAINING DATA
#----------------------------------------------------------------------
# Sum of Squared Errors (SSE)
cat("SSE (Sum of Squared Errors):", train.ts.forecasts_TES$SSE, "\n")
# results: SSE (Sum of Squared Errors): 1.205109 

# Calculate in-sample forecast errors
fitted_values_log <- fitted(train.ts.forecasts_TES)[, "xhat"]
fitted_values <- exp(fitted_values_log)
length(fitted_values)
# result: 2955 (the model uses the first 252 observations for initialization)

length(train.ts)
# result: 3207
# Remove the first 252 observations from train.ts, as the model used the first year for initial fitting.
train.ts_trimmed <- window(train.ts, start = time(train.ts)[252 + 1]) #  252 = length(train.ts) - length(fitted_values)
length(train.ts_trimmed)  # should be 2767, same as fitted_values
# result: 2955

# compute the errors on PRICE RESIDUALS
errors_TES <- train.ts_trimmed - fitted_values 

# Ljung-Box test for autocorrelation- IMPORTANT: THE LJUNG-BOX WILL BE COMPUTED ON PRICE RESIDUALS
lb_test <- Box.test(errors_TES, lag = 20, type = "Ljung-Box")
print(lb_test)
# results: X-squared = 128.43, df = 20, p-value < 2.2e-16
# Interpretation: if p-value > 0.05, residuals are likely white noise
# Comment: The residuals are not white noise, meaning the TES model did not capture all the patterns in the data.
# This indicates the TES model failed to capture all data patterns, so the residuals analysis suggests the model is inadequate.

# Next, we will fit a Holt-Winters model on a shorter training window to address the non-normality issue in the residuals.

#_____________________________________________________________________________________________________________________________________________
#                       STEP 3 – IN-SAMPLE FITTING OF THE TES MODEL OVER A 3-YEAR PERIOD
#_____________________________________________________________________________________________________________________________________________
# IMPORTANT:
# In this step, we test different window lengths (252, 504, 756, 1008, etc.) centered around the 85% split point.
# Within this window, we create a smaller train/test split:  
# - The training set (85%) is aligned with the original training period,  
#   but restricted to this shorter multi-year range.  
# - The test set is fixed at 15% of the original dataset, ensuring consistency in evaluation across models.
# This allows us to check whether the model performs better on a localized dataset.  

# define the train/test split for different window lengths extract their lengths
SP_3y <- ts(tail(SP.ts, 756), start=c(2022, 1), frequency=252)      # try different window lengths (252, 504, 756, 1008, etc.)
n <- length(SP_3y)                 # Total number of observations
split_index <- floor(0.85 * n)      # 80% index
train.ts_3y <- window(SP_3y, end = time(SP_3y)[split_index])
length(train.ts_3y)
# result: 642
test.ts_3y <- window(SP_3y, start = time(SP_3y)[split_index + 1])
length(test.ts_3y)
# result: 114

# Creating Training and Test Sets
n <- length(SP.ts)                 # Total number of observations
split_index <- floor(0.85 * n)      # 80% index
train.ts <- window(SP.ts, end = time(SP.ts)[split_index])
test.ts <- window(SP.ts, start = time(SP.ts)[split_index + 1])
tidx <- time(SP.ts)
# 642 values ending exactly at the split point
last_train  <- window(SP.ts,
                      start = tidx[split_index - length(train.ts_3y) + 1],
                      end   = tidx[split_index])
# 114 values starting right after the split point
first_test  <- window(SP.ts,
                      start = tidx[split_index + 1],
                      end   = tidx[split_index + length(test.ts_3y)])
# Sanity checks
length(last_train)   # expect 642
length(first_test)   # expect 114
# Plot
plot(SP.ts, type = "l", col = "gray", lwd = 1.5,
     main = "Train/Test Window Around the 85% Split",
     xlab = "Time", ylab = "Price")
lines(last_train, col = "blue", lwd = 2)
lines(first_test, col = "red",  lwd = 2)
# Mark the split boundary
abline(v = tidx[split_index], lty = 2)
points(tidx[split_index], SP.ts[split_index], pch = 19)
legend("topleft",
       legend = c("Full series", "Train (last 642)", "Test (first 114)"),
       col = c("gray", "blue", "red"), lwd = 2, lty = 1)
length(last_train)   # expect 642
length(first_test)   # expect 114
train.ts <- last_train
test.ts <- first_test
# transform in log
train.ts_log <- log(train.ts)

# MODEL FIT
train.ts.fit_TES <- HoltWinters(train.ts_log)
train.ts.fit_TES
# results:
#           Smoothing parameters:
#             alpha: 1
#             beta : 0
#             gamma: 0.05690053
#           Coefficients:
#                     [,1]
#             a     4.941915e+00
#             b     1.296506e-03
#             s1    5.750565e-02
#             s2    6.975644e-02
#             s3    7.533605e-02
#             ..       ....
#             s252  6.765285e-02

# plot decomposition of fitted value
plot(train.ts.fit_TES$fitted, main = "In-Sample Fitted Values from TES Model - Log-Transformed")

#----------------------------------------------------------------------
# IN-SAMPLE ERROR EVALUATION AND RESIDUAL DIAGNOSTICS OF THE TES MODEL (3-YEAR PERIOD) VS ACTUAL TRAINING DATA
#----------------------------------------------------------------------
# Sum of Squared Errors (SSE)
cat("SSE (Sum of Squared Errors):", train.ts.fit_TES$SSE, "\n")
# results: SSE (Sum of Squared Errors): 0.2048843 

# Calculate in-sample forecast errors
fitted_values_log <- fitted(train.ts.fit_TES)[, "xhat"]
fitted_values_log
fitted_values <- exp(fitted_values_log)
fitted_values
length(fitted_values)
# result: 390

# Remove the first 252 observations from train.ts, as the model used the first year for initial fitting.
train.ts_trimmed <- window(train.ts, start = time(train.ts)[252 + 1]) #  252 = length(train.ts) - length(fitted_values)
train.ts_trimmed
length(train.ts_trimmed)  # should be 2767, same as fitted_values
# result: 390 -> this should be the same of length(fitted_values)

# compute the errors on PRICE RESIDUALS
errors_TES_price <- train.ts_trimmed - fitted_values 
errors_TES_price

# Ljung-Box test for autocorrelation- IMPORTANT: THE LJUNG-BOX WILL BE COMPUTED ON PRICE RESIDUALS
lb_test <- Box.test(errors_TES_price, lag = 20, type = "Ljung-Box")
print(lb_test)
# results: X-squared = 27.486, df = 20, p-value = 0.1221
# Interpretation: if p-value > 0.05, residuals are likely white noise

# CONVERT PRICES TO RETURNS TO ENSURE COMPARABILITY WITH MODELS TRAINED ON RETURNS
fitted_returns <- diff(fitted_values) / lag(fitted_values, k = -1)
fitted_returns
train_returns <- diff(train.ts_trimmed) / lag(train.ts_trimmed, k = -1)
train_returns

# compute the errors
errors_TES <- train_returns - fitted_returns
errors_TES

MSE <- mean(errors_TES^2)     # Mean Squared Error
RMSE <- sqrt(MSE)         # Root Mean Squared Error
MAE <- mean(abs(errors_TES))  # Mean Absolute Error
MAPE <- mean(abs(errors_TES) / train.ts) * 100  # Mean Absolute Percentage Error
MAD <- median(abs(errors_TES))  # Median Absolute Deviation

# R-squared
SSE <- sum(errors_TES^2)
SST <- sum((train_returns - mean(train_returns))^2)
R2  <- 1 - (SSE / SST)

# Shifted actual and predicted series
ok <- complete.cases(fitted_returns, train_returns)
table(ok)     # quanti TRUE/FALSE
DA <- mean(sign(train_returns[ok]) == sign(fitted_returns[ok]))

cat("MAE:", MAE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAPE:", MAPE, "%",
    "| MAD:", MAD,
    "| DA:", round(DA * 100, 2), "%",
    "| R²:", R2, "\n")

# MAE: 0.02211604 | MSE: 0.001292053 | RMSE: 0.03594513 | MAPE: 0.01425194 % | MAD: 0.01289066 | DA: 65.81 % | R²: -3.085261 

# RESIDUAL ANALYSIS OF MODEL FIT
# Plot residuals over time
plot(errors_TES,
     main = "Residuals of TES Model (In-Sample)",
     ylab = "Residuals",
     xlab = "Time",
     type = "h")
# comment:
# We have to consider that residuals fluctuate around zero with no significant autocorrelation (Ljung-Box p = 0.1221),
# Variance appears inconsistent, indicating potential heteroskedasticity; 
# the strong initial spike may represent an outlier.

# Plot ACF of residuals
acf(errors_TES, lag.max = 20, main = "ACF of In-Sample Residuals (TES Model)")
# comment: ACF shows no significant autocorrelation beyond lag 0; residuals resemble white noise.
pacf(errors_TES, lag.max = 20, main = "PACF of In-Sample Residuals (TES Model)")
# comment: PACF shows small negative spikes at early lags, but no strong partial autocorrelation overall.

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
       main = "In-Sample Forecast Errors vs Normal Distribution (TES Model)", xlab = "Error")
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
  legend("topright", legend = c("Forecast Errors", "Normal Distribution"),
         col = c("red", "blue"), lty = c(1, 1), lwd = c(2, 2))
}
plotForecastErrors(errors_TES)
# comment:
# Residuals are centered around zero and uncorrelated (Ljung-Box p = 0.1221),
# but the histogram shows heavier tails than the normal distribution,
# suggesting forecast errors are not perfectly normally distributed.

# Overall, the residual diagnostics indicate that the TES model is adequate, 
# allowing us to proceed with the out-of-sample forecast.

#_____________________________________________________________________________________________________________________________________________
#                                               FORECAST
#_____________________________________________________________________________________________________________________________________________
#                           STEP 4 – OUT-OF-SAMPLE EVALUATION: TES MODEL FORECAST 
#_____________________________________________________________________________________________________________________________________________
# Now we will use the best training window identified in the previous analysis
SP_3y <- ts(tail(SP.ts, 756), start=c(2022, 1), frequency=252)
n <- length(SP_3y)                 # Total number of observations
split_index <- floor(0.85 * n)      # 80% index
train.ts_3y <- window(SP_3y, end = time(SP_3y)[split_index])
length(train.ts_3y)
# result: 642

# Creating Training and Test Sets
n <- length(SP.ts)                 # Total number of observations
split_index <- floor(0.85 * n)      # 80% index
train.ts <- window(SP.ts, end = time(SP.ts)[split_index])
test.ts <- window(SP.ts, start = time(SP.ts)[split_index + 1])
tidx <- time(SP.ts)
# 642 values ending exactly at the split point
last_train  <- window(SP.ts,
                      start = tidx[split_index - 642 + 1],
                      end   = tidx[split_index])

# Sanity checks
length(last_train)   # expect 642
length(test.ts)   # expect 567

# Plot
plot(SP.ts, type = "l", col = "gray", lwd = 1.5,
     main = "Train/Test Window Around the 85% Split",
     xlab = "Time", ylab = "Price")
lines(last_train, col = "blue", lwd = 2)
lines(test.ts, col = "red",  lwd = 2)
# Mark the split boundary
abline(v = tidx[split_index], lty = 2)
points(tidx[split_index], SP.ts[split_index], pch = 19)
legend("topleft",
       legend = c("Full series", "Train (last 642)", "Test (first 114)"),
       col = c("gray", "blue", "red"), lwd = 2, lty = 1)
length(last_train)   # expect 642
length(test.ts)   # expect 114
train.ts <- last_train
test.ts <- test.ts
# transform in log
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
plot(future_forecast_TES, main = "Out-of-Sample Forecast vs Actual Test Data (TES Model)",
     ylab = "Values", xlab = "Time")
forecast_time <- time(future_forecast_TES$mean)
aligned_time <- forecast_time[1:length(test.ts)]
lines(aligned_time, log(test.ts), col = "red", lwd = 1)
legend("topleft", legend = c("Forecast (DES)", "Actual Test Data"),
       col = c("red", "black"), lty = c(1, 1), lwd = c(2, 2))

#----------------------------------------------------------------------
# OUT-OF-SAMPLE ERROR EVALUATION AND RESIDUAL DIAGNOSTICS OF THE TES MODEL (3-YEAR PERIOD) VS ACTUAL TEST DATA
#----------------------------------------------------------------------

# Sum of Squared Errors (SSE)
cat("In-sample SSE (from training):", train.ts_log.forecasts_TES$SSE, "\n")

# Calculate out-sample forecast errors
predicted_values <- future_forecast_TES$mean
predicted_values

# TRANSFORMATION OF PRICE FORECAST ERRORS INTO RETURN ERRORS
last_price_train <- tail(train.ts, 1)
# out-of-sample prices series
pred_prices <- exp(as.numeric(predicted_values))
test_prices <- as.numeric(test.ts)
pred_prices_full <- c(last_price_train, pred_prices)
test_prices_full  <- c(last_price_train, test_prices)
# simple returns: r_t = (P_t - P_{t-1}) / P_{t-1}
pred_ret <- diff(pred_prices_full) / head(pred_prices_full, -1)
test_ret  <- diff(test_prices_full)  / head(test_prices_full,  -1)
# returns errors
errors_ret <- test_ret - pred_ret
errors_ret

MSE <- mean(errors_ret^2)     # Mean Squared Error
RMSE <- sqrt(MSE)         # Root Mean Squared Error
MAE <- mean(abs(errors_ret))  # Mean Absolute Error
MAPE <- mean(abs(errors_ret) / abs(test.ts_log)) * 100  # Mean Absolute Percentage Error
MAD <- median(abs(errors_ret))  # Median Absolute Deviation

# R-squared
SSE <- sum(errors_ret^2)
SSE # Result: 0.368177
SST <- sum((test_ret - mean(test_ret))^2)
SST # Result: 0.1358913
R2  <- 1 - (SSE / SST)


# align & drop NAs introduced by lag/diff
ok <- complete.cases(pred_ret, test_ret)
table(ok)     # quanti TRUE/FALSE
DA_forecast <- mean(sign(test_ret[ok]) == sign(pred_ret[ok]))

cat("MAE:", MAE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAPE:", MAPE, "%",
    "| MAD:", MAD,
    "| DA:", round(DA_forecast  * 100, 2), "%",
    "| R²:", R2, "\n")

# Results: MAE: 0.01934888 | MSE: 0.0006493422 | RMSE: 0.02548219 | MAPE: 0.3739323 % | MAD: 0.01543567 | DA: 48.32 % | R²: -1.70935 

# Out-of-sample performance shows low absolute errors (MAE, RMSE, MAPE), 
# but the negative R² and Directional Accuracy below 50% indicate poor explanatory 
# and predictive power, suggesting the TES model does not adequately capture 
# the dynamics of the test data.

# To address this, we proceed with a rolling window approach.

#_____________________________________________________________________________________________________________________________________________
#         STEP 5 – OUT-OF-SAMPLE EVALUATION: ROLLING WINDOW TES MODEL (642-POINT WINDOW) VS ACTUAL TEST DATA
#_____________________________________________________________________________________________________________________________________________
# We will use the best training window identified in the previous analysis
SP_3y <- ts(tail(SP.ts, 756), start=c(2022, 1), frequency=252)
n <- length(SP_3y)                 # Total number of observations
split_index <- floor(0.85 * n)      # 80% index
train.ts_3y <- window(SP_3y, end = time(SP_3y)[split_index])
length(train.ts_3y)
# result: 642

n <- length(SP.ts)                 # Total number of observations
split_index <- floor(0.85 * n)      # 80% index
train.ts <- window(SP.ts, end = time(SP.ts)[split_index])
test.ts <- window(SP.ts, start = time(SP.ts)[split_index + 1])
tidx <- time(SP.ts)
# 642 values ending exactly at the split point
last_train  <- window(SP.ts,
                      start = tidx[split_index - 642 + 1],
                      end   = tidx[split_index])

# Sanity checks
length(last_train)   # expect 642
length(test.ts)   # expect 567
# Plot
plot(SP.ts, type = "l", col = "gray", lwd = 1.5,
     main = "Train/Test Window Around the 85% Split",
     xlab = "Time", ylab = "Price")
lines(last_train, col = "blue", lwd = 2)
lines(test.ts, col = "red",  lwd = 2)
# Mark the split boundary
abline(v = tidx[split_index], lty = 2)
points(tidx[split_index], SP.ts[split_index], pch = 19)
legend("topleft",
       legend = c("Full series", "Train (last 642)", "Test (first 114)"),
       col = c("gray", "blue", "red"), lwd = 2, lty = 1)
length(last_train)   # expect 642
length(test.ts)   # expect 114
train.ts <- last_train
test.ts <- test.ts
# transform in log
train.ts_log <- log(train.ts)
test.ts_log <- log(test.ts)

# Rolling Holt-Winters one-step-ahead forecast
# Parameters
train_length <- length(train.ts_log)  # 604
test_length <- length(test.ts_log)    # Should be 152

# Debug: Check actual lengths
cat("Train length:", train_length, "\n")
# Train length: 642 
cat("Test length:", test_length, "\n")
# Test length: 567
cat("Full data length:", length(c(train.ts_log, test.ts_log)), "\n")
# Full data length: 1209

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
# Processed 550 out of 567 forecasts

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

#----------------------------------------------------------------------
# OUT-OF-SAMPLE ERROR EVALUATION AND RESIDUAL DIAGNOSTICS OF THE ROLLING WINDOW TES MODEL (3-YEAR PERIOD) VS ACTUAL TEST DATA
#----------------------------------------------------------------------
# Verify we have exactly 567 predictions
cat("Expected predictions:", test_length, "- Actual predictions:", length(predicted_values), "\n")

# TRANSFORMATION OF PRICE FORECAST ERRORS INTO RETURN ERRORS
last_price_train <- tail(train.ts, 1)
# out-of-sample prices series
pred_prices <- exp(as.numeric(predicted_values))
test_prices <- as.numeric(test.ts)
pred_prices_full <- c(last_price_train, pred_prices)
test_prices_full  <- c(last_price_train, test_prices)
# simple returns: r_t = (P_t - P_{t-1}) / P_{t-1}
pred_ret <- diff(pred_prices_full) / head(pred_prices_full, -1)
test_ret  <- diff(test_prices_full)  / head(test_prices_full,  -1)
# returns errors
errors_ret <- test_ret - pred_ret
errors_ret

# Sum of Squared Errors (SSE) for out-of-sample
sse_out_of_sample <- sum(errors_ret^2)
cat("SSE (Sum of Squared Errors):", round(sse_out_of_sample, 4), "\n")

MSE <- mean(errors_ret^2)     # Mean Squared Error
RMSE <- sqrt(MSE)                       # Root Mean Squared Error
MAE <- mean(abs(errors_ret))  # Mean Absolute Error
MAPE <- mean(abs(errors_ret) / abs(test.ts_log)) * 100  # Mean Absolute Percentage Error
MAD <- median(abs(errors_ret))  # Median Absolute Deviation

# R-squared
SSE <- sum(errors_ret^2)
SSE # result: 0.683306
SST <- sum((test_ret - mean(test_ret))^2)
SST # result: 0.1358913
R2  <- 1 - (SSE / SST)

# align & drop NAs introduced by lag/diff
ok <- complete.cases(pred_ret, test_ret)
table(ok)     # quanti TRUE/FALSE
DA_forecast_roll <- mean(sign(test_ret[ok]) == sign(pred_ret[ok]))

cat("MAE:", MAE,
    "| MSE:", MSE,
    "| RMSE:", RMSE,
    "| MAPE:", MAPE, "%",
    "| MAD:", MAD,
    "| DA:", round(DA_forecast_roll * 100, 2), "%",
    "| R²:", R2, "\n")

# Results: MAE: 0.02618787 | MSE: 0.001205125 | RMSE: 0.03471491 | MAPE: 0.5062138 % | MAD: 0.0204647 | DA: 52.2 % | R²: -4.028328 






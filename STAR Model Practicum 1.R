library(tsDyn)
library(readxl)
library(forecast)  # for the ACF and PACF plots

# Read the data from Excel or CSV file
data <- read_excel("C:/Users/river/OneDrive/Escritorio/Practicum/Final avg temp.xlsx") # If it's an Excel file

# Since there are no column names, assuming the data is in the first column
ts_data <- ts(data[[1]], frequency = 12)  # Replace 12 with the actual frequency of your data

# Check the structure of the time series object
str(ts_data)

# Fit the LSTAR model
lstar_model <- lstar(ts_data, m = 1, series = ts_data, trace = TRUE)

# Output the summary of the model
summary(lstar_model)

# Number of periods ahead you want to forecast
n_ahead <- 36  # For example, if your data is monthly, this would forecast three years ahead.

# Forecast future values using the LSTAR model
forecast_values <- predict(lstar_model, n.ahead = n_ahead)

# Extract the last 5 years of actual data
end_time <- length(ts_data)
start_time <- end_time - (5 * 12 - 1)  # 5 years * 12 months per year - 1 to include the current month
actual_data <- ts_data[(start_time:end_time)]

# Combine actual data and forecasted values
combined_data <- c(actual_data, forecast_values)

# Create a new time index for the combined data
time_index <- seq(start_time, end_time + n_ahead, by = 1)

# Plot the actual data and forecasts
plot(time_index, combined_data, type = "n", xlim = c(start_time, end_time + n_ahead),
     ylim = range(combined_data), xlab = "Time", ylab = "Values", 
     main = "LSTAR Model Forecast (Last 5 Years + Forecast)")
lines(start_time:end_time, actual_data, col = "blue")
lines((end_time + 1):(end_time + n_ahead), forecast_values, col = "red", type = "o")
legend("bottomleft", legend = c("Observed", "Forecast"), col = c("blue", "red"), lty = c(1, NA), pch = c(NA, 1), bty = "n")

# Print the forecasted values
print(forecast_values)

# Assuming the rest of your code is unchanged and above this section

# Calculate residuals
residuals_lstar <- resid(lstar_model)

# Remove missing values (if any) from the residuals before plotting
residuals_lstar <- na.omit(residuals_lstar)

# Plot ACF of residuals only if there are no missing values
if(length(residuals_lstar) > 0) {
  acf(residuals_lstar, main="ACF of Residuals")
}

# Plot PACF of residuals only if there are no missing values
if(length(residuals_lstar) > 0) {
  pacf(residuals_lstar, main="PACF of Residuals")
}
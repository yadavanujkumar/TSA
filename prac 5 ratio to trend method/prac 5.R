#Q. 1 Manually calculating seasonal index using ratio to trend method


# --------------------------
# Ratio-to-Trend Method in R
 # --------------------------

# Sample quarterly sales data (8 quarters = 2 years)
sales <- c(120,150,180,210, 130,160,190,220)
sales
  
time <- 1:length(sales)
time

# Put into a time series object (quarterly, start from 2021 Q1)
sales_ts <- ts(sales, start=c(2021,1), frequency=4)
sales_ts

# 1) Fit a trend line using linear regression
trend_model <- lm(sales ~ time)
trend_values <- fitted(trend_model)
trend_values

# 2) Ratio-to-Trend
ratio_to_trend <- (sales / trend_values) * 100

# 3) Group ratios by quarter
quarters <- cycle(sales_ts)   # gives 1,2,3,4 for each quarter
quarters

ratio_df <- data.frame(Quarter=quarters, Ratio=ratio_to_trend)
ratio_df


 # Average ratio for each quarter
seasonal_index <- aggregate(Ratio ~ Quarter, data=ratio_df, FUN=mean)
seasonal_index

# 4) Adjust seasonal indices to average = 100
overall_avg <- mean(seasonal_index$Ratio)
seasonal_index$Adj_Index <- seasonal_index$Ratio / overall_avg * 100

print(seasonal_index)

# --------------------------
# Plot for visualization
 # --------------------------
par(mfrow=c(2,1))
plot(sales_ts, main="Observed Sales with Fitted Trend")
lines(ts(trend_values, start=c(2021,1), frequency=4), col="red")

barplot(seasonal_index$Adj_Index, names.arg=paste0("Q",1:4),+ main="Seasonal Indices (Ratio-to-Trend)", ylab="Index")


#Q. 2 Calculating seasonality using ratio to trend method


 # ============================================
 # Seasonality Measurement in R (All Methods)
 # Using AirPassengers dataset
 # ============================================

# Load data
data("AirPassengers")
ts_data <- AirPassengers
ts_data


# -------------------------------
# 1. Classical Decomposition
# -------------------------------
decomp_add <- decompose(ts_data, type = "additive")
plot(decomp_add)
 
decomp_mul <- decompose(ts_data, type = "multiplicative")
plot(decomp_mul)
 
# Seasonal indices
cat("Additive seasonal indices:\n")
#Additive seasonal indices:
print(decomp_add$seasonal)

cat("Multiplicative seasonal indices:\n")
print(decomp_mul$seasonal)


 # -------------------------------
# 2. STL Decomposition (Most used in industry)
 # -------------------------------
stl_decomp <- stl(ts_data, s.window = "periodic")
plot(stl_decomp, main = "STL Decomposition")

# Seasonal component
cat("STL Seasonal Component:\n")
print(stl_decomp$time.series[, "seasonal"])


# -------------------------------
# 3. Autocorrelation Function (ACF & PACF)
# -------------------------------
acf(ts_data, lag.max = 48, main = "ACF - Detect Seasonality")
pacf(ts_data, lag.max = 48, main = "PACF - Detect Seasonality")


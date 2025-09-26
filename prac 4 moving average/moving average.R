#Q. 1 Decomposition
# Load dataset
data("AirPassengers")
ts_data <- AirPassengers

# Plot original data
plot(ts_data,
     main = "AirPassengers Data",
     ylab = "Number of Passengers",
     col = "blue")



# --- Additive Decomposition ---
decomp_add <- decompose(ts_data)
plot(decomp_add)

#What you’ll see:
 # Observed → original noisy series
#Trend → long-term upward movement
#Seasonal → repeating seasonal cycle (period = 12 here)
#Random (remainder) → leftover noise not explained by trend + seasonality


#Q. 2 Types of Decomposition
set.seed(123)

time <- 1:120

# Additive pattern: constant seasonality
trend_add <- 0.05 * time
season_add <- rep(c(-10, 0, 10, 0), length.out = 120)   # repeating pattern
noise_add <- rnorm(120, sd=2)
ts_add <- trend_add + season_add + noise_add
ts_add <- ts(ts_add, frequency=12, start=c(2000,1))
plot(ts_add, main="Synthetic Additive Series", col="blue")


# Multiplicative pattern: seasonal effect grows with trend
trend_mult <- 1 + 0.01 * time
season_mult <- rep(c(0.8, 1, 1.2, 1), length.out = 120) # scaling factor
noise_mult <- rnorm(120, sd=0.05)
ts_mult <- trend_mult * season_mult * (1 + noise_mult)
ts_mult <- ts(ts_mult, frequency=12, start=c(2000,1))




# --- Compare plots ---
par(mfrow = c(1,2))
plot(ts_add, main="Synthetic Additive Series", col="blue")
plot(ts_mult, main="Synthetic Multiplicative Series", col="red")


# --- Decomposition ---
decomp_add <- decompose(ts_add, type = "additive")
decomp_mult <- decompose(ts_mult, type = "multiplicative")
plot(decomp_add, xlab="Time")
plot(decomp_mult, xlab="Time")


#Q.3 Simple Moving Average
# Install package if needed
# install.packages("TTR")

library(TTR)

# Generate sample time series (trend + seasonality + noise)
set.seed(123)
time <- 1:100
trend <- 0.1 * time
seasonal <- 2 * sin(2 * pi * time/12)
noise <- rnorm(100, mean = 0, sd = 1)
ts_data <- trend + seasonal + noise

# Plot original series
plot(time, ts_data, type = "l", col = "gray",
     main = "Time Series with Moving Average",
     xlab = "Time", ylab = "Value")

# Moving averages using SMA() function
ma5 <- SMA(ts_data, n = 5)    # 5-period moving average

# Add moving averages to plot
lines(time, ma5, col = "blue", lwd = 2)

# Add legend
legend("topleft", legend = c("Original", "SMA (5)"),
       col = c("gray", "blue"),
       lty = 1, lwd = 2)


Q. 4  Multiple Simple Moving Averages in one plot

# Install package if needed
# install.packages("TTR")

library(TTR)

# Generate sample time series (trend + seasonality + noise)
set.seed(123)
time <- 1:100
trend <- 0.1 * time
seasonal <- 2 * sin(2 * pi * time/12)
noise <- rnorm(100, mean = 0, sd = 1)
ts_data <- trend + seasonal + noise

# Plot original series
plot(time, ts_data, type = "l", col = "gray",
     main = "Time Series with Moving Average (TTR::SMA)",
     xlab = "Time", ylab = "Value")

# Moving averages using SMA() function
ma5 <- SMA(ts_data, n = 5)    # 5-period moving average
ma10 <- SMA(ts_data, n = 10)  # 10-period moving average
ma20 <- SMA(ts_data, n = 20)  # 20-period moving average

# Add moving averages to plot
lines(time, ma5, col = "blue", lwd = 2)
lines(time, ma10, col = "red", lwd = 2)
lines(time, ma20, col = "darkgreen", lwd = 2)

# Add legend
legend("topleft", legend = c("Original", "SMA (5)", "SMA (10)", "SMA (20)"),
       col = c("gray", "blue", "red", "darkgreen"),
       lty = 1, lwd = 2)




#Q.5 
# Install if not already installed
# install.packages("zoo")

library(zoo)

# 1. Create sample time series data with 5-year cycles
set.seed(123)
years <- 1:30
data <- 10 + sin(2*pi*years/5) * 3 + rnorm(30, 0, 0.5)  
# base trend + 5-year cycle + some noise

# 2. Calculate 5-year and 7-year moving averages
ma_5 <- rollmean(data, k = 5, align = "center", fill = NA)
ma_7 <- rollmean(data, k = 7, align = "center", fill = NA)

# 3. Plot the original data and moving averages
plot(years, data, type = "o", col = "grey", pch = 16,
     main = "Comparison of 5-year vs 7-year Moving Average",
     xlab = "Year", ylab = "Value")
lines(years, ma_5, col = "blue", lwd = 2)
lines(years, ma_7, col = "red", lwd = 2, lty = 2)

legend("topright", legend = c("Original Data", "5-year MA", "7-year MA"),
       col = c("grey", "blue", "red"), lty = c(1,1,2), lwd = c(1,2,2),cex=0.6)



#Q.6 Types of moving averages 

library(TTR)

# Sample data
set.seed(123)
x <- cumsum(rnorm(100))

# Different types of MA
sma10 <- SMA(x, n = 10)
ema10 <- EMA(x, n = 10)
wma10 <- WMA(x, n = 10)

plot(x, type="l", col="gray", main="Different Types of Moving Averages")
lines(sma10, col="blue", lwd=2)
lines(ema10, col="red", lwd=2)
lines(wma10, col="darkgreen", lwd=2)
legend("topleft", legend=c("Original", "SMA", "EMA", "WMA"),
       col=c("gray", "blue", "red", "darkgreen"), lty=1, lwd=2)


#  SMA (Simple Moving Average)
#  EMA (Exponential Moving Average — gives more weight to recent values)
#  WMA (Weighted Moving Average — custom weights)


#  SMA = equal weights → simple, stable, but laggy.
#  WMA = linear weights → recent points matter more.
#  EMA = exponential weights → fastest reaction to changes.


#Q. 7 

# Simulated data with sudden jump
set.seed(123)
time <- 1:60
x <- c( rnorm(20, mean=10, sd=1),   # stable period
        rnorm(20, mean=20, sd=1),   # sudden jump
        rnorm(20, mean=15, sd=1))   # new stable level

# Trailing MA (window = 5)
ma_trailing <- stats::filter(x, rep(1/5, 5), sides=1)

# Centered MA (window = 5)
ma_centered <- stats::filter(x, rep(1/5, 5), sides=2)

# Plot
plot(time, x, type="l", col="gray", lwd=1.5,
     main="Centered vs Trailing Moving Average (with sudden jump)",
     xlab="Time", ylab="Value")

lines(ma_trailing, col="blue", lwd=2)
lines(ma_centered, col="red", lwd=2)

legend("topleft",
       legend=c("Original Data", "Trailing MA (sides=1)", "Centered MA (sides=2)"),
       col=c("gray","blue","red"), lty=1, lwd=2)


#What you’ll see:
#  The blue line (Trailing MA) lags behind the actual jump (because it only uses past data).
#The red line (Centered MA) reacts faster around the jump, because it uses both past and future points.
#Trailing MA is more realistic for forecasting (you can’t use future data).
#Centered MA is better for descriptive analysis (smoothing trend for visualization).






#✅ Trailing Moving Average
#At time t, it uses only the past n observations.
#Example:5 - period trailing SMA at time 10 = average of values 6–10.
#Always lags behind the true trend.
#Used in forecasting &
#  finance (because we can’t use “future” data in real time).

#✅ Centered Moving Average
#At time t, it uses values before and after time t.
#Example:5 - period centered SMA at time 10 = average of values 8–12 (2 before, 2 after).
#Much better alignment with the actual trend.
#Common in classical time series decomposition (e.g., seasonal - trend decomposition).
#But:cannot be used for real - time forecasting, because it needs future data.



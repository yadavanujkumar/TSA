cat("Q.1 Plot single modified exponential curve")
# Sample data
x <- 1:10
y <- 2 + 3 * exp(-0.4 * x)

# Base plot with first line
plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "X", ylab = "Y", main = "Three Exponential Lines")


cat("Q.2 Plot 3 modified exponential curve in single plot")
# Sample data
x <- 1:10
y1 <- 2 + 3 * exp(-0.4 * x)
y2 <- 1 + 5 * exp(-0.2 * x)
y3 <- 0.5 + 6 * exp(-0.1 * x)

# Base plot with first line
plot(x, y1, type = "l", col = "blue", lwd = 2, ylim = c(0, max(y1, y2, y3)),
     xlab = "X", ylab = "Y", main = "Three Exponential Lines")

# Add second line
lines(x, y2, col = "red", lwd = 2)

# Add third line
lines(x, y3, col = "green", lwd = 2)

# Add legend
legend("topright", legend = c("Line 1", "Line 2", "Line 3"),
       col = c("blue", "red", "green"), lwd = 2)


cat("Q.3 Fit modified exponential curve model and plot residuals")
# --------------------------------------
# Step 1: Simulate Sample Data
# --------------------------------------
x <- 1:10
y <- c(10.2, 7.5, 5.6, 4.3, 3.5, 3.0, 2.7, 2.5, 2.3, 2.1)

# --------------------------------------
# Step 2: Scatter Plot the Data
# --------------------------------------
plot(x, y, main = "Observed Data",
     xlab = "X", ylab = "Y", pch = 16, col = "darkgreen") 



# --------------------------------------
# Step 3: Fit Modified Exponential Model: y = a + b * exp(c * x)
# --------------------------------------
model <- nls(y ~ a + b * exp(c * x),
             start = list(a = 2, b = 8, c = -0.5))

# --------------------------------------
# Step 4: View Model Summary
# --------------------------------------
summary(model)


# --------------------------------------
# Step 5: Plot Fitted Curve with Data
# --------------------------------------
# Create smooth x values for curve
x_new <- seq(min(x), max(x), length.out = 100)
y_pred <- predict(model, newdata = data.frame(x = x_new))

# Plot data
plot(x, y, pch = 16, col = "darkgreen", xlab = "X", ylab = "Y",
     main = "Modified Exponential Curve Fit")

# Add fitted curve
lines(x_new, y_pred, col = "blue", lwd = 2)



# --------------------------------------
# Step 6: Add Fitted Values to Original Data
# --------------------------------------
fitted_vals <- fitted(model)
data.frame(x = x, y = y, fitted = round(fitted_vals, 2))




# --------------------------------------
# Step 7: Plot Residuals (Observed - Fitted)
# --------------------------------------
residuals <- resid(model)
plot(x, residuals, type = "h", col = "red", lwd = 2,
     main = "Residual Plot", xlab = "X", ylab = "Residuals")
abline(h = 0, col = "black", lty = 2)



# --------------------------------------
# Step 8: Plot Multiple Fits (Optional)
# --------------------------------------
# Fit another model with different starting values (just for comparison)
model2 <- nls(y ~ a + b * exp(c * x),
              start = list(a = 2.5, b = 7, c = -0.3))

y_pred2 <- predict(model2, newdata = data.frame(x = x_new))

# Plot both fits
plot(x, y, pch = 16, col = "black", main = "Comparing Two Fits",
     xlab = "X", ylab = "Y")
lines(x_new, y_pred, col = "blue", lwd = 2)     # Original fit
lines(x_new, y_pred2, col = "red", lwd = 2, lty = 2)  # Second fit
legend("topright", legend = c("Fit 1", "Fit 2"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# --------------------------------------
# Step 9: Interpret Parameters
# --------------------------------------
cat("Estimated coefficients:\n")
print(coef(model))
cat("\nParameter meanings:\n")
cat("a: Asymptote (value as x → ∞)\n")
cat("b: Starting scale (value added to a at x = 0)\n")
cat("c: Rate of decay/growth (negative = decay, positive = growth)\n")

cat("Q. 4 Plot effects of changing a, b and c parameters in modified exponential curve")



# Common x values
x <- seq(0, 10, length.out = 100)

# -----------------------------
# 1. Effect of changing `a`
# -----------------------------
a_vals <- c(0, 2, 4)
b <- 5
c <- -0.5
plot(x, rep(NA, length(x)), type = "n", ylim = c(0, 10), main = "Effect of 'a'",
     xlab = "x", ylab = "y")
for (a in a_vals) {
  y <- a + b * exp(c * x)
  lines(x, y, lwd = 2, col = which(a_vals == a))
}
legend("topright", legend = paste("a =", a_vals), col = 1:3, lwd = 2)





x <- seq(0, 10, length.out = 100)

# -----------------------------
# 2. Effect of changing `b`
# -----------------------------
b_vals <- c(2, 5, 8)
a <- 2
c <- -0.5
plot(x, rep(NA, length(x)), type = "n", ylim = c(0, 10), main = "Effect of 'b'",
     xlab = "x", ylab = "y")
for (b in b_vals) {
  y <- a + b * exp(c * x)
  lines(x, y, lwd = 2, col = which(b_vals == b))
}
legend("topright", legend = paste("b =", b_vals), col = 1:3, lwd = 2)



x <- seq(0, 10, length.out = 100)

# -----------------------------
# 3. Effect of changing `c`
# -----------------------------
c_vals <- c(-1, -0.5, -0.1)
a <- 2
b <- 5
plot(x, rep(NA, length(x)), type = "n", ylim = c(0, 10), main = "Effect of 'c'",
     xlab = "x", ylab = "y")
for (c in c_vals) {
  y <- a + b * exp(c * x)
  lines(x, y, lwd = 2, col = which(c_vals == c))
}
legend("topright", legend = paste("c =", c_vals), col = 1:3, lwd = 2)


cat("Q.5 Plot different types of exponential curves in 2*2 matrix
1. Exponential Growth
2. Exponential Decay
3. Negative Exponential Growth
4. Negative Exponential Decay")

# Set plotting area: 2 rows x 2 columns , mfrow = multi-frame row-wises
par(mfrow = c(2, 2))

# Common x values
x <- seq(0, 10, length.out = 100)

# -------------------------------
# 1. Exponential Growth (Convex Up)
a <- 0; b <- 2; c <- 0.5
y <- a + b * exp(c * x)
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = "Exponential Growth (Convex ↑)", ylab = "y", xlab = "x")

# -------------------------------
# 2. Exponential Decay (Concave Down)
a <- 0; b <- 2; c <- -0.5
y <- a + b * exp(c * x)
plot(x, y, type = "l", col = "red", lwd = 2,
     main = "Exponential Decay (Concave ↓)", ylab = "y", xlab = "x")

# -------------------------------
# 5. Negative Exponential Growth (Concave Up)
a <- 5; b <- -3; c <- -0.5
y <- a + b * exp(c * x)
plot(x, y, type = "l", col = "purple", lwd = 2,
     main = "Neg. Exp. Growth (Concave ↑)", ylab = "y", xlab = "x")

# -------------------------------
# 6. Negative Exponential Decay (Convex Down)
a <- 5; b <- -3; c <- 0.5
y <- a + b * exp(c * x)
plot(x, y, type = "l", col = "brown", lwd = 2,
     main = "Neg. Exp. Decay (Convex ↓)", ylab = "y", xlab = "x")

cat("Q.6 Plot all 4 types of modifed exponential curve in single plot")

curve(0 + 1 * exp(0.5 * x), from=0, to=5, col="blue", lwd=2, ylab="y", main="Different Exponential Curves", ylim=c(-2, 4))
curve(0 + 1 * exp(-0.5 * x), add=TRUE, col="red", lwd=2)
curve(2 - 1 * exp(0.5 * x), add=TRUE, col="darkgreen", lwd=2)
curve(2 - 1 * exp(-0.5 * x), add=TRUE, col="orange", lwd=2)
legend("topright", legend=c("Exp Growth", "Exp Decay", "Neg Exp Growth", "Neg Exp Decay"),
       col=c("blue", "red", "darkgreen", "orange"), lwd=2)

cat("Extra Q.")

# Load required library
library(ggplot2)

# 1. Sample data
set.seed(123)
x <- 1:10
y <- 10 + 5 * exp(0.3 * x) + rnorm(10, 0, 5)  # true model: a + b*e^(c*x)
df <- data.frame(x = x, y = y)

# 2. Fit Model 1: y = a + b * exp(c * x)
fit_exp <- nls(y ~ a + b * exp(c * x), data = df,
               start = list(a = 10, b = 5, c = 0.3))
summary(fit_exp)


# 3. Fit Model 2: y = a + b * c^x
fit_pow <- nls(y ~ a + b * c^x, data = df,
               start = list(a = 10, b = 5, c = 1.3))
summary(fit_pow)



# 4. Create predictions for a smooth curve
x_seq <- seq(min(x), max(x), length.out = 100)
pred_df <- data.frame(
  x = x_seq,
  y_exp = predict(fit_exp, newdata = data.frame(x = x_seq)),
  y_pow = predict(fit_pow, newdata = data.frame(x = x_seq))
)

# 5. Plot
ggplot(df, aes(x, y)) +
  geom_point(size = 2, color = "black") +
  geom_line(data = pred_df, aes(x, y_exp), color = "blue", size = 0.5, linetype = "solid") +
  geom_line(data = pred_df, aes(x, y_pow), color = "red", size = 1.2, linetype = "dashed") +
  labs(title = "Modified Exponential Curve Fitting",
       subtitle = "Blue: y = a + b * exp(c * x) | Red Dashed: y = a + b * c^x",
       x = "x", y = "y") +
  theme_minimal()






cat ("Question - 
  
  How R calculates a,b and c parameters in modified exponential.

Other than plot function how we can draw same graphs in R.

What if we start with different stating points are we going to get same model parameters at the end.

")




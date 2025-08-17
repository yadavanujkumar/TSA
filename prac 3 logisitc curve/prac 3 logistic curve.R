cat("Q.1 Fitting and Plotting Logistic Curves")

# ---------------------------------------
# Step 1: Simulate Logistic-Like Data
# ---------------------------------------
set.seed(123)
x <- 1:20
y <- 100 / (1 + exp(-0.4 * (x - 10))) + rnorm(20, 0, 2)  # Add small noise
# Plot the data
plot(x, y, pch = 16, col = "blue", main = "Logistic-like Data", xlab = "x", ylab = "y")



# ---------------------------------------
# Step 2: Fit Logistic Model using nls()
# Model: y = L / (1 + exp(-k(x - x0)))
# ---------------------------------------
logistic_model <- nls(y ~ L / (1 + exp(-k * (x - x0))),
                      start = list(L = 100, k = 0.5, x0 = 10))
summary(logistic_model)



# ---------------------------------------
# Step 3: Plot Fitted Curve with Data
# ---------------------------------------

x_new <- seq(min(x), max(x), length.out = 100)
y_pred <- predict(logistic_model, newdata = data.frame(x = x_new))
plot(x, y, pch = 16, col = "darkgreen", main = "Fitted Logistic Curve", xlab = "x", ylab = "y")
lines(x_new, y_pred, col = "red", lwd = 2)



# ---------------------------------------
# Step 4: Plot Residuals
# ---------------------------------------
res <- resid(logistic_model)
plot(x, res, type = "h", lwd = 2, col = "orange", main = "Residual Plot", ylab = "Residuals")
abline(h = 0, col = "black", lty = 2)



# ---------------------------------------
# Step 5: Fit Another Model with Different Starting Parameters
# ---------------------------------------
logistic_model2 <- nls(y ~ L / (1 + exp(-k * (x - x0))),
                       start = list(L = 90, k = 0.3, x0 = 8))
y_pred2 <- predict(logistic_model2, newdata = data.frame(x = x_new))
plot(x, y, pch = 16, col = "black", main = "Comparison of Logistic Fits", xlab = "x", ylab = "y")
lines(x_new, y_pred, col = "blue", lwd = 2)
lines(x_new, y_pred2, col = "purple", lwd = 2, lty = 2)
legend("bottomright", legend = c("Model 1", "Model 2"),
       col = c("blue", "purple"), lwd = 2, lty = c(1, 2))



# ---------------------------------------
# Step 6: Interpret Parameters
# ---------------------------------------
cat("Estimated Parameters from Model 1:\n")
print(coef(logistic_model))
cat("\nMeaning:\n")
cat("L: Asymptote (max value as x → ∞)\n")
cat("k: Growth rate (steepness)\n")
cat("x0: Midpoint (x-value at half max)\n")



# ---------------------------------------
# Step 7: ggplot2 Visualization (Optional)
# ---------------------------------------
library(ggplot2)
df <- data.frame(x = x, y = y)
df_pred <- data.frame(x = x_new, y = y_pred)
ggplot(df, aes(x, y)) +
  geom_point(color = "black", size = 2) +
  geom_line(data = df_pred, aes(x, y), color = "blue", size = 1.2) +
  labs(title = "Logistic Curve Fit using ggplot2", x = "x", y = "y") +
  theme_minimal()



cat("Q.2 Code to Visualize Impact of Each Parameter (L, k, x0)")
# Set common x values
x <- seq(0, 20, length.out = 200)
# ---------------------------------------
# Plot 1: Effect of L (Asymptote)
# ---------------------------------------
L_vals <- c(50, 100, 150)
k <- 0.5
x0 <- 10
plot(x, rep(NA, length(x)), type = "n", ylim = c(0, max(L_vals)),
     main = "Effect of 'L' (Asymptote)", xlab = "x", ylab = "y")
for (L in L_vals) {
  y <- L / (1 + exp(-k * (x - x0)))
  lines(x, y, col = which(L_vals == L), lwd = 2)
}
legend("bottomright", legend = paste("L =", L_vals), col = 1:length(L_vals), lwd = 2)



# ---------------------------------------
# Plot 2: Effect of k (Growth Rate)
# ---------------------------------------
L <- 100
k_vals <- c(0.2, 0.5, 1)
x0 <- 10
plot(x, rep(NA, length(x)), type = "n", ylim = c(0, L), 
     main = "Effect of 'k' (Growth Rate)", xlab = "x", ylab = "y")
for (k in k_vals) {
  y <- L / (1 + exp(-k * (x - x0)))
  lines(x, y, col = which(k_vals == k), lwd = 2)
}
legend("bottomright", legend = paste("k =", k_vals), col = 1:length(k_vals), lwd = 2)



# ---------------------------------------
# Plot 3: Effect of x0 (Midpoint)
# ---------------------------------------
L <- 100
k <- 0.5
x0_vals <- c(5, 10, 15)
plot(x, rep(NA, length(x)), type = "n", ylim = c(0, L), 
     main = "Effect of 'x0' (Midpoint)", xlab = "x", ylab = "y")
for (x0 in x0_vals) {
  y <- L / (1 + exp(-k * (x - x0)))
  lines(x, y, col = which(x0_vals == x0), lwd = 2)
}
legend("bottomright", legend = paste("x0 =", x0_vals), col = 1:length(x0_vals), lwd = 2)











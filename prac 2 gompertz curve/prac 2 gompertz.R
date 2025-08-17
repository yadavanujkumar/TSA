cat("Q.1 Code to Visualize Impact of Each Parameter (L, k, x0)")
# Gompertz function
gompertz <- function(a, b, c, x) {
  a * exp(-b * exp(-c * x))
}	

# Common x range
x <- seq(-10, 20, length.out = 200)

# ---------------------------------------
# Effect of a (Asymptote)
# ---------------------------------------
a_vals <- c(50, 100, 150)
b <- 2
c <- 0.5

plot(x, rep(NA, length(x)), type = "n", ylim = c(0, max(a_vals)),
     main = "Effect of a (Asymptote)", xlab = "x", ylab = "y")
for (a in a_vals) {
  y <- gompertz(a, b, c, x)
  lines(x, y, col = which(a_vals == a), lwd = 2)
}
legend("bottomright", legend = paste("a =", a_vals),
       col = 1:length(a_vals), lwd = 2, cex = 0.8)

# ---------------------------------------
# Effect of b (Horizontal Shift)
# ---------------------------------------
a <- 100
b_vals <- c(0.5, 2, 5)
c <- 0.5

plot(x, rep(NA, length(x)), type = "n", ylim = c(0, a),
     main = "Effect of b (Horizontal Shift)", xlab = "x", ylab = "y")
for (b in b_vals) {
  y <- gompertz(a, b, c, x)
  lines(x, y, col = which(b_vals == b), lwd = 2)
}
legend("bottomright", legend = paste("b =", b_vals),
       col = 1:length(b_vals), lwd = 2, cex = 0.8)

# ---------------------------------------
# Effect of c (Growth Rate)
# ---------------------------------------
a <- 100
b <- 2
c_vals <- c(0.2, 0.5, 1)

plot(x, rep(NA, length(x)), type = "n", ylim = c(0, a),
     main = "Effect of c (Growth Rate)", xlab = "x", ylab = "y")
for (c in c_vals) {
  y <- gompertz(a, b, c, x)
  lines(x, y, col = which(c_vals == c), lwd = 2)
}
legend("bottomright", legend = paste("c =", c_vals),
       col = 1:length(c_vals), lwd = 2, cex = 0.8)



cat("Q.2 Comparing Modified Exponential, Logistic and Gompertz Curve ")

# Parameters
a <- 100   # Max value (asymptote)
b <- 5     # Inflection point parameter
c <- 0.3   # Growth rate parameter

# Time sequence
t <- seq(-10, 20, by = 0.1)

# Growth functions
logistic <- function(t, a, b, c) a / (1 + exp(-c * (t - b)))
gompertz <- function(t, a, b, c) a * exp(-b * exp(-c * t))
modified_exponential <- function(t, a, b, c) a * (1 - exp(-c * (t - b)))

# Calculate values
y_logistic <- logistic(t, a, b, c)
y_gompertz <- gompertz(t, a, b, c)
y_modexp <- modified_exponential(t, a, b, c)

# Plot curves
plot(t, y_logistic, type = "l", col = "blue", lwd = 2,
     ylim = c(0, a), xlab = "Time", ylab = "Value",
     main = "Growth Curve Comparison")
lines(t, y_gompertz, col = "red", lwd = 2, lty = 2)
lines(t, y_modexp, col = "green", lwd = 2, lty = 3)

# Add legend
legend("bottomright",
       legend = c("Logistic", "Gompertz", "Modified Exponential"),
       col = c("blue", "red", "green"),
       lty = c(1, 2, 3), lwd = 2, bty = "n", cex=0.5)

# Asymptote annotation
abline(h = a, col = "gray", lty = 3)
text(1, a - 3, "Asymptote", col = "gray40", pos = 4)

# Inflection point for Logistic
points(b, logistic(b, a, b, c), pch = 19, col = "blue")
arrows(b, logistic(b, a, b, c) + 10, b, logistic(b, a, b, c) + 2,
       length = 0.1, col = "blue")
text(b, logistic(b, a, b, c) + 12, "Logistic inflection", col = "blue", cex = 0.8)

# Inflection point for Gompertz
inflect_gomp_t <- log(b) / c  # Approximate
points(inflect_gomp_t, gompertz(inflect_gomp_t, a, b, c), pch = 19, col = "red")
arrows(inflect_gomp_t + 1, gompertz(inflect_gomp_t, a, b, c) - 5,
       inflect_gomp_t, gompertz(inflect_gomp_t, a, b, c),
       length = 0.1, col = "red")
text(inflect_gomp_t + 1.2, gompertz(inflect_gomp_t, a, b, c) - 7,
     "Gompertz inflection", col = "red", cex = 0.8, pos = 4)

# Note for Modified Exponential
text(12, modified_exponential(12, a, b, c), "Gradual saturation", col = "green", cex = 0.8, pos = 4)




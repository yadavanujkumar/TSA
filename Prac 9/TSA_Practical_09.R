cpi <- c(150.3,150.9,151.4,151.9,
        152.5,152.5,152.9,153.2,
        153.7,153.6,153.5,154.4,
        154.9,155.7,156.3,156.6,
        156.7,157.0,157.3,157.8,
        158.3,158.6,158.6,159.1)

month <- seq(as.Date("1995-01-01"),as.Date("1996-12-01"),by = "month")

#Params
lambda <- 0.3
n <- length(cpi)
n

#first - order expnential smoothing
y_tilde <- numeric(n)
y_tilde[1] <- cpi[1] #initial value

for (t in 2:n){
  y_tilde[t] <- lambda*cpi[t]+(1-lambda)*y_tilde[t-1]
}

#Combine results
result1 <- data.frame(
  Month = month,
  Observation = cpi,
  Smoothed = round(y_tilde,4)
)

#plot
plot(month,cpi,type = 'o',col = "blue", pch = 16,ylim = c(144,160),
     main = "First Order Exponential Smoothing(lambda = 0.3)", ylab = "CPI",xlab="Month")
lines(month,y_tilde,col = "red",lwd= 2)
legend("topleft",legend=c("Observed","Smoothed"),col = c("blue","red"),lty =1,pch =16)


#Second order exponential smoothing
y1 <- numeric(n)#first smoothed
y2 <- numeric(n)
forecast <-numeric(n)

#Initial values
y1[1] <- cpi[1]
y2[1] <- y1[1]

forecast[1] <- cpi[1]

for(t in 2:n){
  y1[t] <- lambda*cpi[t] + (1 - lambda)*y1[t-1]
  y2[t] <- lambda*y1[t] + (1-lambda)*y2[t-1]
  forecast[t] <- 2*y1[t]- y2[t]
}

#Combine results
result2 <- data.frame(
  Month = month,
  Observation = cpi,
  y1 = round(y1,4),
  y2 = round(y2,4),
  forecast = round(forecast,4)
)
print(result2)
#plot
plot(month,cpi,type = 'o',col = "blue", pch = 16,ylim = c(144,160),
     main = "Second Order Exponential Smoothing(lambda = 0.3)", ylab = "CPI",xlab="Month")
lines(month,forecast,col = "red",lwd= 2)
legend("topleft",legend=c("Observed","Smoothed"),col = c("red","blue"),lty =1,pch =16)


#genrate time vector
t<-1:300

#genrate stainary time series
set.seed(123)
y_statinary <- rnorm(length(t), mean = 0, sd =1)
y_statinary<- y_statinary/max(y_statinary)


#genrate non statinary time seriws with a trend
set.seed(456)
y_trend<-cumsum(rnorm(length(t),mean = 0, sd= 4))+t /100
y_trend<- y_trend / max(y_trend)

#set up more attraaction layout for the plots
par(mfcol =c(2,3), mar = c(4,4,2,1))

#plor statinmary time series
plot(t,y_statinary, type = 'l', col='darkgreen', xlab = "Time(t)", ylab = "y(t)", main="statinary time series ",
     xex.main =1.2, cex.lab=1.1)

#acf for statinary time series
acf_y_statinary <- acf(y_statinary, lag.max = length(y_statinary),plot=FALSE)
plot(acf_y_statinary, main='ACF-Statinary time series', cex.mioan = 1.2, cex.lab=1.1)

#Non-stationary time series with Trend
plot(t,y_trend, type = 'l',col = 'steelblue', xlab = "time (t)",ylab = "y(t)",main = "Non-stationary time series with Trend", cex.lab = 1.1)

#ACF - Non-stationary time series with trend
acf_y_trend <- acf(y_trend, lag.max = length(y_trend),plot = FALSE)
plot(acf_y_trend,main = 'ACF - Non-stationary time series with trend',cex.main = 1.2,cex.lab = 1.1)
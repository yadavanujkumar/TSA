#step 1 sample data

#quartely sales data for 3 years

sales <-c(120,90,100,130,150,110,120,160,180,130,150,200)
sales

#converty to qaurtely time series

ts_data <- ts(sales,start = c(1,1), frequency = 4)
ts_data



#plot the raw series

plot(ts_data,main="original quaterly sales data", ylab = "sales",col="blue")

#step 2: manual ratio to moving average

ma= stats::filter(ts_data, filter = rep(1/4,4),sides = 2)
ma

#ratio = Actual/ma
ratio<-ts_data/ma
ratio

#grpup by qauter and take mean of ratios
quarter<-cycle(ts_data)
quarter


ratio_by_quarter<-tapply(ratio,quarter,mean,na.rm=TRUE)
ratio_by_quarter

#normaliise seasonal indices(mean =1)
manual_si<-ratio_by_quarter/mean(ratio_by_quarter,na.rm = TRUE)
manual_si

#step 3: using decompose
#multiplcative decompose

mdecomp<-decompose(ts_data,type = "multiplicative")
plot(mdecomp)

#additive decompose

adecomp<-decompose(ts_data,type = "additive")
plot(adecomp)



#step 4:  deseasonalization
#remove seasonality
deseasonalized<-ts_data/mdecomp$seasonal
deseasonalized
plot(deseasonalized,main = "Deseasonalized Data (Trend + Randomness)")

#step 5 simple forecasting
#fit a linear trend model to desasonalized data

time <- 1:length(deseasonalized)
time
fit <- lm(deseasonalized ~ time)
fit
#forecast next 4 quaters
future_time<-length(deseasonalized) + 1:4
future_time
trend_forcast <- predict(fit, newdata = data.frame(time =future_time))
trend_forcast


#add seasonality back
forecast <-length(deseasonalized) +1:4
forecast


#step 6: comparision iwth stl
#stl decomposition(losses smothing)
#Comparison with STL
stl_decomp <- stl(ts_data, s.window = "periodic")
plot(stl_decomp, main = "STL Decomp")

acf(residuals, na.action = na.pass, main = "ACF of Residuals")



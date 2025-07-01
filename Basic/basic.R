#load necessary data
install.packages(c("tseries","zoo"))
library(tseries)
library(zoo)
#load in dataset 
data("lh")

#1.visiual inspection
plot(lh, main="guine pigs tibia lenghts")

#2.summary stats
summary(lh)

#3.adf test
adf_test <-adf.test(lh)
print(adf_test)

data("AirPassengers")

library(TSstudio)

ts_info(AirPassengers)

ts_plot(AirPassengers,
        slider = TRUE,
        title = "Monthly Airline Passenger Numbers 1949-1960",
        Ytitle = "Thousands of Passengers",
        Xtitle = paste("Time Series Analysis, Forecasting and Control"))

data("AirPassengers")

d <- decompose(AirPassengers)

str(d)

plot(d)

d_m <- decompose(AirPassengers, type = "multiplicative")

plot(d_m)

ts_decompose(AirPassengers, type = "both") 

library(plotly)
library(dplyr)
library(lubridate)

ap_smooth <- ts_ma(AirPassengers, n = 6,
                   separate = FALSE)

ap_smooth$plot %>%
  layout(legend = list(x = 0.1, y = 0.9))

df <- ts_to_prophet(AirPassengers) %>% 
  select(date = ds, y) %>% 
  left_join(ts_to_prophet(ap_smooth$ma_6) %>%
              select(date = ds, trend = y), by = "date")


head(df, 8)

df$detrend <- df$y - df$trend

head(df, 8)

ts_plot(df,
        title = "AirPassenger Detrending") %>%
  layout(legend = list(x = 0.1, y = 0.9))


 
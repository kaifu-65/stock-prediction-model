#install.packages(c("tidyverse", "quantmod", "TTR", "xts", "forecast", "prophet", "caret", "httr", "jsonlite", "shiny"))
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(forecast)
library(prophet)
library(caret)
url<-"https://api.coingecko.com/api/v3/coins/bitcoin/market_chart?vs_currency=usd&days=30"
response <- GET(url)
data <- fromJSON(rawToChar(response$content))
btc_prices <- data$prices
#btc stands for bitcoin and df stands for dataframe
btc_df <- data.frame(
  time = as.POSIXct(btc_prices[, 1]/1000, origin = "1970-01-01"),
  price = btc_prices[, 2]
)
#btc_df type kareingae tou data show karreiga
#the use of library(dplyr) is happening over here
btc_df <- btc_df %>%
  arrange(time) %>%
  mutate(price_diff = c(NA, diff(price)))
#the use of library(ggplot2) is happening here
print(ggplot(btc_df, aes(x = time, y = price)) +
  geom_line(color = "steelblue") +
  labs(title = "Bitcoin Price Over Time", x = "Date", y = "Price (USD)")
)
#weere using library forecast over here
#agr hum frequency change bhi kardein tou faida nhi houga graph par affect nhi ayega cuz data seasonal nhi hai aur agr hai bhi tou ussak period sirf 24hr ka phase hai
ts_data <- ts(btc_df$price, frequency =24)
model<- auto.arima(ts_data)
forecasted<- forecast(model, h=10)
plot(forecasted)
#idher hum library prophet in this part also we use it for data prediction pattern in terms of events
#install rstan if it doesnt work
df_prophet <- btc_df %>%
  rename(ds = time, y = price)

m <- prophet(df_prophet, yearly.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 10)
forecast <- predict(m, future)

print(plot(m, forecast))
#using library caret here, also download package stringi
set.seed(123)

# Example: Predict price based on previous values
btc_df <- btc_df %>% mutate(lag1 = lag(price), lag2 = lag(price, 2))
btc_clean <- na.omit(btc_df)

model <- train(price ~ lag1 + lag2, data = btc_clean, method = "lm")
pred <- predict(model, btc_clean)
plot(btc_clean$price, type = 'l', col = "blue")
lines(pred, col = "red")

postResample(pred, btc_clean$price)

#postResample(pred, btc_clean$price)(Function call)
#head(btc_df), View(btc_df)
#plot(ts_data)
#plot(forecasted)
#head(df_prophet)
#head(forecast)
#plot(pred)
#View(btc_clean)
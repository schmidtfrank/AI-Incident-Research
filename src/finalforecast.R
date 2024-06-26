library(ggplot2)
library(forecast)
library(dplyr)
library(readr)
library(lubridate)

#You select here:
decision <- "Automatic"
num_months <- 48
#Import
main <- read_csv("final.csv")

#Readability, not necessary on newest pipeline version
main <- main %>%
  rename(Deployer = Alleged.Deployer, Developer = Alleged.Developer)

#Category
categories <- main %>%
  select(-cleaned_title, -cleaned_description, -State, -City, -County, -Deployer, -Developer, -Country) %>%
  filter(Primary == decision)

#Format
categories$date <- mdy(categories$date)

#Create Year + Month Variables
categories <- categories %>%
  mutate(year = year(date), month = month(date))

#Group and count properly
temp <- categories %>%
  group_by(year, month) %>%
  count() %>%
  arrange()

#Create Dates to fill
fullTime <- data.frame(date = seq(ymd(min(categories$date)), ymd(max(categories$date)), by = "months"))

#Add year + month
fullTime <- fullTime %>%
  mutate(year = year(date), month = month(date)) %>%
  select(-date)

#Join
tsDF <- full_join(temp, fullTime, by = c("year", "month"))

#Clean
tsDF <- tsDF %>%
  mutate(n = if_else(is.na(n), 0, n)) %>%
  arrange(year, month)

#Create Time Series
tsdata <- ts(tsDF$n, frequency = 12, start = c(min(tsDF$year), min(tsDF$month)))

#Fit Model
autoModel <- auto.arima(tsdata)

#Forecast
finalForecast <- forecast(autoModel, h = num_months)

autoplot(finalForecast) + theme_classic() + theme(axis.ticks.length = unit(0.2, "cm"), axis.title.y = element_text(angle = 0, vjust = 0.5), axis.title = element_text(size = 18), title = element_text(size = 20), axis.text = element_text(size = 16)) + labs(title = "Forecasting AI Incidents for the Next 48 Months", caption = expression("Fit with ARIMA(1,1,5)"%*%"(0,0,2)"[12]), x = "Year", y = "Incident Count") + scale_x_continuous(expand = c(0,0.05)) + scale_y_continuous(expand = c(0,0))


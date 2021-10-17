## Austin Cleaning Data 
library(readr)
library(dplyr)
library(tidyverse)

# data_reading ------------------------------------------------------------
## reading in the stock data
stocks_over_time<- read_csv("EOY_US_ETF_data.csv") %>% 
  select(SYMBOL,DATE,ADJCLOSE) %>%
  rename(Date = "DATE") %>%
  filter(SYMBOL == "VTI")

stocks_over_time$Date <- format(as.Date(stocks_over_time$Date, 
                                        format="%Y/%m/%d"),"%Y") 
  
stocks_over_time <- distinct(stocks_over_time, Date, .keep_all = TRUE)
## reading in the gdp data
gdp_over_time <- read_csv("GREEN_GROWTH2018_GDP.csv") %>% 
  rename(country = 'Country Name') %>%
  filter(country == "United States") %>%
  select("Date", "Value") %>%
  rename(gdp = "Value")

gdp_over_time$Date <- format(as.Date(gdp_over_time$Date, 
                                        format="%Y/%m/%d"),"%Y")

## reading in the environment data 1
env<- read_csv("AEI_OTHER2018.csv") %>% 
  rename(country = 'Country Name') %>%
  filter(country == "United States") %>%
  select(Date, Indicator, Value)

env$Indicator <- as.factor(env$Indicator)

env$Date <- format(as.Date(env$Date, 
                          format="%Y/%m/%d"),"%Y")

env <- spread(env, Indicator, Value)

## reading in the environment data 2


## joining the gdp data to the market data
stocks_market <- merge(
  x = gdp_over_time,
  y = stocks_over_time,
  by = "Date"
)

## joining the market and environment data
stocks_env <- merge(
  x = stocks_market,
  y = env,
  by = "Date"
)

stocks_env <- stocks_env[c(1:17),]

write.csv(stocks_env,"./full_table.csv")

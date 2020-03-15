#Loading libraries
library(dplyr) # data-wrangling
library(lubridate) # dealing with date
library(padr) # padding library
library(forecast) # time-series package
library(tseries) # adf.testing package
library(MLmetrics) # error calculation package
require(microbenchmark)
require(stringr)
library(knitr)
library(magrittr)


#Reading dataframes by months in 2019
On_Time_2019_1 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_1.csv")
On_Time_2019_2 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_2.csv")
On_Time_2019_3 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_3.csv")
On_Time_2019_4 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_4.csv")
On_Time_2019_5 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_5.csv")
On_Time_2019_6 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_6.csv")
On_Time_2019_7 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_7.csv")
On_Time_2019_8 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_8.csv")
On_Time_2019_9 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_9.csv")
On_Time_2019_10 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_10.csv")
On_Time_2019_11<- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_11.csv")
On_Time_2019_12 <- read.csv("C:/Users/Sri Kalidindi/Documents/DataMiningProject/DataMiningProject/Data/On_TimeOn_Time_2019_12.csv")


#combining dataframes into one
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_2)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_3)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_4)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_5)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_6)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_7)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_8)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_9)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_10)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_11)
On_Time_2019_1 <- rbind(On_Time_2019_1,On_Time_2019_12)



##selecting Los-Angles airport for time series analysis
#Los-Angles airport ID = 12478
data.frame(ORIGIN_AIRPORT_ID = c(12478), On_Time_2019_1 = data.frame(table(On_Time_2019_1$ORIGIN_AIRPORT_ID))[,1])


#Converting date-time into proper format and dumping into one column
temp <- On_Time_2019_1$CRS_DEP_TIME
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(temp))
temp <- paste0(temp2, temp)
temp
On_Time_2019_1$CRS_DEP_TIME<-format(strptime(temp, format="%H%M"), format = "%H:%M")
On_Time_2019_1$dep_datetime<-as.POSIXct(paste(On_Time_2019_1$FL_DATE, On_Time_2019_1$CRS_DEP_TIME), format="%Y-%m-%d %H:%M")


#Extracting Los-Angeles data 
newark_flight_data <- On_Time_2019_1 %>%
  group_by(ORIGIN_AIRPORT_ID, dep_datetime) %>% 
  summarise(On_Time_2019_1 = sum(n())) %>% 
  ungroup() %>% 
  filter(ORIGIN_AIRPORT_ID == 12478) %>% 
  select(-ORIGIN_AIRPORT_ID)
newark_flight_data

#muting missing records using pad
newark_flight_data <- newark_flight_data %>% 
  pad() %>%
  replace(., is.na(.), 0)


#Feature Engineering - extracting date,month,hour
flight_date <- newark_flight_data %>% 
  mutate(year = year(dep_datetime),
         day = day(dep_datetime),
         month = month(dep_datetime),
         date = paste0(year,"-",month,"-",day),
         date = ymd(date)) %>%
  select(-dep_datetime, -year, -month, -day) %>% 
  group_by(date) %>% 
  summarise(On_Time_2019_1 = sum(On_Time_2019_1))

flight_date

#Finding the range of flight date's
range(flight_date$date)

#Creation of TS object 
flight_ts <- ts(data = flight_date$On_Time_2019_1,
                start = range(flight_date$date)[[1]],
                frequency = 7) # weekly seasonality

#Decomposing TS object
flight_decomposition <- decompose(flight_ts)
autoplot(flight_decomposition)



#Creation of 2nd TS object
flight_date$On_Time_2019_1 %>% 
  msts(seasonal.periods = c(7,7*4)) %>% # multiseasonal ts (weekly, monthly)
  mstl() %>% # multiseasonal ts decomposition
  autoplot() 

#Creation of 3rd TS object
flight_date$On_Time_2019_1 %>% 
  msts(seasonal.periods = c(7*4,7*4*3)) %>% # multiseasonal ts (monthly, monthly)
  mstl() %>% # multiseasonal ts decomposition
  autoplot() 

#Last ts object based on third
flight_msts <- flight_date$On_Time_2019_1 %>% 
  msts(seasonal.periods = c(7*4,7*4*3))

#Verifying for stationaries
adf.test(flight_msts)

#Using differencial
flight_msts %>% 
  diff() %>% 
  adf.test() 

#Splitting into train and test
flight_train <- flight_msts %>% head(length(flight_msts) - 7*4)
flight_test <- flight_msts %>% tail(7*4)


#Holt-Winters ETS
flight_ets <- stlm(flight_train, method = "ets", lambda = 0) 
#SARIMA
flight_arima <- stlm(flight_train, method = "arima", lambda = 0)

#Forecasting
flight_ets_f <- forecast(flight_ets, h = 28)
flight_arima_f <- forecast(flight_arima, h = 28)

flight_ets_f 

#Visualizing
library(ggplot2)
library(gridExtra)

a <- autoplot(flight_ets_f, series = "ETS", fcol = "red") +
  autolayer(flight_msts, series = "Actual", color = "black") + 
  labs(subtitle = "loa angles airport flights, in 2019",
       y = "Freqence of Flights") +
  theme_minimal()

b <- autoplot(flight_arima_f, series = "ARIMA", fcol = "blue") +
  autolayer(flight_msts, series = "Actual", color = "black") +
  labs(subtitle = "los angles flights, in 2019",
       y = "Frequency of Flights") +
  theme_minimal()

grid.arrange(a,b)

#Evaluation of model:RMSE
data.frame(ETS = RMSE(flight_ets_f$mean, flight_test), ARIMA = RMSE(flight_arima_f$mean, flight_test))


#Checking assumptions
shapiro.test(flight_arima_f$residuals) 
hist(flight_arima_f$residuals,breaks=20)

Box.test(flight_arima_f$residuals, type = "Ljung-Box") 
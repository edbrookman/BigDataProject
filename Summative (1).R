setwd("/Users/edbrookman/Documents/Rstuff")

#start time
start <- Sys.time()

# Required packages
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(quantmod)  
library(ggcorrplot)
library(caret)
library(yfR)
library(rpart)
library(randomForest)
library(rpart.plot)
library(gbm)
library(glmnet)
library(readxl)

# Obtain stock price data from Yahoo Finance

# BVSP

first_date <- '2019-01-01'
last_date <- '2024-03-20'

BVSP_data <- yf_get(tickers =  "^BVSP", 
                   first_date = first_date, 
                   last_date = last_date)

range(BVSP_data$ref_date)

write.csv(BVSP_data,  "BVSP_data", row.names = FALSE)

# FTAS

FTAS_data <- yf_get(tickers = "^FTAS",
                  first_date = first_date, 
                  last_date = last_date)

range(FTAS_data$ref_date)

write.csv(FTAS_data,  "FTAS_data.csv", row.names = FALSE)

# First, considering data from 2023 onward (shorter term)

BVSP_s <- BVSP_data %>% 
  filter(year(ref_date) >= 2023) %>%
    select(ref_date, price_open, price_close, price_high, price_low, price_adjusted, volume,ret_adjusted_prices)
BVSP_s$volume= log(BVSP_s$volume)
colnames(BVSP_s) =c("date", "open"  , "close" , "high", "low", "price", "logvolume", "return") 
range(BVSP_s$date)

FTAS_s <- FTAS_data %>% filter(year(ref_date) >= 2023) %>%   select(ref_date, price_open, price_close, price_high, price_low, price_adjusted, volume,ret_adjusted_prices)
FTAS_s$volume= log(FTAS_s$volume)
colnames(FTAS_s) =c("date", "open"  , "close" , "high", "low", "price", "logvolume", "return") 
head(FTAS_s)
range(FTAS_s$date)


#compute direction variable based on daily returns

BVSP_s$direction <- as.factor(ifelse(BVSP_s$return<0,"DOWN","UP"))
FTAS_s$direction <- as.factor(ifelse(FTAS_s$return<0,"DOWN","UP"))

# next day's direction
BVSP_s$direction.1 <- dplyr::lead(BVSP_s$direction,1,order_by=BVSP_s$date)
FTAS_s$direction.1 <- dplyr::lead(FTAS_s$direction,1,order_by=FTAS_s$date)

#compute a different direction variable based on intraday returns
BVSP_s$intraday <- log(BVSP_s$close) - log(BVSP_s$open) 
FTAS_s$intraday <- log(FTAS_s$close) - log(FTAS_s$open)

BVSP_s$dirintra<- as.factor(ifelse(BVSP_s$intraday < -0.0075,"LARGE DOWN", 
                                   ifelse(BVSP_s$intraday > -0.0075 & BVSP_s$intraday <=0, "SMALL DOWN", 
                                          ifelse(BVSP_s$intraday > 0 & BVSP_s$intraday <0.0075, "SMALL UP","LARGE UP"))))

FTAS_s$dirintra<- as.factor(ifelse(FTAS_s$intraday < -0.0075,"LARGE DOWN", 
                                   ifelse(FTAS_s$intraday > -0.0075 & FTAS_s$intraday <=0, "SMALL DOWN", 
                                          ifelse(FTAS_s$intraday > 0 & FTAS_s$intraday <0.0075, "SMALL UP","LARGE UP"))))

BVSP_s$direction.2 <-lead(BVSP_s$dirintra,1,order_by=BVSP_s$date)
FTAS_s$direction.2 <-lead(FTAS_s$dirintra,1,order_by=FTAS_s$date)


### External indicators ----------------------------------------------------------------------------------

## Crude oil price
OIL <- yf_get(tickers = "CL=F",
              first_date = first_date, 
              last_date = last_date)  
OIL <- OIL %>% select(ref_date, price_close) %>%
  rename(date = ref_date, oil_price = price_close)

## Exchange rates

# UK GBP - EUR/USD
GBPUSD <- yf_get(tickers =  "GBPUSD=X", 
               first_date = first_date, 
               last_date = last_date)

GBPUSD <- GBPUSD %>% select(ref_date, price_close) %>%
  rename(date = ref_date, GBPUSD = price_close)

GBPEUR <- yf_get(tickers =  "GBPEUR=X", 
                 first_date = first_date, 
                 last_date = last_date)

GBPEUR <- GBPEUR %>% select(ref_date, price_close) %>%
  rename(date = ref_date, GBPEUR = price_close)

# Brazilian Real - EUR/USD

BRLUSD <- yf_get(tickers =  "BRLUSD=X", 
                 first_date = first_date, 
                 last_date = last_date)

BRLUSD <- BRLUSD %>% select(ref_date, price_close) %>%
  rename(date = ref_date, BRLUSD = price_close)

BRLEUR <- yf_get(tickers =  "BRLEUR=X", 
                 first_date = first_date, 
                 last_date = last_date)

BRLEUR <- BRLEUR %>% select(ref_date, price_close) %>%
  rename(date = ref_date, BRLEUR = price_close)

## Interest rates

# Not using: UKGILT <- yf_get(tickers =  "IGLT.L", 
                 #first_date = first_date, 
                 #last_date = last_date)

UKBOND <- read_csv("UK_1yr_bond.csv") %>% select(Date, Price) %>%
  rename(date = Date, uk_bond = Price)

UKBOND$date <- mdy(UKBOND$date)

BRAZILBOND <- read_csv("Brazil_1yr_bond.csv") %>% select(Date, Price) %>%
  rename(date = Date, brazil_bond = Price)

BRAZILBOND$date <- mdy(BRAZILBOND$date)


## Commodity prices

corn <- yf_get(tickers = "ZC=F",
               first_date = first_date,
               last_date = last_date)

sugar <- yf_get(tickers = "SB=F",
                first_date = first_date,
                last_date = last_date)

gold <- yf_get(tickers = "GC=F",
               first_date = first_date,
               last_date = last_date)

corn <- corn %>% select(ref_date, price_close) %>% 
  rename(date = ref_date, corn_price = price_close)

sugar <- sugar %>% select(ref_date, price_close)%>% 
  rename(date = ref_date, sugar_price = price_close)

gold <- gold %>% select(ref_date, price_close) %>% 
  rename(date = ref_date, gold_price = price_close)

commodities <- merge(corn, sugar, by = "date", all = TRUE) %>%
  merge(gold, by = "date", all = TRUE)

## Economic Policy Uncertainty

EPU <- read.csv("UK_Daily_Policy_Data.csv") 
EPU$date <- as.Date(EPU$date, format = "%d %m %Y")

## Neighbours

brazil_neighbour <- yf_get(tickers =  "EWZ", 
                           first_date = first_date,
                           last_date = last_date)  #measure of the exchange traded funds in Brazil- like a stock but dont involve actual ownership of securities. its a pre-defined basket of goods in the brazilian economy 

brazil_neighbour <- brazil_neighbour %>% select(ref_date, price_close) %>%
  rename(date = ref_date, brazil_neighbour = price_close)

uk_neighbour <- yf_get(tickers =  "^STOXX", 
                       first_date = first_date,
                       last_date = last_date) #Measure of Europe

uk_neighbour <- uk_neighbour %>% select(ref_date, price_close) %>%
  rename(date = ref_date, uk_neighbour = price_close)

## Carbon and electricity prices

carbon_prices <- read_csv("carbon price data.csv") #2005 to oct 2023
electricity_prices <- read_excel("electricity prices data.xlsx") #2020 to 2024

#clean data                                                                                 
carbon_prices <- carbon_prices %>% select(date, eu_price, nz_price, kor_price, uk_price)
carbon_prices$date <- dmy(carbon_prices$date)
carbon_prices <- carbon_prices %>% filter(carbon_prices$date >= first_date) %>%
  rename(eu_carbon_price = eu_price, nz_carbon_price = nz_price, kor_carbon_price = kor_price, uk_carbon_price = uk_price)


electricity_prices <- electricity_prices %>% rename(daily_elec_avg= "Daily average",`7day_elec_avg`= "7-day average", date = Date)
electricity_prices <- electricity_prices %>% filter(electricity_prices$date >= first_date)

## Natural Gas commodity prices
NGAS <- yf_get(tickers =  "NG=F", 
               first_date = first_date,
               last_date = last_date)

NGASdf <- NGAS %>% select(ref_date, price_adjusted)
colnames(NGASdf) = c("date", "NGAS price")

## Temperature data

temperature_brazil <- read_csv("temp_data.csv") %>% select(date, brazil_temp)
temperature_brazil$date <- dmy(temperature_brazil$date)

temperature_uk <- read_csv("temp_data.csv") %>% select(date, uk_temp)
temperature_uk$date <- dmy(temperature_uk$date)


## Merge all external indicators together

external_indicators <- merge(commodities, EPU, by = "date", all = TRUE) %>%
  merge(OIL, by = "date", all = TRUE) %>%
  merge(GBPUSD, by = "date", all = TRUE) %>%
  merge(GBPEUR, by = "date", all = TRUE) %>%
  merge(BRLUSD, by = "date", all = TRUE) %>%
  merge(BRLEUR, by = "date", all = TRUE) %>%
  merge(UKBOND, by = "date", all = TRUE) %>%
  merge(BRAZILBOND, by = "date", all = TRUE) %>%
  merge(brazil_neighbour, by = "date", all = TRUE) %>%
  merge(uk_neighbour, by = "date", all = TRUE) %>%
  merge(carbon_prices, by = "date", all = TRUE) %>%
  merge(electricity_prices, by = "date", all = TRUE) %>%
  merge(NGASdf, by = "date", all = TRUE) %>%
  merge(temperature_brazil, by = "date", all = TRUE) %>%
  merge(temperature_uk, by = "date", all = TRUE)
  

### Technical indicators -----------------------------------------------------------------------------

library(TTR)

# DV Intermediate Oscillator
BVSP_s$DVI2 <- DVI(BVSP_s$price,n=2)
BVSP_s$DVI7 <- DVI(BVSP_s$price,n=7)
BVSP_s$DVI14 <- DVI(BVSP_s$price,n=14)
BVSP_s$DVI21 <- DVI(BVSP_s$price,n=21)

FTAS_s$DVI2 <- DVI(FTAS_s$price,n=2)
FTAS_s$DVI7 <- DVI(FTAS_s$price,n=7)
FTAS_s$DVI14 <- DVI(FTAS_s$price,n=14)
FTAS_s$DVI21 <- DVI(FTAS_s$price,n=21)

# Ultimate Oscillator
BVSP_s$UltOsc <- ultimateOscillator(BVSP_s[,c("high", "low", "close")])
FTAS_s$UltOsc <- ultimateOscillator(FTAS_s[,c("high", "low", "close")])

# Triple Smoothed Exponential Oscillator (TRIX)
BVSP_s$TRIX2 <- TRIX(BVSP_s$price,n=2)
BVSP_s$TRIX5 <- TRIX(BVSP_s$price,n=5)
BVSP_s$TRIX10 <- TRIX(BVSP_s$price,n=10)

FTAS_s$TRIX2 <- TRIX(FTAS_s$price,n=2)
FTAS_s$TRIX5 <- TRIX(FTAS_s$price,n=5)
FTAS_s$TRIX10 <- TRIX(FTAS_s$price,n=10)


# runSum (runFunctions)
BVSP_s$runSum <- runSum(BVSP_s$price,n=10, cumulative=FALSE)
BVSP_s$runMin <- runMin(BVSP_s$price,n=10, cumulative=FALSE)
BVSP_s$runMax <- runMax(BVSP_s$price,n=10, cumulative=FALSE)
BVSP_s$runMedian <- runMedian(BVSP_s$price,n=10, cumulative=FALSE)
BVSP_s$runVar <- runVar(BVSP_s$price, y=NULL, n=10, cumulative=FALSE)
BVSP_s$runSD <- runSD(BVSP_s$price,n=10, cumulative=FALSE)
BVSP_s$runMAD <- runMAD(BVSP_s$price, n=10, center=NULL, 
                        stat="median", constant=1.4826,
                        non.unique="mean", cumulative=FALSE)
BVSP_s$wilderSum <- wilderSum(BVSP_s$price,n=10)

FTAS_s$runSum <- runSum(FTAS_s$price,n=10, cumulative=FALSE)
FTAS_s$runMin <- runMin(FTAS_s$price,n=10, cumulative=FALSE)
FTAS_s$runMax <- runMax(FTAS_s$price,n=10, cumulative=FALSE)
FTAS_s$runMedian <- runMedian(FTAS_s$price,n=10, cumulative=FALSE)
FTAS_s$runVar <- runVar(FTAS_s$price, y=NULL, n=10, cumulative=FALSE)
FTAS_s$runSD <- runSD(FTAS_s$price,n=10, cumulative=FALSE)
FTAS_s$runMAD <- runMAD(FTAS_s$price, n=10, center=NULL, 
                        stat="median", constant=1.4826,
                        non.unique="mean", cumulative=FALSE)
FTAS_s$wilderSum <- wilderSum(FTAS_s$price,n=10)

# Bollinger Bands
BVSP_s$BBands2 <- BBands(BVSP_s$price, n=2)
BVSP_s$BBands5 <- BBands(BVSP_s$price, n=5)
BVSP_s$BBands10 <- BBands(BVSP_s$price, n=10)

FTAS_s$BBands2 <- BBands(FTAS_s$price, n=2)
FTAS_s$BBands5 <- BBands(FTAS_s$price, n=5)
FTAS_s$BBands10 <- BBands(FTAS_s$price, n=10)

# Aroon indicator
BVSP_s$aroon2 <- aroon(BVSP_s[,c("high", "low")], n=2)
BVSP_s$aroon5 <- aroon(BVSP_s[,c("high", "low")], n=5)
BVSP_s$aroon10 <- aroon(BVSP_s[,c("high", "low")], n=10)

FTAS_s$aroon2 <- aroon(FTAS_s[,c("high", "low")], n=2)
FTAS_s$aroon5 <- aroon(FTAS_s[,c("high", "low")], n=5)
FTAS_s$aroon10 <- aroon(FTAS_s[,c("high", "low")], n=10)

# Williams Accumulation/Distribution
BVSP_s$WilliamsAD <- williamsAD(BVSP_s[,c("high", "low", "close")])
FTAS_s$WilliamsAD <- williamsAD(FTAS_s[,c("high", "low", "close")])

# Trend Detection Index
BVSP_s$tdi10 <- TDI(BVSP_s$price, n=10)
BVSP_s$tdi20 <- TDI(BVSP_s$price, n=20)
BVSP_s$tdi30 <- TDI(BVSP_s$price, n=30)

FTAS_s$tdi10 <- TDI(FTAS_s$price, n=10)
FTAS_s$tdi20 <- TDI(FTAS_s$price, n=20)
FTAS_s$tdi30 <- TDI(FTAS_s$price, n=30)

# Chaikin Accumulation/ Distribution

BVSP_s$ChaikinAD <- chaikinAD(BVSP_s[,c("high", "low", "close")], BVSP_s[,"logvolume"])
FTAS_s$ChaikinAD <- chaikinAD(FTAS_s[,c("high", "low", "close")], FTAS_s[,"logvolume"])


# Know Sure Thing (KST) 
#For each day (week, month, etc.), the KST calculates the ROC
#over several periods.
#Those ROCs are smoothed using the given moving averages,
#then multiplied by their respective weighting values.
#The resulting values are summed for each day (month, week, etc.).
FTAS_s$kst <- KST(FTAS_s$price, roc_periods = c(10, 15, 20, 30),
                   sma_periods = c(10, 10, 10, 15),
                   ema_periods = c(10, 10, 10, 10),
                   signal_periods = 9)

BVSP_s$kst <- KST(BVSP_s$price, roc_periods = c(10, 15, 20, 30),
                sma_periods = c(10, 10, 10, 15),
                ema_periods = c(10, 10, 10, 10),
                signal_periods = 9)


#  Chande Momentum Oscillator (CMO)
#The CMO divides the total movement by the net movement
#([up - down] / [up + down]),
#where RSI divides the upward movement by the net movement (up / [up + down]).

FTAS_s$cmo <- CMO(FTAS_s$price, n = 14)

BVSP_s$cmo <- CMO(BVSP_s$price, n = 14)


#Chaikin Money Flow
FTAS_s$cmf <- CMF(FTAS_s[, c("high", "low", "close")],
                  FTAS_s[, c("logvolume")])

BVSP_s$cmf <- CMF(BVSP_s[, c("high", "low", "close")],
                  BVSP_s[, c("logvolume")])


# Donchian Channel

# The top line is the highest high of the past n periods.
# The bottom line is the lowest low of the past n periods.
# The middle line is the average of the top and bottom lines.

FTAS_s$donchianchannel20 <- DonchianChannel(FTAS_s[, c("high",
                                                         "low")],
                                             n = 20)
FTAS_s$donchianchannel10 <- DonchianChannel(FTAS_s[, c("high",
                                                         "low")],
                                             n = 10)


BVSP_s$donchianchannel20 <- DonchianChannel(BVSP_s[, c("high",
                                                   "low")],
                                          n = 20)
BVSP_s$donchianchannel10 <- DonchianChannel(BVSP_s[, c("high",
                                                   "low")],
                                          n = 10)


# Ehlers Correlation trend Indicator

#Ehler's Correlation Trend Indicator (CTI) measures the Spearman
#correlation of the
#price with the ideal trend line: a straight line with increasing slope

FTAS_s$EHcorrel <- CTI(FTAS_s$price, n = 10, slope = 1)

BVSP_s$EHcorrel <- CTI(BVSP_s$price, n = 10, slope = 1)

#True range
FTAS_s$tr <- TR(FTAS_s[,c("high","low", "close")])
BVSP_s$tr <- TR(BVSP_s[,c("high","low", "close")])


#Directional Movement Index
FTAS_s$dx <- ADX(FTAS_s[,c("high","low", "close")], n = 14)
BVSP_s$dx <- ADX(BVSP_s[,c("high","low", "close")], n = 14)


#Relative Strength index
FTAS_s$rsi3 <- RSI(FTAS_s$intraday, n = 3)
FTAS_s$rsi5 <- RSI(FTAS_s$intraday, n = 5)
FTAS_s$rsi10 <- RSI(FTAS_s$intraday, n = 10)
FTAS_s$rsi14 <- RSI(FTAS_s$intraday, n = 14)

BVSP_s$rsi3 <- RSI(BVSP_s$intraday, n = 3)
BVSP_s$rsi5 <- RSI(BVSP_s$intraday, n = 5)
BVSP_s$rsi10 <- RSI(BVSP_s$intraday, n = 10)
BVSP_s$rsi14 <- RSI(BVSP_s$intraday, n = 14)


#Commodity Channel Index

#CCI relates the current price and the average of price over n periods. 
#The CCI usually falls in a channel of -100 to 100.
#A basic CCI trading system is:
#Buy (sell) if CCI rises above 100 (falls below -100)
#and sell (buy) when it falls below 100 (rises above -100).

FTAS_s$cci3 <- CCI(FTAS_s[,c("high","low", "close")],n = 3)
FTAS_s$cci5 <- CCI(FTAS_s[,c("high","low", "close")],n = 5)

BVSP_s$cci3 <- CCI(BVSP_s[,c("high","low", "close")],n = 3)
BVSP_s$cci5 <- CCI(BVSP_s[,c("high","low", "close")],n = 5)

#Close location value

#The CLV will fall in a range of -1 to +1. If the CLV is +/-1,
#the close is at the high/low; if the CLV is 0, the close is
#directly between the high and low.

FTAS_s$clv <- CLV(FTAS_s[,c("high","low", "close")])

BVSP_s$clv <- CLV(BVSP_s[,c("high","low", "close")])

# Guppy Multiple Moving Averages
FTAS_s$GMMA <- GMMA(
  FTAS_s$price,
  short = c(3, 5, 8, 10, 12, 15),
  long = c(30, 35, 40, 45, 50, 60),
)

BVSP_s$GMMA <- GMMA(
  BVSP_s$price,
  short = c(3, 5, 8, 10, 12, 15),
  long = c(30, 35, 40, 45, 50, 60),
)

# Parabolic Stop-and-Reverse
FTAS_s$sar<- SAR(FTAS_s[,c("high","low")], accel = c(0.02, 0.2))
BVSP_s$sar<- SAR(BVSP_s[,c("high","low")], accel = c(0.02, 0.2))

# Signal to Noise Ratio
FTAS_s$snr<- SNR(FTAS_s[,c("high","low", "close")], n=5)
BVSP_s$snr<- SNR(BVSP_s[,c("high","low", "close")], n=5)

# MACD Oscillator
FTAS_s$macd  <- MACD( FTAS_s$price, 12, 26, 9, maType="EMA" )
BVSP_s$macd  <- MACD( BVSP_s$price, 12, 26, 9, maType="EMA" )

# Vertical Horizontal Filter
FTAS_s$vhf <- VHF(FTAS_s$price, n = 28)
BVSP_s$vhf <- VHF(BVSP_s$price, n = 28)

# Zig Zag
FTAS_s$zigzag <- ZigZag(FTAS_s[,c("high","low")], change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE)
BVSP_s$zigzag <- ZigZag(BVSP_s[,c("high","low")], change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE)

# Pbands
FTAS_s$pbands <- PBands(FTAS_s$price)
BVSP_s$pbands <- PBands(BVSP_s$price)

# Lags
FTAS_s$lag <- lag(FTAS_s$price)
BVSP_s$lag <- lag(BVSP_s$price)

# Chaikin Volatility
FTAS_s$chvol <- chaikinVolatility(FTAS_s[,c("high","low")], n = 5)
BVSP_s$chvol <- chaikinVolatility(BVSP_s[,c("high","low")], n = 5)

# Disparity 5
FTAS_s$sma2 <- SMA(FTAS_s$price,n=2)
FTAS_s$disparityfive <- (FTAS_s$price- FTAS_s$sma2)/ (FTAS_s$sma2 *100)

BVSP_s$sma2 <- SMA(BVSP_s$price,n=2)
BVSP_s$disparityfive <- (BVSP_s$price- BVSP_s$sma2)/ (BVSP_s$sma2 *100)

# On Balance Volume (OBV)
FTAS_s$OBV <- OBV(FTAS_s$close, FTAS_s$logvolume)
BVSP_s$OBV <- OBV(BVSP_s$close, BVSP_s$logvolume)

# Money Flow Index
FTAS_s$MFI <- MFI(FTAS_s[,c("high","low", "close")], FTAS_s$logvolume, n=14)
BVSP_s$MFI <- MFI(BVSP_s[,c("high","low", "close")], BVSP_s$logvolume, n=14)

# Simple moving average
FTAS_s$sma5 <- SMA(FTAS_s$price, n=5)
FTAS_s$sma10 <- SMA(FTAS_s$price, n=10)
FTAS_s$sma20 <- SMA(FTAS_s$price, n=20)

BVSP_s$sma5 <- SMA(BVSP_s$price, n=5)
BVSP_s$sma10 <- SMA(BVSP_s$price, n=10)
BVSP_s$sma20 <- SMA(BVSP_s$price, n=20)

# Volume moving average
FTAS_s$volsma2 <- SMA(FTAS_s$logvolume,n=2)
FTAS_s$volsma5 <- SMA(FTAS_s$logvolume,n=5)
FTAS_s$volsma10 <- SMA(FTAS_s$logvolume,n=10)
FTAS_s$volsma10 <- SMA(FTAS_s$logvolume,n=20)

BVSP_s$volsma2 <- SMA(BVSP_s$logvolume,n=2)
BVSP_s$volsma5 <- SMA(BVSP_s$logvolume,n=5)
BVSP_s$volsma10 <- SMA(BVSP_s$logvolume,n=10)
BVSP_s$volsma10 <- SMA(BVSP_s$logvolume,n=20)

# Exponential moving average 
FTAS_s$ema2 <- EMA(FTAS_s$price,n=2)
FTAS_s$ema7 <- EMA(FTAS_s$price,n=7)
FTAS_s$ema14 <- EMA(FTAS_s$price,n=14)
FTAS_s$ema21 <- EMA(FTAS_s$price,n=21)

BVSP_s$ema2 <- EMA(BVSP_s$price,n=2)
BVSP_s$ema7 <- EMA(BVSP_s$price,n=7)
BVSP_s$ema14 <- EMA(BVSP_s$price,n=14)
BVSP_s$ema21 <- EMA(BVSP_s$price,n=21)

#Arms' Ease of Movement Value
FTAS_s$EMV <- EMV(FTAS_s[, c("high", "low")], FTAS_s$logvolume, n=14)
BVSP_s$EMV <- EMV(BVSP_s[, c("high", "low")], BVSP_s$logvolume, n=14)

#William's %R
FTAS_s$WPR <- WPR(FTAS_s[,c("high","low", "close")], n=14)
BVSP_s$WPR <- WPR(BVSP_s[,c("high","low", "close")], n=14)

# Keltner Channels
FTAS_s$keltnerChannels <- keltnerChannels(FTAS_s[,c("high","low", "close")], n=20)
BVSP_s$keltnerChannels <- keltnerChannels(BVSP_s[,c("high","low", "close")], n=20)

# Percent Rank over a Moving Window
FTAS_s$runPercentRank <- runPercentRank(FTAS_s$price, n = 14)
BVSP_s$runPercentRank <- runPercentRank(BVSP_s$price, n = 14)

# Compute Stochastic Oscillator
stoch_FTAS <- stoch(FTAS_s[, c("high", "low", "close")])
FTAS_s$stoch_k <- stoch_FTAS[, 1]
FTAS_s$stoch_d <- stoch_FTAS[, 2]

stoch_BVSP <- stoch(BVSP_s[, c("high", "low", "close")])
BVSP_s$stoch_k <- BVSP_s[, 1]
BVSP_s$stoch_d <- BVSP_s[, 2]

# Volatility
FTAS_s$volatility <- volatility(FTAS_s$price, n = 14)
BVSP_s$volatility <- volatility(BVSP_s$price, n = 14)

# Rate of Change (ROC)
FTAS_s$ROC <- ROC(FTAS_s$price)
BVSP_s$ROC <- ROC(BVSP_s$price)

# Momentum
FTAS_s$M2 <- momentum(FTAS_s$price, n=2)
FTAS_s$M7 <- momentum(FTAS_s$price, n=7)
FTAS_s$M14 = momentum(FTAS_s$price, n=14)
FTAS_s$M21 <-momentum(FTAS_s$price, n=21)

BVSP_s$M2 <- momentum(BVSP_s$price, n=2)
BVSP_s$M7 <- momentum(BVSP_s$price, n=7)
BVSP_s$M14 = momentum(BVSP_s$price, n=14)
BVSP_s$M21 <-momentum(BVSP_s$price, n=21)

#Brookman's genius indicator
FTAS_s$brookman <- (FTAS_s$intraday * FTAS_s$logvolume * FTAS_s$price) / exp(11)
BVSP_s$brookman <- (BVSP_s$intraday * BVSP_s$logvolume * BVSP_s$price) / exp(11)

## Combine technical and external indicators

FTAS_s_combined <- merge(FTAS_s, external_indicators, by = "date", all = TRUE)
glimpse(FTAS_s_combined)

BVSP_s_combined <- merge(BVSP_s, external_indicators, by = "date", all = TRUE)
glimpse(BVSP_s_combined)



# end time
finish <- Sys.time()
finish - start


### STILL TO DO: Prediction models ------------------------------------------------------------


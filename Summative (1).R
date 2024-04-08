setwd("C:/Users/jess-/OneDrive/Documents/MSc Econ and Data science/Term 2/Big data for economics/Summative assessment/Summative")

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

BVSP <- BVSP_data %>% 
    select(ref_date, price_open, price_close, price_high, price_low, price_adjusted, volume,ret_adjusted_prices)
BVSP$volume= log(BVSP$volume)
colnames(BVSP) =c("date", "open"  , "close" , "high", "low", "price", "logvolume", "return") 
range(BVSP$date)

FTAS <- FTAS_data %>%   
  select(ref_date, price_open, price_close, price_high, price_low, price_adjusted, volume,ret_adjusted_prices)
FTAS$volume= log(FTAS$volume)
colnames(FTAS) =c("date", "open"  , "close" , "high", "low", "price", "logvolume", "return") 
head(FTAS)
range(FTAS$date)


#compute direction variable based on daily returns

BVSP$direction <- as.factor(ifelse(BVSP$return<0,"DOWN","UP"))
FTAS$direction <- as.factor(ifelse(FTAS$return<0,"DOWN","UP"))

# next day's direction
BVSP$direction.1 <- dplyr::lead(BVSP$direction,1,order_by=BVSP$date)
FTAS$direction.1 <- dplyr::lead(FTAS$direction,1,order_by=FTAS$date)

#compute a different direction variable based on intraday returns
BVSP$intraday <- log(BVSP$close) - log(BVSP$open) 
FTAS$intraday <- log(FTAS$close) - log(FTAS$open)

BVSP$dirintra<- as.factor(ifelse(BVSP$intraday < -0.0075,"LARGE DOWN", 
                                   ifelse(BVSP$intraday > -0.0075 & BVSP$intraday <=0, "SMALL DOWN", 
                                          ifelse(BVSP$intraday > 0 & BVSP$intraday <0.0075, "SMALL UP","LARGE UP"))))

FTAS$dirintra<- as.factor(ifelse(FTAS$intraday < -0.0075,"LARGE DOWN", 
                                   ifelse(FTAS$intraday > -0.0075 & FTAS$intraday <=0, "SMALL DOWN", 
                                          ifelse(FTAS$intraday > 0 & FTAS$intraday <0.0075, "SMALL UP","LARGE UP"))))

BVSP$direction.2 <-lead(BVSP$dirintra,1,order_by=BVSP$date)
FTAS$direction.2 <-lead(FTAS$dirintra,1,order_by=FTAS$date)


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
BVSP$DVI2 <- DVI(BVSP$price,n=2)
BVSP$DVI7 <- DVI(BVSP$price,n=7)
BVSP$DVI14 <- DVI(BVSP$price,n=14)
BVSP$DVI21 <- DVI(BVSP$price,n=21)

FTAS$DVI2 <- DVI(FTAS$price,n=2)
FTAS$DVI7 <- DVI(FTAS$price,n=7)
FTAS$DVI14 <- DVI(FTAS$price,n=14)
FTAS$DVI21 <- DVI(FTAS$price,n=21)

# Ultimate Oscillator
BVSP$UltOsc <- ultimateOscillator(BVSP[,c("high", "low", "close")])
FTAS$UltOsc <- ultimateOscillator(FTAS[,c("high", "low", "close")])

# Triple Smoothed Exponential Oscillator (TRIX)
BVSP$TRIX2 <- TRIX(BVSP$price,n=2)
BVSP$TRIX5 <- TRIX(BVSP$price,n=5)
BVSP$TRIX10 <- TRIX(BVSP$price,n=10)

FTAS$TRIX2 <- TRIX(FTAS$price,n=2)
FTAS$TRIX5 <- TRIX(FTAS$price,n=5)
FTAS$TRIX10 <- TRIX(FTAS$price,n=10)


# runSum (runFunctions)
BVSP$runSum <- runSum(BVSP$price,n=10, cumulative=FALSE)
BVSP$runMin <- runMin(BVSP$price,n=10, cumulative=FALSE)
BVSP$runMax <- runMax(BVSP$price,n=10, cumulative=FALSE)
BVSP$runMedian <- runMedian(BVSP$price,n=10, cumulative=FALSE)
BVSP$runVar <- runVar(BVSP$price, y=NULL, n=10, cumulative=FALSE)
BVSP$runSD <- runSD(BVSP$price,n=10, cumulative=FALSE)
BVSP$runMAD <- runMAD(BVSP$price, n=10, center=NULL, 
                        stat="median", constant=1.4826,
                        non.unique="mean", cumulative=FALSE)
BVSP$wilderSum <- wilderSum(BVSP$price,n=10)

FTAS$runSum <- runSum(FTAS$price,n=10, cumulative=FALSE)
FTAS$runMin <- runMin(FTAS$price,n=10, cumulative=FALSE)
FTAS$runMax <- runMax(FTAS$price,n=10, cumulative=FALSE)
FTAS$runMedian <- runMedian(FTAS$price,n=10, cumulative=FALSE)
FTAS$runVar <- runVar(FTAS$price, y=NULL, n=10, cumulative=FALSE)
FTAS$runSD <- runSD(FTAS$price,n=10, cumulative=FALSE)
FTAS$runMAD <- runMAD(FTAS$price, n=10, center=NULL, 
                        stat="median", constant=1.4826,
                        non.unique="mean", cumulative=FALSE)
FTAS$wilderSum <- wilderSum(FTAS$price,n=10)

# Bollinger Bands
BVSP$BBands2 <- BBands(BVSP$price, n=2)
BVSP$BBands5 <- BBands(BVSP$price, n=5)
BVSP$BBands10 <- BBands(BVSP$price, n=10)

FTAS$BBands2 <- BBands(FTAS$price, n=2)
FTAS$BBands5 <- BBands(FTAS$price, n=5)
FTAS$BBands10 <- BBands(FTAS$price, n=10)

# Aroon indicator
BVSP$aroon2 <- aroon(BVSP[,c("high", "low")], n=2)
BVSP$aroon5 <- aroon(BVSP[,c("high", "low")], n=5)
BVSP$aroon10 <- aroon(BVSP[,c("high", "low")], n=10)

FTAS$aroon2 <- aroon(FTAS[,c("high", "low")], n=2)
FTAS$aroon5 <- aroon(FTAS[,c("high", "low")], n=5)
FTAS$aroon10 <- aroon(FTAS[,c("high", "low")], n=10)

# Williams Accumulation/Distribution
BVSP$WilliamsAD <- williamsAD(BVSP[,c("high", "low", "close")])
FTAS$WilliamsAD <- williamsAD(FTAS[,c("high", "low", "close")])

# Trend Detection Index
BVSP$tdi10 <- TDI(BVSP$price, n=10)
BVSP$tdi20 <- TDI(BVSP$price, n=20)
BVSP$tdi30 <- TDI(BVSP$price, n=30)

FTAS$tdi10 <- TDI(FTAS$price, n=10)
FTAS$tdi20 <- TDI(FTAS$price, n=20)
FTAS$tdi30 <- TDI(FTAS$price, n=30)

# Chaikin Accumulation/ Distribution

BVSP$ChaikinAD <- chaikinAD(BVSP[,c("high", "low", "close")], BVSP[,"logvolume"])
FTAS$ChaikinAD <- chaikinAD(FTAS[,c("high", "low", "close")], FTAS[,"logvolume"])


# Know Sure Thing (KST) 
#For each day (week, month, etc.), the KST calculates the ROC
#over several periods.
#Those ROCs are smoothed using the given moving averages,
#then multiplied by their respective weighting values.
#The resulting values are summed for each day (month, week, etc.).
FTAS$kst <- KST(FTAS$price, roc_periods = c(10, 15, 20, 30),
                   sma_periods = c(10, 10, 10, 15),
                   ema_periods = c(10, 10, 10, 10),
                   signal_periods = 9)

BVSP$kst <- KST(BVSP$price, roc_periods = c(10, 15, 20, 30),
                sma_periods = c(10, 10, 10, 15),
                ema_periods = c(10, 10, 10, 10),
                signal_periods = 9)


#  Chande Momentum Oscillator (CMO)
#The CMO divides the total movement by the net movement
#([up - down] / [up + down]),
#where RSI divides the upward movement by the net movement (up / [up + down]).

FTAS$cmo <- CMO(FTAS$price, n = 14)

BVSP$cmo <- CMO(BVSP$price, n = 14)


#Chaikin Money Flow
FTAS$cmf <- CMF(FTAS[, c("high", "low", "close")],
                  FTAS[, c("logvolume")])

BVSP$cmf <- CMF(BVSP[, c("high", "low", "close")],
                  BVSP[, c("logvolume")])


# Donchian Channel

# The top line is the highest high of the past n periods.
# The bottom line is the lowest low of the past n periods.
# The middle line is the average of the top and bottom lines.

FTAS$donchianchannel20 <- DonchianChannel(FTAS[, c("high",
                                                         "low")],
                                             n = 20)
FTAS$donchianchannel10 <- DonchianChannel(FTAS[, c("high",
                                                         "low")],
                                             n = 10)


BVSP$donchianchannel20 <- DonchianChannel(BVSP[, c("high",
                                                   "low")],
                                          n = 20)
BVSP$donchianchannel10 <- DonchianChannel(BVSP[, c("high",
                                                   "low")],
                                          n = 10)


# Ehlers Correlation trend Indicator

#Ehler's Correlation Trend Indicator (CTI) measures the Spearman
#correlation of the
#price with the ideal trend line: a straight line with increasing slope

FTAS$EHcorrel <- CTI(FTAS$price, n = 10, slope = 1)

BVSP$EHcorrel <- CTI(BVSP$price, n = 10, slope = 1)

#True range
FTAS$tr <- TR(FTAS[,c("high","low", "close")])
BVSP$tr <- TR(BVSP[,c("high","low", "close")])


#Directional Movement Index
FTAS$dx <- ADX(FTAS[,c("high","low", "close")], n = 14)
BVSP$dx <- ADX(BVSP[,c("high","low", "close")], n = 14)


#Relative Strength index
FTAS$rsi3 <- RSI(FTAS$intraday, n = 3)
FTAS$rsi5 <- RSI(FTAS$intraday, n = 5)
FTAS$rsi10 <- RSI(FTAS$intraday, n = 10)
FTAS$rsi14 <- RSI(FTAS$intraday, n = 14)

BVSP$rsi3 <- RSI(BVSP$intraday, n = 3)
BVSP$rsi5 <- RSI(BVSP$intraday, n = 5)
BVSP$rsi10 <- RSI(BVSP$intraday, n = 10)
BVSP$rsi14 <- RSI(BVSP$intraday, n = 14)


#Commodity Channel Index

#CCI relates the current price and the average of price over n periods. 
#The CCI usually falls in a channel of -100 to 100.
#A basic CCI trading system is:
#Buy (sell) if CCI rises above 100 (falls below -100)
#and sell (buy) when it falls below 100 (rises above -100).

FTAS$cci3 <- CCI(FTAS[,c("high","low", "close")],n = 3)
FTAS$cci5 <- CCI(FTAS[,c("high","low", "close")],n = 5)

BVSP$cci3 <- CCI(BVSP[,c("high","low", "close")],n = 3)
BVSP$cci5 <- CCI(BVSP[,c("high","low", "close")],n = 5)

#Close location value

#The CLV will fall in a range of -1 to +1. If the CLV is +/-1,
#the close is at the high/low; if the CLV is 0, the close is
#directly between the high and low.

FTAS$clv <- CLV(FTAS[,c("high","low", "close")])

BVSP$clv <- CLV(BVSP[,c("high","low", "close")])

# Guppy Multiple Moving Averages
FTAS$GMMA <- GMMA(
  FTAS$price,
  short = c(3, 5, 8, 10, 12, 15),
  long = c(30, 35, 40, 45, 50, 60),
)

BVSP$GMMA <- GMMA(
  BVSP$price,
  short = c(3, 5, 8, 10, 12, 15),
  long = c(30, 35, 40, 45, 50, 60),
)

# Parabolic Stop-and-Reverse
FTAS$sar<- SAR(FTAS[,c("high","low")], accel = c(0.02, 0.2))
BVSP$sar<- SAR(BVSP[,c("high","low")], accel = c(0.02, 0.2))

# Signal to Noise Ratio
FTAS$snr<- SNR(FTAS[,c("high","low", "close")], n=5)
BVSP$snr<- SNR(BVSP[,c("high","low", "close")], n=5)

# MACD Oscillator
FTAS$macd  <- MACD( FTAS$price, 12, 26, 9, maType="EMA" )
BVSP$macd  <- MACD( BVSP$price, 12, 26, 9, maType="EMA" )

# Vertical Horizontal Filter
FTAS$vhf <- VHF(FTAS$price, n = 28)
BVSP$vhf <- VHF(BVSP$price, n = 28)

# Zig Zag
FTAS$zigzag <- ZigZag(FTAS[,c("high","low")], change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE)
BVSP$zigzag <- ZigZag(BVSP[,c("high","low")], change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE)

# Pbands
FTAS$pbands <- PBands(FTAS$price)
BVSP$pbands <- PBands(BVSP$price)

# Lags
FTAS$lag <- lag(FTAS$price)
BVSP$lag <- lag(BVSP$price)

# Chaikin Volatility
FTAS$chvol <- chaikinVolatility(FTAS[,c("high","low")], n = 5)
BVSP$chvol <- chaikinVolatility(BVSP[,c("high","low")], n = 5)

# Disparity 5
FTAS$sma2 <- SMA(FTAS$price,n=2)
FTAS$disparityfive <- (FTAS$price- FTAS$sma2)/ (FTAS$sma2 *100)

BVSP$sma2 <- SMA(BVSP$price,n=2)
BVSP$disparityfive <- (BVSP$price- BVSP$sma2)/ (BVSP$sma2 *100)

# On Balance Volume (OBV)
FTAS$OBV <- OBV(FTAS$close, FTAS$logvolume)
BVSP$OBV <- OBV(BVSP$close, BVSP$logvolume)

# Money Flow Index
FTAS$MFI <- MFI(FTAS[,c("high","low", "close")], FTAS$logvolume, n=14)
BVSP$MFI <- MFI(BVSP[,c("high","low", "close")], BVSP$logvolume, n=14)

# Simple moving average
FTAS$sma5 <- SMA(FTAS$price, n=5)
FTAS$sma10 <- SMA(FTAS$price, n=10)
FTAS$sma20 <- SMA(FTAS$price, n=20)

BVSP$sma5 <- SMA(BVSP$price, n=5)
BVSP$sma10 <- SMA(BVSP$price, n=10)
BVSP$sma20 <- SMA(BVSP$price, n=20)

# Volume moving average
FTAS$volsma2 <- SMA(FTAS$logvolume,n=2)
FTAS$volsma5 <- SMA(FTAS$logvolume,n=5)
FTAS$volsma10 <- SMA(FTAS$logvolume,n=10)
FTAS$volsma10 <- SMA(FTAS$logvolume,n=20)

BVSP$volsma2 <- SMA(BVSP$logvolume,n=2)
BVSP$volsma5 <- SMA(BVSP$logvolume,n=5)
BVSP$volsma10 <- SMA(BVSP$logvolume,n=10)
BVSP$volsma10 <- SMA(BVSP$logvolume,n=20)

# Exponential moving average 
FTAS$ema2 <- EMA(FTAS$price,n=2)
FTAS$ema7 <- EMA(FTAS$price,n=7)
FTAS$ema14 <- EMA(FTAS$price,n=14)
FTAS$ema21 <- EMA(FTAS$price,n=21)

BVSP$ema2 <- EMA(BVSP$price,n=2)
BVSP$ema7 <- EMA(BVSP$price,n=7)
BVSP$ema14 <- EMA(BVSP$price,n=14)
BVSP$ema21 <- EMA(BVSP$price,n=21)

#Arms' Ease of Movement Value
FTAS$EMV <- EMV(FTAS[, c("high", "low")], FTAS$logvolume, n=14)
BVSP$EMV <- EMV(BVSP[, c("high", "low")], BVSP$logvolume, n=14)

#William's %R
FTAS$WPR <- WPR(FTAS[,c("high","low", "close")], n=14)
BVSP$WPR <- WPR(BVSP[,c("high","low", "close")], n=14)

# Keltner Channels
FTAS$keltnerChannels <- keltnerChannels(FTAS[,c("high","low", "close")], n=20)
BVSP$keltnerChannels <- keltnerChannels(BVSP[,c("high","low", "close")], n=20)

# Percent Rank over a Moving Window
FTAS$runPercentRank <- runPercentRank(FTAS$price, n = 14)
BVSP$runPercentRank <- runPercentRank(BVSP$price, n = 14)

# Compute Stochastic Oscillator
stoch_FTAS <- stoch(FTAS[, c("high", "low", "close")])
FTAS$stoch_k <- stoch_FTAS[, 1]
FTAS$stoch_d <- stoch_FTAS[, 2]

stoch_BVSP <- stoch(BVSP[, c("high", "low", "close")])
BVSP$stoch_k <- BVSP[, 1]
BVSP$stoch_d <- BVSP[, 2]

# Volatility
FTAS$volatility <- volatility(FTAS$price, n = 14)
BVSP$volatility <- volatility(BVSP$price, n = 14)

# Rate of Change (ROC)
FTAS$ROC <- ROC(FTAS$price)
BVSP$ROC <- ROC(BVSP$price)

# Momentum
FTAS$M2 <- momentum(FTAS$price, n=2)
FTAS$M7 <- momentum(FTAS$price, n=7)
FTAS$M14 = momentum(FTAS$price, n=14)
FTAS$M21 <-momentum(FTAS$price, n=21)

BVSP$M2 <- momentum(BVSP$price, n=2)
BVSP$M7 <- momentum(BVSP$price, n=7)
BVSP$M14 = momentum(BVSP$price, n=14)
BVSP$M21 <-momentum(BVSP$price, n=21)

#Brookman's genius indicator
FTAS$brookman <- (FTAS$intraday * FTAS$logvolume * FTAS$price) / exp(11)
BVSP$brookman <- (BVSP$intraday * BVSP$logvolume * BVSP$price) / exp(11)

## Combine technical and external indicators

FTAS_combined <- merge(FTAS, external_indicators, by = "date", all.x = TRUE)
glimpse(FTAS_combined)

BVSP_combined <- merge(BVSP, external_indicators, by = "date", all.x = TRUE)
glimpse(BVSP_combined)


# end time
finish <- Sys.time()
finish - start


### STILL TO DO: Prediction models ------------------------------------------------------------


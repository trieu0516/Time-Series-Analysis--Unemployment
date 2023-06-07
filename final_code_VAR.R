### SETTING UP THE DATA ###

#setting the directory for our analysis and checking the directory's contents
setwd('C:/Users/trieu/Desktop/R codes and data')
dir()

#installing packages 
library(readxl)
library(TSstudio)
library('urca')
library(tseries)
library(tis)
library(forecast)
library(tidyverse)
library(vars)

#importing our data file into R
data = read_xlsx('final_data.xlsx')
head(data)
tail(data)

#assigning names to the variables in our data set 

"""
The unmeployment rate variables are name-coded as follows: 

    rt    =   wholesale and retail trade
    af    =   accomodation and food services
    
    unem  =   unemployment rate
    
    young =   aged 15 to 25
    mid   =   aged 25 to 54
    old   =   aged 55 and over
    
    for example: af_unem_young corresponds to: 'unemployment rate of people aged 15 to 24 
                                                working in the accomodation and retail sector'
"""
    
rt_unem_young = ts(as.numeric(as.matrix(data$rt_unem_young)), start = c(1981), frequency = 1)
rt_unem_mid = ts(as.numeric(as.matrix(data$rt_unem_mid)), start = c(1981), frequency = 1)
rt_unem_old = ts(as.numeric(as.matrix(data$rt_unem_old)), start = c(1981), frequency = 1)

af_unem_young = ts(as.numeric(as.matrix(data$af_unem_young)), start = c(1981), frequency = 1)
af_unem_mid = ts(as.numeric(as.matrix(data$af_unem_mid)), start = c(1981), frequency = 1)
af_unem_old = ts(as.numeric(as.matrix(data$af_unem_old)), start = c(1981), frequency = 1)

min_wage = ts(as.numeric(as.matrix(data$min_wage)), start = c(1981), frequency = 1)

gdp = ts(as.matrix(data$gdp), start = c(1981), frequency = 1)

cpi = ts(as.matrix(data$cpi), start = c(1981), frequency = 1)

Date = as.matrix(data$Date)

# Transforming variables into log(variables)
log_gdp = log(gdp)
log_cpi = log(cpi)
log_min_wage = log(min_wage)
log_rt_unem_young = log(rt_unem_young)
log_rt_unem_mid = log(rt_unem_mid)
log_af_unem_young = log(af_unem_young)
log_af_unem_mid = log(af_unem_mid)

### STATIONARITY TESTING ###

# plotting the data to assess whether to include a trend or an intercept in the tests
ts_plot(log_gdp, title = "Ontario GDP 1981-2022", Xtitle = "Time", Ytitle = "log(millions of canadian dollars)", slider = TRUE)
ts_plot(log_cpi, title = "Ontario CPI 1981-2022", Xtitle = "Time", Ytitle = "log(CPI)", slider = TRUE)
ts_plot(log_min_wage, title = "Ontario Minimum Wage 1981-2022", Xtitle = "Time", Ytitle = "log(canadian dollars", slider = TRUE)

ts_plot(log_rt_unem_young, title = "Unemployment for People Aged 15-24 in the Wholesale and Retail Sector 1981-2022", Xtitle = "Time", Ytitle = "Thousands of people", slider = TRUE)
ts_plot(log_rt_unem_mid, title = "Unemployment for People Aged 25-54 in the Wholesale and Retail Sector 1981-2022", Xtitle = "Time", Ytitle = "Thousands of people", slider = TRUE)
ts_plot(log_af_unem_young, title = "Unemployment for People Aged 15-24 in the Accomodation and Food Services Sector 1981-2022", Xtitle = "Time", Ytitle = "Thousands of people", slider = TRUE)
ts_plot(log_af_unem_mid, title = "Unemployment for People Aged 25-54 in the Accomodation and Food Services Sector 1981-2022", Xtitle = "Time", Ytitle = "Thousands of people", slider = TRUE)


# Advanced Dickey-Fuller tests - H0 = unit root
#ADF tests with intercept
ADF_drift_1 = ur.df(log_rt_unem_young, type="drift", selectlags = "AIC")
summary(ADF_drift_1)

ADF_drift_2 = ur.df(log_rt_unem_mid, type="drift", selectlags = "AIC")
summary(ADF_drift_2)

ADF_drift_3 = ur.df(log_af_unem_young, type="drift", selectlags = "AIC")
summary(ADF_drift_3)

ADF_drift_4 = ur.df(log_af_unem_mid, type="drift", selectlags = "AIC")
summary(ADF_drift_4)

ADF_drift_5 = ur.df(log_gdp, type="drift", selectlags = "AIC")
summary(ADF_drift_5)

ADF_drift_6 = ur.df(log_cpi, type="drift", selectlags = "AIC")
summary(ADF_drift_6)

ADF_drift_7 = ur.df(log_min_wage, type="drift", selectlags = "AIC")
summary(ADF_drift_7)

# ADF tests with trend and intercept
ADF_trend_1 = ur.df(log_rt_unem_young, type="trend", selectlags = "AIC")
summary(ADF_trend_1)

ADF_trend_2 = ur.df(log_rt_unem_mid, type="trend", selectlags = "AIC")
summary(ADF_trend_2)

ADF_trend_3 = ur.df(log_af_unem_young, type="trend", selectlags = "AIC")
summary(ADF_trend_3)

ADF_trend_4 = ur.df(log_af_unem_mid, type="trend", selectlags = "AIC")
summary(ADF_trend_4)

ADF_trend_5 = ur.df(log_gdp, type="trend", selectlags = "AIC")
summary(ADF_trend_5)

ADF_trend_6 = ur.df(log_cpi, type="trend", selectlags = "AIC")
summary(ADF_trend_6)

ADF_trend_7 = ur.df(log_min_wage, type="trend", selectlags = "AIC")
summary(ADF_trend_7)

# KPSS tests - H0 = stationarity
# KPSS with intercept
KPSS_drift_1 = ur.kpss(log_rt_unem_young, type="mu", lags = "long")
summary(KPSS_drift_1)

KPSS_drift_2 = ur.kpss(log_rt_unem_mid, type="mu", lags = "long")
summary(KPSS_drift_2)

KPSS_drift_3 = ur.kpss(log_af_unem_young, type="mu", lags = "long")
summary(KPSS_drift_3)

KPSS_drift_4 = ur.kpss(log_af_unem_mid, type="mu", lags = "long")
summary(KPSS_drift_4)

KPSS_drift_5 = ur.kpss(log_gdp, type="mu", lags = "long")
summary(KPSS_drift_5)

KPSS_drift_6 = ur.kpss(log_cpi, type="mu", lags = "long")
summary(KPSS_drift_6)

KPSS_drift_7 = ur.kpss(log_min_wage, type="mu", lags = "long")
summary(KPSS_drift_7)

# KPSS with trend and intercept
KPSS_trend_1 = ur.kpss(log_rt_unem_young, type="tau", lags = "long")
summary(KPSS_trend_1)

KPSS_trend_2 = ur.kpss(log_rt_unem_mid, type="tau", lags = "long")
summary(KPSS_trend_2)

KPSS_trend_3 = ur.kpss(log_af_unem_young, type="tau", lags = "long")
summary(KPSS_trend_3)

KPSS_trend_4 = ur.kpss(log_af_unem_mid, type="tau", lags = "long")
summary(KPSS_trend_4)

KPSS_trend_5 = ur.kpss(log_gdp, type="tau", lags = "long")
summary(KPSS_trend_5)

KPSS_trend_6 = ur.kpss(log_cpi, type="tau", lags = "long")
summary(KPSS_trend_6)

KPSS_trend_7 = ur.kpss(log_min_wage, type="tau", lags = "long")
summary(KPSS_trend_7)

### Transforming the variables into difference logs 
deltalog_gdp = diff(log_gdp)
deltalog_cpi = diff(log_cpi)
deltalog_min_wage = diff(log_min_wage)

# no cointegration because Y is already stationary!!

### Fitting AR Models to the data
#   Checking ACF / PACF of the variables? 
par(mfrow=c(4,1))

acf_rt_unem_young = acf(rt_unem_young, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(acf_rt_unem_young, main = "Autocorrelation Function of log(rt_unem_young)")

pacf_rt_unem_young = pacf(rt_unem_young, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(pacf_rt_unem_young, main = " Partial Autocorrelation Function of log(rt_unem_young)")
     
acf_rt_unem_mid = acf(rt_unem_mid, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(acf_rt_unem_mid, main = "Autocorrelation Function of log(rt_unem_mid)")

pacf_rt_unem_mid = pacf(rt_unem_mid, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(pacf_rt_unem_mid, main = "Partial Autocorrelation Function of log(rt_unem_mid)")

acf_af_unem_young = acf(af_unem_young, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(acf_af_unem_young, main = "Autocorrelation Function of the log(af_unem_young)")

pacf_af_unem_young = pacf(af_unem_young, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(pacf_af_unem_young, main = "Partial Autocorrelation Function of the log(af_unem_young)")

acf_af_unem_mid = acf(af_unem_mid, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(acf_af_unem_mid, main = "Autocorrelation Function of log(af_unem_mid)")

pacf_af_unem_mid = pacf(af_unem_mid, lag.max = NULL, type = c("correlation"), plot = FALSE)
plot(pacf_af_unem_mid, main = "Partial Autocorrelation Function of log(af_unem_mid)")

### Finding the optimal lags ###

op_lag1 <- cbind( deltalog_min_wage[1:31], deltalog_cpi[1:31], deltalog_gdp[1:31], log_rt_unem_young [1:31])
colnames(op_lag1) <- c(  "deltalog_min_wage", "deltalog_cpi", "deltalog_gdp", "log_rt_unem_young")

op_lag2 <- cbind( deltalog_min_wage[1:31], deltalog_cpi[1:31], deltalog_gdp[1:31], log_rt_unem_mid [1:31])
colnames(op_lag2) <- c("deltalog_min_wage", "deltalog_cpi", "deltalog_gdp", "log_rt_unem_mid")

op_lag3 <- cbind( deltalog_min_wage[1:31], deltalog_cpi[1:31], deltalog_gdp[1:31], log_af_unem_young [1:31])
colnames(op_lag3) <- c( "deltalog_min_wage", "deltalog_cpi", "deltalog_gdp", "log_af_unem_young")

op_lag4 <- cbind(deltalog_min_wage[1:31], deltalog_cpi[1:31], deltalog_gdp[1:31], log_af_unem_mid [1:31])
colnames(op_lag4) <- c("deltalog_min_wage", "deltalog_cpi", "deltalog_gdp", "log_af_unem_mid")

lagselect1 <- VARselect(op_lag1, lag.max = 9, type = 'const')
lagselect1  ## lag 4 ##

lagselect2 <- VARselect(op_lag2, lag.max = 9, type = 'const')
lagselect2 ## lag 5 ##

lagselect3 <- VARselect(op_lag3, lag.max = 9, type = 'const')
lagselect3 ## lag 5 ##

lagselect4 <- VARselect(op_lag4, lag.max = 9, type = 'const')
lagselect4  ## laf 5 ##

### Building VAR ###

rt_y_VAR4 <- VAR(op_lag1, p = 4, type = "const")
summary(rt_y_VAR4)

rt_m_VAR5 <- VAR(op_lag2, p = 5, type = "const")
summary(rt_m_VAR5)

af_y_VAR5 <- VAR(op_lag3, p = 5, type = "const")
summary(af_y_VAR5)

af_m_VAR5 <- VAR(op_lag4, p = 5, type = "const")
summary(af_m_VAR5)

### Diagnostic ###

# serial correlation #

rt_y_serl <- serial.test(rt_y_VAR4, lags.pt = 16, type = "PT.asymptotic")
rt_y_serl$serial

rt_m_serl <- serial.test(rt_m_VAR5, lags.pt = 16, type = "PT.asymptotic")
rt_m_serl$serial

af_y_serl <- serial.test(af_y_VAR5, lags.pt = 16, type = "PT.asymptotic")
af_y_serl$serial

af_m_serl <- serial.test(af_m_VAR5, lags.pt = 16, type = "PT.asymptotic")
af_m_serl$serial


### Granger Causality Test ###

grangertest(deltalog_min_wage[1:31] , log_rt_unem_young[1:31], order = 4)
grangertest(deltalog_gdp[1:31] , log_rt_unem_young[1:31], order = 4)
grangertest(deltalog_cpi[1:31], log_rt_unem_young[1:31], order = 4)
# gdp, min wage, cpi #

grangertest(deltalog_min_wage[1:31] , log_rt_unem_mid[1:31], order = 5)
grangertest(deltalog_gdp[1:31], log_rt_unem_mid[1:31], order = 5)
grangertest(deltalog_cpi[1:31] , log_rt_unem_mid[1:31], order = 5)
# gdp, cpi, min wage #

grangertest(deltalog_min_wage[1:31] , log_af_unem_young[1:31], order = 5)
grangertest(deltalog_gdp[1:31] , log_af_unem_young[1:31], order = 5)
grangertest(deltalog_cpi[1:31] , log_af_unem_young[1:31], order = 5)
# gdp, min wage, cpi #

grangertest(deltalog_min_wage[1:31] , log_af_unem_mid[1:31], order = 5)
grangertest(deltalog_gdp[1:31] , log_af_unem_mid[1:31], order = 5)
grangertest(deltalog_cpi[1:31] , log_af_unem_mid[1:31], order = 5)
# gdp, cpi, min wage #

### Impulse Response Analysis ###
par(mfrow=c(2,2))
# retail young #
rt_y_irf1<-irf(rt_y_VAR4, impulse = "log_rt_unem_young", response = c("log_rt_unem_young"), boot =TRUE)
plot(rt_y_irf1)

rt_y_irf2<-irf(rt_y_VAR4, impulse = "deltalog_gdp", response = c("log_rt_unem_young"), boot =TRUE)
plot(rt_y_irf2)

rt_y_irf3<-irf(rt_y_VAR4, impulse = "deltalog_cpi", response = c("log_rt_unem_young"), boot =TRUE)
plot(rt_y_irf3)

rt_y_irf4<-irf(rt_y_VAR4, impulse = "deltalog_min_wage", response = c( "log_rt_unem_young"), boot =TRUE)
plot(rt_y_irf4)

# retail mid #
rt_m_irf1<-irf(rt_m_VAR5, impulse = "log_rt_unem_mid", response = c("log_rt_unem_mid"), boot =TRUE)
plot(rt_m_irf1)

rt_m_irf2<-irf(rt_m_VAR5, impulse = "deltalog_gdp", response = c( "log_rt_unem_mid"), boot =TRUE)
plot(rt_m_irf2)

rt_m_irf3<-irf(rt_m_VAR5, impulse = "deltalog_cpi", response = c("log_rt_unem_mid"), boot =TRUE)
plot(rt_m_irf3)

rt_m_irf4<-irf(rt_m_VAR5, impulse = "deltalog_min_wage", response = c("log_rt_unem_mid"), boot =TRUE)
plot(rt_m_irf4)

# accomodation & food young #
af_y_irf1<-irf(af_y_VAR5, impulse = "log_af_unem_young", response = c("log_af_unem_young"), boot =TRUE)
plot(af_y_irf1)

af_y_irf2<-irf(af_y_VAR5, impulse = "deltalog_gdp", response = c("log_af_unem_young"), boot =TRUE)
plot(af_y_irf2)

af_y_irf3<-irf(af_y_VAR5, impulse = "deltalog_cpi", response = c("log_af_unem_young"), boot =TRUE)
plot(af_y_irf3)

af_y_irf4<-irf(af_y_VAR5, impulse = "deltalog_min_wage", response = c( "log_af_unem_young"), boot =TRUE)
plot(af_y_irf4)

# accomodation & food mid #
af_m_irf1<-irf(af_m_VAR5, impulse = "log_af_unem_mid", response = c("log_af_unem_mid"), boot =TRUE)
plot(af_m_irf1)

af_m_irf2<-irf(af_m_VAR5, impulse = "deltalog_gdp", response = c("log_af_unem_mid"), boot =TRUE)
plot(af_m_irf2)

af_m_irf3<-irf(af_m_VAR5, impulse = "deltalog_min_wage", response = c( "log_af_unem_mid"), boot =TRUE)
plot(af_m_irf3)

af_m_irf4<-irf(af_m_VAR5, impulse = "deltalog_cpi", response = c("log_af_unem_mid"), boot =TRUE)
plot(af_m_irf4)

### Forecast error variance decomposition ###

fevd1 <- fevd(rt_y_VAR4, n.ahead=10)
plot((fevd1), plot.type = "single")

fevd2 <- fevd(rt_m_VAR5, n.ahead=10)
plot((fevd2), plot.type = "single")

fevd3 <- fevd(af_y_VAR5, n.ahead=10)
plot((fevd3), plot.type = "single")

fevd4 <- fevd(af_m_VAR5, n.ahead=10)
plot((fevd4), plot.type = "single")

### Prediction ###

predict1 <- predict(rt_y_VAR4,n.ahead=10,ci=0.95)
predict1$fcst$log_rt_unem_young
fanchart((predict1),colors="red", plot.type = "single")

predict2 <- predict(rt_m_VAR5,n.ahead=10,ci=0.95)
predict2$fcst$log_rt_unem_mid
fanchart((predict2),colors="red", plot.type = "single")

predict3 <- predict(af_y_VAR5,n.ahead=10,ci=0.95)
predict3$fcst$log_af_unem_young
fanchart((predict3),colors="red", plot.type = "single")

predict4 <- predict(af_m_VAR5,n.ahead=10,ci=0.95)
predict4$fcst$log_af_unem_mid
fanchart((predict4),colors="red", plot.type = "single")

fc1 <- predict1$fcst$log_rt_unem_young[,1]
fc2 <- predict2$fcst$log_rt_unem_mid[,1]
fc3 <- predict3$fcst$log_af_unem_young[,1]
fc4 <- predict4$fcst$log_af_unem_mid[,1]

## MSE ##

MSE<-function(real,fc){
  MSE=1/length(real)*sum((real-fc)^2)
  return(MSE)
}

MSE_rt_y <- MSE(log_rt_unem_young[32:41], fc1)
MSE_rt_m <- MSE(log_rt_unem_mid[32:41], fc2)
MSE_af_y <- MSE(log_af_unem_young[32:41], fc3)
MSE_af_m <- MSE(log_af_unem_mid[32:41], fc4)

MSE <- cbind(MSE_rt_y, MSE_rt_m, MSE_af_y, MSE_af_m)
print(MSE)


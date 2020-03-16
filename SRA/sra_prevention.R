#library(readxl)
library(forecast)
library(plotly)
library(rio)
library(ggplot2)
library(mlr)
library(tsoutliers)
library(tseries)
library(fUnitRoots)
library(lmtest)
#library(xlsx)
library(lubridate)
source("read_functions.R")

path = ""

dati=read_basi_calls(path)
eventi=read_calendar(path)

prevention=timeseries$PREVENTION

#unisco gli eventi
tabella_prev=eventi[,c(5,6,10,11,12,17)]
ev_prevention=createDummyFeatures(tabella_prev, cols=c("RECAP","INCASSO","FESTIVITA'","DAZN","NERONE","OUTLIER_PREV"))
ev_prevention=ev_prevention[,c(2,4,6,8,10,12)]
ev_prevention=as.matrix(ev_prevention)
ev_prevention=ev_prevention[c(53:260),]
ev_prev_train=window(ev_prevention, start = 1, end = 166)
ev_prev_test=window(ev_prevention, start = 167, end = 173)


#identificazione tendenza, stagionalità e ciclicità della serie storica

preventionts<-ts(prevention, frequency = 52, start = decimal_date(ymd("2017-01-01")))
component.ts=decompose(preventionts)
plot(component.ts)

#serie aggiustata senza outlier
outliers_prev<-tso(preventionts, types = c("AO", "LS", "SLS"))
outliers_prev
plot(outliers_prev)
prev_adjusted<-outliers_prev$yadj

#Unit Root Test: stazionarietà
adfTest(preventionts)

#correlogrammi
acf(preventionts)
pacf(preventionts)

#togliere stagionalità 
timeseriesseasonallyadjusted <- preventionts- component.ts$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary)

#identificare p e q
acf(tsstationary)
pacf(tsstationary)
tsdisplay(tsstationary)

#arima_model
fitARIMAprev <- Arima(preventionts, order=c(4,0,0),xreg = ev_prev_train, seasonal = list(order = c(0,1,1), period = 52))
coeftest(fitARIMAprev)

#prevention_forecast
futurValprev<-forecast(fitARIMAprev, h = 7, xreg = ev_prev_test) 
plot(futurValprev)
futurValprev


accuracy(futurValprev)
fctprev=as.data.frame(futurValprev$mean)

#testo il modello con auto.arima
autoARIMAprev<-auto.arima(preventionts, seasonal = T, trace = T, xreg = ev_prev_train)
coeftest(autoARIMAprev)

#forecast
futurValprev_auto<-forecast(autoARIMAprev, h = 7, xreg = ev_prev_test) 
plot(futurValprev_auto)
futurValprev_auto

accuracy(futurValprev_auto)
fctprev=as.data.frame(futurValprev_auto$mean)
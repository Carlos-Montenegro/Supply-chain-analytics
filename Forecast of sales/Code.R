# Forecasting
# Exam Assignment
# Carlos Montenegro 


# Loading requiere packages
if(!require("base")) install.packages("base"); library("base")
if(!require("fpp2")) install.packages("fpp2"); library("fpp2")
if(!require("forecast")) install.packages("forecast"); library("forecast")
if(!require("portes")) install.packages("portes"); library("portes")
if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("lmtest")) install.packages("lmtest"); library("lmtest")
if(!require("stringr")) install.packages("stringr"); library("stringr")


# Set working directory
setwd("D:/User/Google Drive/Proyectos/Estudios/Maestría/Courses/21. Forecasting/Exam/Retake exam/My exam")


#########################################################################################################
##############################################  First Part ##############################################
#########################################################################################################

data <- read_excel("NikeSales.xlsx", sheet="Footwear")
data<- data[seq(dim(data)[1],1),]
footwear <- ts(rev(data[,2]), frequency = 4, start = c(2017,1))
foot_train <- window(turnover, end=c(2019,4))
foot_test <-window(turnover, start=c(2020,1))
plot(footwear, main="Quarterly Nike sales of Footwear in EMEA", ylab = "Millions of USD", xlab="Quarter", cex.main=0.9)

# Seasonal plot
seasonplot(footwear,year.labels=TRUE, year.labels.left=TRUE,
           main="Quarterly Nike sales of Footwear in EMEA", cex.main=0.9, 
           ylab="Millions of USD",col=rainbow(20), pch=19)

# Monthplot
monthplot(footwear,ylab="Millions of USD",xlab="Quarter", xaxt="n", main="Quarterly Nike sales of Footwear in EMEA",
          cex.main=0.9, type="l",phase=cycle(footwear))
axis(1, 1:4,c("q1","q2","q3","q4"))

# ACF
Acf(footwear, lag.max=36, plot=TRUE, main="ACF", cex.main=0.8, ylab="")

# Decompose
decompose_unm <- decompose(footwear)
plot(decompose_unm,cex.main=0.9, xlab="year")

#Summary
summary(data$Sales)

# BoxPlot
boxplot(Sales~str_sub(data$Quarter,-2,-1),data=data, xlab="Quarter")

# BoxCox Test
BoxCox.lambda(footwear)

## Forectasting
foot_train <- window(footwear, end=c(2019,4))
foot_test <-window(footwear, start=c(2020,1))
h<-length(foot_test)

# auto ARIMA
auto_arima <- auto.arima(foot_train, stepwise = FALSE, approximation = FALSE)
coeftest(auto_arima)
preds_Arima <- forecast(auto_arima,h=h)
plot(preds_Arima, cex.main=0.9, xlab="year")
lines(foot_test)

accuracy(preds_Arima,foot_test)

checkresiduals(preds_Arima)
res <- residuals(preds_Arima)
tsdisplay(res)
res <- na.omit(res)
LjungBox(res, lags=seq(1,11,1), order=length(auto_arima$coef))

# ETS
ETS_foot <- ets(foot_train)
predsETS<-forecast(ETS_foot, h=h)
plot(predsETS, cex.main=0.9, xlab="year")
lines(foot_test)
ETS_foot

accuracy(predsETS,foot_test)

checkresiduals(predsETS)
res <- residuals(predsETS)
tsdisplay(res)
res <- na.omit(res)
LjungBox(res, lags=seq(1,11,1), order=length(ETS_unm$par))

plot(ETS_foot)

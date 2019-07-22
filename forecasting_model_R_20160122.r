
getwd()

file_name=file.path(("C:/Users/barro/Dropbox/a2i2_flashfarma/Estudos"), "Historico_Categorias_e_lojas_desde_20120101.csv") 

file_name

df = read.table(file=file_name,header = TRUE, sep = ",", dec = ".")

# Dimensions of the dataframe
dim(df)

str(df)   

df$Data = as.Date(df$dataVenda, format = "%d-%m-%y")

head(df, n = 3)

tail(df, n=4)

# Attaches the data frame, so we don´t need to refer to its name and the column name all the time
attach(df)

df$Data[1:10]

str(df$Data)

library(xts)

df$Receita_Liq_Total =df$valor_Loja01 + df$valor_Loja02 + df$valor_Loja03

str(df)

df$Receita_Liq_Total = as.xts(df$Receita_Liq_Total, as.Date(df$Data, format = "%d-%m-%y"))
df$valor_Loja01 = as.xts(df$valor_Loja01, as.Date(df$Data, format = "%d-%m-%y"))
df$valor_Loja02 = as.xts(df$valor_Loja02, as.Date(df$Data, format = "%d-%m-%y"))
df$valor_Loja03 = as.xts(df$valor_Loja03, as.Date(df$Data, format = "%d-%m-%y"))

df$Receita_Liq_Total[1:5]

str(valor_Loja01)

str(df$valor_Loja01)

df$l01_Receita_Liq_Total=lag(df$Receita_Liq_Total,1)

df$l01_Receita_Liq_Total[1:8] 

df$l02_Receita_Liq_Total=lag(df$Receita_Liq_Total,2)

df$l02_Receita_Liq_Total[1:8] 

df$l03_Receita_Liq_Total=lag(df$Receita_Liq_Total,3)

df$l04_Receita_Liq_Total=lag(df$Receita_Liq_Total,4)

df$l05_Receita_Liq_Total=lag(df$Receita_Liq_Total,5)

df$l06_Receita_Liq_Total=lag(df$Receita_Liq_Total,6)

df$l07_Receita_Liq_Total=lag(df$Receita_Liq_Total,7)

df$l08_Receita_Liq_Total=lag(df$Receita_Liq_Total,8)

df$l09_Receita_Liq_Total=lag(df$Receita_Liq_Total,9)

df$l10_Receita_Liq_Total=lag(df$Receita_Liq_Total,10)

df$l11_Receita_Liq_Total=lag(df$Receita_Liq_Total,11)

df$l12_Receita_Liq_Total=lag(df$Receita_Liq_Total,12)

df$l13_Receita_Liq_Total=lag(df$Receita_Liq_Total,13)

df$l14_Receita_Liq_Total=lag(df$Receita_Liq_Total,14)

dim(df)

library(ggplot2)

ggplot(df, aes(Data, Receita_Liq_Total)) + geom_line(color='#000033') + ggtitle("Vendas Liquidas Totais Diarias");

ggplot(df, aes(Receita_Liq_Total))+ geom_histogram (color='navy', fill = '#001a33') + ggtitle("Histograma - Vendas Liquidas Totais Diarias")

acf(df$Receita_Liq_Total, na.action = na.pass,lag.max = 52)

pacf(df$Receita_Liq_Total, na.action = na.pass,lag.max = 52)

ggplot(df, aes(Data, valor_Loja01)) + geom_line(color='#b80000') + ggtitle("Vendas Liquidas Loja 01");

ggplot(df, aes(Data, valor_Loja02)) + geom_line(color='#300052') + ggtitle("Vendas Liquidas Loja 02");

ggplot(df, aes(Data, valor_Loja03)) + geom_line(color='#003b4d') + ggtitle("Vendas Liquidas Loja 03");

library("dynlm")

library("zoo")    # Another time series library

df_como_st = read.zoo(df, format = "%d-%m-%Y")

dfTozoo = function(tsdataframe, dateformat="%d-%m-%y"){

  library(zoo)

  framedates = as.Date(tsdataframe[,1], format=dateformat)
  n=ncol(tsdataframe)
  zoodata = zoo(tsdataframe[,2:n], order.by=framedates)

  return(zoodata)
}

df_como_st = dfTozoo(df)

str(df_como_st)

df_como_st$Receita_Liq_Total[1:10]

df_como_st$Receita_Liq_Total[1474:1484]



df_zoo = zoo(df,df$Data)

str(df_zoo)

rlt_serie = as.numeric(df$Receita_Liq_Total)
rlt.Date <- df$Data
rlt <- zoo(rlt_serie, rlt.Date)

str(rlt)



model <- dynlm(df_zoo$Receita_Liq_Total ~ L(df_zoo$Receita_Liq_Total, k = 1) + L(df_zoo$Receita_Liq_Total, k = 7))
               
               
model

#df_zoo
#++ df_zoo$Carnaval+ L(rlt, k = 14) + L(rlt, k = 21) + L(rlt, k = 28)) +  + Pascoa + Natal + Reveillon

# Fit dynamic linear model - DOES NOT FIT DUMMY VARIABLES! SEE RESULTS BELOW!
model <- dynlm(rlt ~ L(rlt, k = 1) + L(rlt, k = 7) + L(rlt, k = 14) + L(rlt, k = 21) + L(rlt, k = 28)) + Carnaval + Pascoa + Natal + Reveillon
model



library(data.table)
library(forecast)
library(plyr)

# Using the Forecast package - summary of the time series characteristics

# tsdisplay(x, plot.type=c("partial","scatter","spectrum"),
#    points=TRUE, ci.type=c("white", "ma"),
#    lag.max, na.action=na.contiguous,main=NULL, xlab="", ylab="", pch=1, cex=0.5, ...)


tsdisplay(df$Receita_Liq_Total, lag.max = 78, main = "Receita  Liq. Total")

# This script fits a linear model to a time series including trend and seasonality components
# We use tslm() and forecast() in the "forecast" R package 


# tslm function - time series wrapper (analogous to lm)
# USAGE: tslm(formula, data, lambda=NULL, ...)
#
# For example:
#
# fit <- tslm(Sales ~ trend + season + DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday)


ts_fit = function(x) {
  Sales <- ts(x$Sales, frequency = 365)
  DayOfWeek <- x$DayOfWeek
  Open <- x$Open
  Promo <- x$Promo
  StateHoliday <- x$StateHoliday
  SchoolHoliday <- x$SchoolHoliday
  fit <- tslm(Sales ~ trend + season + DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday)
  return(fit)
}

y = ts(df$Receita_Liq_Total, frequency = 7)

yhat = tslm(y ~ trend + season + df$Dia.da.Semana + df$Carnaval + df$Pascoa)

plot(forecast(yhat, h=20))

ytrain <- window(df$Receita_Liq_Total,end="2015-06-30")

str(ytrain)

ytrain = ts(ytrain, frequency = 7)

str(ytrain)



Dia.da.Semana_train = window(df$Dia.da.Semana,end="2015-06-30")

Carnaval_train <- window(df$Carnaval,end="2015-06-30")

Pascoa_train <- window(df$Pascoa,end="2015-06-30")

Natal_train= window(df$Natal, end = '2015-06-30')

str(Dia.da.Semana_train)

fit <- tslm(ytrain ~ trend + season + df$Dia.da.Semana + df$Carnaval + df$Pascoa)

refit <- Arima(hsales, model=fit)
fc <- window(fitted(refit), start=1990)





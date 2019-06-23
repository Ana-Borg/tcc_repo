library(ggplot2)
library(forecast)
library(tseries)


# Séries temporais por produtos
base_pao_frances <- base_modelo[EAN == "300096",]
ts_pao_frances <- ts(base_pao_frances$QTY_TOTAL, frequency = 7)

base_pao_frances$CLEAN_QTY_TOTAL = tsclean(ts_pao_frances)

base_pao_frances$QTY_TOTAL_MA = ma(base_pao_frances$CLEAN_QTY_TOTAL, order=2) 

ggplot() +
  geom_line(data = base_pao_frances, aes(x = DATE, y = CLEAN_QTY_TOTAL, colour = "QTDE"))+
  geom_line(data = base_pao_frances, aes(x = DATE, y = QTY_TOTAL_MA,   colour = "Moving Average"))  +
  ylab('QTDE VENDIDA')

count_ma = ts(na.omit(base_pao_frances$QTY_TOTAL_MA), frequency=7)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main='')

Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,0,0) Model Residuals')

#
fit2 = arima(deseasonal_cnt, order=c(1,0,7))

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')


# Predict
fcast <- forecast(fit2, h=7)
plot(fcast)

# Fit with seasonality
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=7)
plot(seas_fcast)




# ---- Regressão polinomial
x_pao_frances_sen <- base_pao_frances$QTY_TOTAL
x_pao_frances_sen[11] <- base_pao_frances[11]$QTY_MEDIA_DIA_SEMANA
plot(x_pao_frances_sen)

time <- seq(c(1:28))

xc1<-cos(2*pi/7*time)
xs1<-sin(2*pi/7*time)
xc2<-cos(4*pi/7*time)
xs2<-sin(4*pi/7*time)
xc3<-cos(6*pi/7*time)
xs3<-sin(6*pi/7*time)

pao_fit_lm <- lm(x_pao_frances_sen~xc1+xs1+xc2+xs2+xc3+xs3)

summary(pao_fit_lm)
plot(pao_fit_lm$fitted.values)


prox_semanas <- predict(pao_fit_lm, interval = "prediction")
prox_semanas <- prox_semanas[1:21,]

# Gráfico final
base_pao_valid <- variaveis[,c(-3,-10)]
base_pao_valid <- base_pao_valid[DATE >= "2019-05-01" & DATE <= "2019-05-21",]
base_pao_valid <- base_pao_valid[EAN == "300096",]
base_pao_valid <- base_pao_valid[,c(3,10,11)]

output <- as.data.frame(cbind(base_pao_valid$QTY_TOTAL, prox_semanas))
x <- seq(1:21)

p <- ggplot(output, aes(x,output$fit)) +
  geom_point()+
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")+
  geom_line(aes(y = base_pao_valid$QTY_TOTAL), color = "blue")

p

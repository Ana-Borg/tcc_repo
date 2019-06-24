library(ggplot2)
library(forecast)
library(tseries)
library(MASS)
library(pracma)

RMSE = function(m, o){
  dist <- m - o
  dist <- dist[!is.na(dist)]
  dist <- dist^2
  sqrt(mean(dist))
}

# Séries temporais por produtos
base_pao_frances <- base_modelo[EAN == "300096",]
ts_pao_frances <- ts(base_pao_frances$QTY_TOTAL, frequency = 7)

base_pao_frances$CLEAN_QTY_TOTAL = tsclean(ts_pao_frances)


ggplot() +
  geom_line(data = base_pao_frances, aes(x = DATE, y = CLEAN_QTY_TOTAL, colour = "QTDE"))+
  ylab('QTDE VENDIDA') + labs(title = "Qtde vendida - Pão Francês Kg")


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
xc4<-cos(8*pi/7*time)
xs4<-sin(8*pi/7*time)

pao_fit_lm <- lm(x_pao_frances_sen~xc1+xs1+xc2+xs2)


plot(pao_fit_lm$fitted.values)

output <- as.data.table(pao_fit_lm$fitted.values)
colnames(output) <- "output"

p <- ggplot(output, aes(time,output)) +
  geom_point()+
  geom_line(aes(y = x_pao_frances_sen), color = "red", linetype = "dashed")+
  labs(x = "Dia") + labs(y = "QTDE") + labs(title = "Modelo Senoidal - 2h")

p


prox_semanas <- predict(pao_fit_lm, interval = "prediction")
prox_semanas <- as.data.frame(prox_semanas[1:21,])

# Média móvel
model_ma = rollapplyr(base_pao_valid$QTY_TOTAL,list(-(2:1)),mean,fill=NA)

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

# Sinusoidal + média móvel
# prox_semanas$ma <- as.numeric(model_ma)
# sinu_ma <- as.data.table(prox_semanas)
# 
# sinu_ma$lwr_trig <- sinu_ma$fit - (sinu_ma$fit-sinu_ma$lwr)/1.7
# sinu_ma$upr_trig <- sinu_ma$fit + (sinu_ma$upr-sinu_ma$fit)/1.7
# sinu_ma[is.na(sinu_ma$ma),]$ma <- sinu_ma[is.na(sinu_ma$ma),]$fit
# 
# #sinu_ma$max_value <- apply(sinu_ma,1,max)
# #sinu_ma$min_value <- apply(sinu_ma,1,min)
# 
# sinu_ma$output <- sinu_ma$fit
# sinu_ma[ma > upr_trig]$output <- sinu_ma[ma > upr_trig]$upr
# sinu_ma[ma < lwr_trig]$output <- sinu_ma[ma < lwr_trig]$lwr


# Gráfico final

p <- ggplot(output, ) +
  geom_point(aes(x,output$fit, colour = "Modelo Sinusoidal"))+
  geom_line(aes(y = output$fit, colour = "QTDE"), color = "red")+
  geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")+
  geom_line(aes(y = base_pao_valid$QTY_TOTAL), color = "blue")+
  geom_line(aes(y = sinu_ma$ma), color = "green", linetype = "dashed")+
  labs(x = "Dia") + labs(y = "Previsões") + labs(title = "Comparação entre modelos vs observado")


p


#Erro

RMSE(base_pao_valid$QTY_TOTAL, model_ma)
RMSE(base_pao_valid$QTY_TOTAL, output$fit)

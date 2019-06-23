# Referências
# https://mlbook.explained.ai/intro.html#sec:2.2
# https://medium.com/datadriveninvestor/why-wont-time-series-data-and-random-forests-work-very-well-together-3c9f7b271631
# https://towardsdatascience.com/multivariate-time-series-forecasting-using-random-forest-2372f3ecbad1
# https://towardsdatascience.com/selecting-the-best-machine-learning-algorithm-for-your-regression-problem-20c330bad4ef

# ARIMA
# https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials

# Load packages
library(data.table)
library(lubridate)
library(zoo)
#library(randomForest)


variaveis <- dt_sellout[,c(7,2,3,5)]
variaveis <- variaveis[, ':=' (QTY_TOTAL = sum(QTY)), by = .(DATE, EAN, DESCRICAO)]
variaveis <- variaveis[, ':=' (N_VENDAS = length(QTY)), by = .(DATE, EAN, DESCRICAO)]
variaveis <- unique(variaveis[,-4])

variaveis <- variaveis[, ':=' (QTY_MEDIA = mean(QTY_TOTAL)), by = .(EAN, DESCRICAO)]
variaveis <- variaveis[, ':=' (QTY_SD = sd(QTY_TOTAL)), by = .(EAN, DESCRICAO)]

variaveis <- variaveis[, ':=' (N_VENDAS_MEDIA = mean(N_VENDAS)), by = .(EAN, DESCRICAO)]
variaveis <- variaveis[, ':=' (N_VENDAS_SD = sd(N_VENDAS)), by = .(EAN, DESCRICAO)]

variaveis$DIA_SEMANA <-  weekdays(variaveis$DATE)
variaveis$DIA_MES <-  day(variaveis$DATE)


variaveis <- variaveis[, ':=' (QTY_MEDIA_DIA_SEMANA = mean(QTY_TOTAL)), by = .(EAN, DESCRICAO, DIA_SEMANA)]
variaveis <- variaveis[, ':=' (QTY_SD_DIA_SEMANA = sd(QTY_TOTAL)), by = .(EAN, DESCRICAO, DIA_SEMANA)]

variaveis <- variaveis[, ':=' (N_VENDAS_MEDIA_DIA_SEMANA = mean(N_VENDAS)),
                       by = .(EAN, DESCRICAO, DIA_SEMANA)]
variaveis <- variaveis[, ':=' (N_VENDAS_SD_DIA_SEMANA = sd(N_VENDAS)), by = .(EAN, DESCRICAO, DIA_SEMANA)]

variaveis[is.na(QTY_SD),]$QTY_SD <- 0
variaveis[is.na(N_VENDAS_SD),]$N_VENDAS_SD <- 0
variaveis[is.na(QTY_SD_DIA_SEMANA),]$QTY_SD_DIA_SEMANA <- 0
variaveis[is.na(N_VENDAS_SD_DIA_SEMANA),]$N_VENDAS_SD_DIA_SEMANA <- 0

# Base para MVP
base_modelo <- variaveis[,c(-3,-10)]
base_modelo <- base_modelo[DATE <= "2019-04-30" & DATE >= "2019-04-03",]
base_modelo <- base_modelo[EAN %in% produtos_interesse$EAN,]

# Base de treino
base_treino <- base_modelo[,c(1:3,5,10)]
base_treino$NEXT_DATE <- base_treino$DATE + 1
base_treino$NEXT_QTY <- merge(base_treino[,c(2,6)], base_modelo[,c(1:3)], sort = F, all.x = T,
                              by.x = c("EAN", "NEXT_DATE"), by.y = c("EAN", "DATE"))[,3]

base_treino <- base_treino[!is.na(base_treino$NEXT_QTY)]

# RF - Pão

# base_treino <- base_treino[,c(3:5,7)]
# 
# x_treino <- as.data.frame(base_treino[EAN == "300096" & DATE <= "2019-04-21",c(3,5)])
# y_treino <- as.data.frame(base_treino[EAN == "300096" & DATE <= "2019-04-21",7])
# 
# x_test <- base_treino[EAN == "300096" & DATE >= "2019-04-22",c(3,5)]
# y_test <- base_treino[EAN == "300096" & DATE >= "2019-04-22",7]
# 
# pao.rf <- randomForest(NEXT_QTY ~ ., data = pao_treino, ntree = 100)




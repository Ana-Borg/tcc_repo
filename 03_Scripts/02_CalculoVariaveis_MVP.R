# Load packages
library(data.table)
library(readxl)
library(stringr)
library(lubridate)
library(zoo)


variaveis <- dt_sellout[,c(1,2,3,5)]
variaveis <- variaveis[, ':=' (QTY_TOTAL = sum(QTY)), by = .(CREATE_DATE, EAN, DESCRICAO)]
variaveis <- variaveis[, ':=' (N_VENDAS = length(QTY)), by = .(CREATE_DATE, EAN, DESCRICAO)]
variaveis <- variaveis[, ':=' (QTY_MEDIA = mean(QTY)), by = .(CREATE_DATE, EAN, DESCRICAO)]
variaveis <- variaveis[, ':=' (QTY_SD = sd(QTY)), by = .(CREATE_DATE, EAN, DESCRICAO)]

variaveis[is.na(QTY_SD),]$QTY_SD <- 0

base_treino <- unique(variaveis[,-4])

ts_pao_frances <- base_treino[EAN == "300096",]
ts_pao_frances <- ts(ts_pao_frances$QTY_TOTAL, frequency = 7)

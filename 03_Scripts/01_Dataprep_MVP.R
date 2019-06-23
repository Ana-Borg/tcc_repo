# Referências
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html

# Load packages
library(data.table)
library(readxl)
library(stringr)
library(lubridate)
library(zoo)

# Set working directory and load data
setwd("C:/Users/anacb/Documents/Poli/TCC/tcc_repo/01_Bases")

sellin <- read_excel("SellIn.xlsx", col_types = c("text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", "numeric"))

sellout <- read_excel("SellOut.xlsx", col_types = c("text", 
                                                    "text", "text", "text", "numeric", "numeric", 
                                                    "numeric", "skip", "skip"))

dt_sellin <- setDT(sellin)
dt_sellout <- setDT(sellout)

# Data prep
dt_sellout <- dt_sellout[STORE_CNPJ == "21137886000190",]
dt_sellout$STORE_CNPJ <- NULL
dt_sellout$DATE <- tstrsplit(dt_sellout$CREATE_DATE, " ")[1]
dt_sellout$DATE <- str_replace(dt_sellout$DATE, "APR", "04")
dt_sellout$DATE <- str_replace(dt_sellout$DATE, "MAY", "05")
dt_sellout$DATE <- as.Date(dt_sellout$DATE, "%d-%m-%y")

dt_sellin <-dt_sellin[,c(-3:-4)]
dt_sellin$ISSUE_DATE <- tstrsplit(dt_sellin$ISSUE_DATE, " ")[1]
dt_sellin$ISSUE_DATE <- str_replace(dt_sellin$ISSUE_DATE, "APR", "04")
dt_sellin$CREATE_DATE <- str_replace(dt_sellin$CREATE_DATE, "MAY", "05")
dt_sellin$ISSUE_DATE <- as.Date(dt_sellin$ISSUE_DATE, "%d-%m-%y")
dt_sellin$ISSUE_DATE <- tstrsplit(dt_sellin$ISSUE_DATE, " ")[1]
dt_sellin$SECURITY_VALUE <- NULL
dt_sellin$TOTAL_FREIGHT <- NULL

# Deparas
depara_ean_descricao <- unique(dt_sellout[,c(2,3)])
depara_ean_product_name <- unique(dt_sellin[,c(3,5)])

# Analise Exploratoria
receita_produto <- dt_sellout[, lapply(.SD, sum), by = .(EAN, DESCRICAO), .SDcols="SUBTOTAL"]
receita_produto <- receita_produto[order(-receita_produto$SUBTOTAL)]
receita_total <- sum(receita_produto$SUBTOTAL)

vendas_produto <- dt_sellout[, lapply(.SD, sum), by = .(EAN, DESCRICAO), .SDcols="QTY"]
vendas_produto <- vendas_produto[order(-vendas_produto$QTY)]
vendas_total <- sum(vendas_produto$QTY)

top15_receita <- receita_produto[1:15,]
top10_vendas <- vendas_produto[1:10,]

(sum(top15_receita$SUBTOTAL)/receita_total)*100
(sum(top10_vendas$QTY)/vendas_total)*100

produtos_interesse <- merge(top15_receita, top10_vendas, sort = F, by = c("EAN", "DESCRICAO"))
produtos_interesse <- produtos_interesse[EAN != 17,]

(sum(produtos_interesse$SUBTOTAL)/receita_total)*100
(sum(produtos_interesse$QTY)/vendas_total)*100

#View(dt_sellin[EAN %in% produtos_interesse$EAN,])
#View(dt_sellout[EAN %in% produtos_interesse$EAN,])

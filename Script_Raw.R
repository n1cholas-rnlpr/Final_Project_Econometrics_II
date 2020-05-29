
# Limpar tudo
rm(list=ls())

# Selecionar diretorio
setwd("/Users/nicholaslepetit/Library/Mobile Documents/com~apple~CloudDocs/Documents/ECONOMIA/02.2019/ECONOMETRIA II/trab.final/")
dir()

dbmilho <- read.csv2("milho3.csv")
head(dbmilho)

library(anytime)
dbmilho$date <- as.Date(dbmilho$date, tryFormats = c("%d/%m/%Y"))
dbmilho
summary(dbmilho)

library(zoo)
dbmilho$date <- as.yearmon(dbmilho$date, "%d/%m/%Y")
summary(dbmilho$date)

library(tidyverse)
dbmilho <- dbmilho %>% 
  group_by(date) %>% 
  summarise(preco_milho = mean(preco_milho))


dbcambio <- read.csv2("cambio_08.2004_11.2019.csv", header = FALSE)
head(dbcambio)
names(dbcambio)[1] <- "cambio"

db1 <- data.frame(dbmilho$preco_milho, dbcambio)
names(db1)[1] <- "pmilho"
head(db1)

db1.ts <- ts(db1, start = c(2004,8), end = c(2019,11), frequency = 12)
head(db1.ts)

library(dynlm)
mod1 <- dynlm(pmilho ~ cambio, data = db1.ts)
summary(mod1)

dbmilho.ts <- ts(db1$pmilho, start = c(2004,8), end = c(2019,11), frequency = 12)
dbcambio.ts <- ts(db1$cambio, start = c(2004,8), end = c(2019,11), frequency = 12)


plot.ts(dbmilho.ts, type = "l", ylab="Preço da Saca de 60kg de Milho", xlab = "Tempo")
abline(h = mean(dbmilho$preco_milho))

cambio.ts <- ts(dbcambio, start = c(2004, 8), end = c(2019, 11), frequency = 12)
plot.ts(cambio.ts, ylab="Taxa de câmbio R$/US$ - comercial - venda", xlab="Tempo")
abline(h = mean(cambio.ts))


## Testes AR(1)
milho.ar <- ar(dbmilho.ts, aic=TRUE, method="ols", order.max = 1)
milho.ar
cambio.ar <- ar(dbcambio.ts, aic=TRUE, method="ols", order.max = 1)
cambio.ar

acf(db1.ts) #pmilho = 20 ; cambio = 20
acf(dbmilho.ts, lag.max = 60) # 50 lags para o teste
acf(dbcambio.ts, lag.max = 50) # 46 lags para o teste



library(tseries)
adf.test(dbmilho.ts, k=50)
adf.test(dbcambio.ts, k=46)


# Identificar a ordem da integracao: funcao ndiff
library(forecast)
ndiffs(dbmilho.ts)

ndiffs(dbcambio.ts)

# Obtendo as ordens integradas de cada db
milho.i1.ts <- diff(dbmilho.ts)
cambio.i2.ts <- diff(dbcambio.ts, differences = 2)
cambio.i1.ts <- diff(dbcambio.ts)
# Plotando antes e depois do dbmilho
split.screen(figs = c(2,1))
screen(1)
plot.ts(dbmilho.ts, main="BANCO DE DADOS ORIGINAL DO PREÇO DO MILHO", xlab="Tempo")
screen(2)
plot.ts(milho.i1.ts,  main="DIFERENÇAS DE PRIMEIRA ORDEM DO PREÇO DO MILHO", xlab="Tempo")
abline(h=0)

# Plotando antes e depois do dbcambio
split.screen(figs = c(2,1))
screen(1)
plot.ts(dbcambio.ts, main="BANCO DE DADOS ORIGINAL DO CÂMBIO", xlab="Tempo")
screen(2)
plot.ts(cambio.i2.ts,  main="DIFERENÇAS DE SEGUNDA ORDEM DO CÂMBIO", xlab="Tempo")
abline(h=0)

# Testando estacionariedade para ambas as séries
split.screen(figs = c(2,1))
screen(1)
acf(milho.i1.ts, main="CORRELOGRAMA DAS DIFERENÇAS DE PRIMEIRA ORDEM DO PREÇO DO MILHO")# lag p/ teste = 1
screen(2)
acf(cambio.i2.ts, main="CORRELOGRAMA DAS DIFERENÇAS DE SEGUNDA ORDEM DO CÂMBIO")# lag p/ teste = 2

adf.test(milho.i1.ts, k=1)
adf.test(cambio.i2.ts, k=2)

mod2 <- dynlm(milho.i1.ts ~ cambio.i2.ts)
summary(mod2)

acf(cambio.i1.ts)
adf.test(cambio.i1.ts, k=1)
mod3 <- dynlm(milho.i1.ts ~ cambio.i1.ts)
summary(mod3)

dbx <- as.matrix(cbind(db1$pmilho,db1$cambio))
dbx
po.test(dbx) # H0: Nao haá cointegracao entre as series.
# Resultado: p-valor > 0,15 ----- Portanto, nao ha evidencias estatisticas para rejeitarmos H0 e assu-
# mirmos a HA de cointegracao.







db2 <- read.csv("db3_03.2011_10.2019.csv")
db2 <- read.csv2("db4.csv")
head(db2)
db2.ts <- ts(db2, start = c(2011,3), end = c(2019,11), frequency = 12)
head(db2.ts)

mod2 <- dynlm(icred ~ m1pp, data = db2.ts)
summary(mod2)

# Goodness-of-fit 
# Importing Energy data
library(readxl)
setwd("C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/distrib_energy_price/data")
EnergyPrice20 <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "Preco_OMIE_ES_2020")
attach(EnergyPrice20)
head(EnergyPrice20)

library(fitdistrplus)

# Fitting normal distribution --> Best
fitnormal <- fitdist(EnergyPrice20$Valor_OMIE, "norm")
fitnormal
plot(fitnormal)
x_norm <- gofstat(fitnormal)

# Fitting gamma distribution
fitgamma <- fitdist(EnergyPrice20$Valor_OMIE, "gamma")
fitgamma
plot(fitgamma)
x_gam <- gofstat(fitgamma)

# Fitting exponencial distribution
fitexp <- fitdist(EnergyPrice20$Valor_OMIE, "exp")
fitexp
plot(fitexp)
x_exp <- gofstat(fitexp)

# Fitting weibull distribution --> Frontrunner
fitweibull <- fitdist(EnergyPrice20$Valor_OMIE, "weibull")
fitweibull
plot(fitweibull)
x_wei <- gofstat(fitweibull)

# Fitting lognormal distribution
fitlognormal <- fitdist(EnergyPrice20$Valor_OMIE, "lnorm")
fitlognormal
plot(fitlognormal)
x_log <- gofstat(fitlognormal)

# Fitting Gumbel distribution
dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
qgumbel <- function(p, a, b) a-b*log(-log(p))
fitgumbel <- fitdist(EnergyPrice20$Valor_OMIE, "gumbel", start=list(a=10, b=10))
fitgumbel
plot(fitgumbel)
x_gum <- gofstat(fitgumbel)

# All distributions fitting summary
x_norm
x_norm$kstest
x_gam
x_gam$kstest
x_exp
x_exp$kstest
x_wei
x_wei$kstest
x_log
x_log$kstest
x_gum
x_gum$kstest

# Cálculo da probabilidade com base na distribuição lognormal
#plnorm(5.12, 0.2979401, 0.6397913, lower.tail = FALSE)

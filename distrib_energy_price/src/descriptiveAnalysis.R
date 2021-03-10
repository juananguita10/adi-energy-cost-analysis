# Importing Energy data
library(readxl)
setwd("C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/distrib_energy_price/data")
EnergyPrice20 <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "Preco_OMIE_ES_2020")
attach(EnergyPrice20)
head(EnergyPrice20)

# Global Descriptive Statistics
mean(Valor_OMIE)
var(Valor_OMIE)
sd(Valor_OMIE)
cv <- sd(Valor_OMIE)/mean(Valor_OMIE)*100
cv
median(Valor_OMIE)
percentis <- quantile(Valor_OMIE, c(0.01, 0.02, 0.5, 0.70, 0.95, 0.99))
percentis

# Q1 Descriptive Statistics
# Q2 Descriptive Statistics
# Q3 Descriptive Statistics
# Q4 Descriptive Statistics

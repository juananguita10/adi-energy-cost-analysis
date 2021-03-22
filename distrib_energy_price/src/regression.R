library(readxl)
library(lmtest)
library(plotly)

# Importing Energy data
rm(list=ls()) # Clean the Global Environment
setwd("C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/distrib_energy_price/data")
EnergyPrice20 <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "TEcnologia")
attach(EnergyPrice20)
head(EnergyPrice20)

# Estimate the linear regression model
reg1 <- lm(omie ~ "Tecnologia Codigo", data = EnergyPrice20)
summary(reg1)

# Estimates of the coefficients
res1 <- reg1$residuals
plot(res1)
reg1$coefficients
reg1$coefficients[1]
reg1$coefficients[2]

# OLS residuals
residuals(reg1)
sum(residuals(reg1))
mean(residuals(reg1))

# Predicted values
yhat <- predict(reg1)
sum(yhat)
sum(price)
plot(price ~ yhat)
mean(price)
mean(yhat)

# R-Squared and adjusted R-Squared
summary(reg1)$r.squared
summary(reg1)$adj.r.squared

# TSS
TSS <- sum((price-mean(price))^2) # Variação total em torno da média
ESS <- sum((price-mean(price))^2) # Variação explicada
e <- price-yhat # e_i = Y_i - Y^_i ## Cálculo dos resíduos
e[1]
e
RSS <- sum(e^2)
RSS
RSS <- sum((price-yhat)^2) # Residual Sum of Squares: Variação não explicada
R21 <- ESS/TSS # Coeficiente de determinação, R^2
R21
(R21 <- ESS/TSS)
R22 <- 1- RSS/TSS # Coeficiente de determinação, R^2
R22
(R22 <- 1- RSS/TSS)

# F-test
summary(reg1)$fstatistic
summary(reg1)[10]
summary(reg1)[4]

# t-tests
coef(summary(reg1))[, "t value"]

# Previsão das vendas se  advertising = 500
priceprev <- reg1$coefficients[1]+reg1$coefficients[2]*500
priceprev

yhat <- reg1$coefficients[1] + reg1$coefficients[2]*techn
yhat

# Estimar o modelo de regressão linear múltipla
varexp <- cbind(techn, q)
reg2 <- lm(price ~ techn + q, data = EnergyPrice20)
summary(reg2)
reg21 <- lm(price ~ varexp, data = EnergyPrice20)
summary(reg21) # para aceder ao conteúdo do objeto reg21

# Estimate the quadratic function between price and technology
techn2 <- techn^2
reg3 <- lm(price ~ techn + techn2, data = EnergyPrice20)
summary(reg3)

# Estimate lin-lin
regC31 <- lm(price ~ techn, data = EnergyPrice20)
summary(regC31)

# Estimate log-lin
regC32 <- lm(log(price) ~ techn, data = EnergyPrice20)
summary(regC32)

# Estimate lin-log
regC33 <- lm(price ~ log(techn), data = EnergyPrice20)
summary(regC33)

# Estimate log-log
regC34 <- lm(log(price) ~ log(techn), data = EnergyPrice20)
summary(regC34)

# Comparing R-squared from different models
# R2 from regC31
R2C31 <- summary(regC31)$r.squared
AR2C31 <- summary(regC31)$adj.r.squared

# R2 from regC32
# Na regC32 LOG(Y) = X2 (então a var dependente é o LOG(Y))
predict(regC32) # ^log(Y) não ^Y
yhat2 <- exp(predict(regC32))
R2C32 <- cor(price, yhat2)^2
AR2C32 <- 1-(1-R2C32)*(60-1)/(60-2)

# R2 from regC33
R2C33 <- summary(regC33)$r.squared
AR2C33 <- summary(regC33)$adj.r.squared

# R2 from regC34
yhat4 <- exp(predict(regC34))
R2C34 <- cor(price, yhat4)^2
AR2C34 <- 1-(1-R2C34)*(60-1)/(60-2)

# R2 from reg2
yhat2 <- predict(reg2)
R2C35 <- cor(price, yhat2)^2
(AR2C35 <- 1-(1-R2C35)*(60-1)/(60-5))

tableR2 <- plot_ly(
  type = 'table',
  header = list(
    values = c('<b>GOODNESS-OF-FIT STATISTICS</b>', '<b>regC31</b>','<b>regC32</b>','<b>regC33</b>','<b>regC34</b>','<b>regC35</b>'),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12)
  ),
  cells = list(
    values = rbind(
      c('R-Squared', 'Adjusted R-squared'),
      c(round(R2C31, digits=4), round(AR2C31, digits=4)),
      c(round(R2C32, digits=4), round(AR2C32, digits=4)),
      c(round(R2C33, digits=4), round(AR2C33, digits=4)),
      c(round(R2C34, digits=4), round(AR2C34, digits=4)),
      c(round(R2C35, digits=4), round(AR2C35, digits=4)))),
  line = list(color = '#506784'),
  fill = list(color = c('#25FEFD', 'white')),
  align = c('left', 'center'),
  font = list(color = c('#506784'), size = 12)
)
tableR2


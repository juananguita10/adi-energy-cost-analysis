library(readxl)
library(fitdistrplus)
library(tseries)

# Best fit search libraries
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

# Importing Energy data
rm(list=ls()) # Clean the Global Environment
setwd("C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/distrib_energy_price/data")
EnergyPrice <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "yearsR")

# Function for Gumbel distribution calculation
dgumbel <- function(x, a, b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
pgumbel <- function(q, a, b) exp(-exp((a-q)/b))
qgumbel <- function(p, a, b) a-b*log(-log(p))

# Function for fitting a distribution to dataset (d)
fit_dist_energy <- function(d, distr, p=FALSE) {
  if(distr == "gumbel") { 
    fitD <- fitdist(d, distr, start=list(a=10, b=10)) 
  } else { 
    fitD <- fitdist(d, distr) 
  }
  
  ret <- gofstat(fitD)

  if (p) {
    print(fitD)
    plot(fitD)
    
    print(ret)
    print(ret$kstest)
  }
  
  return(ret)
}

best_dist_basic <- function(d, distrArray=c("norm", "gamma", "exp", "weibull", "lnorm", "gumbel")) {
  bestDistr <- ""
  bestAICBIC <- 999999999
  for (distr in distrArray) {
    x <- fit_dist_energy(d, distr)
    
    if ((x$aic+x$bic) < bestAICBIC) {
      bestDistr <- distr
      bestAICBIC <- x$aic+x$bic
    }
  }
  
  print(bestDistr)
  
  return(fit_dist_energy(d, bestDistr, p=TRUE))
}

best_dist_adv <- function(d) {
  fitD <- fitDist(d, k = 2, type = "realplus", trace = FALSE, try.gamlss = TRUE)
  summary(fitD)
  histDist(d, family=fitD$family[1], nbins=30, line.col="darkblue", line.wd=2.5)
  
  return (fitD)
}

# Get probability of price being > 50 using normal distribution
#pnorm(50, 33.96226, 11.40810, lower.tail = FALSE)

# 2020
d2020 <- subset(EnergyPrice, year==2020)
bdb2020 <- best_dist_basic(d2020$price) # Normal
jarque.bera.test(d2020$price) # Testing normality -> rejected
bda2020 <- best_dist_adv(d2020$price) # BCCGo

# 2019
d2019 <- subset(EnergyPrice, year==2019)
bdb2019 <- best_dist_basic(d2019$price) # Normal
jarque.bera.test(d2019$price) # Testing normality -> rejected
bda2019 <- best_dist_adv(d2019$price) # BCPEo

# 2018
d2018 <- subset(EnergyPrice, year==2018)
bdb2018 <- best_dist_basic(d2018$price) # Weibull
jarque.bera.test(d2020$price) # Testing normality -> rejected # weibull should be tested
bda2018 <- best_dist_adv(d2018$price) # BCPEo

# 2017
d2017 <- subset(EnergyPrice, year==2017)
bdb2017 <- best_dist_basic(d2017$price) # Normal
jarque.bera.test(d2017$price) # Testing normality -> rejected
bda2017 <- best_dist_adv(d2017$price) # BCPE

# 2016
d2016 <- subset(EnergyPrice, year==2016)
bdb2016 <- best_dist_basic(d2016$price) # Normal
jarque.bera.test(d2016$price) # Testing normality -> rejected
bda2016 <- best_dist_adv(d2016$price) # BCPE

# Every year
bdb <- best_dist_basic(EnergyPrice$price) # Normal
jarque.bera.test(EnergyPrice$price) # Testing normality -> rejected
bda <- best_dist_adv(EnergyPrice$price) # BCPE

#----------------------------------- (delete)Part 1 ------------------------------
# Histogram
hist(price)

(skew <- skewness(price)) # 0 - no caso da normal
(kurt <- kurtosis(price)) # 3 - no caso da normal

# Quantiles / Percentiles Salary
(quant <- quantile(price, c(0.01, 0.05, 0.95, 0.99)))

# Testing the normality of salaries
lillie.test(price) # Kolmogorov-Smirnov with Lilliefors correction # (30-50)
ad.test(price) # Anderson-Darling
cvm.test(price) # Cramer-von Mises
pearson.test(price) # Pearson Chi-Square
sf.test(price) # Shapiro-Francia # (<30)
jarque.bera.test(price) # Jarque-Bera test (package "tseries") # (>50)
jarque.test(price) # Jarque-Bera test
# Em todos rejeitamos a normal

# Getting q1 subset
q1 <- subset(EnergyPrice, q==1)
# Energy price = 35 in Q1 test # Testing the nullity of returns' mean
t.test(q1, mu=35) # H0 # mean = 35 -> not rejected

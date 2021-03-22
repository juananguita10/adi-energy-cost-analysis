library(moments)
library(readxl)
library(tseries)
library("nortest")

# Importing Energy data
rm(list=ls()) # Clean the Global Environment
setwd("C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/distrib_energy_price/data")
EnergyPrice <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "yearsR")
EnergyPrice20 <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "quartersR")

# ----------- Correlation Analysis 2016 vs 2017 vs 2018 vs 2019 vs 2020 --------
# Test to compare every quarter mean
tapply(EnergyPrice$price, EnergyPrice$year, mean) # Get
tapply(EnergyPrice$price, EnergyPrice$year, var) # Get

#tapply(EnergyPrice$price, EnergyPrice$year, lillie.test) # normallity test not necessary

bartlett.test(EnergyPrice$price ~ EnergyPrice$year) # variances test -> rejected, difference between variances is statistically significant

#fitanova1 <- aov(EnergyPrice$price ~ EnergyPrice20$year, data=EnergyPrice) # ANOVA test
#summary(fitanova1) # ANOVA can't be used due to variances significant difference  

kruskal.test(EnergyPrice$price ~ EnergyPrice$year, data=EnergyPrice) # Kruskal-Wallis test -> rejected, difference between means is statistically significant

# Comparing the variances
get_pvalue_var <- function(p, y) {
  r <- bartlett.test(p ~ y) 
  return(r$p.value)
}

sset <- subset(EnergyPrice, year==2016 | year==2017)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2016 | year==2018)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2016 | year==2019)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2016 | year==2020)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2017 | year==2018)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2017 | year==2019)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2017 | year==2020)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2018 | year==2019)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2018 | year==2020)
get_pvalue_var(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2019 | year==2020)
get_pvalue_var(sset$price, sset$year)

# Comparing the means
get_pvalue_mean <- function(p, y) {
  r <- t.test(p ~ y, paired=TRUE, var.equal=FALSE, conf.level = 0.95) 
  return(r$p.value)
}

sset <- subset(EnergyPrice, year==2016 | year==2017)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2016 | year==2018)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2016 | year==2019)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2016 | year==2020)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2017 | year==2018)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2017 | year==2019)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2017 | year==2020)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2018 | year==2019)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2018 | year==2020)
get_pvalue_mean(sset$price, sset$year)
sset <- subset(EnergyPrice, year==2019 | year==2020)
get_pvalue_mean(sset$price, sset$year)

# Getting 2018 and 2020 subset
y18y20 <- subset(EnergyPrice, year==2018 | year==2020)
y18 <- subset(EnergyPrice, year==2018)

# Comparing the variances
tapply(y18y20$price, y18y20$year, var) # Get
var.test(y18y20$price ~ y18y20$year) # F test to test equality of variances -> rejected, difference between both variances is statistically significant
bartlett.test(y18y20$price ~ y18y20$year) # Bartlett test to test equality of variances -> rejected, difference between both variances is statistically significant

# Comparing the means
tapply(y18y20$price, y18y20$year, mean) # Get
t.test(y18y20$price ~ y18y20$year, paired=TRUE, var.equal=FALSE, conf.level = 0.95) # Test equality of means -> rejected, difference between both means is statistically significant

# ------------- Correlation Analysis Q1 vs Q2 vs Q3 vs Q4 ----------------------
# Test to compare every quarter mean
tapply(EnergyPrice20$price, EnergyPrice20$q, mean) # Get

#tapply(EnergyPrice20$price, EnergyPrice20$q, lillie.test) # normallity test not necessary

bartlett.test(EnergyPrice20$price ~ EnergyPrice20$q) # variances test -> rejected, difference between variances is statistically significant

#fitanova1 <- aov(EnergyPrice20$price ~ EnergyPrice20$q, data=EnergyPrice20) # ANOVA test
#summary(fitanova1) # ANOVA can't be used due to variances significant difference  

kruskal.test(EnergyPrice20$price ~ EnergyPrice20$q, data=EnergyPrice20) # Kruskal-Wallis test -> rejected, difference between means is statistically significant

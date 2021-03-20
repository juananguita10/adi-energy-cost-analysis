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

#tapply(EnergyPrice$price, EnergyPrice$year, lillie.test) # normallity test not necessary

bartlett.test(EnergyPrice$price ~ EnergyPrice$year) # variances test -> rejected, difference between variances is statistically significant

#fitanova1 <- aov(EnergyPrice$price ~ EnergyPrice20$year, data=EnergyPrice) # ANOVA test
#summary(fitanova1) # ANOVA can't be used due to variances significant difference  

kruskal.test(EnergyPrice$price ~ EnergyPrice$year, data=EnergyPrice) # Kruskal-Wallis test -> rejected, difference between means is statistically significant

# Getting 2019 and 2020 subset
y19y20 <- subset(EnergyPrice, year==2019 | year==2020)
y19 <- subset(EnergyPrice, year==2019)
y20 <- subset(EnergyPrice, year==2020)

# Comparing the variances
tapply(y19y20$price, y19y20$year, var) # Get
var.test(y19y20$price ~ y19y20$year) # F test to test equality of variances -> not rejected, difference between both variances is not statistically significant
bartlett.test(y19y20$price ~ y19y20$year) # Bartlett test to test equality of variances -> not rejected, difference between both variances is not statistically significant

# Comparing the means
tapply(y19y20$price, y19y20$year, mean) # Get
t.test(y19y20$price ~ y19y20$year, paired=TRUE, var.equal=TRUE, conf.level = 0.95) # Test equality of means -> rejected, difference between both means is statistically significant

# Getting 2018 and 2020 subset
y18y20 <- subset(EnergyPrice, year==2018 | year==2020)
y18 <- subset(EnergyPrice, year==2018)

# Comparing the variances
tapply(y18y20$price, y18y20$year, var) # Get
var.test(y18y20$price ~ y18y20$year) # F test to test equality of variances -> rejected, difference between both variances is statistically significant
bartlett.test(y18y20$price ~ y18y20$year) # Bartlett test to test equality of variances -> rejected, difference between both variances is statistically significant

# Comparing the means
tapply(y18y20$price, y18y20$year, mean) # Get
t.test(y18y20$price ~ y18y20$year, paired=TRUE, var.equal=TRUE, conf.level = 0.95) # Test equality of means -> rejected, difference between both means is statistically significant

# ------------- Correlation Analysis Q1 vs Q2 vs Q3 vs Q4 ----------------------
# Test to compare every quarter mean
tapply(EnergyPrice20$price, EnergyPrice20$q, mean) # Get

#tapply(EnergyPrice20$price, EnergyPrice20$q, lillie.test) # normallity test not necessary

bartlett.test(EnergyPrice20$price ~ EnergyPrice20$q) # variances test -> rejected, difference between variances is statistically significant

#fitanova1 <- aov(EnergyPrice20$price ~ EnergyPrice20$q, data=EnergyPrice20) # ANOVA test
#summary(fitanova1) # ANOVA can't be used due to variances significant difference  

kruskal.test(EnergyPrice20$price ~ EnergyPrice20$q, data=EnergyPrice20) # Kruskal-Wallis test -> rejected, difference between means is statistically significant

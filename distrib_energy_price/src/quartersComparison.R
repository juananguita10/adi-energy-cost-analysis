library(moments)
library(readxl)
library(tseries)
library("nortest")

# Importing Energy data
rm(list=ls()) # Clean the Global Environment
setwd("C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/distrib_energy_price/data")
EnergyPrice20 <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "quartersR")
attach(EnergyPrice20)
head(EnergyPrice20)

# Function passing array of numbers to condition
get_condition_str <- function(arr, nm) {
  if (length(arr) == 0 | nm == "") {
    return("")  
  }
  
  arrStr <- ""
  for (i in arr) {
    if (typeof(i) == "character") {
      i <- paste("'", i, "'", sep="")
    }
    
    if (arrStr == "") {
      arrStr <- paste(arrStr, "(", nm, "==", i)
    } else {
      arrStr <- paste(arrStr, "|", nm, "==", i)
    }
  }
  arrStr <- paste(arrStr, ")")
  
  return(arrStr)
}

# Function returning a subset of the quarters needed
get_subset_energy <- function(qArray=c(), eArray=c()) {
  qStr <- get_condition_str(qArray, "q")
  eStr <- get_condition_str(eArray, "tech")
  
  if (qStr == "" & eStr == "" ) {
    return(price)
  }
  
  if (qStr != "" & eStr != "") {
    cStr <- paste(qStr, eStr, sep=" & ")
  } else if (qStr != "") {
    cStr <- qStr
  } else if (eStr != "") {
    cStr <- eStr
  }
  print(cStr)
  
  return(subset(price, eval(parse(text=cStr))))
}

#----------------------------------- Part 1 ------------------------------
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

# Getting q1 subset
q1 <- get_subset_energy(qArray=c(1))
# Energy price = 35 in Q1 test # Testing the nullity of returns' mean
t.test(q1$price, mu=35) # H0 # mean = 35 -> not rejected

#----------------------------------- Part 2 ------------------------------
# Getting q1 and q2 subset
q1q2 <- get_subset_energy(qArray=c(1,2))
# Comparing variances q1 vs q2
tapply(q1q2$price, q1q2$q, var)
var.test(q1q2$price ~ q1q2$q) # F test
bartlett.test(q1q2$price ~ q1q2$q) # Bartlett test

# Test to compare the means q1 vs q2
tapply(q1q2$price, q1q2$q, mean)
t.test(q1q2$price ~ q1q2$q, paired = FALSE, var.equal=FALSE, conf.level = 0.95)

# Test to compare the means (all the quarters)
tapply(price, q, mean)

tapply(price, q, lillie.test)

bartlett.test(price ~ q)

fitanova1 <- aov(price ~ q, data=EnergyPrice20) # ANOVA test
summary(fitanova1)

kruskal.test(price ~ q, data=EnergyPrice20) # Kruskal-Wallis test





# Example getting tech's subsets
allBG <- get_subset_energy(eArray=c("BG"))
q1BG <- get_subset_energy(qArray=c(1), eArray=c("BG"))

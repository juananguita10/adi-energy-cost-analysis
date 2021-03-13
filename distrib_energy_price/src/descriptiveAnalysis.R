library(readxl)
library(hash)

# Importing Energy data
rm(list=ls()) # Clean the Global Environment
setwd("C:/Projects/ISCTE/ADI/adi-energy-cost-analysis/distrib_energy_price/data")
EnergyPrice20 <- read_excel("OMIE_ES_MARCA_TECNOL_1_01_01_2020_30_04_2020_js.xlsx", sheet = "quartersR")
attach(EnergyPrice20)
head(EnergyPrice20)

# Function returning a hash with descriptive analysis from data
desc_analysis <- function(d) {
  h <- hash()  
  h[["length"]] <- length(d)
  
  if (length(d) > 0) {
    h[["mean"]] <- mean(d)
    h[["median"]] <- median(d)
    h[["quantile"]] <- quantile(d, c(0.01, 0.02, 0.5, 0.70, 0.95, 0.99))
  }
  
  if (length(d) > 1) {
    h[["var"]] <- var(d)
    h[["standard_deviation"]] <- sd(d)
    h[["variant_coefficient"]] <- (sd(d)/mean(d)*100)
  } else {
    h[["var"]] <- 0
    h[["standard_deviation"]] <- 0
    h[["variant_coefficient"]] <- 0
  }

  return(h)
}

complete_quarter_da <- function(qtr) {
  print(paste("Descriptive analysis for quarter", qtr, " (every technology):", sep=" "))
  ds <- subset(EnergyPrice20, q==qtr)
  print(desc_analysis(ds$price))
  print(paste("Descriptive analysis for quarter", qtr, "by Technology:", sep=" "))
  tapply(ds$price, ds$tech, desc_analysis)
}

# Global Descriptive Statistics
(da_global <- desc_analysis(price))

# Descriptive Statistics by quarter
complete_quarter_da(1) #Q1
complete_quarter_da(2) #Q2
complete_quarter_da(3) #Q3
complete_quarter_da(4) #Q4

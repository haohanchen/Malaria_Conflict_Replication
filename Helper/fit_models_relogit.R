################################
# Fit models
# Author: Haohan Chen
################################

library(MASS)
library(tidyverse)
theme_set(theme_minimal())
library(sjPlot)
library(Zelig)
library(texreg)


formula = list()

formula[[1]] <-
  as.formula(sprintf(
    "`%s` ~ 
    `%s tlag` +
    `%s splag` +
    Year + Country", 
    DV, DV, DV))

formula[[2]] <- 
  as.formula(sprintf(
    "`%s` ~ 
    `%s tlag` +
    `%s splag` +
    
    `%s` + 
    
    Year + Country", 
    DV, DV, DV, EV))

formula[[3]] <- 
  as.formula(sprintf(
    "`%s` ~ 
    `%s tlag` +
    `%s splag` +
    
    `%s` + 
    
    `Prop. Forest Area` +
    `Prop. Mountainous area` +
    `Precipitation total` +
    
    Year + Country", 
    DV, DV, DV, EV))

formula[[4]] <- 
  as.formula(sprintf(
    "`%s` ~ 
    `%s tlag` +
    `%s splag` +
    
    `%s` + 
    
    `Prop. Forest Area` +
    `Prop. Mountainous area` +
    `Precipitation total` +
    
    `Nightlight (mean)` +
    `Prop. Urban Area` +
    `Population` +
    `Area irrigated` +

    Year + Country", 
    DV, DV, DV, EV))

formula[[5]] <- 
  as.formula(sprintf(
    "`%s` ~ 
    `%s tlag` +
    `%s splag` +
    
    `%s` + 
    
    `Prop. Forest Area` +
    `Prop. Mountainous area` +
    `Precipitation total` +
    
    `Nightlight (mean)` +
    `Prop. Urban Area` +
    `Population` +
    `Area irrigated` +
    
    `Distance to border` + `Distance to capital` +

    Year + Country", 
    DV, DV, DV, EV))

formula[[6]] <- 
  as.formula(sprintf(
    "`%s` ~ 
    `%s tlag` +
    `%s splag` +
    
    `%s` + 
    
    `Prop. Forest Area` +
    `Prop. Mountainous area` +
    `Precipitation total` +
    
    `Nightlight (mean)` +
    `Prop. Urban Area` +
    `Population` +
    `Area irrigated` +
    
    `Distance to border` + `Distance to capital` +
    
    `Refugee Camps` +

    Year + Country", 
    DV, DV, DV, EV))

model_names <- c("1. AR1", "2. Malaria", "3. Rough", "4. Dev", "5. SC", "6. HM")

# Fit models ----

models <- list()

for (i in seq_along(formula)){
  print(formula[[i]])
  models[[i]] <- zelig(
    formula[[i]],
    model = 'relogit', tau = NULL,
    data = d_out    
  )
}

names(models) <- model_names

rm(formula, i, model_names)


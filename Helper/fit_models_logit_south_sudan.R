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
    Year", 
    DV, DV, DV))

formula[[2]] <- 
  as.formula(sprintf(
    "`%s` ~ 
    `%s tlag` +
    `%s splag` +
    
    `%s` + 
    
    Year", 
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
    
    Year", 
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

    Year", 
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

    Year", 
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

    Year", 
    DV, DV, DV, EV))

model_names <- c("1. AR1", "2. Malaria", "3. Rough", "4. Dev", "5. SC", "6. HM")

# Fit models ----

models <- list()

for (i in seq_along(formula)){
  print(formula[[i]])
  models[[i]] <- glm(
    formula[[i]],
    family = binomial(link = 'logit'),
    data = d_out
  )
}

names(models) <- model_names


# Simulate confidence intervals of coefficients ----

ci <- list()

for (i in 1:length(models)){
  coef <- coef(models[[i]])
  covariance <- vcov(models[[i]])
  ci_sim <- MASS::mvrnorm(10000, coef, covariance)
  ci_sim <- exp(ci_sim)
  ci[[i]] <- t(apply(ci_sim, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))) %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    as_tibble() %>%
    add_column(model = !!(names(models)[i]), .before = 1) %>%
    filter(!str_detect(variable, "Year")) %>%
    filter(!str_detect(variable, "Country"))
}

ci_df <- map_df(ci, bind_rows)

rm(ci, ci_sim, covariance, coef, formula, i, model_names)


# Fit Models with GridID fixed effect ----

formula = list()

formula[[1]] <-
  as.formula(sprintf(
    "`%s` ~
    `%s tlag` +
    `%s splag` +
    Year + GridID",
    DV, DV, DV))

formula[[2]] <-
  as.formula(sprintf(
    "`%s` ~
    `%s tlag` +
    `%s splag` +

    `%s` +

    Year + GridID",
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

    Year + GridID",
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

    Year + GridID",
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

    Year + GridID",
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

    Year + GridID",
    DV, DV, DV, EV))

model_names <- c("1. AR1", "2. Malaria", "3. Rough", "4. Dev", "5. SC", "6. HM")

# Fit models ----

models_GridFE <- list()

for (i in seq_along(formula)){
  print(formula[[i]])
  models_GridFE[[i]] <- glm(
    formula[[i]],
    family = binomial(link = 'logit'),
    data = d_out
  )
}

names(models_GridFE) <- model_names

rm(i, formula)

##############################################
# Load data and prepare them for modeling
# Author: Haohan Chen
##############################################

rm(list=ls())

library(tidyverse)

# load data ----
d <- read_rds("Replication_Data.rds")
d_splag <- read_rds("Helper/splag.rds") %>%
  mutate(`Grid ID` = as.integer(`Grid ID`))

# Transform variables ----

## Standardize independent variables
d_t <- d %>%
  mutate_at(vars(`Malaria risk (Pfpr2-10)`:`Refugee Camps`), 
            ~as.numeric(scale(.)))

# Get temporally lagged independent variables
d_ev_tlag <- d_t %>%  
  dplyr::select(`Grid ID`, `Year`, `Malaria risk (Pfpr2-10)`:`Refugee Camps`) %>%
  group_by(`Grid ID`) %>%
  mutate_at(vars(-Year, -`Grid ID`), ~lag(., n = 1, order_by = Year))


# Include temporally lagged dependent variables
d_dv_tlag <- d_t %>%  
  dplyr::select(`Grid ID`, Year, starts_with("Conflicts")) %>%
  dplyr::select(-ends_with("splag")) %>% # Remove the splag variables
  group_by(`Grid ID`) %>%
  mutate_at(vars(starts_with("Conflicts")), ~lag(., n = 1, order_by = Year)) %>%
  rename_at(vars(starts_with("Conflicts")), ~paste0(., " tlag")) %>%
  ungroup() 

## Put data together

d_out <- d_t %>%
  dplyr::select(-(`Malaria risk (Pfpr2-10)`:`Refugee Camps`)) %>%
  inner_join(d_ev_tlag, by = c("Grid ID", "Year")) %>%
  inner_join(d_dv_tlag, by = c("Grid ID", "Year")) %>%
  inner_join(d_splag, by = c("Grid ID", "Year"))

## Remove 2000 (Missing EV)
d_out <- d_out %>%
  filter(Year != 2000)

## Make Year and Grid ID categorical variables
d_out$Year <- as.factor(d_out$Year)
d_out$`Grid ID` <- as.factor(d_out$`Grid ID`)

# Rename "Grid ID" variable
d_out <- d_out %>% mutate(GridID = `Grid ID`)

rm(d, d_splag, d_dv_tlag, d_ev_tlag, d_t)


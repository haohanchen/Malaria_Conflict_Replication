##############################################
# Statistical Analysis for South Sudan subset
# Author: Haohan Chen
##############################################

rm(list=ls())

library(MASS)
library(tidyverse)
theme_set(theme_minimal())
library(sjPlot)
library(Zelig)
library(texreg)

# Load data ----

d <- read_rds("Replication_Data.rds")
d_splag <- read_rds("Helper/splag.rds") %>%
  mutate(`Grid ID` = as.integer(`Grid ID`))

# Get subset of South Sudan data
d_GridID_SouthSudan <- d %>% filter(Country == "South Sudan") %>%
  dplyr::select(`Grid ID`) %>% distinct()

d <- d %>% inner_join(d_GridID_SouthSudan, by = "Grid ID")
d <- d %>% filter(Year >= 2010)

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

## Include temporally lagged dependent variables
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
  inner_join(d_splag, by = c("Grid ID", "Year")) %>%
  rename("GridID" = "Grid ID")

# Remove 2010 (Start from South Sudan establishment)
d_out <- d_out %>% filter(Year >= 2011)

# Make Year and GridID categorical variables
d_out$Year <- as.factor(d_out$Year)
d_out$GridID <- as.factor(d_out$GridID)

rm(d_splag, d_dv_tlag, d_ev_tlag, d_t, d_GridID_SouthSudan)

# d_out %>%
#   ggplot(aes(x = Lon, y = Lat)) +
#   geom_tile(aes(fill = is.na(`Distance to capital`))) +
#   facet_wrap(~Year)

# 
# 
# ## Plot descriptive statistics
# 
# p1 <- d %>%
#   filter(Country == "South Sudan") %>%
#   group_by(`Grid ID`, Lon, Lat) %>%
#   summarise(`Average malaria risk (Pfpr2-10)` = mean(`Malaria risk (Pfpr2-10)`)) %>%
#   ggplot(aes(x = Lon, y = Lat, fill = `Average malaria risk (Pfpr2-10)`)) +
#   geom_tile() +
#   scale_fill_viridis_c(option = "A") +
#   theme_void() +
#   theme(legend.position = "bottom")
# 
# ggsave("figures_tables_202304/south_sudan/map_prpf2_10.png", plot = p1, width = 5, height = 4)
# 
# p2 <- d %>%
#   group_by(`Grid ID`, Lon, Lat) %>%
#   filter(Country == "South Sudan") %>%
#   summarise(`Years with state-based conflicts` = sum(`Conflicts Occurrence (state-based)`)) %>%
#   ggplot(aes(x = Lon, y = Lat, fill = `Years with state-based conflicts`)) +
#   geom_tile() +
#   scale_fill_viridis_c(option = "A") +
#   theme_void() +
#   theme(legend.position = "bottom")
# 
# ggsave("figures_tables_202304/south_sudan/map_conflict_ucdp_statebased.png", plot = p2, width = 5, height = 4)
# 
# rm(d_splag, d_dv_tlag, d_ev_tlag, d_t)
# 
# # Subset to South Sudan ====
# 
# d_out <- d_out %>%
#   filter(Country == "South Sudan")
# 
# # Specify DV and EV ====
# 
# DV <- "Conflicts Occurrence (state-based)"
# EV <- "Malaria risk (Pfpr2-10)"
# 
# 
# 
# 
# 
# # Fit Models ====
# 
# d_out <- d_out %>% rename("GridID" = "Grid ID")
# 
# formula = list()
# 
# formula[[1]] <-
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     Year", 
#     DV, DV, DV))
# 
# formula[[2]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     Year", 
#     DV, DV, DV, EV))
# 
# formula[[3]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     Year", 
#     DV, DV, DV, EV))
# 
# formula[[4]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     `Nightlight (mean)` +
#     `Prop. Urban Area` +
#     `Population` +
#     `Area irrigated` +
# 
#     Year", 
#     DV, DV, DV, EV))
# 
# formula[[5]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     `Nightlight (mean)` +
#     `Prop. Urban Area` +
#     `Population` +
#     `Area irrigated` +
#     
#     `Distance to border` + `Distance to capital` +
# 
#     Year", 
#     DV, DV, DV, EV))
# 
# formula[[6]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     `Nightlight (mean)` +
#     `Prop. Urban Area` +
#     `Population` +
#     `Area irrigated` +
#     
#     `Distance to border` + `Distance to capital` +
#     
#     `Refugee Camps` +
# 
#     Year", 
#     DV, DV, DV, EV))
# 
# model_names <- c("1. AR1", "2. Malaria", "3. Rough", "4. Dev", "5. SC", "6. HM")
# 
# # Fit models ----
# 
# models <- list()
# 
# for (i in seq_along(formula)){
#   print(formula[[i]])
#   models[[i]] <- glm(
#     formula[[i]],
#     family = binomial(link = 'logit'),
#     data = d_out
#   )
# }
# 
# names(models) <- model_names
# 
# # Output model summary in table ----
# 
# htmlreg(models, omit.coef = "GridID|Year", 
#         caption = sprintf("%s ~ %s", DV, EV),
#         caption.above = TRUE,
#         stars = c(0.01, 0.05, 0.10),
#         booktabs = TRUE,
#         custom.model.names = names(models),
#         file = file.path(output_repo_full, "regression.html")
# )
# 
# 
# # Output predictive interval of explanatory variable ----
# 
# # Plot
# for (i in seq_along(models)){
#   if (i == 1) next
#   p <- plot_model(models[[i]], type = "pred", terms = sprintf("%s [all]", EV))
#   p + ggtitle(NULL) + ylab("Probability of Conflict")
#   ggsave(sprintf("%s/PredictPlot_M_%s.png", output_repo_full, i), width = 5, height = 4)
#   message(i)
# }
# 
# 
# # Simulate confidence intervals of coefficients ----
# 
# ci <- list()
# 
# i <- 1
# for (i in 1:length(models)){
#   coef <- coef(models[[i]])
#   covariance <- vcov(models[[i]])
#   row_col_to_keep <- which(!str_detect(colnames(covariance), "GridID"))
#   
#   coef <- coef[row_col_to_keep]
#   covariance <- covariance[row_col_to_keep, row_col_to_keep]
#   ci_sim <- MASS::mvrnorm(10000, coef, covariance)
#   ci_sim <- exp(ci_sim)
#   ci[[i]] <- t(apply(ci_sim, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))) %>%
#     as.data.frame() %>%
#     rownames_to_column("variable") %>%
#     as_tibble() %>%
#     add_column(model = !!(names(models)[i]), .before = 1) %>%
#     filter(!str_detect(variable, "Year")) %>%
#     filter(!str_detect(variable, "GridID"))
# }
# 
# ci_df <- map_df(ci, bind_rows)
# 
# write_csv(ci_df, file.path(output_repo_full, "CI.csv"))
# 
# 
# # Visualize CI ====
# 
# ci <- read_csv(file.path(output_repo_full, "CI.csv"))
# 
# var_select <- c("Malaria risk (Pfpr2-10)", 
#                 "Prop. Forest Area", "Prop. Mountainous area", "Precipitation total",
#                 "Nightlight (mean)", "Prop. Urban Area", "Population",
#                 "Distance to border", "Distance to capital",
#                 "Refugee Camps")
# 
# var_select <- c("Malaria risk (Pfpr2-10)")
# 
# ci <- ci %>%
#   mutate(variable = str_remove_all(variable, "`")) %>%
#   filter(variable %in% var_select) %>%
#   mutate(variable = factor(variable, 
#                            levels = rev(var_select)))
# 
# 
# p3 <- ci %>%
#   ggplot(aes(color = model)) +
#   geom_hline(yintercept=1, linetype = "dashed", color = "red") +
#   geom_point(aes(y = `50%`, x = variable), position=position_dodge(width=0.3)) +
#   geom_linerange(aes(x = variable, ymin = `2.5%`, ymax = `97.5%`), 
#                  position=position_dodge(width=0.3)) +
#   ylab("Odds Ratio (95% CI)") + xlab("Independent Variable") +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   theme(axis.title.x = element_blank())
# 
# ggsave("figures_tables_202304/south_sudan/ci_main.png", plot = p3, width = 6, height = 3)
# 
# 
# # install.packages("patchwork")
# library(patchwork)
# 
# p_merge <- (p1 | p2) / p3 + plot_annotation(tag_levels = "a")
# p_merge
# 
# ggsave("figures_tables_202304/south_sudan/all.png", plot = p_merge, width = 8, height = 6)
# 
# 
# # Magnitude
# 
# ci %>%
#   filter(variable == "Malaria risk (Pfpr2-10)") %>%
#   mutate(ch = 1 - `50%`) %>%
#   .$ch %>%
#   quantile()
# 
# 
# # Robustness check: Year-GridID Fixed effect
# 
# # Fit Models ====
# 
# formula = list()
# 
# formula[[1]] <-
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     Year + GridID", 
#     DV, DV, DV))
# 
# formula[[2]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     Year + GridID", 
#     DV, DV, DV, EV))
# 
# formula[[3]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     Year + GridID", 
#     DV, DV, DV, EV))
# 
# formula[[4]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     `Nightlight (mean)` +
#     `Prop. Urban Area` +
#     `Population` +
#     `Area irrigated` +
# 
#     Year + GridID", 
#     DV, DV, DV, EV))
# 
# formula[[5]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     `Nightlight (mean)` +
#     `Prop. Urban Area` +
#     `Population` +
#     `Area irrigated` +
#     
#     `Distance to border` + `Distance to capital` +
# 
#     Year + GridID", 
#     DV, DV, DV, EV))
# 
# formula[[6]] <- 
#   as.formula(sprintf(
#     "`%s` ~ 
#     `%s tlag` +
#     `%s splag` +
#     
#     `%s` + 
#     
#     `Prop. Forest Area` +
#     `Prop. Mountainous area` +
#     `Precipitation total` +
#     
#     `Nightlight (mean)` +
#     `Prop. Urban Area` +
#     `Population` +
#     `Area irrigated` +
#     
#     `Distance to border` + `Distance to capital` +
#     
#     `Refugee Camps` +
# 
#     Year + GridID", 
#     DV, DV, DV, EV))
# 
# model_names <- c("1. AR1", "2. Malaria", "3. Rough", "4. Dev", "5. SC", "6. HM")
# 
# # Fit models ----
# 
# models <- list()
# 
# for (i in seq_along(formula)){
#   print(formula[[i]])
#   models[[i]] <- glm(
#     formula[[i]],
#     family = binomial(link = 'logit'),
#     data = d_out
#   )
# }
# 
# names(models) <- model_names
# 
# # Output model summary in table ----
# 
# htmlreg(models, omit.coef = "GridID|Year", 
#         caption = sprintf("%s ~ %s", DV, EV),
#         caption.above = TRUE,
#         stars = c(0.01, 0.05, 0.10),
#         booktabs = TRUE,
#         custom.model.names = names(models),
#         file = file.path(output_repo_full, "regression_gridFE.html")
# )


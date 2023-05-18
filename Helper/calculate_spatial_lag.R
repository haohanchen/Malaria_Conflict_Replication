##########################################
# Purpose: Calculate spatial lag
# Author: Haohan Chen
# Note: This one exclude the t-1 value of self, separate both time and spatial lag.
##########################################

rm(list=ls())

library("RANN")
library(tidyverse)
library(Matrix)

# Load data ----

d <- readRDS("Replication_Data.rds")

# Get list of Grids
d_loc <- d %>% dplyr::select(`Grid ID`, Lon, Lat) %>% distinct()

# # Diagnostics (check that we have all the points)
# d_loc %>%
#   mutate(anomaly = `Grid ID` %in% c("")) %>%
#   ggplot() +
#   geom_tile(aes_string(x = "Lon", y = "Lat")) +
#   coord_fixed()
  

# Get Weight matrix ----

kn = 11 #set k-nearest 
radius = 5 # radois is based on the decimal between long/lat coordinate about 1=78km

W_raw <- RANN::nn2(d_loc %>% dplyr::select(-`Grid ID`), 
               d_loc %>% dplyr::select(-`Grid ID`), 
               k = kn, searchtype = "radius", radius = radius)


# Make sparse matrix (nrow(d) * nrow(d))

# RowIdx, ColIdx, weights
W_idx <- W_raw$nn.idx %>%
  as_tibble() %>%
  mutate(RowIdx = row_number()) %>% # Start from ZERO
  pivot_longer(cols = -"RowIdx", names_to = "vname", values_to = "ColIdx") %>%
  dplyr::select(-vname)

W_dist <- W_raw$nn.dists %>%
  as_tibble() %>%
  mutate(RowIdx = row_number()) %>%
  pivot_longer(cols = -"RowIdx", names_to = "vname", values_to = "dist") %>%
  dplyr::select(-vname)

W_el <- W_idx %>% bind_cols(W_dist %>% dplyr::select(dist)) 

# Check Anomaly: ZERO -- Super large distance
idx_anomaly <- W_el %>%
  filter(ColIdx == 0) %>%
  .$RowIdx %>% unique()

# Check where they are: Isolated points may have super large distance from others
d_loc %>%
  mutate(anomaly = `Grid ID` %in% !!(d_loc$`Grid ID`[idx_anomaly])) %>%
  ggplot() +
  geom_tile(aes_string(x = "Lon", y = "Lat", fill = "anomaly")) +
  coord_fixed()
# Results: No anomaly!

summary(W_el)

# Clean W_el (remove anomaly and self)
W_el <- W_el %>%
  filter(ColIdx != 0) %>%
  filter(RowIdx != ColIdx)

# Clean trash
rm(W_idx, W_dist)

summary(W_el)

# Normalize W
W_el <- W_el %>%
  group_by(RowIdx) %>%
  mutate(weight_1 = 1 / n(), weight_2 = (1/dist) / sum(1/dist))


W <- Matrix::sparseMatrix(
  i = W_el$RowIdx,
  j = W_el$ColIdx,
  x = W_el$weight_1,
  dim = rep(nrow(d_loc), 2),
  dimnames = list(d_loc$`Grid ID`, d_loc$`Grid ID`)
)

rm(W_el, W_raw, d_loc, idx_anomaly, kn, radius)


# Get the matrix of variable for spatial lag ----

get_splag <- function(df, weight, varname){
  Y_df <- df %>%
    dplyr::select("Grid ID", "Year", !!(varname)) %>%
    pivot_wider(names_from = "Year", values_from = !!(varname))
  
  # Reorder rows (make sure it matches with the matrix's order)
  Y_df <- Y_df %>%
    slice(match(as.character(Y_df$`Grid ID`), rownames(weight)))
  
  Y <- Y_df %>% dplyr::select(as.character(2000:2019)) %>% as.matrix()
  rownames(Y) <- Y_df$`Grid ID`
  
  splag <- W %*% Y
  
  splag_df <- as.matrix(splag) %>% as_tibble() %>%
    mutate(`Grid ID` = rownames(splag)) %>%
    mutate(varname = !!(paste(varname, "splag")))

  return(splag_df)  
}

varnames <- names(d)
varnames <- varnames[str_detect(varnames, "^Conflicts")]
# print(varnames)

# Generate outputs ----

out_ls <- map(varnames, ~get_splag(d, W, .))

d_splag <- do.call(bind_rows, out_ls) %>%
  pivot_longer(cols = -c("Grid ID", "varname"), names_to = "Year", values_to = "value") %>%
  pivot_wider(names_from = "varname", values_from = "value") %>%
  mutate(Year = as.integer(Year)) %>%
  drop_na() # Some points have no neighbours

saveRDS(d_splag, "Helper/splag.rds")

# Re-load output ----

rm(out_ls, W, varnames, get_splag)

d_splag <- read_rds("Helper/splag.rds")

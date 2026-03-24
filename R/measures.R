# R/prep.R
library(tidyverse)
library(here)

# euclidean_distance ----
euclidean_distance <- function(x1, y1, x2, y2) {
  df <- data.frame(x1, y1, x2, y2) %>%
    dplyr::mutate(euc_dist = sqrt((x2 - x1)^2 + (y2 - y1)^2)) %>%
    dplyr::pull(euc_dist)
  
  return(df)
}
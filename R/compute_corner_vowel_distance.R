library(tidyverse)
library(emuR)
library(here)
source(here::here("R/measures.R"))

get_midpoint <- function(data, measure) {
  
  midpoint_data <- data %>%
    dplyr::mutate(midpoint = (max(time, na.rm = T) - min(time, na.rm = T)) / 2 + min(time, na.rm = T),
                  midpoint_diff = abs(time - midpoint)) %>%
    dplyr::filter(which.min(midpoint_diff) == row_number())
  
  midpoint_data <- midpoint_data %>%
    pull({{measure}}) %>%
    as.numeric()
  
  return(midpoint_data)
  
}

compute_corner_vowel_distance <- function(data, speakers, spec_name, saveRDS) {
  midpoint_data <- data %>%
    dplyr::filter(label == "a" | label == "i" | label == "ae" | label == "o") %>% 
    dplyr::group_by(database_id, group, condition, phrase, label, token) %>% 
    nest() %>%
    dplyr::rename(aco_data = data) %>%
    dplyr::mutate(time_midpoint = as.numeric(map(aco_data, get_midpoint, measure = time)),
                  f1_midpoint = as.numeric(map(aco_data, get_midpoint, measure = f1)),
                  f2_midpoint = as.numeric(map(aco_data, get_midpoint, measure = f2)),
                  f3_midpoint = as.numeric(map(aco_data, get_midpoint, measure = f3))
    ) %>%
    dplyr::mutate(rep = base::sub(pattern = ".*[.]",
                                  replacement = "",
                                  x = phrase),
                  rep = as.numeric(rep) + 1,
                  rep = case_when(
                    is.na(rep) ~ 1,
                    TRUE ~ rep,
                  ),
                  phrase = base::sub(pattern = "[.].*",
                                     replacement = "",
                                     x = phrase)) %>%
    dplyr::rename(f1 = f1_midpoint,
                  f2 = f2_midpoint
    ) %>%
    dplyr::mutate(f1_bark = emuR::bark(f1),
                  f2_bark = emuR::bark(f2)) %>%
    dplyr::group_by(database_id,
                    group,
                    condition,
                    label,
    ) %>%
    dplyr::summarise(f1 = mean(f1, na.rm = T),
                     f2 = mean(f2, na.rm = T),
                     f1_bark = mean(f1_bark, na.rm = T),
                     f2_bark = mean(f2_bark, na.rm = T),
                     .groups = "drop"
    )
  
  center_data <- midpoint_data %>%
    dplyr::filter(condition == "conv") %>%
    dplyr::group_by(database_id,
                    group) %>%
    dplyr::summarize(
      center_f1_bark = mean(f1_bark),
      center_f2_bark = mean(f2_bark),
      center_f1 = mean(f1),
      center_f2 = mean(f2),
      .groups = "drop")
  
  corner_vowel_distance <- midpoint_data %>%
    base::merge(., center_data, all = T) %>%
    dplyr::group_by(database_id, group, condition) %>%
    dplyr::mutate(
      corner_vowel_distance = euclidean_distance(
        x1 = center_f2_bark,
        y1 = center_f1_bark,
        x2 = f2_bark,
        y2 = f1_bark
      ),
      corner_vowel_distance_unit = "Bark",
    ) %>%
    dplyr::filter(condition != "lessClear") %>%
    base::merge(speakers, ., all = T)
  
  vowel_data <- list(
    midpoint_data = midpoint_data,
    corner_vowel_distance = corner_vowel_distance
  )
  
  if (saveRDS == T) {
    saveRDS(
      vowel_data,
      here::here("manuscript", spec_name, "00_data", "data_vowel.RDS")
    )
  }
  return(vowel_data)
  
}

prep_corner_vowel_distance_data <- function(corner_vowel_distance_data, db_data) {
  corner_vowel_distance <- corner_vowel_distance_data %>%
    base::merge(., db_data %>%
                  dplyr::group_by(database_id, sex, group, severity,
                                age,
                                condition) %>%
                  dplyr::summarise(db = mean(db, na.rm = T),
                                   .groups = "drop")) %>%
    dplyr::group_by(database_id) %>%
    dplyr::mutate(db_centered = scale(db, center = T, scale = F)[,1]) %>%
    dplyr::ungroup()
  
  return(corner_vowel_distance)
}

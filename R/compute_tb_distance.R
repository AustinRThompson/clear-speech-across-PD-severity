# R/compute_tb_distance.R
library(tidyverse)

ai_kin <- function(data, measure) {
  
  # Kinematic Distance ----
  onsetData <- data %>%
    dplyr::filter(row_number() == which.min(data$time))
  
  offsetData <- data %>%
    dplyr::filter(row_number() == which.max(data$time))
  
  # Speed
  data <- data %>%
    dplyr::mutate(time_diff = diff(c(NA,time)),
                  tb_dist = c(NA,base::sqrt(diff(.$tb_x)^2 + diff(.$tb_y)^2)),
                  tb_speed = tb_dist/time_diff,
                  dtb_dist = c(NA,base::sqrt(diff(.$decoupled_tb_x)^2 + diff(.$decoupled_tb_y)^2)),
                  dtb_speed = dtb_dist/time_diff,
                  jaw_dist = c(NA,base::sqrt(diff(.$jaw_x)^2 + diff(.$jaw_y)^2)),
                  jaw_speed = jaw_dist/time_diff,
    )
  
  
  movement_measures <- tibble() %>%
    add_row() %>%
    dplyr::mutate(
      onset = min(data$time),
      offset = max(data$time),
      duration = offset - onset,
      # X and Y for Kinematic Distance
      onset_tb_x = onsetData$tb_x,
      onset_tb_y = onsetData$tb_y,
      offset_tb_x = offsetData$tb_x,
      offset_tb_y = offsetData$tb_y,
      ## tb measures
      #tb_speed_avg = base::mean(data$tb_speed, na.rm = T),
      #tb_speed_max = base::max(data$tb_speed, na.rm = T),
      tb_distance = base::sum(data$tb_dist, na.rm = T),
      #tb_displacement = euclidean_distance(x1 = onsetData$tb_x,
      #                                          y1 = onsetData$tb_y,
      #                                          x2 = offsetData$tb_x,
      #                                          y2 = offsetData$tb_y),
      ## dtb measures
      #dtb_speed_avg = base::mean(data$dtb_speed, na.rm = T),
      #dtb_speed_max = base::max(data$dtb_speed, na.rm = T),
      #dtb_distance = base::sum(data$dtb_dist, na.rm = T),
      #dtb_displacement = euclidean_distance(x1 = onsetData$decoupled_tb_x,
      #                                           y1 = onsetData$decoupled_tb_y,
      #                                           x2 = offsetData$decoupled_tb_x,
      #                                           y2 = offsetData$decoupled_tb_y),
      ## jaw measures
      #jaw_speed_avg = base::mean(data$jaw_speed, na.rm = T),
      #jaw_speed_max = base::max(data$jaw_speed, na.rm = T),
      #jaw_distance = base::sum(data$jaw_dist, na.rm = T),
      #jaw_displacement = euclidean_distance(x1 = onsetData$jaw_x,
      #                                           y1 = onsetData$jaw_y,
      #                                           x2 = offsetData$jaw_x,
      #                                           y2 = offsetData$jaw_y),
    )
  
  
  movement_measures <- movement_measures %>%
    pull({{measure}})
  
  return(movement_measures)
  
}

compute_tb_distance <- function(movement_data, speakers, spec_name, saveRDS) {
  diphthong_data <- movement_data %>%
    dplyr::filter(label == "ai") %>%
    arrange(time) %>%
    arrange(database_id) %>%
    dplyr::group_by(database_id,condition,token) %>%
    dplyr::mutate(
      time_min_x = ifelse(length(tb_x) > 0, time[which.min(tb_x)], NA),
      time_max_x = ifelse(length(tb_x) > 0, time[which.max(tb_x)], NA),
      trim = case_when(
        time < time_min_x ~ TRUE,
        time > time_max_x ~ TRUE,
        TRUE ~ FALSE
      ),
      trim = if_else(database_id == "Sub25", FALSE, trim)) %>% #Sub25 has some strange /ai/ movement
    #dplyr::relocate(trim, .after = database_id) %>%
    dplyr::ungroup() %>%
    dplyr::filter(trim != TRUE) %>% 
    group_by(database_id,group,condition,phrase,label,token, time_min_x, time_max_x) %>% 
    nest() %>%
    dplyr::ungroup() %>%
    dplyr::rename(kin_data = data) %>%
  
  # apply the function to each group defined by DatabaseID, condition, and token
    dplyr::mutate(
      onset = as.numeric(map(kin_data, ai_kin, measure = onset)),
      offset = as.numeric(map(kin_data, ai_kin, measure = offset)),
      duration = offset - onset,
      # tb measures
      tb_distance = map(kin_data, ai_kin, measure = tb_distance)
    ) %>%
    dplyr::mutate(tb_distance = ifelse(tb_distance==0,NA,tb_distance),
                  tb_distance_unit = "mm") %>%
    
    # Cleaning up the dataframe
    dplyr::select(database_id, group, condition, phrase, label, token, onset, offset, duration, tb_distance, tb_distance_unit) %>%
    base::merge(speakers,.) %>%
    dplyr::arrange(database_id, condition, token)
  
  if (saveRDS == T) {
    saveRDS(
      diphthong_data,
      here::here("manuscript", spec_name, "00_data", "data_diphthong.RDS")
    )
  }
  return(diphthong_data)
}


prep_tb_distance_data <- function(tb_distance_data, db_data) {
  tb_distance <- tb_distance_data %>%
    dplyr::mutate(tb_distance = as.numeric(tb_distance)) %>%
    dplyr::filter(!is.na(tb_distance)) %>%
    dplyr::ungroup() %>%
    base::merge(
      .,
      db_data %>%
        dplyr::ungroup() %>%
        dplyr::filter(grepl(pattern = "bsent", phraseToken) == T) %>%
        dplyr::select(database_id,
                      group,
                      severity,
                      condition,
                      sex,
                      phrase = phraseToken,
                      db),
    ) %>%
    dplyr::group_by(database_id) %>%
    dplyr::mutate(db_centered = scale(db, center = T, scale = F)[,1],
                  tb_distance = as.numeric(tb_distance)) %>%
    dplyr::ungroup()
  
  return(tb_distance)
}

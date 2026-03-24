# R/compute_tf_distance.R
library(tidyverse)

cvc_kin <- function(data, measure) {
  
  # Kinematic Distance ----
  onsetData <- data %>%
    dplyr::filter(row_number() == which.min(data$time))
  
  offsetData <- data %>%
    dplyr::filter(row_number() == which.max(data$time))
  
  # Speed
  data <- data %>%
    dplyr::mutate(time_diff = diff(c(NA,time)),
                  tf_dist = c(NA,base::sqrt(diff(.$tf_x)^2 + diff(.$tf_y)^2)),
                  tf_speed = tf_dist/time_diff,
                  dtf_dist = c(NA,base::sqrt(diff(.$decoupled_tf_x)^2 + diff(.$decoupled_tf_y)^2)),
                  dtf_speed = dtf_dist/time_diff,
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
      ## tf measures
      tf_distance = base::sum(data$tf_dist, na.rm = T),
      ## tb measures
      #tb_speed_avg = base::mean(data$tb_speed, na.rm = T),
      #tb_speed_max = base::max(data$tb_speed, na.rm = T),
      #tb_distance = base::sum(data$tb_dist, na.rm = T),
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

compute_tf_distance <- function(movement_data, speakers, spec_name, saveRDS) {
  cvc_data <- movement_data %>%
    arrange(time) %>%
    arrange(database_id) %>%
    group_by(database_id,group,condition,phraseToken,word,word_token) %>% 
    nest() %>%
    dplyr::ungroup() %>%
    dplyr::rename(kin_data = data) %>%
  
  # apply the function to each group defined by DatabaseID, condition, and word_token
    dplyr::mutate(
      onset = as.numeric(map(kin_data, cvc_kin, measure = onset)),
      offset = as.numeric(map(kin_data, cvc_kin, measure = offset)),
      duration = offset - onset,
      # tf measures
      tf_distance = map(kin_data, cvc_kin, measure = tf_distance)
    ) %>%
    dplyr::mutate(tf_distance = ifelse(tf_distance==0,NA,tf_distance),
                  tf_distance_unit = "mm") %>%
    
    # Cleaning up the dataframe
    dplyr::select(database_id, group, condition, phraseToken, word, word_token, onset, offset, duration, tf_distance, tf_distance_unit) %>%
    base::merge(speakers,.) %>%
    dplyr::arrange(database_id, condition, word_token)
  
  if (saveRDS == T) {
    saveRDS(
      cvc_data,
      here::here("manuscript", spec_name, "00_data", "data_cvc_distance.RDS")
    )
  }
  return(cvc_data)
}


prep_tf_distance_data <- function(tf_distance_data, db_data) {
  tf_distance <- tf_distance_data %>%
    dplyr::mutate(tf_distance = as.numeric(tf_distance)) %>%
    dplyr::filter(!is.na(tf_distance)) %>%
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
                  tf_distance = as.numeric(tf_distance)) %>%
    dplyr::ungroup()
  
  return(tf_distance)
}

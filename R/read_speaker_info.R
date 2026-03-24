library(tidyverse)

read_speaker_info <- function(spec_name, saveRDS) {
  speaker_info <- read.csv(file = here::here("data","00_raw_data","listener_data","02_Thompson_2024_data","thompson-2024_listener-ratings.csv")) %>%
    dplyr::select(!X) %>%
    dplyr::select(
      database_id = DatabaseID,
      study_id = StudyID,
      group = Group,
      sex = Sex,
      age = Age,
      token = Token,
      condition,
      sentence = Sent,
      rating_type = ratingType,
      rating = Rating
    ) %>%
    dplyr::group_by(group, database_id, sex, age, condition, rating_type) %>%
    dplyr::summarise(rating_for_severity = mean(rating),
                     .groups = "drop") %>%
    dplyr::filter(condition == "conv",
                  rating_type == "Int") %>%
    dplyr::mutate(
      severity = case_when(
        rating_for_severity > 94 ~ "Normal",
        between(rating_for_severity, 85, 94.99) ~ "Mild",
        between(rating_for_severity, 70, 84.99) ~ "Moderate",
        between(rating_for_severity, 45, 69.99) ~ "Severe",
        rating_for_severity < 45 ~ "Profound"
      ),
      severity = ifelse(group == "HC", "Control", severity),
      severity = factor(severity, levels = c("Control","Mild","Moderate","Severe","Profound"))
    ) %>%
    dplyr::select(database_id, sex, age, group, severity)
  
  if (saveRDS == T) {
    saveRDS(speaker_info,
            here::here("manuscript", spec_name, "00_data", "speaker_info.RDS"))
  }
  return(speaker_info)
}

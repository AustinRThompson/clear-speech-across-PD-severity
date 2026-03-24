library(here)
library(tidyverse)
library(hms) # install.packages("hms")

read_listener_info <- function(spec_name, saveRDS) {
  listener_info <- list.files(
    path = here("data", "00_raw_data", "listener_data", "01_gorilla_data"),
    full.names = TRUE,
    pattern = "question"
  ) |>
    rio::import() |>
    dplyr::select(
      data_time   = `UTC Date and Time`,
      gorilla_id  = `Participant Private ID`,
      question    = Question,
      response_type = `Response Type`,
      key         = Key,
      response    = Response
    ) |>
    dplyr::filter(
      response_type == "response",
      question != "What is your Prolific ID?",
      key != "quantised",
      response != 0
    ) |>
    dplyr::mutate(
      question_id = case_when(
        question == "How old are you (in years)?" ~ "age",
        question == "What is your gender identity?" ~ "gender",
        question == "What is your race?" ~ "race",
        question == "What is your ethnicity?" ~ "ethnicity",
        grepl(pattern = "This study requires listening to audio samples.", question) == T ~ "quiet_environment",
        grepl(pattern = "It is recommended that you use earphones", question) == T ~ "audio_source"
      ),
      response = case_when(
        question_id == "gender" & response == 1 ~ key,
        question_id == "race" & response == 1 ~ key,
        TRUE ~ response
      ),
      date = sub(" .*$", "", data_time),
      time = paste0(sub(".* ", "", data_time), ":00"),
      time = as_hms(time)
    ) %>%
    dplyr::group_by(gorilla_id) %>%
    dplyr::mutate(start_time = as_hms(min(time))) %>%
    dplyr::group_by(date, start_time, gorilla_id, question_id) %>%
    dplyr::summarise(response = paste(response, collapse = ", "),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = question_id, values_from = response) %>%
    dplyr::mutate(age = as.numeric(age))
  
  if (saveRDS == T) {
    saveRDS(
      listener_info,
      here::here("manuscript", spec_name, "00_data", "listener_info.RDS")
    )
  }
  return(listener_info)
}

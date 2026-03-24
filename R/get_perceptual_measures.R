library(hms)
library(tidyverse)

get_listener_ratings <- function() {
  # Loading the files for the new perceptual data
  file_names <- list.files(path = here::here("Data","00_raw_data","listener_data","01_gorilla_data/"), full.names = T, pattern = "task")
  
  all_listener_ratings <- base::do.call(rbind,lapply(file_names,read.csv)) %>%
    as.data.frame() %>%
    dplyr::select(gorilla_id = `Participant.Private.ID`,
                  data_time = `UTC.Date.and.Time`,
                  list = `counterbalance.4zxg`,
                  status = `Participant.Status`,
                  display = Display,
                  response_type = `Response.Type`,
                  rating = Response,
                  trial_number = `Trial.Number`,
                  group = `Spreadsheet..Group`,
                  database_id = `Spreadsheet..DatabaseID`,
                  stimuli = `Spreadsheet..label`,
                  Audio1 = `Spreadsheet..Audio1`,
                  Audio2 = `Spreadsheet..Audio2`,
                  Audio3 = `Spreadsheet..Audio3`,
                  slider_condition = `Object.Name`) %>%
    dplyr::filter(response_type == "response") %>%
    dplyr::filter(display != "Instructions_Int") %>%
    tidyr::pivot_longer(
      cols = Audio1:Audio3,
      values_to = "audio_file",
      names_to = "audio_num"
    ) %>%
    dplyr::mutate(
      slider_condition = gsub(pattern = "Int_",
                              replacement = "",
                              slider_condition),
      condition = case_when(
        grepl(pattern = "conv", audio_file) ~ "conv",
        grepl(pattern = "lessClear", audio_file) ~ "lessClear",
        grepl(pattern = "moreClear", audio_file) ~ "moreClear"
      )) %>%
    dplyr::filter(condition != "lessClear") %>%
    dplyr::filter(audio_num == slider_condition) %>%
    dplyr::filter(status == "complete") %>%
    dplyr::mutate(rating = as.numeric(rating),
                  rating_type = ifelse(grepl(pattern = "Int",
                                             x = display),
                                       "Int",
                                       "AP"),
                  gorilla_id = factor(gorilla_id),
                  date = sub(" .*$", "", data_time),
                  time = paste0(sub(".* ", "", data_time),":00"),
                  time = as_hms(time)) %>%
    dplyr::select(date, time,
                  gorilla_id,
                  list,
                  display,
                  database_id,
                  group,
                  stimuli,
                  condition,
                  audio_file,
                  rating_type,
                  rating) %>%
    dplyr::group_by(gorilla_id) %>%
    dplyr::mutate(end_time = hms::as_hms(max(time))) %>%
    dplyr::ungroup()
  
  return(all_listener_ratings)
}

check_listener_quality <- function(all_listener_ratings, spec_name, saveRDS) {
  listener_reliability_trials <- all_listener_ratings %>%
    dplyr::filter(grepl(
      pattern = "rel",
      x = display
    )) %>%
    dplyr::rename(
      rel_rating = rating
    ) %>%
    dplyr::select(gorilla_id,
                  database_id,
                  group,
                  stimuli,
                  condition,
                  audio_file,
                  rating_type,
                  rel_rating)
  
  # Exclude the reliability ratings
  listener_ratings <- all_listener_ratings %>%
    dplyr::filter(!grepl(
      pattern = "rel",
      x = display
    )) %>%
    dplyr::select(!display)
  
  # Identifying poor quality listeners
  qc_failed_listeners <- base::merge(listener_ratings, listener_reliability_trials) %>%
    dplyr::mutate(difference = abs(rating - rel_rating)) %>%
    # Calculate each listeners' total error
    dplyr::group_by(gorilla_id) %>%
    dplyr::summarize(listener_error = mean(difference, na.rm = T),
                     .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(listener_error_z = as.numeric(
      abs(scale(listener_error)))) %>%
    dplyr::filter(listener_error_z > 2) %>%
    dplyr::ungroup()
  
  # Remove poor quality listeners
  qc_passed_listeners <- base::merge(listener_ratings, listener_reliability_trials, all = T) %>%
    dplyr::filter(!gorilla_id %in% qc_failed_listeners$gorilla_id) %>%
    dplyr::select(gorilla_id, database_id, group, condition, stimuli, audio_file, list, rating_type, rating, rel_rating) %>%
    dplyr::mutate(
      rep = str_extract(audio_file, "\\d+(?=\\.[^.]+$)") |> as.integer())
  
  # Getting data for intra-rater reliability checks
  intra_rel_data <- all_listener_ratings %>%
    dplyr::filter(!gorilla_id %in% qc_failed_listeners$gorilla_id) %>%
    dplyr::select(gorilla_id, database_id, group, display, condition, stimuli, audio_file, list, rating_type, rating)
  
  all_ratings <- list(
    qc_passed = qc_passed_listeners,
    qc_failed  = qc_failed_listeners
  )
  
  if(saveRDS == T) {
    saveRDS(all_ratings,here::here("manuscript",spec_name,"00_data","listener_ratings.RDS"))
  }
  return(all_ratings)
  
}


get_perceptual_measures <- function(listeners, speaker_info, spec_name, saveRDS) {
  
  # Aggregate the new ratings by speaker, rating type, and condition
  speaker_ratings <- listeners %>%
    dplyr::group_by(group, database_id, condition, stimuli, audio_file, rating_type) %>%
    dplyr::summarise(
      rating_n = NROW(rating),
      rating = mean(rating, na.rm = T),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      rep = str_split(audio_file, '_', simplify = TRUE)[,4],
      rep = gsub(pattern = ".mp3", replacement = "", x = rep)) %>%
    dplyr::select(
      group,
      database_id,
      condition,
      sentence = stimuli,
      audio_file,
      rating_type,
      rep,
      rating,
      rating_n
    ) %>%
    dplyr::group_by(group, database_id, condition, rating_type) %>%
    dplyr::summarise(
      rating_n = sum(rating_n),
      rating = mean(rating),
      .groups = "drop") %>%
    base::merge(speaker_info, .)
  
  all_quality_listener_ratings <- base::merge(speaker_info, listeners) %>%
    dplyr::select(gorilla_id,
                  database_id,
                  sex,
                  group,
                  severity,
                  condition,
                  stimuli,
                  audio_file,
                  rep,
                  list,
                  rating_type,
                  rating) %>%
    base::merge(speaker_info, .)
  
  data_perceptual <- list(
    summarized_by_speaker = speaker_ratings,
    all_quality_listener_ratings = all_quality_listener_ratings
  )
  
  if (saveRDS == T) {
    saveRDS(
      data_perceptual,
      here::here("manuscript", spec_name, "00_data", "data_perceptual.RDS")
    )
  }
  return(data_perceptual)
  
}

prep_perceptual_model_data <- function(perceptual_data, measure, db_data) {
  epsilon <- 1e-5
  if(measure == "intelligibility") {
    perceptual_data %>%
      dplyr::filter(rating_type == "Int") %>%
      dplyr::rename(intelligibility = rating) %>%
      dplyr::select(
        database_id,
        group,
        sex,
        age,
        severity,
        condition,
        stimuli,
        rep,
        gorilla_id,
        intelligibility
      ) %>%
      dplyr::mutate(
        intelligibility_01 = intelligibility / 100,
        # the following makes sure that 0 and 1 are not included in the beta distribution
        intelligibility_01 = intelligibility_01 * ((nrow(.) - 1) + .5) / nrow(.),
        intelligibility_01 = intelligibility_01 * (1 - 2 * epsilon) + epsilon,
        listener_id = gorilla_id
      ) %>%
      base::merge(., db_data %>%
                    dplyr::select(database_id, sex, group, severity,
                                  age,
                                  condition,
                                  stimuli = label,
                                  rep,
                                  db
                                  )) %>%
      dplyr::group_by(database_id) %>%
      dplyr::mutate(db_centered = scale(db, center = T, scale = F)[,1]) %>%
      dplyr::ungroup()
  } else if (measure == "precision") {
   perceptual_data %>%
      dplyr::filter(rating_type == "AP") %>%
      dplyr::rename(precision = rating) %>%
      dplyr::select(
        database_id,
        group,
        sex,
        age,
        severity,
        condition,
        stimuli,
        rep,
        gorilla_id,
        precision
      ) %>%
      dplyr::mutate(
        precision_01 = precision / 100,
        precision_01 = precision_01 * ((nrow(.) - 1) + .5) / nrow(.),
        precision_01 = precision_01 * (1 - 2 * epsilon) + epsilon,
        listener_id = gorilla_id
      ) %>%
      base::merge(., db_data %>%
                    dplyr::select(database_id, sex, group, severity,
                                  age,
                                  condition,
                                  stimuli = label,
                                  rep,
                                  db
                    )) %>%
      dplyr::group_by(database_id) %>%
      dplyr::mutate(db_centered = scale(db, center = T, scale = F)[,1]) %>%
      dplyr::ungroup()
  }
  
}

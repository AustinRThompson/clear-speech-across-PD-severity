library(tidyverse)

get_database_files <- function(speaker_info) {
  speakers <- speaker_info %>%
    
    # Making sure dxTime is numeric
    dplyr::select(database_id, severity) %>%
    distinct() %>%
    dplyr::mutate(has_ratings = T)
  
  database_files <- rio::import(file = here::here("data","00_raw_data","File Inventories.xlsx")) %>%
    dplyr::select(
      database_id = `Sub #`,
      group = Group,
      sex = Sex,
      age = Age,
      sentence_rep = `Sentence Repetition`,
      sentence_rep_backup = `Sentence Repetition_2`,
    ) %>%
    dplyr::mutate(across(everything(), ~ ifelse(grepl("\\*", .), NA, .)),
                  sentence_rep = case_when(
                    database_id == "Sub25" ~ sentence_rep_backup,
                    TRUE ~ sentence_rep
                  )) %>%
    dplyr::select(!contains("_backup")) %>%
    dplyr::mutate(group = as.factor(group),
                  sex = as.factor(sex),
                  age = as.numeric(age)) %>%
    dplyr::filter(age > 35) %>%
    base::merge(., speakers, all = T) %>%
    dplyr::filter(!is.na(sentence_rep),
                  has_ratings == TRUE) %>%
    dplyr::select(database_id,
                  group,
                  severity,
                  sex,
                  age,
                  sentence_rep) %>%
    dplyr::mutate(sentence_rep = sub("_sync", "", sentence_rep),
                  audio_folder = sub("_(?!.*_).*", "", sentence_rep, perl = TRUE))
  
  return(database_files)
}

collect_source_files <- function(database_files) {
  k <- 1
  while(k <= NROW(database_files)) {
    speaker_file <- database_files[k,]
    
    speaker_folder <- list.files(path = "../../Databases/Coartic-Database",
                                 pattern = speaker_file$database_id,
                                 full.names = T,
                                 ignore.case = T)
    
    speaker_audio_file <- list.files(file.path(speaker_folder, speaker_file$audio_folder),
                                     pattern = speaker_file$sentence_rep,
                                     ignore.case = T,
                                     full.names = T) %>%
      .[grepl(".wav", .)]
    
    speaker_movement_file <- list.files(file.path(speaker_folder, "Decoupled Movement"),
                                        pattern = paste0(speaker_file$sentence_rep),
                                        ignore.case = T,
                                        full.names = T)
    
    # Copy files
    file.copy(
      from = speaker_audio_file,
      to = paste0(
        "Data/00_raw_data/speaker_data/00_audio_files/",
        sub(".*/", "", speaker_audio_file)
      ),
      overwrite = F,
    )
    
    file.copy(
      from = speaker_movement_file,
      to = paste0(
        "Data/00_raw_data/speaker_data/00_movement_files/",
        sub(".*/", "", speaker_movement_file)
      ),
      overwrite = F
    )
    
    k <- k + 1
    rm(speaker_file,
       speaker_folder,
       speaker_audio_file,
       speaker_movement_file)
  }
}

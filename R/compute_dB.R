# R/compute_dB.R

library(tidyverse)
library(rPraat)
library(tuneR)
library(seewave)
source(here::here("R/collect_source_files.R"))
source(here::here("R/read_speaker_info.R"))

compute_db <- function(speakers, tg_path, out_path, spec_name, saveRDS) {
  
  speakers <- get_database_files(speakers)
  
  n <- NROW(speakers)
  if (n == 0) {
    return(list(acoustic = tibble(), movement = tibble()))
  }
  
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)
  
  NC <- 1
  
  while (NC <= NROW(speakers)) {
    # IDing Speaker & Pulling their info from the master File Inventories.xlsx
    speaker <- speakers[NC,]
    
    audio_file <- list.files(path = here::here("../../Databases/Coartic-Database/",
                                        speaker$database_id,speaker$audio_folder),
                             pattern = paste0(speaker$sentence_rep,"_sync.wav"),
                             ignore.case = T,
                             full.names = T)
    
    snd <- suppressWarnings(tuneR::readWave(audio_file))
    
    
    tg_file <- list.files(path = tg_path,
                          pattern = speaker$sentence_rep,
                          full.names = TRUE,
                          ignore.case = TRUE)
    
    # Reading TextGrid
    tg <- rPraat::tg.read(fileNameTextGrid = tg_file)
    
    condition <- base::cbind(tg[["condition"]][["t1"]],
                             tg[["condition"]][["t2"]],
                             tg[["condition"]][["label"]]) %>%
      as.data.frame() %>%
      dplyr::rename(onset = 1,
                    offset = 2,
                    label = 3) %>%
      dplyr::mutate(onset = base::as.numeric(onset),
                    offset = base::as.numeric(offset),
                    label = base::gsub(pattern = " ",
                                       replacement = "",
                                       x = label),
                    label = base::tolower(label)) %>%
      dplyr::filter(label != "")
    
    phrase <- base::cbind(tg[["phrase"]][["t1"]],
                          tg[["phrase"]][["t2"]],
                          tg[["phrase"]][["label"]]) %>%
      as.data.frame() %>%
      dplyr::rename(onset = 1,
                    offset = 2,
                    label = 3) %>%
      dplyr::mutate(onset = base::as.numeric(onset),
                    offset = base::as.numeric(offset),
                    label = base::gsub(pattern = " ",
                                       replacement = "",
                                       x = label),
                    label = base::tolower(label),
                    condition = case_when(
                      onset > condition$onset[condition$label == "conv"] &
                        offset < condition$offset[condition$label == "conv"] ~ "conv",
                      onset > condition$onset[condition$label == "moreclear"] &
                        offset < condition$offset[condition$label == "moreclear"] ~ "moreClear",
                      onset > condition$onset[condition$label == "lessclear"] &
                        offset < condition$offset[condition$label == "lessclear"] ~ "lessClear",
                    )) %>%
      dplyr::filter(label != "") %>%
      dplyr::group_by(condition) %>%
      dplyr::mutate(phraseToken = make.unique(label)) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(condition, .after = phraseToken)
    
    k <- 1
    while(k <= NROW(phrase)) {
      target_phrase <- phrase %>% slice(k) %>%
        dplyr::mutate(database_id = speaker$database_id,
                      group = speaker$group,
                      severity = speaker$severity,
                      sex = speaker$sex,
                      age = speaker$age)
      
      # Compute dB
      snd_segment <- tuneR::extractWave(snd,
                                        from = target_phrase$onset,
                                        to = target_phrase$offset, 
                                        xunit = "time")
      
      rms_val <- rms(snd_segment@left)  # raw RMS amplitude (0–1 scale if normalized)
      
      # 4. Convert to dB SPL relative to full scale (FS)
      dB <- 20 * log10(rms_val)
      
      target_phrase <- target_phrase %>%
        dplyr::mutate(db = dB)
      
      
      if(k == 1) {
        speaker_phrases <- target_phrase
      } else {
        speaker_phrases <- rbind(speaker_phrases, target_phrase)
      }
      
      rm(target_phrase, dB, rms_val, snd_segment)
      k <- k + 1
    }
    
    if(NC == 1) {
      all_speaker_db <- speaker_phrases %>%
        dplyr::select(database_id:age,
                      label, phraseToken, condition,
                      onset, offset, db)
    } else {
      all_speaker_db <- base::rbind(all_speaker_db,
                                    speaker_phrases)
    }
  
    utils::setTxtProgressBar(pb, NC)  # tick the bar
    NC <- NC + 1
  }
  
  all_speaker_db <- all_speaker_db %>%
    arrange(onset) %>%
    dplyr::group_by(database_id, group, severity, sex, age, label, condition) %>%
      dplyr::mutate(rep = row_number(phraseToken)) %>%
    dplyr::filter(condition != "lessClear")
  
  if (saveRDS == T) {
    saveRDS(all_speaker_db, out_path)
  }
  
  return(all_speaker_db)
}

get_target_phrase_segments <- function(speakers, tg_path, out_path, spec_name, saveRDS) {
  
  speakers <- get_database_files(speakers)
  
  
  n <- NROW(speakers)
  if (n == 0) {
    return(list(acoustic = tibble(), movement = tibble()))
  }
  
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)
  
  NC <- 1
  
  while (NC <= NROW(speakers)) {
    # IDing Speaker & Pulling their info from the master File Inventories.xlsx
    speaker <- speakers[NC,]
    
    tg_file <- list.files(path = tg_path,
                          pattern = speaker$sentence_rep,
                          full.names = TRUE,
                          ignore.case = TRUE)
    
    # Reading TextGrid
    tg <- rPraat::tg.read(fileNameTextGrid = tg_file)
    
    condition <- base::cbind(tg[["condition"]][["t1"]],
                             tg[["condition"]][["t2"]],
                             tg[["condition"]][["label"]]) %>%
      as.data.frame() %>%
      dplyr::rename(onset = 1,
                    offset = 2,
                    label = 3) %>%
      dplyr::mutate(onset = base::as.numeric(onset),
                    offset = base::as.numeric(offset),
                    label = base::gsub(pattern = " ",
                                       replacement = "",
                                       x = label),
                    label = base::tolower(label)) %>%
      dplyr::filter(label != "")
    
    phrase <- base::cbind(tg[["phrase"]][["t1"]], tg[["phrase"]][["t2"]], tg[["phrase"]][["label"]]) %>%
      as.data.frame() %>%
      dplyr::rename(onset = 1,
                    offset = 2,
                    label = 3) %>%
      dplyr::mutate(
        database_id = speaker$database_id,
        onset = base::as.numeric(onset),
        offset = base::as.numeric(offset),
        label = base::gsub(
          pattern = " ",
          replacement = "",
          x = label
        ),
        label = base::tolower(label),
        condition = case_when(
          onset > condition$onset[condition$label == "conv"] &
            offset < condition$offset[condition$label == "conv"] ~ "conv",
          onset > condition$onset[condition$label == "moreclear"] &
            offset < condition$offset[condition$label == "moreclear"] ~ "moreClear",
          onset > condition$onset[condition$label == "lessclear"] &
            offset < condition$offset[condition$label == "lessclear"] ~ "lessClear",
        )
      ) %>%
      dplyr::filter(label != "") %>%
      dplyr::group_by(condition) %>%
      dplyr::mutate(phraseToken = make.unique(label)) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(condition, .after = phraseToken)
    
    # Pulling the formant Data
    formant_file <- list.files(path = here::here("data","01_interim","03_fbw_files"),
                               pattern = speaker$sentence_rep,
                               full.names = T,
                               ignore.case = T) %>%
      as.character()
    
    formants <- read_tsv(
      formant_file,
      col_names = c("time", "f1", "f2", "f3", "c5", "c6", "flag"),
      col_types = cols(
        time    = col_double(),
        f1   = col_double(),
        f2   = col_double(),
        f3   = col_double(),
        c5   = col_double(),
        c6   = col_double(),
        flag = col_integer()
      ),
      quote = "\"",
      # default; keep unless you suspect quotes in fields
      locale = locale(encoding = "UTF-8"),
      progress = FALSE
    ) %>%
      dplyr::select(time, f1, f2, f3) %>%
      dplyr::mutate(
        time = time / 1000,
        f1 = f1 * 1000,
        f2 = f2 * 1000,
        f3 = f3 * 1000
      )
    
    # Pulling the movement data
    movement_file <- list.files(path = here::here("Data","00_raw_data","speaker_data","00_movement_files"), 
                                pattern = gsub(speaker$sentence_rep, pattern = "_sync", replacement = ""),
                                full.names = TRUE,
                                ignore.case = TRUE)
    
    movement <- rio::import(movement_file)
    
    
    k <- 1
    while(k <= NROW(phrase)) {
      
      target_acoustic <- formants %>%
        dplyr::filter(
          dplyr::between(x = time,
                         left = phrase$onset[k],
                         right = phrase$offset[k])) %>%
        dplyr::mutate(database_id = phrase$database_id[k],
                      group = phrase$group[k],
                      condition = phrase$condition[k],
                      phrase = phrase$phrase[k],
                      label = phrase$label[k],
                      token = phrase$phraseToken[k]) %>%
        dplyr::relocate(database_id:token, .before = time)
      
      target_movement <- movement %>%
        dplyr::filter(time >= phrase$onset[k]) %>%
        dplyr::filter(time <= phrase$offset[k]) %>%
        dplyr::mutate(database_id = phrase$database_id[k],
                      group = phrase$group[k],
                      condition = phrase$condition[k],
                      phrase = phrase$phrase[k],
                      label = phrase$label[k],
                      token = phrase$phraseToken[k]) %>%
        dplyr::relocate(database_id:token, .before = time)
      
      if(k == 1) {
        speaker_acoustic <- target_acoustic
        speaker_movement <- target_movement
      } else {
        speaker_acoustic <- rbind(speaker_acoustic, target_acoustic)
        speaker_movement <- rbind(speaker_movement, target_movement)
      }
      
      rm(target_acoustic, target_movement)
      k <- k + 1
    }
    
    if(NC == 1) {
      all_speaker_acoustic <- speaker_acoustic
      all_speaker_movement <- speaker_movement
    } else {
      all_speaker_acoustic <- base::rbind(all_speaker_acoustic,
                                          speaker_acoustic)
      all_speaker_movement <- base::rbind(all_speaker_movement,
                                          speaker_movement)
    }
    
    # Saving the Acoustic Segments
    #dir.create(here::here("data","01_interim","04_acoustic_segments"),
    #           showWarnings = F)
    #rio::export(x = speaker_acoustic,
    #            file = here::here("data","01_interim","04_acoustic_segments",
    #                              gsub(pattern = "_sync",
    #                                   replacement = "_acoustic_segments.csv",
    #                                   x = speaker$sentence_rep)))
    
    # Saving the Movement Segments
    #dir.create(here::here("data","01_interim","04_movement_segments"),
    #           showWarnings = F)
    #rio::export(x = speaker_movement,
    #            file = here::here("data","01_interim","04_movement_segments",
    #                          gsub(pattern = "_sync",
    #                               replacement = "_movement_segments.csv",
    #                               x = speaker$sentence_rep)))
    
    utils::setTxtProgressBar(pb, NC)  # tick the bar
    NC <- NC + 1
  }
  target_segments <- list(
    acoustic = all_speaker_acoustic %>%
      dplyr::filter(condition != "lessClear"),
    movement = all_speaker_movement %>%
      dplyr::filter(condition != "lessClear")
  )
  
  if(saveRDS == T) {
    saveRDS(
      target_segments,
      out_path
    )
  }
  
  return(target_segments)
}
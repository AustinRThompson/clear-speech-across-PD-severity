# R/mfa_merge.R
library(tidyverse)
source("R/collect_source_files.R")

mfa_merge <- function(speaker_info = speaker_info,
                      textgrid_files = textgrid_manifest,
                      audio_folder = here::here("data", "00_raw_data","speaker_data", "00_audio_files"),
                      audio_files = get_database_files(speaker_info),
                      aligned_phrase_folder = here::here("data", "01_interim","04_transcripts","1_aligned_phrase_data"),
                      output_folder = here::here("data", "01_interim","04_transcripts","2_aligned_passage_data")){
  
  # Create the aligned passage textgrid output folder
  dir.create(path = output_folder,
             showWarnings = F,
             recursive = T)
  
  unaligned_passage_textgrids <- textgrid_files$path
  audio_files <- get_database_files(speaker_info)
  all_audio <- list.files(audio_folder, full.names = T)
  all_textgrids <- textgrid_files$path
  
  n <- NROW(speaker_info)
  if (n == 0) {
    return(list(acoustic = tibble(), movement = tibble()))
  }
  
  pb <- utils::txtProgressBar(min = 0, max = n, style = 3)
  on.exit(close(pb), add = TRUE)
  
  c <- 1
  while(c <= NROW(speaker_info)){
    # Identify the target speaker
    speaker <- speaker_info %>%
      slice(c) %>%
      pull(database_id)
    
    # Find and read in their unaligned passage textgrid
    speaker_audio_file <- audio_files %>%
      dplyr::filter(database_id == speaker) %>%
      pull(sentence_rep)
    speaker_audio <- all_audio[grep(pattern = speaker_audio_file, ignore.case = T, x = all_audio)]
    
    speaker_textgrid <- all_textgrids[grep(pattern = speaker_audio_file, ignore.case = T, x = all_textgrids)] %>%
      rPraat::tg.read(.)
    
    # Find their aligned phrase textgrids
    speaker_aligned_phrase_textgrids <- list.files(file.path(aligned_phrase_folder,speaker),
                                                   full.names = T)
  
    # Identifying the conditions so that we can assign them to each phrase
    speaker_conditions <- speaker_textgrid$condition %>%
      as.data.frame() %>%
      dplyr::mutate(label = trimws(label)) %>%
      dplyr::filter(label != "")
    
    clear <- speaker_conditions %>%
      dplyr::filter(label == "moreClear")
    
    conv <- speaker_conditions %>%
      dplyr::filter(label == "conv")
    
    less_clear <- speaker_conditions %>%
      dplyr::filter(label == "lessClear")
    
    # Creating the transcript text
    speaker_passage <- speaker_textgrid$phrase %>%
      as.data.frame() %>%
      dplyr::mutate(condition = case_when(
        between(t1, conv$t1, conv$t2) == T ~ "conv",
        between(t1, clear$t1, clear$t2) == T ~ "clear",
        between(t1, less_clear$t1, less_clear$t2) == T ~ "less_clear",
        TRUE ~ "silence"
      ),
      transcript = case_when(
        grepl(pattern = "bsent", x = label) == T ~ "buy bobby a puppy",
        grepl(pattern = "tsent", x = label) == T ~ "tess told dan to stay fit",
        grepl(pattern = "ksent", x = label) == T ~ "carl got a croaking frog",
        TRUE ~ ""
      )) %>%
      dplyr::group_by(condition) %>%
      dplyr::mutate(phrase_number = sprintf("%03d",row_number()))
    
    k <- 1
    while(k <= NROW(speaker_aligned_phrase_textgrids)) {
      # Getting the important meta data for each phrase: phrase number and condition
      phrase_num <- gsub("\\.TextGrid$", "", speaker_aligned_phrase_textgrids[k]) %>%
        str_extract("(?<=-)\\d+") %>%
        as.numeric()
      phrase_condition <- dplyr::case_when(
        grepl(pattern = "conv",x = speaker_aligned_phrase_textgrids[k]) == T ~ "conv",
        grepl(pattern = "clear",x = speaker_aligned_phrase_textgrids[k]) == T ~ "clear"
      )
      
      # Getting the matching phrase from the unaligned passage
      phrase_from_passage <- speaker_passage %>%
        dplyr::filter(condition == phrase_condition) %>%
        dplyr::mutate(phrase_number = as.numeric(phrase_number)) %>%
        dplyr::filter(phrase_number == phrase_num)
      
      if(NROW(phrase_from_passage) > 1) {
        print("Issue with identifying the phrase from the passage.")
        stop()
      }
      
      # reading the aligned phrase
      alligned_phrase <- rPraat::tg.read(speaker_aligned_phrase_textgrids[k])
      
      words  <- alligned_phrase$words  %>% as.data.frame()
      phones <- alligned_phrase$phones %>% as.data.frame()
      
      words <- words %>%
        dplyr::mutate(
          t1 = t1 + phrase_from_passage$t1,
          t2 = t2 + phrase_from_passage$t1,
          t2 = ifelse(t2 == max(t2), max(phrase_from_passage$t2), t2),
          label = ifelse(label == "[bracketed]", "", label)
        )
      
      phones <- phones %>%
        dplyr::mutate(
          t1 = t1 + phrase_from_passage$t1,
          t2 = t2 + phrase_from_passage$t1,
          t2 = ifelse(t2 == max(t2), max(phrase_from_passage$t2), t2),
          label = ifelse(label == "[bracketed]", "", label)
        )
      
      # Merging them together to rebuild the passage textgrid
      if (k == 1) {
        all_words  <- words
        all_phones <- phones
      } else {
        all_words  <- rbind(all_words,  words)
        all_phones <- rbind(all_phones, phones)
      }
      
      rm(words, phones, phrase_from_passage)
      k <- k + 1
    }
    rm(k)
    
    # collapse contiguous blanks
    all_words <- all_words %>%
      dplyr::mutate(
        blank = label == "",
        group = cumsum(!blank | dplyr::lag(!blank, default = TRUE))
      ) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        name  = dplyr::first(name),
        type  = dplyr::first(type),
        t1    = dplyr::first(t1),
        t2    = dplyr::last(t2),
        label = dplyr::first(label),
        .groups = "drop"
      )
    
    all_phones <- all_phones %>%
      dplyr::mutate(
        blank = label == "",
        group = cumsum(!blank | dplyr::lag(!blank, default = TRUE))
      ) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(
        name  = dplyr::first(name),
        type  = dplyr::first(type),
        t1    = dplyr::first(t1),
        t2    = dplyr::last(t2),
        label = dplyr::first(label),
        .groups = "drop"
      )
    
    # Building the final TextGrid
    aligned_tg <- speaker_textgrid %>%
      rPraat::tg.insertNewIntervalTier(., newTierName = "words") %>%
      rPraat::tg.insertNewIntervalTier(., newTierName = "phones")
    
    k <- 1
    while (k < nrow(all_words)) {
      current_word <- all_words[k, ]
      aligned_tg <- rPraat::tg.insertInterval(
        aligned_tg, tierInd = "words",
        tStart = current_word$t1, tEnd = current_word$t2, label = current_word$label
      )
      rm(current_word); k <- k + 1
    }
    
    k <- 1
    while (k < nrow(all_phones)) {
      current_word <- all_phones[k, ]
      aligned_tg <- rPraat::tg.insertInterval(
        aligned_tg, tierInd = "phones",
        tStart = current_word$t1, tEnd = current_word$t2, label = current_word$label
      )
      rm(current_word); k <- k + 1
    }
    
    rPraat::tg.write(aligned_tg, fileNameTextGrid = file.path(output_folder,paste0(speaker_audio_file,".TextGrid")))
    
    utils::setTxtProgressBar(pb, c)  # tick the bar
    c <- c + 1
    }
  
}
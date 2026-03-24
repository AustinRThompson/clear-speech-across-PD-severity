# mfa_data_prep.R
library(tidyverse)

mfa_data_prep <- function(speaker_info,
                          audio_folder,
                          audio_files,
                          textgrid_files,
                          output_folder) {
  
  all_audio <- list.files(audio_folder, full.names = T)
  all_textgrids <- textgrid_files$path
  
  c <- 1
  while(c <= NROW(speaker_info)){
    # Identify the target speaker
    speaker <- speaker_info %>%
      slice(c) %>%
      pull(database_id)
    
    print(paste0("Working on ", speaker))
    
    # Create the output path for that speaker
    speaker_output_folder <- paste0(output_folder,"/",speaker)
    dir.create(path = speaker_output_folder,
               showWarnings = F,
               recursive = T)
    
    # Find and read in their passage audio and textgrid
    speaker_audio_file <- audio_files %>%
      dplyr::filter(database_id == speaker) %>%
      pull(sentence_rep)
    speaker_audio <- all_audio[grep(pattern = speaker_audio_file, ignore.case = T, x = all_audio)]
    snd <- suppressWarnings(rPraat::snd.read(speaker_audio))
    
    speaker_textgrid <- all_textgrids[grep(pattern = speaker_audio_file, ignore.case = T, x = all_textgrids)] %>%
      rPraat::tg.read(.)
    
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
    speaker_phrases <- speaker_textgrid$phrase %>%
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
    
    rm(speaker_conditions,
       clear,
       conv,
       less_clear)
    
    # Getting just the phrases that we want to align
    phrases_to_align <- speaker_phrases %>%
      dplyr::filter(label != "") %>%
      dplyr::filter(condition != "less_clear")
    
    # iterate labeled intervals
    for (i in seq_len(nrow(phrases_to_align))) {
      phrase_number <- phrases_to_align$phrase_number[i]
      condition <- phrases_to_align$condition[i]
      start <- phrases_to_align$t1[i]; end <- phrases_to_align$t2[i]
      if (!is.finite(start) || !is.finite(end) || end <= start) next
      
      seg <- rPraat::snd.cut(snd = snd, Start = start, End = end)
      
      base <- sprintf("%s_%s_phrase-%s", speaker, condition, phrase_number)
      wav_out <- file.path(speaker_output_folder, paste0(base, ".wav"))
      lab_out <- file.path(speaker_output_folder, paste0(base, ".lab"))
      
      rPraat::snd.write(seg, wav_out)
      writeLines(phrases_to_align$transcript[i], con = lab_out, useBytes = TRUE)
    }
    
    print(paste0("Completed with ", speaker))
    c <- c + 1
    rm(speaker,
       speaker_conditions,
       speaker_phrases,
       speaker_audio,
       speaker_output_folder,
       speaker_textgrid)
  }
  
  
}

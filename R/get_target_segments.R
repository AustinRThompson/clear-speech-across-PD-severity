library(tidyverse)
source(here::here("R/collect_source_files.R"))
source(here::here("R/read_speaker_info.R"))

# Target Segments: "Buy" & corner vowels ----
get_target_vowel_segments <- function(speakers, tg_path, out_path, spec_name, saveRDS, save_plots = T) {

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
    
    vowel <- base::cbind(tg[["vowel"]][["t1"]],
                         tg[["vowel"]][["t2"]],
                         tg[["vowel"]][["label"]]) %>%
      as.data.frame() %>%
      dplyr::rename(onset = 1,
                    offset = 2,
                    label = 3) %>%
      dplyr::mutate(database_id = speaker$database_id,
                    group = speaker$group,
                    onset = base::as.numeric(onset),
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
                    ),
                    condition = as.factor(condition)) %>%
      dplyr::filter(label != "") %>%
      group_by(database_id, condition) %>%
      dplyr::mutate(token = make.unique(label),
                    phrase = NA) %>%
      dplyr::select(database_id,group,condition,phrase,label,token,onset,offset)
    
    # Assigning Phrase to each vowel
    k <- 1
    while(k <= NROW(vowel)) {
      onset <- vowel$onset[k]
      offset <- vowel$offset[k]
      midpoint <- ((offset - onset)/2) + onset
      targetPhrase <- phrase %>%
        dplyr::filter(midpoint > onset & midpoint < offset) %>%
        pull(phraseToken)
      
      vowel$phrase[k] <- targetPhrase
      
      k <- k + 1
    }
    rm(k)
    
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
    while(k <= NROW(vowel)) {
      
      target_acoustic <- formants %>%
        dplyr::filter(
          dplyr::between(x = time,
                         left = vowel$onset[k] - ((vowel$offset[k]-vowel$onset[k])*.10), # Here I am padding the manual segmentations to make sure we capture the true movement extrema
                         right = vowel$offset[k] + ((vowel$offset[k]-vowel$onset[k])*.10))) %>%
        dplyr::mutate(database_id = vowel$database_id[k],
                      group = vowel$group[k],
                      condition = vowel$condition[k],
                      phrase = vowel$phrase[k],
                      label = vowel$label[k],
                      token = vowel$token[k]) %>%
        dplyr::relocate(database_id:token, .before = time)
      
      target_movement <- movement %>%
        dplyr::filter(time >= vowel$onset[k]-((vowel$offset[k]-vowel$onset[k])/2)) %>%
        dplyr::filter(time <= vowel$offset[k]+((vowel$offset[k]-vowel$onset[k])/2)) %>%
        dplyr::mutate(database_id = vowel$database_id[k],
                      group = vowel$group[k],
                      condition = vowel$condition[k],
                      phrase = vowel$phrase[k],
                      label = vowel$label[k],
                      token = vowel$token[k]) %>%
        dplyr::relocate(database_id:token, .before = time)
      
      ## Automatically get the extrema of /aI/ movement in ventral–dorsal (x) dimension ----
      
      if (vowel$label[k] == "ai" & !all(is.na(target_movement$tb_x))) {
        boundary_onset <- target_movement %>%
          dplyr::filter(time < vowel$onset[k]+(vowel$offset[k]-vowel$onset[k])/2) %>%
          dplyr::filter(row_number() == which.min(tb_x)) %>%
          pull(time)
        
        boundary_offset <- target_movement %>%
          dplyr::filter(time > vowel$onset[k]+(vowel$offset[k]-vowel$onset[k])/2) %>%
          dplyr::filter(row_number() == which.max(tb_x)) %>%
          pull(time)
        
        target_movement <- target_movement %>%
          dplyr::mutate(target = case_when(
            between(time, boundary_onset, boundary_offset) == T ~ TRUE,
            TRUE ~ FALSE
          ))
        
        if (save_plots == T) {
          segment_plot <- target_movement %>%
            ggplot() +
            aes(x = time, y = tb_x) +
            geom_line(color = "grey") +
            geom_point(aes(color = target)) +
            geom_vline(xintercept = boundary_onset) +
            geom_vline(xintercept = boundary_offset) +
            labs(
              title = paste0(
                speaker$database_id,
                " - ",
                vowel$condition[k],
                " - /aI/ (",
                vowel$token[k],
                ")"
              )
            ) +
            theme_classic() +
            theme(legend.position = "none")
          
          dir.create(
            path = here::here(
              "manuscript",
              SPECS,
              "00_data",
              "automatic_tb_segments"
            ),
            recursive = T,
            showWarnings = F
          )
          ggsave(
            plot = segment_plot,
            filename = here::here(
              "manuscript",
              SPECS,
              "00_data",
              "automatic_tb_segments",
              paste0(
                speaker$database_id,
                "-",
                vowel$condition[k],
                "-",
                vowel$token[k],
                ".png"
              )
            ),
            create.dir = T,
            height = 3,
            width = 5.5,
            unit = "in"
          )
        }
        
        target_movement <- target_movement %>%
          dplyr::filter(target == T) %>%
          dplyr::select(!target)
      } else {
        target_movement <- target_movement %>%
          dplyr::filter(time >= vowel$onset[k],
                        time <= vowel$offset[k])
      }
      
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

# Target Segments: "Tess" ----
get_target_tf_segment <- function(speakers = speaker_info,
                                  tg_path = here::here("data",
                                                       "01_interim",
                                                       "04_transcripts",
                                                       "2_aligned_passage_data"),
                                  movement_path = here::here("Data", "00_raw_data", "speaker_data", "00_movement_files"),
                                  out_path = here::here("manuscript", SPECS, "00_data", "target_tf_segments.RDS"),
                                  saveRDS = T,
                                  save_plots = F) {
  
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
    
    movement_file <- list.files(path = movement_path,
                                pattern = speaker$sentence_rep,
                                full.names = TRUE,
                                ignore.case = TRUE) %>%
      rio::import()
    
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
    
    words <- base::cbind(tg[["words"]][["t1"]],
                          tg[["words"]][["t2"]],
                          tg[["words"]][["label"]]) %>%
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
                    ),
                    midpoint = (offset-onset)/2 + onset) %>%
      dplyr::filter(label != "") %>%
      dplyr::relocate(condition, .after = label)
    
    # Finding the Phrase Token for each word
    word_counter <- 1
    words <- words %>%
      dplyr::mutate(phraseToken = NA)
    while (word_counter <= NROW(words)) {
      target_phrase <- phrase %>%
        dplyr::filter(onset < words$midpoint[word_counter] &
                        offset > words$midpoint[word_counter])
      
      words$phraseToken[word_counter] <- target_phrase$phraseToken
      
      word_counter <- word_counter + 1
      rm(target_phrase)
    }
    rm(word_counter)
    
    phones <- base::cbind(tg[["phones"]][["t1"]],
                         tg[["phones"]][["t2"]],
                         tg[["phones"]][["label"]]) %>%
      as.data.frame() %>%
      dplyr::rename(onset = 1,
                    offset = 2,
                    label = 3) %>%
      dplyr::mutate(onset = base::as.numeric(onset),
                    offset = base::as.numeric(offset),
                    label = base::gsub(pattern = " ",
                                       replacement = "",
                                       x = label),
                    label = base::toupper(label),
                    condition = case_when(
                      onset > condition$onset[condition$label == "conv"] &
                        offset < condition$offset[condition$label == "conv"] ~ "conv",
                      onset > condition$onset[condition$label == "moreclear"] &
                        offset < condition$offset[condition$label == "moreclear"] ~ "moreClear",
                      onset > condition$onset[condition$label == "lessclear"] &
                        offset < condition$offset[condition$label == "lessclear"] ~ "lessClear",
                    ),
                    midpoint = (offset-onset)/2 + onset) %>%
      dplyr::filter(label != "") %>%
      dplyr::relocate(condition, .after = label)
    
    # Finding the word & Phrase Token for each word
    phone_counter <- 1
    phones <- phones %>%
      dplyr::mutate(phraseToken = NA,
                    word = NA)
    while (phone_counter <= NROW(phones)) {
      target_words <- words %>%
        dplyr::filter(onset < phones$midpoint[phone_counter] &
                        offset > phones$midpoint[phone_counter])
      
      phones$phraseToken[phone_counter] <- ifelse(length(target_words$phraseToken) == 0, NA,target_words$phraseToken)
      phones$word[phone_counter] <- ifelse(length(target_words$label) == 0, NA, target_words$label)
      
      phone_counter <- phone_counter + 1
      rm(target_words)
    }
    rm(phone_counter)
    
    # Now that we have the phone onset/offsets, we want to pull up the movement for
    # each "Tess" and do the automatic boundary identification
    tess_data <- phones %>%
      dplyr::filter(word == "tess") %>%
      dplyr::group_by(condition, phraseToken, word) %>%
      dplyr::mutate(segment_onset = min(onset),
                    segment_offset = max(offset)) %>%
      dplyr::group_by(condition, phraseToken, word, segment_onset, segment_offset) %>%
      tidyr::nest() %>%
      dplyr::ungroup() %>%
      dplyr::rename(phone_data = data) %>%
      dplyr::mutate(duration = segment_offset-segment_onset) %>%
      dplyr::group_by(condition, word) %>%
      dplyr::mutate(word_token = row_number(),
                    database_id = speaker$database_id,
                    group = speaker$group,
                    severity = speaker$severity,
                    sex = speaker$sex,
                    age = speaker$age) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(database_id:age, .before = 1)
    
    segment_counter <- 1
    while(segment_counter <= NROW(tess_data)){
      target_tess <- tess_data %>%
        dplyr::slice(segment_counter)
      segment <- target_tess$phone_data[[1]]
      segment_condition <- target_tess$condition
      
      eh <- segment %>%
        dplyr::filter(grepl(pattern = "eh", label, ignore.case = T))
      
      ss <- segment %>%
        dplyr::filter(grepl(pattern = "s", label, ignore.case = T))
      
      segment_movement <- movement_file %>%
        dplyr::filter(time >= tess_data$segment_onset[segment_counter] - (tess_data$duration[segment_counter]*.50) &
                        time <= tess_data$segment_offset[segment_counter] + (tess_data$duration[segment_counter]*.50))
      
      # Automatically define onset and offset ----
      if (!all(is.na(segment_movement$tf_y))) {
        boundary_onset <- segment_movement %>%
          dplyr::filter(time < eh$onset) %>%
          dplyr::filter(row_number() == which.max(tf_y)) %>%
          pull(time)
        
        boundary_offset <- segment_movement %>%
          dplyr::filter(time > eh$offset) %>%
          dplyr::filter(time < ss$offset) %>%
          dplyr::mutate(
            dt = c(NA, diff(time)),
            dpos = c(NA, diff(tf_y)),
            velocity = dpos / dt,
            velocity_prev = lag(velocity),
            zero_crossing = case_when(
              is.na(velocity) | is.na(velocity_prev) ~ FALSE,
              # sign change (positive to negative or negative to positive)
              velocity * velocity_prev < 0 ~ TRUE,
              # treat exact zeros as a crossing event (e.g., prev non-zero -> zero or zero -> next non-zero)
              velocity == 0 & velocity_prev != 0 ~ TRUE,
              velocity_prev == 0 & velocity != 0 ~ TRUE,
              # optionally: treat consecutive zeros as NOT a crossing
              TRUE ~ FALSE
            ),
            before_zero_cross = lead(zero_crossing)
          )
        
        if (!any(boundary_offset$zero_crossing == T)) {
          boundary_offset <- boundary_offset %>%
            dplyr::filter(row_number() == which.max(tf_y)) %>%
            pull(time)
        } else {
          boundary_offset <- boundary_offset %>%
            dplyr::filter(before_zero_cross == TRUE) %>%
            dplyr::slice(1) %>%
            pull(time)
        }
        
        segment_movement <- segment_movement %>%
          dplyr::mutate(target = case_when(
            between(time, boundary_onset, boundary_offset) == T ~ TRUE,
            TRUE ~ FALSE
          ))
        
        if (save_plots == T) {
          segment_plot <- segment_movement %>%
            ggplot() +
            aes(x = time, y = tf_y) +
            geom_line(color = "grey") +
            geom_point(aes(color = target)) +
            geom_vline(xintercept = boundary_onset) +
            geom_vline(xintercept = boundary_offset) +
            labs(
              title = paste0(
                speaker$database_id,
                " - ",
                segment_condition,
                " - Tess ",
                tess_data$word_token[segment_counter]
              )
            ) +
            theme_classic() +
            theme(legend.position = "none")
          
          dir.create(
            path = here::here(
              "manuscript",
              SPECS,
              "00_data",
              "automatic_tf_segments"
            ),
            recursive = T,
            showWarnings = F
          )
          ggsave(
            plot = segment_plot,
            filename = here::here(
              "manuscript",
              SPECS,
              "00_data",
              "automatic_tf_segments",
              paste0(
                speaker$database_id,
                "-",
                segment_condition,
                "-tess",
                tess_data$word_token[segment_counter],
                ".png"
              )
            ),
            height = 3,
            width = 5.5,
            unit = "in"
          )
        }
        
        segment_movement <- base::merge(
          target_tess %>%
            dplyr::select(database_id:segment_offset, word_token),
          segment_movement,
          all = T
        ) %>%
          dplyr::filter(target == T)
        
        if (segment_counter == 1) {
          speaker_movement <- segment_movement
        } else {
          speaker_movement <- rbind(speaker_movement, segment_movement)
        }
        
        rm(segment_movement)
      }
      segment_counter <- segment_counter + 1
    }
    
    if(NC == 1) {
      all_speaker_movement <- speaker_movement
    } else {
      all_speaker_movement <- base::rbind(all_speaker_movement,
                                          speaker_movement)
    }
    
    utils::setTxtProgressBar(pb, NC)  # tick the bar
    NC <- NC + 1
  }
  
  # Removing tokens with sensor errors
  all_speaker_movement <- all_speaker_movement %>%
    dplyr::mutate(exclude = case_when(
      database_id == "Sub54" & condition == "moreClear" & word_token >= 4 ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::filter(exclude == FALSE) %>%
    dplyr::select(!exclude)
  
  if(saveRDS == T) {
    saveRDS(
      all_speaker_movement,
      out_path
    )
  }
  
  return(all_speaker_movement)
}

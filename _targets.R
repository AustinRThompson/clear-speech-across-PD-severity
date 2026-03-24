# _targets.R
library(targets)      # install.packages('targets')
library(tarchetypes)  # install.packages('tarchetypes')

tar_option_set(
  packages = c(
    "tidyverse","tibble","here","yaml","digest","rio","rPraat",
    "brms","bayesplot","posterior","loo","ggplot2"
  )
)

# source your step functions
purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

SPECS <- "initial"  # change to "rev1", "rev2" etc. when ready

base::list(
  
  # Data Preparation ----
  ## 01: Demographic info ----
  # Generate the demographic information for the speakers and listeners
  tar_target(listener_info, read_listener_info(spec_name = SPECS,
                                               saveRDS = F)),
  tar_target(speaker_info,  read_speaker_info(spec_name = SPECS,
                                              saveRDS = F)),
  
  ## 02: Clean listener data ----
  # This target caches the Gorilla files
  tar_target(
    gorilla_data_manifest,
    {
      dir <- here::here("data", "00_raw_data", "listener_data", "01_gorilla_data")
      files <- list.files(dir,
                          pattern = "\\.csv$",
                          full.names = TRUE,
                          recursive = TRUE)
      info <- file.info(files)[, c("size", "mtime")]
      tibble::tibble(path = files,
                     size = info$size,
                     mtime = info$mtime)
    },
    cue = tar_cue(mode = "thorough")
  ), 
  
  # Clean the listener ratings and quality check listeners
  tar_target(
    listener_ratings,
    {
      gorilla_data_manifest
      get_listener_ratings() |>
        check_listener_quality(spec_name = SPECS, 
                               saveRDS = T)
    }), 
  
  # For the reliable listeners, collect their ratings and summarize by speaker
  tar_target(
    perceptual_measures,
    get_perceptual_measures(
      listeners = listener_ratings$qc_passed,
      speaker_info = speaker_info,
      spec_name = SPECS,
      saveRDS = T
    )
  ), 
  
  ## 03: Collect speaker database files ----
  # Find the target audio and movement files
  tar_target(db_files,
             get_database_files(speaker_info),
             cue = tar_cue(mode = "thorough")),
  
  # Copies the target audio and movement files to the 00_raw_data folder (skips if nothing changed)
  tar_target(db_files_copied,
             collect_source_files(db_files),
             pattern = map(db_files)), 
  
  ## 04: Cache TextGrid, lbl, and FBW files ----
  # This target caches the TextGrid files
  tar_target(
    textgrid_manifest, 
    {
      dir <- here::here("data","01_interim","01_textgrids")
      files <- list.files(dir, pattern = "\\.TextGrid$", full.names = TRUE, recursive = TRUE)
      info <- file.info(files)[, c("size", "mtime")]
      tibble::tibble(path = files, size = info$size, mtime = info$mtime)
    },
    cue = tar_cue(mode = "thorough")),
  
  # Turn the TextGrid files into lbl files for TF32
  tar_target(
    lbl_files,
    {
      textgrid_manifest
      tg_to_lbl()
    },
    cue = tar_cue(mode = "thorough"),
    format = "file"),
  
  # This target reads the FBW files
  tar_target(
    fbw_manifest,
    {
      dir <- here::here("data","01_interim","03_fbw_files")
      files <- list.files(dir, pattern = "\\.FBW$", full.names = TRUE, recursive = TRUE)
      info <- file.info(files)[, c("size", "mtime")]
      tibble::tibble(path = files, size = info$size, mtime = info$mtime)
    },
    cue = tar_cue(mode = "thorough")),
  
  ## 05: Force Align transcripts ----
  # Prep the mfa data but getting the unaligned wav and lbl segments
  tar_target(
    prep_mfa_data,
    {
      mfa_data_prep(
        speaker_info = speaker_info,
        audio_folder = here::here("data", "00_raw_data","speaker_data", "00_audio_files"),
        audio_files = get_database_files(speaker_info),
        textgrid_files = textgrid_manifest,
        output_folder = here::here("data", "01_interim","04_transcripts","0_unaligned_phrase_data")
      )
    },
    format = "file"
  ),
  
  # Run the MFA manually
  
  # Merge the MFA output phrases back into the full passage
  tar_target(
    mfa_merge_phrase,
    {
      mfa_merge(
        speaker_info = speaker_info,
        textgrid_files = textgrid_manifest,
        audio_folder = here::here("data", "00_raw_data","speaker_data", "00_audio_files"),
        audio_files = get_database_files(speaker_info),
        aligned_phrase_folder = here::here("data", "01_interim","04_transcripts","1_aligned_phrase_data"),
        output_folder = here::here("data", "01_interim","04_transcripts","2_aligned_passage_data")
      )
    }
  ),
  
  ## 06: Target segment data ----
  # Extract the acoustic and movement data for the target segments (/ai/ and the corner vowels)
  tar_target(
    target_vowel_segments,
    {
      textgrid_manifest
      fbw_manifest
      get_target_vowel_segments(speakers = speaker_info,
                                tg_path = here::here("data","01_interim","01_textgrids"),
                                out_path = here::here("manuscript",
                                                      SPECS,
                                                      "00_data",
                                                      "target_vowel_segments.RDS"),
                                spec_name = SPECS,
                                saveRDS = F,
                                save_plots = F)
    },
    cue = tar_cue(mode = "thorough")
  ),
  
  # Extract the movement data for "tess"
  tar_target(
    target_tf_segment,
    {
      get_target_tf_segment(
        speakers = speaker_info,
        tg_path = here::here("data",
                             "01_interim",
                             "04_transcripts",
                             "2_aligned_passage_data"),
        movement_path = here::here("Data", "00_raw_data", "speaker_data", "00_movement_files"),
        out_path = here::here("manuscript", SPECS, "00_data", "target_tf_segments.RDS"),
        saveRDS = T,
        save_plots = F
      )
    },
    cue = tar_cue(mode = "thorough")
  ),
  
  ## 07: Calculate measures ----
  # Calculate the corner vowel data (corner vowel distance)
  tar_target(vowel_data,
             {
               target_vowel_segments
               compute_corner_vowel_distance(data = target_vowel_segments$acoustic,
                                             speakers = speaker_info,
                                             spec_name = SPECS,
                                             saveRDS = F)
             }
  ),
  # Calculate the TB diphthong data (TB distance)
  tar_target(
    diphthong_data, {
      target_vowel_segments
      compute_tb_distance(
        movement_data = target_vowel_segments$movement,
        speakers = speaker_info,
        spec_name = SPECS,
        saveRDS = F
      )
    }
  ),
  
  # Calculate the TF CVC segment data (TF distance)
  tar_target(cvc_tf_data, {
    compute_tf_distance(
      movement_data = target_tf_segment,
      speakers = speaker_info,
      spec_name = SPECS,
      saveRDS = F
    )
  }), 
  
  # Calculate the phrase level measures (dB)
  tar_target(
    db_data, 
    {
      compute_db(
        speakers = speaker_info,
        tg_path = here::here("data", "01_interim", "01_textgrids"),
        out_path = here::here(
          "manuscript",
          SPECS,
          "00_data",
          "data_db.RDS"
        ),
        spec_name = SPECS,
        saveRDS = T
      )
    }), 
  
  ## 08: Save Data ----
  # Save the prepped datasets (file targets so changes are tracked)
  # Intelligibility
  tar_target(data_intelligibility, {
    p <- here::here("manuscript", SPECS, "00_data", "data_intelligibility.rds")
    intelligibility <- prep_perceptual_model_data(
      perceptual_data = perceptual_measures$all_quality_listener_ratings,
      db_data = db_data,
      measure = "intelligibility"
    )
    
    saveRDS(intelligibility, p)
    p
  }, format = "file"),
  
  # Precision
  tar_target(data_precision, {
    p <- here::here("manuscript", SPECS, "00_data", "data_precision.rds")
    precision <- prep_perceptual_model_data(
      perceptual_data = perceptual_measures$all_quality_listener_ratings,
      db_data = db_data,
      measure = "precision"
    )
    
    saveRDS(precision, p)
    p
  }, format = "file"),
  
  # Corner Vowel Distance
  tar_target(data_corner_vowel_distance, {
    p <- here::here("manuscript", SPECS, "00_data", "data_corner_vowel_distance.rds")
    corner_vowel_distance <- prep_corner_vowel_distance_data(
      corner_vowel_distance_data = vowel_data$corner_vowel_distance,
      db_data = db_data
    )
    saveRDS(corner_vowel_distance, p)
    p
  }, format = "file"),
  
  # TF distance
  tar_target(data_tf_distance, {
    p <- here::here("manuscript", SPECS, "00_data", "data_tf_distance.rds")
    tf_distance <- prep_tf_distance_data(
      tf_distance_data = cvc_tf_data,
      db_data = db_data
    )
    
    saveRDS(tf_distance, p)
    p
  }, format = "file"), 
  
  # TB distance
  tar_target(data_tb_distance, {
    p <- here::here("manuscript", SPECS, "00_data", "data_tb_distance.rds")
    tb_distance <- prep_tb_distance_data(
      tb_distance_data = diphthong_data,
      db_data = db_data
    )
    
    saveRDS(tb_distance, p)
    p
  }, format = "file"), 
  
  # Listener info
  tar_target(listener_info_rds, {
    p <- here::here("manuscript", SPECS, "00_data", "listener_info.rds")
    saveRDS(listener_info, p)
    p
  }, format = "file"), 
  
  # Speaker info
  tar_target(speaker_info_rds, {
    p <- here::here("manuscript", SPECS, "00_data", "speaker_info.rds")
    saveRDS(speaker_info, p)
    p
  }, format = "file"), 
  
  # Listener ratings
  tar_target(listener_ratings_rds, {
    p <- here::here("manuscript", SPECS, "00_data", "listener_ratings.rds")
    listener_data <- base::list(
      reliable_listeners = listener_ratings$qc_passed,
      unreliable_listeners = listener_ratings$qc_failed,
      all_reliable_ratings = perceptual_measures$all_quality_listener_ratings,
      ratings_summarized_by_speaker = perceptual_measures$summarized_by_speaker
    )
    saveRDS(listener_data, p)
    p
  }, format = "file"), 
  
  # Data Analysis ----
  # --- Model registry ---
  tar_target(model_defs, model_registry_tbl(SPECS)),
  
  ## 01: Build models ----
  # Build: Intelligibility
  tar_target(
    m_intelligibility_fit,
    {
      stopifnot(is.data.frame(model_defs))
      def <- get_def(model_defs, "m_intelligibility")
      model_data <- readRDS(data_intelligibility) %>% as.data.frame()
      
      fit <- fit_brms_model(model_data, def)  # returns a brmsfit
      out_path <- here::here("manuscript", SPECS, "01_models", "m_intelligibility.rds")
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(fit, out_path)
      out_path
    },
    format = "file"
  ),
  
  # Build: Precision
  tar_target(
    m_precision_fit,
    {
      stopifnot(is.data.frame(model_defs))
      def <- get_def(model_defs, "m_precision")
      model_data <- readRDS(data_precision) %>% as.data.frame()
      
      fit <- fit_brms_model(model_data, def)  # returns a brmsfit
      out_path <- here::here("manuscript", SPECS, "01_models", "m_precision.rds")
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(fit, out_path)
      out_path
    },
    format = "file"
  ),
  
  # Build: Corner Vowel Distance
  tar_target(
    m_corner_vowel_distance_fit,
    {
      stopifnot(is.data.frame(model_defs))
      def <- get_def(model_defs, "m_corner_vowel_distance")
      model_data <- readRDS(data_corner_vowel_distance) %>% as.data.frame()
      
      fit <- fit_brms_model(model_data, def)  # returns a brmsfit
      out_path <- here::here("manuscript", SPECS, "01_models", "m_corner_vowel_distance.rds")
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(fit, out_path)
      out_path
    },
    format = "file"
  ),
  
  # Build: TF Distance
  tar_target(
    m_tf_distance_fit,
    {
      stopifnot(is.data.frame(model_defs))
      def <- get_def(model_defs, "m_tf_distance")
      model_data <- readRDS(data_tf_distance) %>%
        as.data.frame() %>%
        dplyr::mutate(tf_distance = as.numeric(tf_distance)) %>%
        dplyr::filter(!is.na(tf_distance)) %>% # Removing missing data
        dplyr::group_by(database_id, group, sex, age, severity, condition) %>%
        dplyr::summarise(tf_distance = mean(tf_distance, na.rm = T),
                         duration = mean(duration, na.rm = T),
                         db_centered = mean(db_centered, na.rm = T),
                         .groups = "drop") %>%
        dplyr::ungroup()
      
      fit <- fit_brms_model(model_data, def)  # returns a brmsfit
      out_path <- here::here("manuscript", SPECS, "01_models", "m_tf_distance.rds")
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(fit, out_path)
      out_path
    },
    format = "file"
  ),
  
  # Build: TB Distance
  tar_target(
    m_tb_distance_fit,
    {
      stopifnot(is.data.frame(model_defs))
      def <- get_def(model_defs, "m_tb_distance")
      model_data <- readRDS(data_tb_distance) %>%
        as.data.frame() %>%
        dplyr::mutate(tb_distance = as.numeric(tb_distance)) %>%
        dplyr::filter(!is.na(tb_distance)) %>% # Removing missing data
        dplyr::filter(database_id != "Sub54") %>% # removed because of monothongization of /aI/
        dplyr::filter(database_id != "Sub23") %>% # removed because of missing TB data
        dplyr::group_by(database_id, group, sex, age, severity, condition) %>%
        dplyr::summarise(tb_distance = mean(tb_distance, na.rm = T),
                         duration = mean(duration, na.rm = T),
                         db_centered = mean(db_centered, na.rm = T),
                         .groups = "drop") %>%
        dplyr::ungroup()
      
      fit <- fit_brms_model(model_data, def)  # returns a brmsfit
      out_path <- here::here("manuscript", SPECS, "01_models", "m_tb_distance.rds")
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(fit, out_path)
      out_path
    },
    format = "file"
  ),
  
  ## 02: Check models ----
  # Diagnostics: Intelligibility
  tar_target(
    m_intelligibility_diag,
    {
      fit <- readRDS(m_intelligibility_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "01_diagnostics", "check_intelligibility.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_diagnostics_rds(fit, out, ndraws_pp = 500, re_formula = NA)
    },
    format = "file"
  ),
  
  # Diagnostics: Precision
  tar_target(
    m_precision_diag,
    {
      fit <- readRDS(m_precision_fit)
      out <- here::here("manuscript", SPECS, "01_models", "01_diagnostics", "check_precision.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_diagnostics_rds(fit, out, ndraws_pp = 500, re_formula = NA)
    },
    format = "file"
  ),
  
  # Diagnostics: Corner Vowel Distance
  tar_target(
    m_corner_vowel_distance_diag,
    {
      fit <- readRDS(m_corner_vowel_distance_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "01_diagnostics", "check_corner_vowel_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_diagnostics_rds(fit, out, ndraws_pp = 500, re_formula = NA)
    },
    format = "file"
  ),
  
  # Diagnostics: TF Distance
  tar_target(
    m_tf_distance_diag,
    {
      fit <- readRDS(m_tf_distance_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "01_diagnostics", "check_tf_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_diagnostics_rds(fit, out, ndraws_pp = 500, re_formula = NA)
    },
    format = "file"
  ),
  
  # Diagnostics: TB Distance
  tar_target(
    m_tb_distance_diag,
    {
      fit <- readRDS(m_tb_distance_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "01_diagnostics", "check_tb_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_diagnostics_rds(fit, out, ndraws_pp = 500, re_formula = NA)
    },
    format = "file"
  ),
  
  ## 03: Summarize models ----
  # Summarize: Intelligibility
  tar_target(
    m_intelligibility_summary,
    {
      fit <- readRDS(m_intelligibility_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_intelligibility.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_model_summary(fit, out)
      
      out <- here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_intelligibility.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_emmeans(fit, out)
    },
    format = "file"
  ),
  
  # Compute 7% pd for MCID in discussion
  tar_target(
    mcid_intelligibility_summary,
    {
      mcid <- get_7_mcid(
        fit = m_intelligibility_fit,
        out = here::here("manuscript", SPECS, "04_manuscript", "01_data_summaries", "summary_mcid.rds")
        )
    },
    format = "file"
  ),
  
  # Summarize: Precision
  tar_target(
    m_precision_summary,
    {
      fit <- readRDS(m_precision_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_precision.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_model_summary(fit, out)
      
      out <- here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_precision.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_emmeans(fit, out)
    },
    format = "file"
  ),
  
  # Summarize: corner_vowel_distance
  tar_target(
    m_corner_vowel_distance_summary,
    {
      fit <- readRDS(m_corner_vowel_distance_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_corner_vowel_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_model_summary(fit, out)
      
      out <- here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_corner_vowel_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_emmeans(fit, out)
    },
    format = "file"
  ),
  
  # Summarize: tf_distance
  tar_target(
    m_tf_distance_summary,
    {
      fit <- readRDS(m_tf_distance_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_tf_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_model_summary(fit, out)
      
      out <- here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_tf_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_emmeans(fit, out)
    },
    format = "file"
  ),
  
  # Summarize: tb_distance
  tar_target(
    m_tb_distance_summary,
    {
      fit <- readRDS(m_tb_distance_fit) 
      out <- here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_tb_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_model_summary(fit, out)
      
      out <- here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_tb_distance.rds")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      save_emmeans(fit, out)
    },
    format = "file"
  ),
  
  # Reliability ----
  ### Intra-Measurer ----
  
  #### 01: Demographic info ----
  tar_target(
    speaker_info_intra_rel,
    {
      speaker_info_intra_rel <- rio::import(here::here("data",
                                                       "01_interim",
                                                       "00_reliability", 
                                                       "rel_speaker_database_files.csv")) %>%
        dplyr::filter(intra_rel == TRUE)
      speaker_info_intra_rel
    }
  ),
  
  #### 05: Target segment data ----
  # Extract the acoustic and movement data for the target segments (/ai/ and the corner vowels)
  tar_target(target_vowel_segments_intra_rel,
             {
               textgrid_manifest
               fbw_manifest
               out <- here::here(
                 "manuscript",
                 SPECS,
                 "00_data",
                 "00_reliability",
                 "intra_rel_target_vowel_segments.RDS"
               )
               dir.create(out, showWarnings = F)
               
               get_target_vowel_segments(speakers = speaker_info_intra_rel,
                                         tg_path = here::here("data","01_interim","00_reliability", "01_textgrids", "intra_rel"),
                                         out_path = out,
                                         spec_name = SPECS,
                                         saveRDS = F,
                                         save_plots = F)
             },
             cue = tar_cue(mode = "thorough")
  ),
  
  #### 06: Calculate measures ----
  # Calculate the corner vowel data (corner vowel distance)
  tar_target(vowel_data_intra_rel,
             {
               target_vowel_segments
               target_vowel_segments_intra_rel
               compute_corner_vowel_distance(data = target_vowel_segments_intra_rel$acoustic,
                                             speakers = speaker_info_intra_rel,
                                             spec_name = SPECS,
                                             saveRDS = F)
             }
  ),
  # Calculated the diphthong data (TB distance)
  tar_target(diphthong_data_intra_rel,
             {
               target_vowel_segments
               compute_tb_distance(movement_data = target_vowel_segments_intra_rel$movement,
                                   speakers = speaker_info_intra_rel,
                                   spec_name = SPECS,
                                   saveRDS = F)
             }
  ),
  
  #### 07: Save Data ----
  # Save the prepped datasets (file targets so changes are tracked)
  # Corner Vowel Distance
  tar_target(data_corner_vowel_distance_intra_rel, {
    vowel_data_intra_rel
    p <- here::here("manuscript", SPECS, "00_data", "reliability", "intra_rel_data_corner_vowel_distance.rds")
    dir.create(here::here("manuscript", SPECS, "00_data", "reliability"), showWarnings = F)
    corner_vowel_distance <- vowel_data_intra_rel$corner_vowel_distance
    saveRDS(corner_vowel_distance, p)
    p
  }, format = "file"),
  
  # TB distance
  tar_target(data_tb_distance_intra_rel, {
    p <- here::here("manuscript", SPECS, "00_data", "reliability", "intra_rel_data_tb_distance.rds")
    dir.create(here::here("manuscript", SPECS, "00_data", "reliability"), showWarnings = F)
    tb_distance <- diphthong_data_intra_rel %>%
      dplyr::mutate(tb_distance = as.numeric(tb_distance))
    saveRDS(tb_distance, p)
    p
  }, format = "file"),
  
  ### Inter-Measurer ----
  
  #### 01: Demographic info ----
  tar_target(
    speaker_info_inter_rel,
    {
      speaker_info_inter_rel <- rio::import(here::here("data",
                                                       "01_interim",
                                                       "00_reliability", 
                                                       "rel_speaker_database_files.csv")) %>%
        dplyr::filter(inter_rel == TRUE)
      speaker_info_inter_rel
    }
  ),
  
  #### 05: Target segment data ----
  # Extract the acoustic and movement data for the target segments (/ai/ and the corner vowels)
  tar_target(target_vowel_segments_inter_rel,
             {
               textgrid_manifest
               fbw_manifest
               out <- here::here(
                 "manuscript",
                 SPECS,
                 "00_data",
                 "00_reliability",
                 "inter_rel_target_vowel_segments.RDS"
               )
               dir.create(out, showWarnings = F)
               
               get_target_vowel_segments(speakers = speaker_info_inter_rel,
                                         tg_path = here::here("data","01_interim","00_reliability", "01_textgrids", "inter_rel"),
                                         out_path = out,
                                         spec_name = SPECS,
                                         saveRDS = F,
                                         save_plots = F)
             },
             cue = tar_cue(mode = "thorough")
  ),
  
  #### 06: Calculate measures ----
  # Calculate the corner vowel data (corner vowel distance)
  tar_target(vowel_data_inter_rel,
             {
               target_vowel_segments
               compute_corner_vowel_distance(data = target_vowel_segments_inter_rel$acoustic,
                                             speakers = speaker_info_inter_rel,
                                             spec_name = SPECS,
                                             saveRDS = F)
             }
  ),
  # Calculated the diphthong data (TB distance)
  tar_target(diphthong_data_inter_rel,
             {
               target_vowel_segments
               compute_tb_distance(movement_data = target_vowel_segments_inter_rel$movement,
                                   speakers = speaker_info_inter_rel,
                                   spec_name = SPECS,
                                   saveRDS = F)
             }
  ),
  
  #### 07: Save Data ----
  # Save the prepped datasets (file targets so changes are tracked)
  # Corner Vowel Distance
  tar_target(data_corner_vowel_distance_inter_rel, {
    p <- here::here(
      "manuscript",
      SPECS,
      "00_data",
      "reliability",
      "inter_rel_data_corner_vowel_distance.rds"
    )
    dir.create(here::here("manuscript", SPECS, "00_data", "reliability"),
               showWarnings = F)
    corner_vowel_distance <- vowel_data_inter_rel$corner_vowel_distance
    saveRDS(corner_vowel_distance, p)
    p
  }, format = "file"), 
  
  # TB distance
  tar_target(data_tb_distance_inter_rel, {
    p <- here::here("manuscript", SPECS, "00_data", "reliability", "inter_rel_data_tb_distance.rds")
    dir.create(here::here("manuscript", SPECS, "00_data", "reliability"), showWarnings = F)
    tb_distance <- diphthong_data_inter_rel %>%
      dplyr::mutate(tb_distance = as.numeric(tb_distance))
    saveRDS(tb_distance, p)
    p
  }, format = "file"),
  
  ### Calculate Reliability Measures ----
  # Calculate measures of reliability for both listeners and measurers.
  tar_target(
    reliability_measures,
    {
      data_corner_vowel_distance
      data_corner_vowel_distance_intra_rel
      data_corner_vowel_distance_inter_rel
      
      data_tb_distance
      data_tb_distance_intra_rel
      data_tb_distance_inter_rel
      
      compute_reliability(listeners = listener_ratings$qc_passed,
                          out_path = here::here("manuscript",
                                                SPECS,
                                                "04_manuscript",
                                                "01_data_summaries",
                                                "summary_reliability.rds"),
                          spec_version = SPECS)
    },
    format = "file"
  ),
  
  # Figures ----
  ## 01 - Inter-listener Reliability ----
  tar_target(
    fig_01_interlistener_reliability,
    {
      data_intelligibility
      data_precision
      
      out <- here::here("manuscript", SPECS, "02_figures", "fig_01_interlistener_reliability.png")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      
      create_interlistener_reliability_figure(
        intelligibility_data = readRDS(data_intelligibility),
        precision_data = readRDS(data_precision),
        file_path = out
      )
    },
    format = "file"
  ),
  
  ## 02 - Intelligibility ----
  tar_target(
    fig_02_intelligibility,
    {
      m_intelligibility_summary
      fit <- readRDS(m_intelligibility_fit) 
      out <- here::here("manuscript", SPECS, "02_figures", "fig_02_intelligibility.png")
      emmeans <- readRDS(here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_intelligibility.rds"))
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      create_model_estimation_figures(
        model = fit,
        measure = "intelligibility",
        emmeans = emmeans,
        path = out,
        title = "Intelligibility",
        a_subtitle = "Predicted intelligibility ratings after controlling\nspeaker age.",
        a_x_axis_text = "Predicted intelligibility rating (%)",
        a_xlims = c(0, 1),
        b_subtitle = "Group effect (PwPD - Control)\nat conversational.",
        b_x_axis_text = "Average marginal effect of group\n(Δ% intelligibility)",
        c_subtitle = "Condition effect (clear - conversational)\nper group.",
        c_x_axis_text = "Average marginal effect of condition\n(Δ% intelligibility)"
      )
    },
    format = "file"
  ),
  
  ## 03 - Precision ----
  tar_target(
    fig_03_precision,
    {
      m_precision_summary
      fit <- readRDS(m_precision_fit) 
      out <- here::here("manuscript", SPECS, "02_figures", "fig_03_precision.png")
      emmeans <- readRDS(here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_precision.rds"))
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      create_model_estimation_figures(
        model = fit,
        measure = "precision",
        emmeans = emmeans,
        path = out,
        title = "Articulatory Precision",
        a_subtitle = "Predicted precision ratings after controlling\nspeaker age.",
        a_x_axis_text = "Predicted precision rating (%)",
        a_xlims = c(0, 1),
        b_subtitle = "Group effect (PwPD - Control)\nat conversational.",
        b_x_axis_text = "Average marginal effect of group\n(Δ% precision)",
        c_subtitle = "Condition effect (clear - conversational)\nper group.",
        c_x_axis_text = "Average marginal effect of condition\n(Δ% precision)"
      )
    },
    format = "file"
  ),
  
  ## 04 - Corner Vowel Distance ----
  tar_target(
    fig_04_corner_vowel_distance,
    {
      m_corner_vowel_distance_summary
      fit <- readRDS(m_corner_vowel_distance_fit) 
      out <- here::here("manuscript", SPECS, "02_figures", "fig_04_corner_vowel_distance.png")
      emmeans <- readRDS(here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_corner_vowel_distance.rds"))
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      create_model_estimation_figures(
        model = fit,
        measure = "corner_vowel_distance",
        emmeans = emmeans,
        path = out,
        title = "Corner Vowel Distance",
        a_subtitle = "Predicted corner vowel distance after\ncontrolling speaker age and sex.",
        a_x_axis_text = "Predicted Corner Vowel Distance (Bark)",
        a_xlims = NULL,
        b_subtitle = "Group effect (PwPD - Control)\nat conversational.",
        b_x_axis_text = "Average marginal effect of group\n(Δ Bark)",
        c_subtitle = "Condition effect (clear - conversational)\nper group.",
        c_x_axis_text = "Average marginal effect of condition\n(Δ Bark)"
      )
    },
    format = "file"
  ),
  
  ## 05 - TF Distance ----
  tar_target(
    fig_05_tf_distance,
    {
      m_tf_distance_summary
      fit <- readRDS(m_tf_distance_fit) 
      out <- here::here("manuscript", SPECS, "02_figures", "fig_05_tf_distance.png")
      emmeans <- readRDS(here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_tf_distance.rds"))
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      create_model_estimation_figures(
        model = fit,
        measure = "tf_distance",
        emmeans = emmeans,
        path = out,
        title = "TF Distance",
        a_subtitle = "Predicted TF distance for /tɛs/ after\ncontrolling duration, speaker age and sex.",
        a_x_axis_text = "Predicted TF distance (mm)",
        a_xlims = NULL,
        b_subtitle = "Group effect (PwPD - Control)\nat conversational.",
        b_x_axis_text = "Average marginal effect of group\n(Δ mm)",
        c_subtitle = "Condition effect (clear - conversational)\nper group.",
        c_x_axis_text = "Average marginal effect of condition\n(Δ mm)"
      )
    },
    format = "file"
  ),
  
  ## 06 - TB Distance ----
  tar_target(
    fig_06_tb_distance,
    {
      m_tb_distance_summary
      fit <- readRDS(m_tb_distance_fit) 
      out <- here::here("manuscript", SPECS, "02_figures", "fig_06_tb_distance.png")
      emmeans <- readRDS(here::here("manuscript", SPECS, "01_models", "03_emmeans", "emmeans_tb_distance.rds"))
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      create_model_estimation_figures(
        model = fit,
        measure = "tb_distance",
        emmeans = emmeans,
        path = out,
        title = "TB Distance",
        a_subtitle = "Predicted TB distance for /aɪ/ after\ncontrolling duration, speaker age and sex.",
        a_x_axis_text = "Predicted TB distance (mm)",
        a_xlims = NULL,
        b_subtitle = "Group effect (PwPD - Control)\nat conversational.",
        b_x_axis_text = "Average marginal effect of group\n(Δ mm)",
        c_subtitle = "Condition effect (clear - conversational)\nper group.",
        c_x_axis_text = "Average marginal effect of condition\n(Δ mm)"
      )
    },
    format = "file"
  ),
  
  # Tables ----
  
  ## 01 - Speaker demographics ----
  tar_target(
    tbl_01_speaker_demo,
    {
      out <- here::here("manuscript", SPECS, "03_tables", "tbl_01_speaker_demo.html")
      dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
      
      create_speaker_demo(
        speaker_info = speaker_info,
        file = out
      )
    },
    format = "file"
  ),
  
  ## 02 - Perceptual Models Table ----
  tar_target(
    tbl_02_perceptual_models,
    {
      create_perceptual_summary_tbl(
        int_model_summary = base::readRDS(here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_intelligibility.rds")),
        precision_model_summary = base::readRDS(here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_precision.rds")),
        file_path = here::here("manuscript", SPECS, "03_tables", "supptbl_01_perceptual_models.html")
      )
    }
  ),
  
  ## 03 - Corner Vowel Models Table ----
  tar_target(
    tbl_04_corner_vowel_model,
    {
      create_corner_vowel_summary_tbl(
        corner_vowel_distance_model_summary = base::readRDS(here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_corner_vowel_distance.rds")),
        file_path = here::here("manuscript", SPECS, "03_tables", "supptbl_02_corner_vowel_models.html")
      )
    }
  ),
  
  ## 04 - TF & TB Models Table ----
  tar_target(
    tbl_05_lingual_model,
    {
      create_lingual_summary_tbl(
        tf_distance_model_summary = base::readRDS(here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_tf_distance.rds")),
        tb_distance_model_summary = base::readRDS(here::here("manuscript", SPECS, "01_models", "02_summaries", "summary_tb_distance.rds")),
        file_path = here::here("manuscript", SPECS, "03_tables", "supptbl_03_lingual_models.html")
      )
    }
  ),
  
  ## 05 - Clear Post hoc Comparisons ----
  tar_target(
    tbl_06_clear_comparisons,
    {
      create_clear_comparison_tbl(
        file_path = here::here("manuscript", SPECS, "03_tables", "supptbl_04_clear_comparisons.html")
      )
    }
  ),
  
  # Manuscript ----
  
  ## Prep RQ results ----
  tar_target(
    emmeans_results,
    {
      m_intelligibility_fit
      m_intelligibility_summary
      m_precision_fit
      m_precision_summary
      m_corner_vowel_distance_fit
      m_corner_vowel_distance_summary
      m_tb_distance_fit
      m_tb_distance_summary
      m_tf_distance_fit
      m_tf_distance_summary
      
      emmeans_dir <- here::here("manuscript", SPECS, "01_models", "03_emmeans")
      out <- here::here("manuscript", SPECS, "04_manuscript", "01_data_summaries", "summary_research_questions.rds")
      dir.create(out, recursive = T, showWarnings = F)
      
      save_rq_lists(
        emmeans_dir = emmeans_dir,
        out_path = out
      )
    },
    format = "file"
  )
  
)
# R/listener_reliability.R

library(tidyverse)
library(irr)
library(weights)

compute_reliability <- function(listeners, out_path, spec_version) {
  intra_absolute_error <- listeners %>%
    dplyr::filter(!is.na(rel_rating)) %>%
    dplyr::mutate(abs_error = abs(rating - rel_rating),
                  measure = case_when(
                    rating_type == "Int" ~ "intelligibility",
                    rating_type == "AP" ~ "precision"
                  )) %>%
    dplyr::group_by(measure) %>%
    dplyr::summarise(mae = mean(abs_error, na.rm = T),
                     sdae = sd(abs_error, na.rm = T)) %>%
    dplyr::mutate(apa_ae = paste0("MAE = ",weights::rd(mae),", SDAE = ",weights::rd(sdae)))

  intra_icc_int <- listeners %>%
    dplyr::filter(!is.na(rel_rating)) %>% 
    dplyr::filter(rating_type == "Int") %>%
    dplyr::select(rating, rel_rating) %>%
    irr::icc(.,
           model = "twoway",
           type = "agreement",
           unit = "single")
  
  intra_icc_precision <- listeners %>%
    dplyr::filter(!is.na(rel_rating)) %>% 
    dplyr::filter(rating_type == "AP") %>%
    dplyr::select(rating, rel_rating) %>%
    irr::icc(.,
             model = "twoway",
             type = "agreement",
             unit = "single")
  
  intra_listener <- tibble::tibble_row(
      measure = "intelligibility",
      model = intra_icc_int$model,
      type = intra_icc_int$type,
      unit = intra_icc_int$unit,
      icc = intra_icc_int$value,
      ll = intra_icc_int$lbound,
      ul = intra_icc_int$ubound,
      apa = paste0("ICC(2,1)=",
                   weights::rd(intra_icc_int$value),
                   ", 95% CI [",
                   weights::rd(intra_icc_int$lbound),
                   ", ",
                   weights::rd(intra_icc_int$ubound),
                   "]")) %>%
    tibble::add_row(
      measure = "precision",
      model = intra_icc_precision$model,
      type = intra_icc_precision$type,
      unit = intra_icc_precision$unit,
      icc = intra_icc_precision$value,
      ll = intra_icc_precision$lbound,
      ul = intra_icc_precision$ubound,
      apa = paste0("ICC(2,1)=",
                   weights::rd(intra_icc_precision$value),
                   ", 95% CI [",
                   weights::rd(intra_icc_precision$lbound),
                   ", ",
                   weights::rd(intra_icc_precision$ubound),
                   "]")
    ) %>%
    dplyr::mutate(interpretation = case_when(
      icc < .5 ~ "poor",
      dplyr::between(icc, .5, .75) ~ "moderate",
      dplyr::between(icc, .75, .9) ~ "good",
      icc > .9 ~ "excellent"
    )) %>%
    base::merge(intra_absolute_error)
  
  ## Create obejcts for measures
  intelligibility <- list(
    intra = intra_listener %>% dplyr::filter(measure == "intelligibility")
  )
  
  precision <- list(
    intra = intra_listener %>% dplyr::filter(measure == "precision")
  )

# Measurer Data ----
  ## Corner Vowel Distance ----
  rel_corner_vowel_distance <- 
    # Load in the full data
    readRDS(
      here::here("manuscript", spec_version, "00_data", "data_corner_vowel_distance.rds")
    ) %>%
    base::merge(
    .,
    # Load in the intra rel data
    readRDS(
      here::here(
        "manuscript",
        spec_version,
        "00_data",
        "reliability",
        "intra_rel_data_corner_vowel_distance.rds"
      )
    ) %>%
      dplyr::select(database_id, group, condition, label, intra_rel = corner_vowel_distance),
    all = T
  ) %>%
    base::merge(
      .,
      # Load in the inter rel data
      readRDS(
        here::here(
          "manuscript",
          spec_version,
          "00_data",
          "reliability",
          "inter_rel_data_corner_vowel_distance.rds"
        )
      ) %>%
        dplyr::select(database_id, group, condition, label, inter_rel = corner_vowel_distance),
      all = T
    )

  # Intra-Measurer Reliability Measures
  intra_ae_corner_vowel_distance <- rel_corner_vowel_distance %>%
    dplyr::filter(!is.na(intra_rel)) %>%
    dplyr::mutate(
      measure = "corner_vowel_distance",
      abs_error = abs(corner_vowel_distance - intra_rel)) %>%
    dplyr::group_by(measure, corner_vowel_distance_unit) %>%
    dplyr::summarise(mae = mean(abs_error),
                     sdae = sd(abs_error),
                     .groups = "drop") %>%
    dplyr::mutate(apa_ae = paste0("MAE = ",weights::rd(mae),", SDAE = ",weights::rd(sdae)))
  
  intra_icc_corner_vowel_distance <- rel_corner_vowel_distance %>%
    dplyr::filter(!is.na(intra_rel)) %>%
    dplyr::select(corner_vowel_distance, intra_rel) %>%
    irr::icc(.,
             model = "twoway",
             type = "agreement",
             unit = "single")
  
  intra_corner_vowel_distance <- tibble::tibble_row(
    measure = "corner_vowel_distance",
    model = intra_icc_corner_vowel_distance$model,
    type = intra_icc_corner_vowel_distance$type,
    unit = intra_icc_corner_vowel_distance$unit,
    icc = intra_icc_corner_vowel_distance$value,
    ll = intra_icc_corner_vowel_distance$lbound,
    ul = intra_icc_corner_vowel_distance$ubound,
    apa = paste0("ICC(2,1)=",
                 weights::rd(intra_icc_corner_vowel_distance$value),
                 ", 95% CI [",
                 weights::rd(intra_icc_corner_vowel_distance$lbound),
                 ", ",
                 weights::rd(intra_icc_corner_vowel_distance$ubound),
                 "]")) %>%
    dplyr::mutate(interpretation = case_when(
      icc < .5 ~ "poor",
      dplyr::between(icc, .5, .75) ~ "moderate",
      dplyr::between(icc, .75, .9) ~ "good",
      icc > .9 ~ "excellent"
    )) %>%
    base::merge(intra_ae_corner_vowel_distance, .)
  
  # Inter-Measurer Reliability Measures
  inter_ae_corner_vowel_distance <- rel_corner_vowel_distance %>%
    dplyr::filter(!is.na(inter_rel)) %>%
    dplyr::mutate(
      measure = "corner_vowel_distance",
      abs_error = abs(corner_vowel_distance - inter_rel)) %>%
    dplyr::group_by(measure, corner_vowel_distance_unit) %>%
    dplyr::summarise(mae = mean(abs_error),
                     sdae = sd(abs_error),
                     .groups = "drop") %>%
    dplyr::mutate(apa_ae = paste0("MAE = ",weights::rd(mae),", SDAE = ",weights::rd(sdae)))
  
  inter_icc_corner_vowel_distance <- rel_corner_vowel_distance %>%
    dplyr::filter(!is.na(inter_rel)) %>%
    dplyr::select(corner_vowel_distance, inter_rel) %>%
    irr::icc(.,
             model = "twoway",
             type = "agreement",
             unit = "single")
  
  inter_corner_vowel_distance <- tibble::tibble_row(
    measure = "corner_vowel_distance",
    model = inter_icc_corner_vowel_distance$model,
    type = inter_icc_corner_vowel_distance$type,
    unit = inter_icc_corner_vowel_distance$unit,
    icc = inter_icc_corner_vowel_distance$value,
    ll = inter_icc_corner_vowel_distance$lbound,
    ul = inter_icc_corner_vowel_distance$ubound,
    apa = paste0("ICC(2,1)=",
                 weights::rd(inter_icc_corner_vowel_distance$value),
                 ", 95% CI [",
                 weights::rd(inter_icc_corner_vowel_distance$lbound),
                 ", ",
                 weights::rd(inter_icc_corner_vowel_distance$ubound),
                 "]")) %>%
    dplyr::mutate(interpretation = case_when(
      icc < .5 ~ "poor",
      dplyr::between(icc, .5, .75) ~ "moderate",
      dplyr::between(icc, .75, .9) ~ "good",
      icc > .9 ~ "excellent"
    )) %>%
    base::merge(inter_ae_corner_vowel_distance, .)
  
  corner_vowel_distance <- list(
    intra = intra_corner_vowel_distance,
    inter = inter_corner_vowel_distance
  )
  
  ## TB Distance ----
  rel_tb_distance <- base::merge(
    # Load in the full data
    readRDS(
      here::here("manuscript", spec_version, "00_data", "data_tb_distance.rds")
    ),
    # Load in the intra rel data
    readRDS(
      here::here(
        "manuscript",
        spec_version,
        "00_data",
        "reliability",
        "intra_rel_data_tb_distance.rds"
      )
    ) %>%
      dplyr::select(database_id, group, condition, label, intra_rel = tb_distance),
    all = T
  ) %>%
    base::merge(
      .,
      # Load in the inter rel data
      readRDS(
        here::here(
          "manuscript",
          spec_version,
          "00_data",
          "reliability",
          "inter_rel_data_tb_distance.rds"
        )
      ) %>%
        dplyr::select(database_id, group, condition, label, inter_rel = tb_distance),
      all = T
    )
  
  # Intra-Measurer Reliability Measures
  intra_ae_tb_distance <- rel_tb_distance %>%
    dplyr::filter(!is.na(intra_rel)) %>%
    dplyr::mutate(
      measure = "tb_distance",
      abs_error = abs(tb_distance - intra_rel)) %>%
    dplyr::group_by(measure, tb_distance_unit) %>%
    dplyr::summarise(mae = mean(abs_error),
                     sdae = sd(abs_error),
                     .groups = "drop") %>%
    dplyr::mutate(apa_ae = paste0("MAE = ",weights::rd(mae),", SDAE = ",weights::rd(sdae)))
  
  intra_icc_tb_distance <- rel_tb_distance %>%
    dplyr::filter(!is.na(intra_rel)) %>%
    dplyr::select(tb_distance, intra_rel) %>%
    irr::icc(.,
             model = "twoway",
             type = "agreement",
             unit = "single")
  
  intra_tb_distance <- tibble::tibble_row(
    measure = "tb_distance",
    model = intra_icc_tb_distance$model,
    type = intra_icc_tb_distance$type,
    unit = intra_icc_tb_distance$unit,
    icc = intra_icc_tb_distance$value,
    ll = intra_icc_tb_distance$lbound,
    ul = intra_icc_tb_distance$ubound,
    apa = paste0("ICC(2,1)=",
                 weights::rd(intra_icc_tb_distance$value),
                 ", 95% CI [",
                 weights::rd(intra_icc_tb_distance$lbound),
                 ", ",
                 weights::rd(intra_icc_tb_distance$ubound),
                 "]")) %>%
    dplyr::mutate(interpretation = case_when(
      icc < .5 ~ "poor",
      dplyr::between(icc, .5, .75) ~ "moderate",
      dplyr::between(icc, .75, .9) ~ "good",
      icc > .9 ~ "excellent"
    )) %>%
    base::merge(intra_ae_tb_distance, .)
  
  # Inter-Measurer Reliability Measures
  inter_ae_tb_distance <- rel_tb_distance %>%
    dplyr::filter(!is.na(inter_rel)) %>%
    dplyr::mutate(
      measure = "tb_distance",
      abs_error = abs(tb_distance - inter_rel)) %>%
    dplyr::group_by(measure, tb_distance_unit) %>%
    dplyr::summarise(mae = mean(abs_error),
                     sdae = sd(abs_error),
                     .groups = "drop") %>%
    dplyr::mutate(apa_ae = paste0("MAE = ",weights::rd(mae),", SDAE = ",weights::rd(sdae)))
  
  inter_icc_tb_distance <- rel_tb_distance %>%
    dplyr::filter(!is.na(inter_rel)) %>%
    dplyr::select(tb_distance, inter_rel) %>%
    irr::icc(.,
             model = "twoway",
             type = "agreement",
             unit = "single")
  
  inter_tb_distance <- tibble::tibble_row(
    measure = "tb_distance",
    model = inter_icc_tb_distance$model,
    type = inter_icc_tb_distance$type,
    unit = inter_icc_tb_distance$unit,
    icc = inter_icc_tb_distance$value,
    ll = inter_icc_tb_distance$lbound,
    ul = inter_icc_tb_distance$ubound,
    apa = paste0("ICC(2,1)=",
                 weights::rd(inter_icc_tb_distance$value),
                 ", 95% CI [",
                 weights::rd(inter_icc_tb_distance$lbound),
                 ", ",
                 weights::rd(inter_icc_tb_distance$ubound),
                 "]")) %>%
    dplyr::mutate(interpretation = case_when(
      icc < .5 ~ "poor",
      dplyr::between(icc, .5, .75) ~ "moderate",
      dplyr::between(icc, .75, .9) ~ "good",
      icc > .9 ~ "excellent"
    )) %>%
    base::merge(inter_ae_tb_distance, .)
  
  tb_distance <- list(
    intra = intra_tb_distance,
    inter = inter_tb_distance
  )
  
  
  ## Final object to be saved
  reliability <- list(
    intelligibility = intelligibility,
    precision = precision,
    corner_vowel_distance = corner_vowel_distance,
    tb_distance = tb_distance
  )
  
  saveRDS(reliability, file = out_path)
}


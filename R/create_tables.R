# R/create_tables.R

library(tidyverse)
library(tidyr)
library(gt)
source("R/models_registry.R")

# Tbl 1: Speaker Demographics ----
create_speaker_demo <- function(speaker_info, file) {
  speaker_list <-
    speaker_info %>%
    
    # Making sure dxTime is numeric
    dplyr::select(database_id, sex, group, severity, age) %>%
    distinct() %>%
    
    # Refactoring Sex
    dplyr::mutate(sex = factor(
      sex,
      levels = c("M",
                 "F"),
      labels = c("Male Speakers",
                 "Female Speakers")
    ))
  
  ## 1) Building the main table
  speakers <- rbind(
    speaker_list %>% mutate(sex = "All"),
    speaker_list
  )
  
  speaker_table <- speakers %>%
    rbind(., speakers %>% mutate(group = "All", severity = "")) %>%
    mutate(
      group = factor(
        group,
        levels = c("HC", "PD", "All"),
        labels = c("Control", "PwPD", "Total")
      ),
      severity = factor(
        severity,
        levels = c("Control", "Mild", "Moderate", "Severe", "Profound", "All"),
        labels = c("Control", "Mild", "Moderate", "Severe", "Profound", "All")
      )
    ) %>%
    group_by(group, sex, severity) %>%
    summarise(
      n     = NROW(database_id),
      age_m = mean(age, na.rm = TRUE),
      age_sd = sd(age, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = sex,
      values_from = c(n, age_m, age_sd),
      names_sep = "-"
    ) %>%
    select(
      group,
      severity,
      contains("-All"),
      contains("-Female Speakers"),
      contains("-Male Speakers")
    )
  
  ## 2) Create the PwPD "Total" row from raw data (this yields correct pooled SD)
  pw_total <- speaker_list %>%
    filter(group == "PD") %>%                              # just PwPD
    mutate(sex = recode(sex, "Female" = "Female Speakers", "Male"   = "Male Speakers")) %>%
    bind_rows(., mutate(., sex = "All")) %>%               # add All view
    group_by(sex) %>%
    summarise(
      `n`     = dplyr::n(),
      `age_m` = mean(age, na.rm = TRUE),
      `age_sd` = sd(age, na.rm = TRUE),
      # pooled b/c from raw
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = sex,
      values_from = c(n, age_m, age_sd),
      names_sep = "-"
    ) %>%
    mutate(
      group   = factor(
        "PD",
        levels = c("HC", "PD"),
        labels = c("HC", "PwPD")
      ),
      severity = factor("All", levels = levels(speaker_table$severity))
    ) %>%
    select(names(speaker_table))                            # match columns
  
  ## 3) Append the PwPD Total row and (optionally) place it last in PwPD
  speaker_table2 <- bind_rows(speaker_table, pw_total) %>%
    dplyr::mutate(group = factor(group,
                                 levels = c("Control", "PwPD", "Total PwPD", "Total"))) %>%
    arrange(group, if_else(severity == "Total", 1L, 0L))   # Total at bottom per group
  
  ## 4) Render with gt — no age summary_rows needed now
  speaker_table2 %>%
    gt(groupname_col = "group", row_group_as_column = TRUE) %>%
    tab_spanner(
      label = "Age",
      columns = contains("age"),
      level = 1,
      gather = F
    ) %>%
    tab_spanner(label = "All Speakers",
                columns = contains("all"),
                level = 2) %>%
    tab_spanner(label = "Female Speakers",
                columns = contains("-Female"),
                level = 2) %>%
    tab_spanner(label = "Male Speakers",
                columns = contains("-Male"),
                level = 2) %>%
    sub_missing(missing_text = "—") %>%
    gt::sub_missing(columns = "severity", missing_text = "") %>%
    fmt_number(columns = contains("age"), decimals = 2) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = group == "Total")) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_row_groups(groups = "Total")) %>%
    cols_label(
      "severity" ~ "Severity",
      contains("n-") ~ md("*n*"),
      contains("_m") ~ md("*M*"),
      contains("_sd") ~ md("*SD*")
    ) %>%
    
    gt::gtsave(filename = file)
}

# Tbl 2: Model Summaries - Intelligibility & Precision ----
create_perceptual_summary_tbl <- function(int_model_summary, precision_model_summary, file_path) {
  
  random_effects <- base::merge(
    get_random_effects_data(readRDS(here::here("manuscript", SPECS,"01_models", "m_intelligibility.rds"))) %>%
    dplyr::rename(Int_estimate = estimate),
    
    get_random_effects_data(readRDS(here::here("manuscript", SPECS,"01_models", "m_precision.rds"))) %>%
      dplyr::rename(AP_estimate = estimate)) %>%
    
    dplyr::mutate(term = factor(measure,
                                   levels = c("r2_marginal", "r2_condition", "n_speakers", "n_listeners", "observations")),
                  effect = "Random Effects",
                  Int_estimate = case_when(
                    term %in% c("r2_marginal", "r2_condition") ~ weights::rd(Int_estimate, 2),
                    term %in% c("n_speakers", "n_listeners", "observations") ~ weights::rd(Int_estimate, digits=0)
                  ),
                  AP_estimate = case_when(
                    term %in% c("r2_marginal", "r2_condition") ~ weights::rd(AP_estimate, 2),
                    term %in% c("n_speakers", "n_listeners", "observations") ~ weights::rd(AP_estimate, digits=0)
                  )) %>%
    arrange(term) %>%
    dplyr::select(!measure)
  
  tbl_data <- base::merge(
    int_model_summary %>%
      dplyr::filter(effect == "fixed") %>%
      dplyr::select(
        effect,
        term,
        Int_estimate = estimate,
        Int_l_95_CI = l_95_CI,
        Int_u_95_CI = u_95_CI,
        Int_pd = pd,
        Int_robust = robust
      ) %>%
      dplyr::mutate(term = as.factor(term)),
    
    precision_model_summary %>%
      dplyr::filter(effect == "fixed") %>%
      dplyr::select(
        effect,
        term,
        AP_estimate = estimate,
        AP_l_95_CI = l_95_CI,
        AP_u_95_CI = u_95_CI,
        AP_pd = pd,
        AP_robust = robust
      ) %>%
      dplyr::mutate(term = as.factor(term))
  ) %>%
    
    # Making the terms prettier
    dplyr::mutate(
      across(contains("CI"), ~ weights::rd(., digits = 2)),
      across(contains("Estimate"), ~ weights::rd(., digits = 2)),
      across(contains("_pd"), ~ scales::percent(round(.,digits = 2))),
      effect = factor(effect,
                      levels = c("fixed"),
                      labels = c("Fixed Effects"))) %>% 
    
    base::merge(., random_effects, all = T) %>%
    dplyr::mutate(terms = factor(
      term,
      levels = c(
        "(Intercept)",
        "severityMild",
        "severityModerate",
        "severitySevere",
        "severityProfound",
        "conditionmoreClear",
        "severityMild:conditionmoreClear",
        "severityModerate:conditionmoreClear",
        "severitySevere:conditionmoreClear",
        "severityProfound:conditionmoreClear",
        "age",
        "r2_marginal",
        "r2_condition",
        "n_speakers",
        "n_listeners",
        "observations"
      ),
      labels = c(
        "Intercept (HC × Conversational)",
        "Mild × Conversational",
        "Moderate × Conversational",
        "Severe × Conversational",
        "Profound × Conversational",
        "HC × Clear",
        "Mild × Clear",
        "Moderate × Clear",
        "Severe × Clear",
        "Profound × Clear",
        "Age",
        md("Marginal R^2^"),
        md("Conditional R^2^"),
        md("N~speakers~"),
        md("N~listeners~"),
        "Observations"
      ),
    )) %>%
    dplyr::relocate(terms, .after = term) %>%
    arrange(terms) %>%
    dplyr::select(effect,
                  term,
                  terms,
                  Int_estimate,
                  Int_l_95_CI,
                  Int_u_95_CI,
                  Int_pd,
                  Int_robust,
                  AP_estimate,
                  AP_l_95_CI,
                  AP_u_95_CI,
                  AP_pd,
                  AP_robust)
  
  tbl_data %>%
    dplyr::select(!term) %>%
    gt(groupname_col = "effect",
       rowname_col = "terms") %>%
    gt::tab_spanner(label = "95% CrI",
                    columns = contains("_CI"),
                    level = 1,
                    gather = F) %>%
    gt::tab_spanner(label = "Intelligibility",
                    columns = contains("Int_"),
                    level = 2) %>%
    gt::tab_spanner(label = "Articulatory Precision",
                    columns = contains("AP_"),
                    level = 2) %>%
    ## Intelligibility
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_body(columns = contains("Int_"),
                                 rows = Int_robust == "robust")
    ) |> 
    ## Articulatory precision
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_body(columns = contains("AP_"),
                                 rows = AP_robust == "robust")) %>%
    gt::cols_hide(contains("robust")) %>%
    gt::cols_label(
      contains("estimate") ~ "Estimate",
      contains("l_95") ~ "LL",
      contains("u_95") ~ "UL",
      contains("_pd") ~ "pd",
      terms ~ "Terms"
    ) %>%
    fmt_markdown(columns = terms) %>%
    
    cols_align(
      align = "left",
      columns = "terms"
    ) %>%
    
    tab_stub_indent(
      rows = everything(),
      indent = 5
    ) %>% 
    
    gt::sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = ""
    ) %>%

  gt::gtsave(filename = file_path)
}

# Tbl 3: Model Summaries - Vowel Distance ----
create_corner_vowel_summary_tbl <- function(corner_vowel_distance_model_summary, file_path) {
  
  # NA-safe wrappers
  safe_rd <- function(x, digits = 2) {
    if (!is.numeric(x)) return(x)
    out <- x
    ok  <- !is.na(x)
    out[ok] <- weights::rd(x[ok], digits = digits)
    out
  }
  
  safe_percent <- function(x, digits = 2) {
    # returns character, preserves NA
    out <- rep(NA_character_, length(x))
    ok  <- !is.na(x)
    out[ok] <- scales::percent(round(x[ok], digits = digits))
    out
  }
  
  random_effects <- get_random_effects_data(readRDS(here::here("manuscript", SPECS,"01_models", "m_corner_vowel_distance.rds"))) %>%
      dplyr::rename(corner_vowel_distance_estimate = estimate) %>%
    
    dplyr::mutate(term = factor(measure,
                                levels = c("r2_marginal", "r2_condition", "n_speakers", "observations")),
                  effect = "Random Effects",
                  corner_vowel_distance_estimate = case_when(
                    term %in% c("r2_marginal", "r2_condition") ~ weights::rd(corner_vowel_distance_estimate, 2),
                    term %in% c("n_speakers", "observations") ~ weights::rd(corner_vowel_distance_estimate, digits=0)
                  )) %>%
    arrange(term) %>%
    dplyr::select(!measure)
  
  tbl_data <- corner_vowel_distance_model_summary %>%
      dplyr::filter(effect == "fixed") %>%
      dplyr::select(
        effect,
        term,
        corner_vowel_distance_estimate = estimate,
        corner_vowel_distance_l_95_CI = l_95_CI,
        corner_vowel_distance_u_95_CI = u_95_CI,
        corner_vowel_distance_pd = pd,
        corner_vowel_distance_robust = robust
      ) %>%
      dplyr::mutate(term = as.factor(term)) %>%
    
    # Making the terms prettier
    dplyr::mutate(
      across(contains("CI"), safe_rd),        
      across(contains("Estimate"), safe_rd),
      across(contains("_pd"), safe_percent),
      effect = factor(effect,
                      levels = "fixed",
                      labels = "Fixed Effects")
    ) %>% 
    
    base::merge(., random_effects, all = T) %>%
    dplyr::mutate(terms = factor(
      term,
      levels = c(
        "(Intercept)",
        "severityMild",
        "severityModerate",
        "severitySevere",
        "severityProfound",
        "conditionmoreClear",
        "severityMild:conditionmoreClear",
        "severityModerate:conditionmoreClear",
        "severitySevere:conditionmoreClear",
        "severityProfound:conditionmoreClear",
        "age",
        "sexM",
        "duration",
        "r2_marginal",
        "r2_condition",
        "n_speakers",
        "n_listeners",
        "observations"
      ),
      labels = c(
        "Intercept (HC × Conversational)",
        "Mild × Conversational",
        "Moderate × Conversational",
        "Severe × Conversational",
        "Profound × Conversational",
        "HC × Clear",
        "Mild × Clear",
        "Moderate × Clear",
        "Severe × Clear",
        "Profound × Clear",
        "Age",
        "Sex [Male]",
        "Duration",
        md("Marginal R^2^"),
        md("Conditional R^2^"),
        md("N~speakers~"),
        md("N~listeners~"),
        "Observations"
      ),
    )) %>%
    dplyr::relocate(terms, .after = term) %>%
    arrange(terms) %>%
    dplyr::select(effect,
                  term,
                  terms,
                  corner_vowel_distance_estimate,
                  corner_vowel_distance_l_95_CI,
                  corner_vowel_distance_u_95_CI,
                  corner_vowel_distance_pd,
                  corner_vowel_distance_robust)
  
  tbl_data %>%
    dplyr::select(!term) %>%
    gt(groupname_col = "effect",
       rowname_col = "terms") %>%
    gt::tab_spanner(label = "95% CrI",
                    columns = contains("_CI"),
                    level = 1,
                    gather = F) %>%
    gt::tab_spanner(label = "Corner Vowel Distance",
                    columns = contains("corner_vowel_distance_"),
                    level = 2) %>%
    ## Corner Vowel Distance
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_body(columns = contains("corner_vowel_distance_"),
                                 rows = corner_vowel_distance_robust == "robust")
    ) |> 
    gt::cols_hide(contains("robust")) %>%
    gt::cols_label(
      contains("estimate") ~ "Estimate",
      contains("l_95") ~ "LL",
      contains("u_95") ~ "UL",
      contains("_pd") ~ "pd",
      terms ~ "Terms"
    ) %>%
    fmt_markdown(columns = terms) %>%
    
    cols_align(
      align = "left",
      columns = "terms"
    ) %>%
    
    tab_stub_indent(
      rows = everything(),
      indent = 5
    ) %>% 
    
    gt::sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = ""
    ) %>%
    
    gt::gtsave(filename = file_path)
}

# Tbl 4: Model Summaries - TF & TB Distance ----
create_lingual_summary_tbl <- function(tf_distance_model_summary, tb_distance_model_summary, file_path) {
  
  random_effects <- base::merge(
    get_random_effects_data(readRDS(here::here("manuscript", SPECS,"01_models", "m_tf_distance.rds"))) %>%
      dplyr::rename(tf_estimate = estimate),
    
    get_random_effects_data(readRDS(here::here("manuscript", SPECS,"01_models", "m_tb_distance.rds"))) %>%
      dplyr::rename(tb_estimate = estimate)) %>%
    
    dplyr::mutate(term = factor(measure,
                                levels = c("r2_marginal", "r2_condition", "n_speakers", "n_listeners", "observations")),
                  effect = "Random Effects",
                  tf_estimate = case_when(
                    term %in% c("r2_marginal", "r2_condition") ~ weights::rd(tf_estimate, 2),
                    term %in% c("n_speakers", "n_listeners", "observations") ~ weights::rd(tf_estimate, digits=0)
                  ),
                  tb_estimate = case_when(
                    term %in% c("r2_marginal", "r2_condition") ~ weights::rd(tb_estimate, 2),
                    term %in% c("n_speakers", "n_listeners", "observations") ~ weights::rd(tb_estimate, digits=0)
                  )) %>%
    arrange(term) %>%
    dplyr::select(!measure)
  
  tbl_data <- base::merge(
    tf_distance_model_summary %>%
      dplyr::filter(effect == "fixed") %>%
      dplyr::select(
        effect,
        term,
        tf_estimate = estimate,
        tf_l_95_CI = l_95_CI,
        tf_u_95_CI = u_95_CI,
        tf_pd = pd,
        tf_robust = robust
      ) %>%
      dplyr::mutate(term = as.factor(term)),
    
    tb_distance_model_summary %>%
      dplyr::filter(effect == "fixed") %>%
      dplyr::select(
        effect,
        term,
        tb_estimate = estimate,
        tb_l_95_CI = l_95_CI,
        tb_u_95_CI = u_95_CI,
        tb_pd = pd,
        tb_robust = robust
      ) %>%
      dplyr::mutate(term = as.factor(term))
  ) %>%
    
    # Making the terms prettier
    dplyr::mutate(
      across(contains("CI"), ~ weights::rd(., digits = 2)),
      across(contains("Estimate"), ~ weights::rd(., digits = 2)),
      across(contains("_pd"), ~ scales::percent(round(.,digits = 2))),
      effect = factor(effect,
                      levels = c("fixed"),
                      labels = c("Fixed Effects"))) %>% 
    
    base::merge(., random_effects, all = T) %>%
    dplyr::mutate(terms = factor(
      term,
      levels = c(
        "(Intercept)",
        "severityMild",
        "severityModerate",
        "severitySevere",
        "severityProfound",
        "conditionmoreClear",
        "severityMild:conditionmoreClear",
        "severityModerate:conditionmoreClear",
        "severitySevere:conditionmoreClear",
        "severityProfound:conditionmoreClear",
        "age",
        "sexM",
        "duration",
        "r2_marginal",
        "r2_condition",
        "n_speakers",
        "n_listeners",
        "observations"
      ),
      labels = c(
        "Intercept (HC × Conversational)",
        "Mild × Conversational",
        "Moderate × Conversational",
        "Severe × Conversational",
        "Profound × Conversational",
        "HC × Clear",
        "Mild × Clear",
        "Moderate × Clear",
        "Severe × Clear",
        "Profound × Clear",
        "Age",
        "Sex (M)",
        "Duration",
        md("Marginal R^2^"),
        md("Conditional R^2^"),
        md("N~speakers~"),
        md("N~listeners~"),
        "Observations"
      ),
    )) %>%
    dplyr::relocate(terms, .after = term) %>%
    arrange(terms) %>%
    dplyr::select(effect,
                  term,
                  terms,
                  tf_estimate,
                  tf_l_95_CI,
                  tf_u_95_CI,
                  tf_pd,
                  tf_robust,
                  tb_estimate,
                  tb_l_95_CI,
                  tb_u_95_CI,
                  tb_pd,
                  tb_robust)
  
  tbl_data %>%
    dplyr::select(!term) %>%
    gt(groupname_col = "effect",
       rowname_col = "terms") %>%
    gt::tab_spanner(label = "95% CrI",
                    columns = contains("_CI"),
                    level = 1,
                    gather = F) %>%
    gt::tab_spanner(label = "TF Distance",
                    columns = contains("tf_"),
                    level = 2) %>%
    gt::tab_spanner(label = "TB Distance",
                    columns = contains("tb_"),
                    level = 2) %>%
    ## TF Distance
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_body(columns = contains("tf_"),
                                 rows = tf_robust == "robust")
    ) |> 
    ## TB Distance
    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_body(columns = contains("tb_"),
                                 rows = tb_robust == "robust")) %>%
    gt::cols_hide(contains("robust")) %>%
    gt::cols_label(
      contains("estimate") ~ "Estimate",
      contains("l_95") ~ "LL",
      contains("u_95") ~ "UL",
      contains("_pd") ~ "pd",
      terms ~ "Terms"
    ) %>%
    fmt_markdown(columns = terms) %>%
    
    cols_align(
      align = "left",
      columns = "terms"
    ) %>%
    
    tab_stub_indent(
      rows = everything(),
      indent = 5
    ) %>% 
    
    gt::sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = ""
    ) %>%
    
    gt::gtsave(filename = file_path)
}


# Tbl 5: Post Hoc Clear Speech ----
create_clear_comparison_tbl <- function(file_path) {
  
  emmeans_data<- rbind(
    readRDS(
      here::here(
        "manuscript",
        SPECS,
        "01_models",
        "03_emmeans",
        "emmeans_intelligibility.rds"
      )
    )$condition %>%
      dplyr::mutate(measure = "Intelligibility (% VAS)"), 
    
    readRDS(
        here::here(
          "manuscript",
          SPECS,
          "01_models",
          "03_emmeans",
          "emmeans_precision.rds"
        )
      )$condition %>%
      dplyr::mutate(measure = "Articulatory Precision (% VAS)"),
    deparse.level = 1
  ) %>%
    base::rbind(
      .,
      readRDS(
        here::here(
          "manuscript",
          SPECS,
          "01_models",
          "03_emmeans",
          "emmeans_corner_vowel_distance.rds"
        )
      )$condition %>%
        dplyr::mutate(measure = "Corner Vowel Distance (Bark)"),
      deparse.level = 1
    ) %>%
    base::rbind(
      .,
      readRDS(
        here::here(
          "manuscript",
          SPECS,
          "01_models",
          "03_emmeans",
          "emmeans_tf_distance.rds"
        )
      )$condition %>%
        dplyr::mutate(measure = "TF Distance (mm)"),
      deparse.level = 1
    ) %>%
    base::rbind(
      .,
      readRDS(
        here::here(
          "manuscript",
          SPECS,
          "01_models",
          "03_emmeans",
          "emmeans_tb_distance.rds"
        )
      )$condition %>%
        dplyr::mutate(measure = "TB Distance (mm)"),
      deparse.level = 1
    ) 
  
  emmeans_data %>%
    dplyr::select(
      measure,
      group = severity,
      estimate,
      l_95_HPD,
      u_95_HPD,
      pd,
      robust
    ) %>%
    dplyr::mutate(pd = paste0(round(pd * 100, digits = 2), "%"),
                  estimate = weights::rd(estimate),
                  l_95_HPD = weights::rd(l_95_HPD),
                  u_95_HPD = weights::rd(u_95_HPD)) %>%
    gt(groupname_col = "measure",
       rowname_col = "group",
       row_group_as_column = TRUE) %>%
    gt::tab_spanner(label = "95% HPD",
                    columns = contains("_95"),
                    level = 1,
                    gather = F) %>%

    gt::tab_style(
      style = list(gt::cell_text(weight = "bold")),
      locations = gt::cells_body(columns = everything(),
                                 rows = robust == "robust")) %>%
    
    gt::cols_hide(contains("robust")) %>%
    gt::cols_label(
      contains("estimate") ~ "Estimate",
      contains("l_95") ~ "LL",
      contains("u_95") ~ "UL",
    ) %>%
    
    cols_align(
      align = "left",
      columns = "group"
    ) %>%
    
    gt::sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = ""
    ) %>%
    
    gt::gtsave(filename = file_path)
}

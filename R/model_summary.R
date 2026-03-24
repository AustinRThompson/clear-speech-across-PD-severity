# R/model_summary.R
library(weights)
library(tidyverse)

save_model_summary <- function(model, path) {
  
  # Probability of direction (pd)
  pd <- bayestestR::p_direction(model) %>%
    dplyr::rename(term = Parameter,
                  effect = Effects,
                  component = Component) %>%
    dplyr::mutate(
      component = ifelse(component == "conditional", "cond", component),
      term = gsub(pattern = "b_", replacement = "", term),
      term = ifelse(term == "Intercept", "(Intercept)", term))
  
  # Model results
  model_summary <- tidy(model) %>%
    dplyr::rename(l_95_CI = conf.low,
                  u_95_CI = conf.high) %>%
    dplyr::mutate(order = row_number()) %>%
    base::merge(.,pd, all = T) %>%
    dplyr::arrange(order) %>%
    dplyr::select(!order) %>%
    dplyr::mutate(
      robust = case_when(
        as.integer(!(l_95_CI <= 0 & u_95_CI > 0)) == 1 & pd > .95 ~ "robust",
        TRUE ~ "not robust"
      )
    )
  
  rm(pd)
  saveRDS(object = model_summary, file = path)
}

label_er <- function(er) {
  dplyr::case_when(
    is.na(er) ~ NA_character_,
    
    # Handle infinities first, so they don't get swallowed by numeric rules
    is.infinite(er) & er > 1 ~ "extreme evidence for H1",
    is.infinite(er) & er < 1 ~ "extreme evidence for H0",
    
    # Evidence for H0 (ER < 1)
    er <= 1/100                    ~ "extreme evidence for H0",
    er >  1/100 & er <= 1/30       ~ "very strong evidence for H0",
    er >  1/30  & er <= 1/10       ~ "strong evidence for H0",
    er >  1/10  & er <= 1/3        ~ "moderate evidence for H0",
    er >  1/3   & er <  1          ~ "anecdotal evidence for H0",
    
    # Around 1: no evidence either way
    dplyr::near(er, 1)             ~ "no evidence",
    
    # Evidence for H1 (ER > 1)
    er >  1    & er <= 3           ~ "anecdotal evidence for H1",
    er >  3    & er <= 10          ~ "moderate evidence for H1",
    er >  10   & er <= 30          ~ "strong evidence for H1",
    er >  30   & er <= 100         ~ "very strong evidence for H1",
    er >  100                      ~ "extreme evidence for H1",
    
    TRUE                           ~ NA_character_
  )
}


save_emmeans <- function(model, path) {
  
  measure <- dplyr::case_when(
    model[["formula"]][["resp"]] == "intelligibility01" ~ "Intelligibility",
    model[["formula"]][["resp"]] == "precision01"       ~ "Precision",
    model[["formula"]][["resp"]] == "cornervoweldistance" ~ "Vowel Distance",
    model[["formula"]][["resp"]] == "tbdistance"        ~ "TB Distance",
    model[["formula"]][["resp"]] == "tfdistance"        ~ "TF Distance"
  )
  
  # ---- severity levels (derived from model$data) ----
  sev_levels      <- levels(model[["data"]][["severity"]])
  base_severity   <- sev_levels[1]                       # should be "Control"
  other_severity  <- sev_levels[sev_levels != base_severity]
  

  # CONDITION CONTRASTS (RQ2: Clear - Conv within severity)

  
  ## pd for emmeans contrasts (unchanged)
  pd_emmeans_condition <- bayestestR::p_direction(
    model %>%
      emmeans::emmeans(
        ~ condition | severity,
        epred = TRUE,
        re_formula = NA,
      ) %>%
      emmeans::contrast(method = "revpairwise")
  ) |>
    as.data.frame()
  
  emmeans_condition <- model %>%
    emmeans::emmeans( ~ condition | severity,
                      epred = TRUE,
                      re_formula = NA,
    ) %>%
    emmeans::contrast(method = "revpairwise") %>%
    broom::tidy() |>
    dplyr::rename(l_95_HPD = lower.HPD, u_95_HPD = upper.HPD) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    base::merge(., pd_emmeans_condition) |>
    dplyr::arrange(order) %>%
    dplyr::select(!order) %>%
    dplyr::mutate(
      # Back transforming the estimate for the perceptual VAS measures
      estimate = case_when(
        measure == "Intelligibility" |
          measure == "Precision" ~ estimate * 100,
        TRUE ~ estimate
      ),
      l_95_HPD = case_when(
        measure == "Intelligibility" |
          measure == "Precision" ~ l_95_HPD * 100,
        TRUE ~ l_95_HPD
      ),
      u_95_HPD = case_when(
        measure == "Intelligibility" |
          measure == "Precision" ~ u_95_HPD * 100,
        TRUE ~ u_95_HPD
      ),
      robust = dplyr::case_when(
        as.integer(!(l_95_HPD <= 0 & u_95_HPD > 0)) == 1 & pd > .95 ~ "robust",
        TRUE ~ "not robust"
      ),
      unit = case_when(
        measure == "Intelligibility"|
          measure == "Precision" ~ "%",
        measure == "Vowel Distance" ~ " Bark",
        measure == "TB Distance" |
          measure == "TF Distance" ~ " mm"
      ),
      apa_full = paste0(
        "Δ", measure,
        " = ", weights::rd(estimate, digits = 2),
        ", 95% HPD [",
        weights::rd(l_95_HPD, digits = 2),
        ", ",
        weights::rd(u_95_HPD, digits = 2),
        "], pd = ",
        weights::rd(pd, digits = 2)
      ),
      apa_estimate = paste0(weights::rd(estimate, digits = 2),unit),
      apa_hpd = paste0("95% HPD [",
                       weights::rd(l_95_HPD, digits = 2),
                       ", ",
                       weights::rd(u_95_HPD, digits = 2),
                       "]"),
      apa_pd = paste0("pd = ",
                      weights::rd(pd, digits = 2)),
      apa_hpd_pd = paste0(apa_hpd,", ",apa_pd)
    )
  
  rm(pd_emmeans_condition)
  
  # ---- RQ2 Bayes factors for Clear vs Conv within each severity ----
  # condition factor and clear level must match your model
  cond_term   <- paste0("condition", "moreClear")  # "conditionmoreClear"
  
  # H1: Clear - Conv > 0 for Control + each severity
  hyp_rq2 <- c(
    paste0(cond_term, " > 0"),
    paste0(
      cond_term, " + severity",
      other_severity, ":", cond_term,
      " > 0"
    )
  )
  
  h_rq2 <- brms::hypothesis(model, hyp_rq2)
  
  bf_rq2 <- h_rq2$hypothesis |>
    dplyr::mutate(
      severity   = c(base_severity, other_severity),
      BF10       = Evid.Ratio,
      BF10_label = label_er(Evid.Ratio)
    ) |>
    dplyr::select(severity, BF10, BF10_label)
  
  # join BF10 onto emmeans_condition by severity
  emmeans_condition <- emmeans_condition %>%
    dplyr::left_join(bf_rq2, by = "severity")
  
  

  # GROUP CONTRASTS (RQ1: Control vs each severity at conv)

  
  ## pd for emmeans contrasts (unchanged)
  pd_emmeans_group <- bayestestR::p_direction(
    model %>%
      emmeans::emmeans(
        ~ severity,
        at = list(condition = "conv"),
        epred = TRUE,
        re_formula = NA,
      ) %>%
      emmeans::contrast(method = "revpairwise")
  ) |>
    as.data.frame() %>%
    dplyr::filter(grepl("Control", x = contrast) == TRUE)
  
  emmeans_group <- model %>%
    emmeans::emmeans(
      ~ severity,
      at = list(condition = "conv"),
      epred = TRUE,
      re_formula = NA,
    ) %>%
    emmeans::contrast(method = "revpairwise") %>%
    broom::tidy() |>
    dplyr::rename(l_95_HPD = lower.HPD, u_95_HPD = upper.HPD) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::filter(grepl("Control", x = contrast) == TRUE) %>%
    base::merge(., pd_emmeans_group) |>
    dplyr::arrange(order) %>%
    dplyr::select(!order) %>%
    dplyr::mutate(
      # Back transforming the estimate for the perceptual VAS measures
      estimate = case_when(
        measure == "Intelligibility" |
          measure == "Precision" ~ estimate * 100,
        TRUE ~ estimate
      ),
      l_95_HPD = case_when(
        measure == "Intelligibility" |
          measure == "Precision" ~ l_95_HPD * 100,
        TRUE ~ l_95_HPD
      ),
      u_95_HPD = case_when(
        measure == "Intelligibility" |
          measure == "Precision" ~ u_95_HPD * 100,
        TRUE ~ u_95_HPD
      ),
      robust = dplyr::case_when(
        as.integer(!(l_95_HPD <= 0 & u_95_HPD > 0)) == 1 & pd > .95 ~ "robust",
        TRUE ~ "not robust"
      ),
      unit = case_when(
        measure == "Intelligibility"|
          measure == "Precision" ~ "%",
        measure == "Vowel Distance" ~ " Bark",
        measure == "TB Distance" |
          measure == "TF Distance" ~ " mm"
      ),
      apa_full = paste0(
        "Δ", measure,
        " = ", weights::rd(estimate, digits = 2),
        ", 95% HPD [",
        weights::rd(l_95_HPD, digits = 2),
        ", ",
        weights::rd(u_95_HPD, digits = 2),
        "], pd = ",
        weights::rd(pd, digits = 2)
      ),
      apa_estimate = paste0(weights::rd(estimate, digits = 2),unit),
      apa_hpd = paste0("95% HPD [",
        weights::rd(l_95_HPD, digits = 2),
        ", ",
        weights::rd(u_95_HPD, digits = 2),
        "]"),
      apa_pd = paste0("pd = ",
                      weights::rd(pd, digits = 2)),
      apa_hpd_pd = paste0(apa_hpd,", ",apa_pd)
    )
  
  rm(pd_emmeans_group)
  
  # ---- RQ1 Bayes factors: severity vs Control (conversational) ----
  # H1: severityLevel < 0 (dysarthric < Control on link scale)
  hyp_rq1 <- paste0("severity", other_severity, " < 0")
  
  h_rq1 <- brms::hypothesis(model, hyp_rq1)
  
  bf_rq1 <- h_rq1$hypothesis |>
    dplyr::mutate(
      severity   = other_severity,
      BF10       = Evid.Ratio,
      BF10_label = label_er(Evid.Ratio)
    ) |>
    dplyr::select(severity, BF10, BF10_label)
  
  # Extract the non-Control severity label from emmeans contrast string
  # This works whether contrast is "Control - Mild" or "Mild - Control"
  emmeans_group <- emmeans_group %>%
    dplyr::mutate(
      severity = contrast,
      severity = stringr::str_replace(severity,
                                      paste0(base_severity, " - "), ""),
      severity = stringr::str_replace(severity,
                                      paste0(" - ", base_severity), "")
    ) %>%
    dplyr::left_join(bf_rq1, by = "severity")
  
  # (optionally drop the helper 'severity' column, or keep it)
  # emmeans_group <- emmeans_group %>% dplyr::select(-severity)
  
  # COMBINE & SAVE
  
  emmeans <- list(
    condition = emmeans_condition,
    group     = emmeans_group
  )
  
  saveRDS(object = emmeans, file = path)
}

# R/get_mcid.R
library(tidyverse)
library(brms)
library(tidybayes)
library(weights)

get_7_mcid <- function(fit = m_intelligibility_fit,
                       out = out) {
  
  intelligibility_draws <- readRDS(fit) %>%
    emmeans::emmeans( ~ condition | severity,
                      #at = list(severity = "Profound"),
                      epred = TRUE,
                      re_formula = NA,
    ) %>%
    emmeans::contrast(method = "revpairwise") %>%
    gather_emmeans_draws() %>%
    dplyr::group_by(contrast,severity) %>%
    dplyr::summarise(pd_07 = mean(.value > 0.07),
                     est_gain = mean(.value),
                     .groups = "drop") %>%
    dplyr::mutate(pd_07_apa = case_when(
      pd_07 < .01 ~ "<1%",
      TRUE ~ paste0(weights::rd(pd_07*100),"%")
    ),
                  est_gain_apa = paste0(weights::rd(est_gain*100),"%"))
  
  mcid <- intelligibility_draws %>%
    split(.$severity) %>%
    purrr::map(~ as.list(.x))
  
  saveRDS(mcid,
          file = out)
  
}


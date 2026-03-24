# R/create_figures.R
library(tidyverse)
library(ggrepel)
library(extraDistr)   # install.packages("extraDistr")
library(HDInterval)   # install.packages("HDAPerval")
library(tidybayes)    # install.packages("tidybayes")
library(bayesplot)    # install.packages("bayesplot")
library(modelr)
library(broom.mixed)  # install.packages("broom.mixed")
library(brms)         # install.packages("brms")
library(ggthemes)
library(patchwork)

theme_set(theme_minimal())

# Creating a theme function used for visualizations
theme_clean <- function() {
  theme_minimal(base_family = "Arial") +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = rel(1), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold"))
}

# Formatting information
text_size_title <- 9 # text_size_title
text_size_subtitle <- 9 # text_size_subtitle
text_size_axis_title <- 9 # text_size_axis_title

bf_pal <- c("#CC79A7")
#"#E69F00" "#56B4E9" "#009E73" "#F0E442" "#0072B2" "#D55E00" "#CC79A7" "#999999" "#000000"


# Fig: Model Estimations ----
create_model_estimation_figures <- function(model,
                                            measure,
                                            emmeans,
                                            path,
                                            title,
                                            a_subtitle,
                                            a_x_axis_text,
                                            a_xlims,
                                            b_subtitle,
                                            b_x_axis_text,
                                            c_subtitle,
                                            c_x_axis_text) {
  
  data_posterior <- model %>%
    tidybayes::epred_draws(
      newdata = tidyr::expand_grid(
        severity = c("Control", "Mild", "Moderate", "Severe", "Profound"),
        condition = c("conv", "moreClear"),
        sex = c("M", "F"),
        age = 65.65, # average age across all speakers
        # duration data for TF & TB distance
        duration = ifelse(measure == "tb_distance" | measure == "tf_distance",mean(model$data$duration, na.rm = T),NA),
      ),
      re_formula = NA
    ) %>%
    dplyr::mutate(
      severity = factor(
        severity,
        levels = c("Control", "Mild", "Moderate", "Severe", "Profound")
      ),
      condition = factor(
        condition,
        levels = c("conv", "moreClear"),
        labels = c("conversational", "clear")
      ))
  
  if(measure == "intelligibility" | measure == "precision") {
    epsilon <- 1e-5
    data_posterior <- data_posterior %>%
      dplyr::mutate(.epred = (.epred - epsilon) / (1 - 2 * epsilon),
                    # Step 1 & 2: Reverse the offset and scaling
                    .epred = .epred * nrow(.) / ((nrow(.) - 1) + .5)
      )
  }
  
  if(measure == "corner_vowel_distance") {
    data_posterior <- data_posterior %>%
      group_by(.draw, severity, condition) %>%
      summarise(.epred = mean(.epred), .groups = "drop")
  }

  # Plot A ----
  plot_a_grandMean <- data_posterior %>%
    ggplot() + 
    aes(x = .epred,
        y = severity, 
        fill = condition) +
    ggdist::stat_halfeye(alpha = .9,
                         shape = 21) +
    ggokabeito::scale_fill_okabe_ito(guide = guide_legend(nrow = 1)) +
    labs(x = a_x_axis_text,
         y = NULL,
         fill = "Condition",
         title = "(a) Posterior Predictions",
         subtitle = a_subtitle) +
    scale_y_discrete(limits = rev) +
    theme_clean() +
    theme(legend.position = "bottom",
          axis.title = element_text(size = text_size_axis_title),
          plot.title = element_text(size = text_size_title),
          plot.subtitle = element_text(size = text_size_subtitle))
  
  if(!is.null(a_xlims)) {
    plot_a_grandMean <- plot_a_grandMean +
      coord_cartesian(xlim = a_xlims)
  }
    
  if (measure == "intelligibility" | measure == "precision") {
    plot_a_grandMean <- plot_a_grandMean +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  }
  
  # Plot B ----
  plot_b_group_data <- model %>%
    emmeans::emmeans( ~  severity,
                      at = list(condition = "conv"),
                      epred = TRUE,
                      re_formula = NA,
    ) %>%
    emmeans::contrast(method = "revpairwise") %>%
    gather_emmeans_draws() %>%
    dplyr::mutate(
      contrast = factor(contrast,
                        levels = c(
                          " ",
                          "Mild - Control",
                          "Moderate - Control",
                          "Severe - Control",
                          "Profound - Control"
                          #"Moderate - Mild",
                          #"Severe - Moderate",
                          #"Profound - Severe"
                        ))
    ) %>%
    dplyr::filter(!is.na(contrast)) %>%
    base::merge(., emmeans$group) %>%
    dplyr::mutate(robust = base::factor(robust,
                                        levels = c("not robust",
                                                   "robust")),
                  BF10_label = base::factor(BF10_label,
                                            levels = c("extreme evidence for H0",
                                                       "very strong evidence for H0",
                                                       "strong evidence for H0",
                                                       "moderate evidence for H0",
                                                       "anecdotal evidence for H0",
                                                       "no evidence",
                                                       "anecdotal evidence for H1",
                                                       "moderate evidence for H1",
                                                       "strong evidence for H1",
                                                       "very strong evidence for H1",
                                                       "extreme evidence for H1")),
                  BF_simple_label = case_when(
                    grepl(pattern = "extreme", x = BF10_label) ~ "extreme",
                    grepl(pattern = "very", x = BF10_label) ~ "very strong",
                    grepl(pattern = "strong", x = BF10_label) ~ "strong",
                    grepl(pattern = "moderate", x = BF10_label) ~ "moderate",
                    grepl(pattern = "anecdotal", x = BF10_label) ~ "anecdotal/no evidence",
                    TRUE ~ "anecdotal/no evidence"
                  ),
                  BF_simple_label = factor(BF_simple_label,
                                           levels = c("extreme",
                                                      "very strong",
                                                      "strong",
                                                      "moderate",
                                                      "anecdotal/no evidence")))
  
  if (measure == "intelligibility" | measure == "precision") {
    epsilon <- 1e-5
    plot_b_group_data <- plot_b_group_data %>%
      dplyr::mutate(
        .value = (.value - epsilon) / (1 - 2 * epsilon),
        # Step 1 & 2: Reverse the offset and scaling
        .value = .value * nrow(.) / ((nrow(.) - 1) + .5)
      )
  }
  
  plot_b_group <- plot_b_group_data %>%
    dplyr::mutate(robust = factor(robust, levels = c("not robust",
                                                     "robust"))) %>%
    ggplot() +
    aes(x = .value,
        y = contrast,
        #fill = robust,
        shape = robust) +
    geom_vline(xintercept = 0, alpha = .5) + 
    ggdist::stat_halfeye(alpha = .9,
                         fill = bf_pal) +
    scale_shape_manual(values = c(21,19),
                       drop = FALSE) +
    labs(
      x = b_x_axis_text,
      y = NULL,
      title = "(b) Research Question 1",
      subtitle = b_subtitle,
      fill = "Robust at 95% Crl & pd",
      shape = "Robust at 95% Crl & pd"
    ) +
    scale_y_discrete(limits = rev, drop = FALSE) +
    scale_fill_manual(values = bf_pal,
                      drop = FALSE) +
    guides(shape = guide_legend(nrow = 1),
           fill = guide_legend(nrow = 1)) +
    annotate("text",x = 0, y = 6, label = "", size = 5)  + # this adds an empty annotation at the "control" line
    theme_clean() +
    theme(legend.position = "bottom",
          axis.title = element_text(size = text_size_axis_title),
          plot.title = element_text(size = text_size_title),
          plot.subtitle = element_text(size = text_size_subtitle))
  
  
  if (measure == "intelligibility" | measure == "precision") {
    plot_b_group <- plot_b_group +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  }
  
  
  # Plot C ----
  plot_c_condition_data <- model %>%
    emmeans::emmeans( ~ condition | severity,
                      #at = list(severity = "Profound"),
                      epred = TRUE,
                      re_formula = NA,
    ) %>%
    emmeans::contrast(method = "revpairwise") %>%
    gather_emmeans_draws() %>%
    base::merge(., emmeans$condition) %>%
    dplyr::mutate(robust = base::factor(robust,
                                        levels = c("not robust",
                                                   "robust")),
                  BF10_label = base::factor(BF10_label,
                                            levels = c("extreme evidence for H0",
                                                       "very strong evidence for H0",
                                                       "strong evidence for H0",
                                                       "moderate evidence for H0",
                                                       "anecdotal evidence for H0",
                                                       "no evidence",
                                                       "anecdotal evidence for H1",
                                                       "moderate evidence for H1",
                                                       "strong evidence for H1",
                                                       "very strong evidence for H1",
                                                       "extreme evidence for H1")),
                  BF_simple_label = case_when(
                    grepl(pattern = "extreme", x = BF10_label) ~ "extreme",
                    grepl(pattern = "very", x = BF10_label) ~ "very strong",
                    grepl(pattern = "strong", x = BF10_label) ~ "strong",
                    grepl(pattern = "moderate", x = BF10_label) ~ "moderate",
                    grepl(pattern = "anecdotal", x = BF10_label) ~ "anecdotal/no evidence",
                    TRUE ~ "anecdotal/no evidence"
                  ),
                  BF_simple_label = factor(BF_simple_label,
                                           levels = c("extreme",
                                                      "very strong",
                                                      "strong",
                                                      "moderate",
                                                      "anecdotal/no evidence")))
  
  if (measure == "intelligibility" | measure == "precision") {
    epsilon <- 1e-5
    plot_c_condition_data <- plot_c_condition_data %>%
      dplyr::mutate(
        .value = (.value - epsilon) / (1 - 2 * epsilon),
        # Step 1 & 2: Reverse the offset and scaling
        .value = .value * nrow(.) / ((nrow(.) - 1) + .5)
      )
  }
  
  plot_c_condition <- plot_c_condition_data %>%
    dplyr::mutate(robust = factor(robust, levels = c("not robust",
                                                     "robust"))) %>%
    ggplot() +
    aes(x = .value,
        y = severity,
        #fill = robust,
        shape = robust) +
    geom_vline(xintercept = 0, alpha = .5) +
    ggdist::stat_halfeye(alpha = .9,
                         fill = bf_pal) +
    scale_shape_manual(values = c(21,19),
                       drop = FALSE) +
    labs(
      x = c_x_axis_text,
      y = NULL,
      title = "(c) Research Question 2",
      subtitle = c_subtitle,
      fill = "Robust at 95% Crl & pd",
      shape = "Robust at 95% Crl & pd"
    ) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = bf_pal,
                      drop = FALSE) +
    guides(shape = guide_legend(nrow = 1),
           fill = guide_legend(nrow = 1)) +
    theme_clean() +
    theme(legend.position = "bottom",
          axis.title = element_text(size = text_size_axis_title),
          plot.title = element_text(size = text_size_title),
          plot.subtitle = element_text(size = text_size_subtitle))
  
  if (measure == "intelligibility" | measure == "precision") {
    plot_c_condition <- plot_c_condition +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1))
  }
  
  
  # Combined plot ----
  combined_plot <-  plot_a_grandMean + plot_b_group + plot_c_condition +
    patchwork::plot_layout(ncol = 3, 
                           heights = c(1),
                           guides = "collect") +
    plot_annotation(title = title,
                    #subtitle = "Predicted intelligibility ratings across group/severity and speaking conditions.",
                    theme = theme_clean())  &
    theme(
      legend.position = "bottom",
      #legend.direction = "vertical",
      legend.box = "horizontal",
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 9),
      legend.key.width  = unit(1.5, "lines"),
      legend.key.height = unit(1, "lines")
      )
  
  ggsave(
    plot = combined_plot,
    filename = path,
    height = 6,
    width = 10,
    unit = "in",
    scale = .9,
    bg = "white"
  )
}


# Fig: Inter-listener reliability ----
create_interlistener_reliability_figure <- function(intelligibility_data,
                                                    precision_data,
                                                    file_path) {
  
  plot_data <-
    rbind(
      intelligibility_data %>%
        dplyr::filter(condition != "lessClear") %>%
        dplyr::group_by(database_id, group, sex, age, severity) %>%
        dplyr::summarise(
          measure = "Intelligibility",
          M = mean(intelligibility),
          sd = sd(intelligibility),
          n_ratings = NROW(gorilla_id),
          .groups = "drop"
        ),
      
      precision_data %>%
        dplyr::filter(condition != "lessClear") %>%
        dplyr::group_by(database_id, group, sex, age, severity) %>%
        dplyr::summarise(
          measure = "Articulatory Precision",
          M = mean(precision),
          sd = sd(precision),
          n_ratings = NROW(gorilla_id),
          .groups = "drop"
        )
    ) %>%
    dplyr::mutate(measure = factor(measure,
                                   levels = c("Intelligibility",
                                              "Articulatory Precision")),
                  group = factor(group,
                                 levels = c(
                                   "HC",
                                   "PD"
                                 ),
                                 labels = c(
                                   "Control",
                                   "PwPD"
                                 )))
  
  interlistener_reliability <- plot_data %>%
    ggplot() +
    aes(x = M, y = sd, color = group) +
    geom_point() +
    geom_smooth(inherit.aes = F, aes(x = M, y = sd),
                method = "lm",
                formula = y~poly(x,2),se=F,
                color = "grey") +
    ggokabeito::scale_color_okabe_ito() +
    labs(
      title = "Inter-listener Reliability",
      y = "Rating Variability (SD % VAS)",
      x = "Average Rating (Mean % VAS)",
      color = "Group"
    ) +
    facet_wrap( ~ measure) +
    theme_clean() &
    theme(panel.border = element_rect(fill = NULL, color = "darkgrey"),
          aspect.ratio = 1,
          legend.position = "bottom",
          strip.background = element_rect(fill = "#E5E4E2"))
  
    ggsave(
      plot = interlistener_reliability,
      filename = file_path,
           height = 3.5,
           width = 5,
           units = "in",
           scale = 1.2,
      bg = "white")
  
}

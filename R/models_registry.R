# R/models_registry.R
library(brms)
library(insight) # install.packages("insight")
library(tidyverse)

model_registry_tbl <- function(spec_name) {
  tibble::tibble(
    model_id = c(
      "m_intelligibility",
      "m_precision",
      "m_corner_vowel_distance",
      "m_tf_distance",
      "m_tb_distance"
    ), # which prepped dataset each model consumes (target names below)
    data_target = c(
      "data_intelligibility",
      "data_precision",
      "data_corner_vowel_distance",
      "data_tf_distance",
      "data_tb_distance"
    ), 
    
    formula = base::list(
      "intelligibility_01      ~ severity*condition + age + (1|database_id:stimuli) + (1|listener_id)",
      "precision_01            ~ severity*condition + age + (1|database_id:stimuli) + (1|listener_id)",
      "corner_vowel_distance   ~ severity*condition + age + sex + (1|database_id)",
      "tf_distance             ~ severity*condition + age + sex + duration + (1|database_id)",
      "tb_distance             ~ severity*condition + age + sex + duration + (1|database_id)"
    ),
    family = base::list(
      brms::Beta(),
      brms::Beta(),
      stats::gaussian(),
      stats::gaussian(),
      stats::gaussian()
    ),
    priors = base::list(
      # m_intelligibility
      c(brms::prior(normal(0,10), class = "Intercept"),
        brms::prior(normal(0,10), class = "b"),
        brms::prior(cauchy(0,10), class = "sd"),
        brms::prior(gamma(10,.8), class = "phi")),
      # m_precision
      c(brms::prior(normal(0,10), class = "Intercept"),
        brms::prior(normal(0,10), class = "b"),
        brms::prior(cauchy(0,10), class = "sd"),
        brms::prior(gamma(10,.8), class = "phi")),
      # m_corner_vowel_distance
      c(brms::prior(normal(0,10), class = "Intercept"),
        brms::prior(normal(0,10), class = "b"),
        brms::prior(cauchy(0,10), class = "sd"),
        brms::prior(cauchy(0,10), class = "sigma")),
      # m_tf_distance
      c(brms::prior(normal(0,10), class = "Intercept"),
        brms::prior(normal(0,10), class = "b"),
        brms::prior(cauchy(0,10), class = "sigma"),
        brms::prior(cauchy(0,10), class = "sd")),
      # m_tb_distance
      c(brms::prior(normal(0,10), class = "Intercept"),
        brms::prior(normal(0,10), class = "b"),
        brms::prior(cauchy(0,10), class = "sigma"),
        brms::prior(cauchy(0,10), class = "sd"))
    ),
    
    # run options
    iter   = 4000,
    warmup = 1000,
    chains = 4,
    cores  = 4,
    seed   = 2025,
    sample_prior = "yes",
    control = list(adapt_delta = 0.99),
    
    # where brms will cache model fits per spec
    file = c(
      here::here("manuscript", spec_name, "01_models", "m_intelligibility.rds"),
      here::here("manuscript", spec_name, "01_models", "m_precision.rds"),
      here::here("manuscript", spec_name, "01_models", "m_corner_vowel_distance.rds"),
      here::here("manuscript", spec_name, "01_models", "m_tf_distance.rds"),
      here::here("manuscript", spec_name, "01_models", "m_tb_distance.rds")
    ),
    file_refit = "always"#"on_change"
  )
}

get_def <- function(registry_tbl, id) {
  stopifnot(is.data.frame(registry_tbl))
  out <- registry_tbl[registry_tbl$model_id == id, , drop = FALSE]
  if (nrow(out) != 1L) stop("Model id not found or duplicated: ", id)
  out
}

fit_brms_model <- function(df, def) {
  stopifnot(is.data.frame(df))
  brms::brm(
    formula    = brms::bf(def$formula[[1]]),
    data       = df,
    family     = def$family[[1]],
    prior      = def$priors[[1]],
    iter       = def$iter[1],
    chains     = def$chains[1],
    seed       = def$seed[1],
    cores      = def$cores[1],
    file       = def$file[1],
    file_refit = def$file_refit[1]
  )
}

get_random_effects_data <- function(model){
  r2 <- performance::r2_bayes(model, ci = .95)
  
  random_effects <- tibble::tibble(measure = c("r2_condition",
                                               "r2_marginal",
                                               "n_speakers",
                                               ifelse(!is.null(summary(model)[["ngrps"]][["listener_id"]]),"n_listeners",NA),
                                               "observations"),
                                   estimate = c(r2$R2_Bayes,
                                                r2$R2_Bayes_marginal,
                                                ifelse(is.null(summary(model)[["ngrps"]][["database_id"]]),
                                                       summary(model)[["ngrps"]][["database_id:stimuli"]],
                                                       summary(model)[["ngrps"]][["database_id"]]),
                                                ifelse(!is.null(summary(model)[["ngrps"]][["listener_id"]]),summary(model)[["ngrps"]][["listener_id"]],NA),
                                                summary(model)[["nobs"]])) %>%
    dplyr::filter(!is.na(estimate))
  
  return(random_effects)
}

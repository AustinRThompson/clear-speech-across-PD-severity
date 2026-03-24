# R/model_diagnostics.R

suppressPackageStartupMessages({
  library(brms)
  library(bayesplot)
  library(posterior)
  library(ggplot2)
  library(purrr)
})

# Detect monotonic effects so CE doesn’t choke on mo()
.detect_mo_effects <- function(fit) {
  vars <- colnames(posterior::as_draws_df(fit))
  mos  <- unique(sub("^b_mo([^\\[]+).*", "\\1", vars[grepl("^b_mo", vars)]))
  mos  <- mos[nzchar(mos)]
  if (!length(mos)) character(0) else paste0("mo(", mos, ")")
}

# Keep trace plots light: pick a few parameters
.pick_trace_params <- function(fit, max_params = 10) {
  vars <- colnames(posterior::as_draws_df(fit))
  b  <- vars[grepl("^b_", vars)]
  sd <- vars[grepl("^sd_", vars)]
  cr <- vars[grepl("^cor_", vars)]
  choice <- c(head(b, max_params),
              head(sd, max(0, max_params - length(b))),
              head(cr, max(0, max_params - length(b) - length(sd))))
  if (!length(choice)) choice <- head(setdiff(vars, c(b, sd, cr)), max_params)
  unique(choice)
}

# ------ Build diagnostics (no files) ------------------------------------------
build_diagnostics <- function(fit,
                              ndraws_pp  = 500,
                              re_formula = NA,   # set to NULL to condition on REs
                              max_trace_params = 10) {
  stopifnot(inherits(fit, "brmsfit"))
  
  # 1) Posterior predictive (ggplot)
  pp <- brms::pp_check(fit, ndraws = ndraws_pp)
  
  # 2) Conditional effects (use brms-native plots; skip any that fail)
  ce_plots <- base::plot(conditional_effects(fit), ask = FALSE)
  
  # 3) Chain mixing (trace) as a single bayesplot ggplot
  trace <- base::plot(fit, ask=FALSE)

  # return a compact bundle
  structure(
    list(
      pp       = pp,        # ggplot
      ce_plots = ce_plots,  # named list of lists of ggplots
      trace    = trace      # ggplot (or NULL)
    ),
    class = "brms_diagnostics_bundle"
  )
}

# ------ Save/load as a single RDS ---------------------------------------------
save_diagnostics_rds <- function(fit, path,
                                 ndraws_pp = 500,
                                 re_formula = NA,
                                 max_trace_params = 10) {
  diag <- build_diagnostics(fit, ndraws_pp, re_formula, max_trace_params)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(diag, path)
  path
}

# ------ Convenience: print to current device ----------------------------------
print_diagnostics <- function(bundle) {
  stopifnot(inherits(bundle, "brms_diagnostics_bundle"))
  # pp
  if (inherits(bundle$pp, "ggplot")) print(bundle$pp)
  # CE (each effect may have multiple panels)
  if (length(bundle$ce_plots)) {
    for (nm in names(bundle$ce_plots)) {
      for (gp in bundle$ce_plots[[nm]]) if (inherits(gp, "ggplot")) print(gp)
    }
  }
  # trace
  if (inherits(bundle$trace, "ggplot")) print(bundle$trace)
  invisible(NULL)
}

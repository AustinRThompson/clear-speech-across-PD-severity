# R/emmeans_results.R

suppressPackageStartupMessages({
  library(purrr)
  library(dplyr)
})

`%||%` <- function(x, y) if (!is.null(x)) x else y

.emm_files <- function(dir) {
  c(
    intelligibility   = file.path(dir, "emmeans_intelligibility.rds"),
    precision         = file.path(dir, "emmeans_precision.rds"),
    corner_vowel_distance = file.path(dir, "emmeans_corner_vowel_distance.rds"),
    tf_distance       = file.path(dir, "emmeans_tf_distance.rds"),
    tb_distance       = file.path(dir, "emmeans_tb_distance.rds")
  )
}

.pick_col <- function(df, candidates) {
  nm <- names(df)
  hit <- candidates[candidates %in% nm]
  if (length(hit)) hit[1] else NA_character_
}

# helpers
.pick_one <- function(df, row = 1, col) {
  if (is.na(col) || !col %in% names(df)) return(NULL)
  x <- df[[col]][[row]]
  if (length(x) == 0) return(NULL)
  x
}

.num_or_na <- function(x) {
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

.chr_or_na <- function(x) {
  if (is.null(x)) return(NA_character_)
  as.character(x)
}

.build_indexed <- function(df, key_col) {
  stopifnot(is.data.frame(df))
  if (!key_col %in% names(df)) {
    stop("Expected column '", key_col, "' not found. Available: ",
         paste(names(df), collapse = ", "))
  }
  
  est_col <- .pick_col(df, c("estimate", "emmean", "Estimate", "b", "estimate__"))
  low_col <- .pick_col(df, c("lower", "lower.CL", "lower.CL.", "lower__",
                             "LCL", "lower.CL95", "lower.HPD", "lower.hpd"))
  upp_col <- .pick_col(df, c("upper", "upper.CL", "upper.CL.", "upper__",
                             "UCL", "upper.CL95", "upper.HPD", "upper.hpd"))
  pd_col  <- .pick_col(df, c("pd", "pd__", "prob_direction", "p_direction"))
  
  # your new apa columns (with some reasonable fallbacks)
  apa_full_col     <- .pick_col(df, c("apa_full", "apa", "apa__full"))
  apa_est_col      <- .pick_col(df, c("apa_estimate", "apa_est", "apa__estimate"))
  apa_hpd_col      <- .pick_col(df, c("apa_hpd", "apa_ci", "apa_hpd", "apa__cri"))
  apa_pd_col       <- .pick_col(df, c("apa_pd", "apa__pd"))
  apa_hpd_pd_col   <- .pick_col(df, c("apa_hpd_pd", "apa__cri__pd"))
  
  keys <- as.character(unique(df[[key_col]]))
  keys_sanitized <- gsub(" - ", "_", keys)
  
  out <- setNames(vector("list", length(keys)), keys_sanitized)
  
  for (i in seq_along(keys)) {
    k <- keys[i]
    key_clean <- keys_sanitized[i]
    r <- df[df[[key_col]] == k, , drop = FALSE]
    if (!nrow(r)) next
    r <- r[1, , drop = FALSE]
    
    entry <- list(
      estimate      = .num_or_na(.pick_one(r, 1, est_col)),
      lower         = .num_or_na(.pick_one(r, 1, low_col)),
      upper         = .num_or_na(.pick_one(r, 1, upp_col)),
      pd            = .num_or_na(.pick_one(r, 1, pd_col)),
      apa_full      = .chr_or_na(.pick_one(r, 1, apa_full_col)),
      apa_estimate  = .chr_or_na(.pick_one(r, 1, apa_est_col)),
      apa_hpd       = .chr_or_na(.pick_one(r, 1, apa_hpd_col)),
      apa_pd        = .chr_or_na(.pick_one(r, 1, apa_pd_col)),
      apa_hpd_pd        = .chr_or_na(.pick_one(r, 1, apa_hpd_pd_col))
    )
    
    out[[key_clean]] <- entry
  }
  
  out
}

# ----- main builder -----
# rq1 = group (index by `contrast`)
# rq2 = condition (index by `severity`)
make_rq_lists <- function(emmeans_dir) {
  files <- .emm_files(emmeans_dir)
  
  objs <- imap(files, function(path, measure_name) {
    if (!file.exists(path)) stop("Missing file: ", path)
    obj <- readRDS(path)
    if (!all(c("group", "condition") %in% names(obj))) {
      stop("'", basename(path), "' must contain $group and $condition data.frames.")
    }
    list(
      rq1 = .build_indexed(obj$group,      key_col = "contrast"),
      rq2 = .build_indexed(obj$condition,  key_col = "severity")
    )
  })
  
  rq1 <- lapply(objs, `[[`, "rq1")
  rq2 <- lapply(objs, `[[`, "rq2")
  
  # ensure names are measures
  names(rq1) <- names(files)
  names(rq2) <- names(files)
  
  list(rq1 = rq1, rq2 = rq2)
}

# Save as one RDS
save_rq_lists <- function(emmeans_dir, out_path) {
  res <- make_rq_lists(emmeans_dir)
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(res, out_path)
  out_path
}


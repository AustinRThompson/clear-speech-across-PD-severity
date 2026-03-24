# Helpers

out_path <- function(spec, subdir, filename) {
  dir <- file.path("manuscript", spec, subdir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, filename)
}
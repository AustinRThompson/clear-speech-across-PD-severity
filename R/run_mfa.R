# R/run_mfa.R

run_mfa_alignment <- function(
    unaligned_dir = here::here("data", "01_interim", "04_transcripts", "0_unaligned_phrase_data"),
    aligned_dir   = here::here("data", "01_interim", "04_transcripts", "1_aligned_phrase_data"),
    conda_env     = "aligner"
) {
  
  # Make sure output dir exists
  dir.create(aligned_dir, recursive = TRUE, showWarnings = FALSE)
  
  in_dir  <- normalizePath(unaligned_dir)
  out_dir <- normalizePath(aligned_dir)
  
  # Build the shell command you normally type:
  # conda activate aligner
  # mfa align --clean in_dir english_us_arpa english_us_arpa out_dir
  
  cmd_string <- paste(
    "conda activate", conda_env, "&&",
    "mfa align --clean",
    shQuote(in_dir),
    "english_us_arpa english_us_arpa",
    shQuote(out_dir)
  )
  
  # Run via bash -lc so conda is available
  res <- system2(
    "bash",
    c("-lc", cmd_string),
    stdout = TRUE,
    stderr = TRUE
  )
  
  # Optionally print or log MFA output if you want
  # cat(paste(res, collapse = "\n"))
  
  # Collect TextGrids to return as a file vector
  tgs <- list.files(
    out_dir,
    pattern = "[.]TextGrid$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (!length(tgs)) {
    stop(
      "MFA did not produce any TextGrids in ", out_dir, ".\n",
      "MFA output:\n", paste(res, collapse = "\n")
    )
  }
  
  tgs
}

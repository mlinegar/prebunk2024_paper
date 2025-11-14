# =============================================================================
# Main Analysis Script for Prebunking Study
# =============================================================================
# Sources all modular components and runs the complete analysis pipeline.
# =============================================================================

# Determine repository root based on script location
detect_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  file_arg <- cmd_args[grepl(file_flag, cmd_args)]
  if (length(file_arg) > 0) {
    return(normalizePath(sub(file_flag, "", file_arg[1])))
  }
  frame_paths <- vapply(sys.frames(), function(env) {
    if (!is.null(env$ofile)) return(env$ofile)
    return(NA_character_)
  }, character(1))
  frame_paths <- frame_paths[!is.na(frame_paths)]
  if (length(frame_paths) > 0) {
    return(normalizePath(frame_paths[length(frame_paths)]))
  }
  return(NULL)
}

script_path <- detect_script_path()
repo_root <- if (!is.null(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."))
} else {
  normalizePath(getwd())
}
setwd(repo_root)

# Convenience helper for sourcing files within the repository
source_repo <- function(...) {
  source(file.path(repo_root, ...))
}

# Load configuration, helpers, and modular scripts
source_repo("config", "config.R")  # Libraries and configuration
source_repo("helper_functions.R")  # Shared helper utilities
source_repo("functions", "data_processing.R")  # Process data

# OPTION 1: Use quick version (loads pre-computed permutation results from CSV)
# This is MUCH faster but requires running balance_and_checks_permutations.R first
source_repo("functions", "balance_and_checks_quick.R")

# OPTION 2: Run full version including permutations (slow, ~10 min)
# Uncomment this line and comment out the quick version above:
# source_repo("functions", "balance_and_checks.R")

source_repo("functions", "modeling_helpers.R")  # Load modeling helper functions
source_repo("functions", "modeling.R")  # Run main regression models (weighted & unweighted)
source_repo("functions", "modeling_interactions.R")  # Run all interactions
source_repo("functions", "modeling_sensitivity.R")  # Sensitivity analysis (attentive sample)
source_repo("functions", "tables.R")  # Generate summary statistics
source_repo("functions", "plotting.R")  # Create all plots

# Print completion message
cat("\n=============================================================================\n")
cat("Analysis complete!\n")
cat("=============================================================================\n")
cat("\nOutputs generated:\n")
cat("- Data: prebunk_full.csv\n")
cat("- Tables: Multiple .tex files for LaTeX\n")
cat("- Plots: Multiple .pdf files\n")
cat("\nAll files saved to:", getwd(), "\n")

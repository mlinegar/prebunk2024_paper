# =============================================================================
# Public Replication Analysis Script
# =============================================================================
# Runs the complete analysis pipeline from the published minimal public
# replication dataset instead of the restricted raw YouGov exports.
# =============================================================================

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
Sys.setenv(PREBUNK_PROJECT_ROOT = repo_root)

source_repo <- function(...) {
  source(file.path(repo_root, ...))
}

source_repo("config", "config.R")
source_repo("helper_functions.R")
source_repo("functions", "public_data.R")

public_data_file <- Sys.getenv(
  "PREBUNK_PUBLIC_DATA_FILE",
  unset = public_replication_path()
)

initialize_public_replication_data(load_public_replication_data(public_data_file))

source_repo("functions", "balance_and_checks_quick.R")
source_repo("functions", "modeling_helpers.R")
source_repo("functions", "modeling.R")
source_repo("functions", "modeling_interactions.R")
source_repo("functions", "modeling_sensitivity.R")
source_repo("functions", "tables.R")
source_repo("functions", "plotting.R")

cat("\n=============================================================================\n")
cat("Public replication analysis complete!\n")
cat("=============================================================================\n")
cat("\nInputs used:\n")
cat("- Public replication data:", public_data_file, "\n")
cat("\nOutputs generated:\n")
cat("- Tables: writing_draft/tables/\n")
cat("- Plots: writing_draft/figures/\n")
cat("\nAll files saved to:", getwd(), "\n")

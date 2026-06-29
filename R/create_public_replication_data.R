# =============================================================================
# Create Public Replication Dataset
# =============================================================================
# Builds the minimal public-facing CSV used by the replication pipeline from
# locally held restricted raw YouGov exports in data/raw/.
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
Sys.setenv(PREBUNK_WRITE_FULL_DATA = "false")

source_repo <- function(...) {
  source(file.path(repo_root, ...))
}

source_repo("config", "config.R")
source_repo("helper_functions.R")
source_repo("functions", "data_processing.R")
source_repo("functions", "public_data.R")

write_public_replication_data(dat_final)

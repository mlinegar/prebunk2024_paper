# =============================================================================
# Main Analysis Script for Prebunking Study
# =============================================================================
# This script sources all modular components and runs the complete analysis
# in logical order.
# =============================================================================

# Set working directory to project root
setwd("/Users/mlinegar/code/prebunk2024")

# Source all required files
source("./code_draft/config/config.R")  # Load libraries and configuration
source("./helper_functions.R")  # Load helper functions
source("./code_draft/functions/data_processing.R")  # Process data

# OPTION 1: Use quick version (loads pre-computed permutation results from CSV)
# This is MUCH faster but requires running balance_and_checks_permutations.R first
source("./code_draft/functions/balance_and_checks_quick.R")

# OPTION 2: Run full version including permutations (slow, ~10 min)
# Uncomment this line and comment out the quick version above:
# source("./code_draft/functions/balance_and_checks.R")

source("./code_draft/functions/modeling_helpers.R")  # Load modeling helper functions
source("./code_draft/functions/modeling.R")  # Run main regression models (weighted & unweighted)
source("./code_draft/functions/modeling_interactions.R")  # Run all interactions
source("./code_draft/functions/modeling_sensitivity.R")  # Sensitivity analysis (attentive sample)
source("./code_draft/functions/tables.R")  # Generate summary statistics
source("./code_draft/functions/plotting.R")  # Create all plots

# Print completion message
cat("\n=============================================================================\n")
cat("Analysis complete!\n")
cat("=============================================================================\n")
cat("\nOutputs generated:\n")
cat("- Data: prebunk_full.csv\n")
cat("- Tables: Multiple .tex files for LaTeX\n")
cat("- Plots: Multiple .pdf files\n")
cat("\nAll files saved to:", getwd(), "\n")

# Towards Generalizable AI-Assisted Misinformation Inoculation: Protecting Confidence Against False Election Narratives

Replication code for all analyses, figures, and tables reported in *Towards Generalizable AI-Assisted Misinformation Inoculation: Protecting Confidence Against False Election Narratives*.

## Usage
1. **Clone or download the repository** to any local directory; the main analysis script automatically detects the repository root.
2. **Acquire the raw survey files** (released upon publication) and place them inside `data/raw/`:
   - `caltech_elections_august24.sav`
   - `caltech_elections_augustrecontact24.sav`
3. **Install required R packages** listed in `config/config.R` (survey design, tidyverse, plotting, and table-export dependencies).
4. **Run the full pipeline** with `Rscript R/main_analysis.R` from the repository root (or `Rscript code_draft/R/main_analysis.R` from the parent project). The script sources every module, processes data, runs balance checks, estimates all models, and exports tables/figures.
5. **Choose your balance-check mode** by keeping either `balance_and_checks_quick.R` (cached permutations) or `balance_and_checks.R` (full permutation routine) active inside `R/main_analysis.R`. Regenerate the cached draws via `functions/balance_and_checks_permutations.R` if needed.

Outputs include the processed dataset (`prebunk_full.csv`), LaTeX tables, and PDF figures, all placed in the working directory defined within `R/main_analysis.R`.

Set the `PREBUNK_PROJECT_ROOT` environment variable if you need to point the scripts to a different project root (e.g., when running inside a larger workspace while keeping raw data elsewhere); otherwise the repository root is used by default.

## Repository Contents
- `config/config.R` – Central configuration for package loading, factor labels, treatment definitions, survey weights, and file-path constants shared across modules.
- `R/main_analysis.R` – Orchestrates the entire replication workflow by sourcing configuration, helpers, data processing, balance checks, modeling, table creation, and plotting scripts.
- `functions/data_processing.R` – Reads the raw survey datasets, harmonizes variables, builds indices (misinformation susceptibility, populism, conspiracy), merges the main and recontact waves, applies weights, and writes the cleaned analysis file.
- `functions/balance_and_checks.R` – Comprehensive randomization-check pipeline, including permutation-based difference tests and diagnostic exports.
- `functions/balance_and_checks_permutations.R` – Standalone routine that regenerates the cached permutation draws consumed by the quick balance-check script.
- `functions/balance_and_checks_quick.R` – Loads stored permutation summaries to provide rapid balance diagnostics during typical replication runs.
- `functions/modeling_helpers.R` – Shared helper functions for model formulas, label management, weighting utilities, and tidy output handling.
- `functions/modeling.R` – Core regression estimators that reproduce pooled treatment effects, rumor-specific models, recontact effects, and other headline estimates.
- `functions/modeling_interactions.R` – Interaction models covering treatment variants, ideology, party identification, conspiracy beliefs, and other moderators highlighted in the manuscript and appendix.
- `functions/modeling_sensitivity.R` – Robustness checks (e.g., attentive-only samples, alternative specifications) reported in supplementary materials.
- `functions/plotting.R` – Generates main-text visualizations: treatment effect plots, time-series comparisons, density plots, and other publication figures.
- `functions/plotting_interactions.R` – Produces interaction-focused graphics, including subgroup coefficient plots for the appendix.
- `functions/tables.R` – Builds descriptive, regression, and proportion tables, exporting LaTeX-ready files for both the paper and supplementary documents.
- `functions/verify_appendix_tables.R` – Internal validation script that cross-checks appendix table ordering and contents before manuscript exports.
- `helper_functions.R` – Shared utility functions for handling skipped values, plotting defaults, and file-output helpers.
- `data/raw/` – Placeholder directory for the raw `.sav` files (ignored by Git; populate locally before running).
- `REGRESSION_PLOT_MAPPING.md` – Reference mapping between regression objects and the figures/tables that report them, useful for cross-walking reviewer requests.
- `.gitignore` – Project-level ignore rules for local artifacts (R histories, logs, OS files, generated outputs).

## Data Availability
- `caltech_elections_august24.sav` (main wave) – released upon publication, to be located in `data/raw/`.
- `caltech_elections_augustrecontact24.sav` (recontact wave) – released upon publication, to be located in `data/raw/`.

## Outputs
- **Processed data:** `prebunk_full.csv`, produced by `functions/data_processing.R`.
- **Tables:** LaTeX files summarizing descriptive statistics, pooled and rumor-specific regressions, interaction models, and sensitivity analyses.
- **Figures:** PDF plots for main-text and appendix visuals, including pooled treatment effects, subgroup comparisons, and confidence trajectories.

Ensure that any newly released data are added to the specified location and paths in `config/config.R` are updated before running the pipeline.

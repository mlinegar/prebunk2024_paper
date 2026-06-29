# Toward Scalable AI-Assisted Prebunking of Election Misinformation

Replication code for all analyses, figures, and tables reported in *Toward Scalable AI-Assisted Prebunking of Election Misinformation: Evidence from a Preregistered U.S. Panel Experiment*.

## Usage
1. **Clone or download the repository** to any local directory; the main analysis script automatically detects the repository root.
2. **For public replication**, place the published Data S1 file at `data/public/prebunk_public_replication.csv`, then run:
   ```bash
   Rscript R/main_analysis_public.R
   ```
   This is the preferred replication path for GitHub/Dataverse users because it does not require the restricted raw survey exports.
3. **For internal raw-data rebuilds**, place the restricted raw survey files inside `data/raw/`:
   - `caltech_elections_august24.sav`
   - `caltech_elections_augustrecontact24.sav`
4. **Install required R packages** listed in `config/config.R` (survey design, tidyverse, plotting, and table-export dependencies).
5. **Create the public replication dataset** from the restricted raw files, if needed:
   ```bash
   Rscript R/create_public_replication_data.R
   ```
   This writes `data/public/prebunk_public_replication.csv` and `data/public/prebunk_public_replication_codebook.csv`.
6. **Run the full internal pipeline** with `Rscript R/main_analysis.R` from the repository root if you need to rebuild from the restricted raw files. The script sources every module, processes data, runs balance checks, estimates all models, and exports tables/figures.
7. **Choose your balance-check mode** by keeping either `balance_and_checks_quick.R` (default; uses cached permutation results in `data/cache/`) or `balance_and_checks.R` (full permutation routine) active inside the analysis script. Regenerate the cached permutation results via `functions/balance_and_checks_permutations.R` if the data change.

Outputs include LaTeX tables and PDF figures. Generated manuscript assets are written under `writing_draft/`:
- `writing_draft/figures/` for PDF figures
- `writing_draft/tables/` for LaTeX tables and lightweight diagnostic CSVs

Set the `PREBUNK_PROJECT_ROOT` environment variable if you need to point the scripts to a different project root (e.g., when running inside a larger workspace while keeping raw data elsewhere); otherwise the repository root is used by default. Set `PREBUNK_PUBLIC_DATA_FILE` if the public replication CSV is stored outside `data/public/`.

## Repository Contents
- `config/config.R` – Central configuration for package loading, factor labels, treatment definitions, survey weights, and file-path constants shared across modules.
- `R/main_analysis.R` – Orchestrates the entire replication workflow by sourcing configuration, helpers, data processing, balance checks, modeling, table creation, and plotting scripts.
- `R/main_analysis_public.R` – Runs the same analysis workflow from the public Data S1 CSV rather than the restricted raw exports.
- `R/create_public_replication_data.R` – Builds the minimal public-facing Data S1 CSV and codebook from the restricted raw survey exports.
- `functions/data_processing.R` – Reads the raw survey datasets, harmonizes variables, builds indices (misinformation susceptibility, populism, conspiracy), merges the main and recontact waves, applies weights, and writes the cleaned analysis file.
- `functions/public_data.R` – Defines the public replication columns, codebook, and type-restoration logic used by the public export and analysis scripts.
- `functions/balance_and_checks.R` – Comprehensive randomization-check pipeline, including permutation-based difference tests and diagnostic exports.
- `functions/balance_and_checks_permutations.R` – Standalone routine that regenerates the cached permutation results consumed by the quick balance-check script.
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
- `data/public/` – Placeholder directory for the published minimal replication CSV. The respondent-level CSV is ignored by Git; the codebook is safe to share.
- `data/cache/` – Small cached permutation-test results used by the default quick balance-check path.
- `REGRESSION_PLOT_MAPPING.md` – Reference mapping between regression objects and the figures/tables that report them, useful for cross-walking reviewer requests.
- `.gitignore` – Project-level ignore rules for local artifacts (R histories, logs, OS files, generated outputs).

## Data Availability
- `prebunk_public_replication.csv` – minimal respondent-level Data S1 file needed to reproduce the reported models, tables, and figures. Place it in `data/public/` or set `PREBUNK_PUBLIC_DATA_FILE` to its location.
- `prebunk_public_replication_codebook.csv` – variable definitions and factor levels for the public replication file.
- `caltech_elections_august24.sav` (main wave) – restricted raw export used only for internal rebuilds; place locally in `data/raw/`.
- `caltech_elections_augustrecontact24.sav` (recontact wave) – restricted raw export used only for internal rebuilds; place locally in `data/raw/`.
- `data/cache/balance_wald_test.csv` and `data/cache/attrition_tests.csv` are included so the default quick pipeline reports the preregistered permutation diagnostics without rerunning the 10,000-iteration permutation routine.

## Outputs
- **Public replication data:** `data/public/prebunk_public_replication.csv`, produced by `R/create_public_replication_data.R` from the restricted raw exports and used by `R/main_analysis_public.R`.
- **Tables:** LaTeX files under `writing_draft/tables/` summarizing descriptive statistics, pooled and rumor-specific regressions, interaction models, and sensitivity analyses.
- **Figures:** PDF plots under `writing_draft/figures/` for main-text and appendix visuals, including pooled treatment effects, subgroup comparisons, and confidence trajectories.

Ensure that any newly released data are added to the specified location and paths in `config/config.R` are updated before running the raw-data pipeline.

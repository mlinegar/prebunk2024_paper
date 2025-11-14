# Modular Prebunking Analysis Code

This directory contains the modularized version of the prebunking analysis, extracted from the original 2136-line `prebunk.R` script.

## Directory Structure

```
code_draft/
├── config/
│   └── config.R              # Configuration, libraries, and variable definitions
├── functions/
│   ├── data_processing.R     # Data loading and transformation
│   ├── modeling.R            # Regression models and statistical tests
│   ├── plotting.R            # All visualization code
│   └── tables.R              # Summary statistics and table generation
├── R/
│   └── main_analysis.R       # Main script that runs the complete analysis
└── README.md                 # This file
```

## File Descriptions and Line Mappings

### 1. config/config.R (130 lines)
**Extracted from:** prebunk.R lines 1-127

**Contains:**
- Library imports (27 packages)
- File path definitions
- Variable definitions (outcome variables, model variables)
- Variable labels and mappings
- Constants (rating scales, populism/conspiracy questions)
- Treatment labels and output configuration

### 2. functions/data_processing.R (214 lines)
**Extracted from:** prebunk.R lines 38-333

**Contains:**
- Data loading (main survey and recontact data)
- Data transformations and mutations
- Variable recoding (MIST questions, demographic variables)
- Scale flipping for reverse-coded items
- Factor creation and labeling
- Data merging (main and recontact datasets)
- Survey weight calculations
- Final data export and survey design setup
- Covariate label generation

**Key operations:**
- Handles skipped values across multiple variable types
- Recodes MIST questions to correct/incorrect
- Creates CISA relevance variables based on randomization
- Generates populism and conspiracy bins
- Calculates difference scores (pre-post, pre-recontact)
- Removes cases with missing model variables
- Renames variables using the label mapping

### 3. functions/modeling.R (502 lines)
**Extracted from:** prebunk.R lines 464-875, 1519-1599

**Contains:**
- Mini regression models (sanity checks)
- Individual rumor models
- Pooled regression models
- Recontact/followup models
- Interaction models (HITL, party identification)
- CISA-specific models
- Motivation to debunk models
- Statistical tests (t-tests, Cohen's d)

**Key analyses:**
- Simple treatment effect models
- Models with rumor type controls
- Human-in-the-loop comparisons
- Full covariate-adjusted models
- Interaction analyses by party/ideology
- Effect size calculations
- Both immediate and delayed (recontact) effects

### 4. functions/plotting.R (798 lines)
**Extracted from:** prebunk.R lines 877-1514, 2033-2136

**Contains:**
- Probability density plots (treatment effects distribution)
- Pre-post scatter plots
- Pre-post difference plots
- Time series plots (3-wave: pre, post, recontact)
- Coefficient plots (treatment effects visualization)

**Plot variations:**
- By treatment status
- By rumor type
- By party identification
- By conspiracy beliefs
- Collapsed vs. disaggregated versions
- Main survey vs. recontact comparisons

**Outcomes visualized:**
- Confidence in ballot counting (own, county, national)
- Belief in election rumors (specific and pooled)
- CISA question responses

### 5. functions/tables.R (393 lines)
**Extracted from:** prebunk.R lines 335-462, 1600-1985

**Contains:**
- Summary statistics generation (weighted and unweighted)
- Factor variable summaries
- Cross-wave comparison tables
- Proportion tables (confidence thresholds)
- LaTeX table export

**Table types:**
- Descriptive statistics by group
- Factor variable distributions
- Pre/post/recontact proportions
- By party identification
- By rumor type
- Confidence interval calculations

### 6. R/main_analysis.R (27 lines)
**Main orchestration script**

**Execution order:**
1. Load configuration (config.R)
2. Load helper functions (helper_functions.R from parent directory)
3. Process data (data_processing.R)
4. Generate tables (tables.R)
5. Run models (modeling.R)
6. Create plots (plotting.R)

## Usage

To run the complete analysis:

```r
# Set working directory to project root
setwd("/Users/mlinegar/code/prebunk2024")

# Run the main analysis script
source("./code_draft/R/main_analysis.R")
```

Alternatively, source individual files for specific tasks:

```r
# Just data processing
source("./code_draft/config/config.R")
source("./helper_functions.R")
source("./code_draft/functions/data_processing.R")

# Just create plots (requires data processing first)
source("./code_draft/functions/plotting.R")
```

## Dependencies

**Required R packages:**
- survey, forcats, haven, tidyverse, dplyr, ggplot2
- margins, effects, svyVGAM, stargazer, broom
- scales, moments, kableExtra, rlang, effsize
- wesanderson, stringr, data.table, ggrepel, ggpp, purrr, see, xtable

**Required data files:**
- caltech_elections_august24.sav
- caltech_elections_augustrecontact24.sav
- helper_functions.R (in parent directory)

## Output Files

**Data:**
- prebunk_full.csv (processed dataset)

**Tables (LaTeX .tex files):**
- Summary statistics (overall, by group, weighted/unweighted)
- Regression tables (pooled, by rumor, interactions)
- Proportion tables (by party, by rumor)

**Plots (PDF files):**
- Probability density plots
- Pre-post scatter plots
- Time series plots
- Coefficient plots
- Treatment effects visualizations

## Original File Information

**Original file:** prebunk.R (2136 lines)
**Modularized into:** 6 files (2064 lines total, plus documentation)
**Extraction date:** 2025

## Notes

- All code has been extracted exactly as it appears in the original file
- No modifications or improvements have been made to the code logic
- Comments and structure from the original file are preserved
- The helper_functions.R file from the parent directory is still required
- File paths in config.R may need adjustment for different systems

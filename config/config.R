# =============================================================================
# Configuration File for Prebunking Analysis
# =============================================================================
# This file contains all library imports, file paths, variable definitions,
# and configuration constants used throughout the analysis.
# Extracted from prebunk.R (lines 1-127)
# =============================================================================

#### Load libraries ####
library(survey)
library(forcats)
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(margins)
library(effects)
library(svyVGAM)
library(stargazer)
library(broom)
library(scales)
library(moments)
library(kableExtra)
library(rlang)
library(effsize)
library(wesanderson)
library(stringr)
library(data.table)
library(effects)
library(ggrepel)
library(ggpp)
library(broom)
library(purrr)
library(see)
library(xtable)

#### Define key fixed variables ####
# Allow overriding the project root via environment variable
project_root <- Sys.getenv("PREBUNK_PROJECT_ROOT", unset = getwd())
data_dir <- project_root
data_file <- file.path("data", "raw", "caltech_elections_august24.sav")
recontact_file <- file.path("data", "raw", "caltech_elections_augustrecontact24.sav")

# Define interaction variables to test
# Each will generate separate regression tables
# H5 (preregistered exploratory): Initial belief in rumors (cisa_fake = baseline average of all 5 rumors)
# H6 (preregistered exploratory): Party ID, Ideology, MIST-8, News sources/Political Interest, Populism
int_vars <- c(
  "Party_Identification",  # H6: Partisanship
  "Ideology",              # H6: Ideology
  "MIST_Correct",          # H6: Misinformation susceptibility
  "Political_Interest",    # H6: News sources/interest
  "Populism_Score",        # H6: Populism
  "Conspiracy_Score",      # Additional exploratory
  "Human_In_The_Loop"      # Additional exploratory
)

# Define which moderators to highlight in main text summary plot
# These are the most theoretically important for understanding heterogeneous effects
int_vars_main_text <- c(
  "Party_Identification",  # Most important: partisan differences in trust
  "Ideology",              # Second: ideological differences
  "Conspiracy_Score"       # Third: role of conspiracy thinking
)

# Define primary outcome for main text summary plots
# This is the main dependent variable of theoretical interest
primary_outcome <- "Post_Confidence_Country_Ballots"  # National ballot confidence

# Define treatment variable names (for plotting and model extraction)
treatment_var_name <- "Election_Rumor_Placebo_RandomizationTreatment"  # Main treatment coefficient
treatment_var_base <- "Election_Rumor_Placebo_Randomization"  # Base variable name for interactions

int_var <- "Party_Identification"  # Default for backward compatibility

# Define constants
total_populism_questions <- 6
total_conspiracy_questions <- 6
rating_max <- 5
rating_min <- 1

output_label <- ""
# output_label <- "_pid"

# Define variables in model and their labels
y_vars_post <- c("ballotcount_2_scale", "ballotcounty_2_scale", "ballotcountry_2_scale")
y_vars_pre <- c("ballotcount_scale", "ballotcounty_scale", "ballotcountry_scale")
y_vars_diff <- c("ballotcount_diff", "ballotcounty_diff", "ballotcountry_diff")
y_vars_recontact <- c("ballotcount_scale_recontact", "ballotcounty_scale_recontact", "ballotcountry_scale_recontact")
y_vars_diff_recontact <- c("ballotcount_diff_recontact", "ballotcounty_diff_recontact", "ballotcountry_diff_recontact")

# CISA outcome variables (rumors and facts)
cisa_y_vars_pre <- c("cisa_fake", "cisa_true")
cisa_y_vars_post <- c("cisa_fake_2", "cisa_true_2")
cisa_y_vars_recontact <- c("cisa_fake_recontact", "cisa_true_recontact")
cisa_y_vars_diff_recontact <- c("cisa_rel_diff", "cisa_rel_diff_recontact", "cisa_fake_diff_recontact", "cisa_true_diff_recontact")

motivated_y_vars_recontact <- c("changemind_recontact", "election1_scale_recontact", "election2_scale_recontact", "election3_scale_recontact")
hitl_var <- "human_in_the_loop"

# Model set configurations - specify which outcomes to use for each model type
model_sets <- list(
  ballot_confidence = list(
    post = y_vars_post,
    pre = y_vars_pre,
    diff = y_vars_diff,
    recontact = y_vars_recontact,
    recontact_diff = y_vars_diff_recontact
  ),
  cisa = list(
    diff = c("cisa_rel_diff"),
    recontact = c("cisa_rel_recontact"),
    recontact_diff = c("cisa_rel_diff_recontact")
  ),
  motivated = list(
    recontact = motivated_y_vars_recontact
  )
)

variables_in_model <- c("age4", "gender", "race4", "educ4", "pid3", "ideo3",
                        "region", "urbancity", "newsint", "populism", "conspiracy", "mist")

treatment_label <- "Treatment (vs. Placebo)"
# Create a list of nice labels for variables
variable_labels <- list(
  age4 = "Age_Group",
  gender = "Gender",
  race4 = "Race_Ethnicity",
  educ4 = "Education_Level",
  pid3 = "Party_Identification",
  ideo3 = "Ideology",
  region = "Region",
  urbancity = "Urban_Rural",
  newsint = "Political_Interest",
  populism = "Populism_Score",
  conspiracy = "Conspiracy_Score",
  mist = "MIST_Correct",
  populism_bin = "Populism_Bin",
  conspiracy_bin = "Conspiracy_Bin",
  electionrumor_rand = "Election_Rumor_Randomization",
  electionrumor_placebo_rand = "Election_Rumor_Placebo_Randomization",
  ballotcount_scale = "Pre_Confidence_Own_Ballot",
  ballotcounty_scale = "Pre_Confidence_County_Ballots",
  ballotcountry_scale = "Pre_Confidence_Country_Ballots",
  ballotcount_2_scale = "Post_Confidence_Own_Ballot",
  ballotcounty_2_scale = "Post_Confidence_County_Ballots",
  ballotcountry_2_scale = "Post_Confidence_Country_Ballots",
  ballotcount_scale_recontact = "Recontact_Confidence_Own_Ballot",
  ballotcounty_scale_recontact = "Recontact_Confidence_County_Ballots",
  ballotcountry_scale_recontact = "Recontact_Confidence_Country_Ballots",
  ballotcount_diff = "Confidence_Own_Ballot_Diff",
  ballotcounty_diff = "Confidence_County_Ballots_Diff",
  ballotcountry_diff = "Confidence_Country_Ballots_Diff",
  ballotcount_diff_recontact = "Recontact_Confidence_Own_Ballot_Diff",
  ballotcounty_diff_recontact = "Recontact_Confidence_County_Ballots_Diff",
  ballotcountry_diff_recontact = "Recontact_Confidence_Country_Ballots_Diff",
  cisa_rel = "Rumor",
  prebunking4_scale = "Rumor_Post",
  cisa_rel_diff = "Rumor_Diff",
  cisa_rel_recontact = "Rumor_Recontact",
  cisa_rel_diff_recontact = "Rumor_Diff_Recontact",
  cisa_fake = "Pre_All_Rumors",
  cisa_fake_2 = "Post_All_Rumors",
  cisa_fake_recontact = "Recontact_All_Rumors",
  cisa_fake_diff_recontact = "All_Rumors_Diff_Recontact",
  cisa_true = "Pre_All_Facts",
  cisa_true_2 = "Post_All_Facts",
  cisa_true_recontact = "Recontact_All_Facts",
  cisa_true_diff_recontact = "All_Facts_Diff_Recontact",
  article_recalled = "Article_Recalled_Recontact",
  changemind_recontact = "Tried_To_Change_Friends_Mind",
  election1_scale_recontact = "Motivated_To_Debunk",
  election2_scale_recontact = "Equipped_To_Push_Back",
  election3_scale_recontact = "Willing_To_Argue_Against",
  human_in_the_loop = "Human_In_The_Loop"
)


variables_in_model_labels <- variable_labels[variables_in_model] %>% unlist() %>% as.character()
post_y_vars_labels <- variable_labels[y_vars_post] %>% unlist() %>% as.character()
pre_y_vars_labels <- variable_labels[y_vars_pre] %>% unlist() %>% as.character()
diff_y_vars_labels <- variable_labels[y_vars_diff] %>% unlist() %>% as.character()
recontact_y_var_labels <- variable_labels[y_vars_recontact] %>% unlist() %>% as.character()
recontact_diff_y_var_labels <- variable_labels[y_vars_diff_recontact] %>% unlist() %>% as.character()
recontact_cisa_diff_y_var_labels <- variable_labels[cisa_y_vars_diff_recontact] %>% unlist() %>% as.character()
motivated_y_vars_recontact_labels <- variable_labels[motivated_y_vars_recontact] %>% unlist() %>% as.character()

# CISA outcome variable labels
cisa_pre_y_vars_labels <- variable_labels[cisa_y_vars_pre] %>% unlist() %>% as.character()
cisa_post_y_vars_labels <- variable_labels[cisa_y_vars_post] %>% unlist() %>% as.character()
cisa_recontact_y_vars_labels <- variable_labels[cisa_y_vars_recontact] %>% unlist() %>% as.character()

all_var_labels <- c(pre_y_vars_labels, post_y_vars_labels, diff_y_vars_labels, variables_in_model_labels)

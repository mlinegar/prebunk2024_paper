# =============================================================================
# Modeling and Statistical Analysis Functions
# =============================================================================
# This file contains all regression models, statistical tests, and analysis code
# for the prebunking analysis.
# Extracted from prebunk.R (lines 464-875, 1519-1599)
# =============================================================================

### REGRESSIONS ###


### TESTING: MINI REGRESSIONS TO DOUBLE CHECK THINGS ####


tiny_lm_own <- lm(Confidence_Own_Ballot_Diff~Election_Rumor_Placebo_Randomization, data = dat_final)
tiny_lm_county <- lm(Confidence_County_Ballots_Diff~Election_Rumor_Placebo_Randomization, data = dat_final)
tiny_lm_country <- lm(Confidence_Country_Ballots_Diff~Election_Rumor_Placebo_Randomization, data = dat_final)

create_ols_summary_table(
      models = list(tiny_lm_own, tiny_lm_county, tiny_lm_country),
      title = paste("OLS Regression Results for Smallest Pooled Model"),
      column.labels = paste0("Diff ", c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence")),
      covariate.labels = c(treatment_label),
      out.file = "tiny_diff.tex",
      label = paste0("tab:tiny")
    )

# recontact test
tiny_lm_own_recontact <- lm(Recontact_Confidence_Own_Ballot ~ Pre_Confidence_Own_Ballot + Election_Rumor_Placebo_Randomization, data = dat_final)
tiny_lm_county_recontact <- lm(Recontact_Confidence_County_Ballots~ Pre_Confidence_County_Ballots + Election_Rumor_Placebo_Randomization, data = dat_final)
tiny_lm_country_recontact <- lm(Recontact_Confidence_Country_Ballots~ Pre_Confidence_Country_Ballots + Election_Rumor_Placebo_Randomization, data = dat_final)

mini_lm_own_recontact <- lm(Confidence_Own_Ballot_Diff~Election_Rumor_Placebo_Randomization + Election_Rumor_Randomization, data = dat_final)
mini_lm_county_recontact <- lm(Confidence_County_Ballots_Diff~Election_Rumor_Placebo_Randomization + Election_Rumor_Randomization, data = dat_final)
mini_lm_country_recontact <- lm(Confidence_Country_Ballots_Diff~Election_Rumor_Placebo_Randomization + Election_Rumor_Randomization, data = dat_final)


mini_lm_own <- lm(Confidence_Own_Ballot_Diff~Election_Rumor_Placebo_Randomization + Election_Rumor_Randomization, data = dat_final)
mini_lm_county <- lm(Confidence_County_Ballots_Diff~Election_Rumor_Placebo_Randomization + Election_Rumor_Randomization, data = dat_final)
mini_lm_country <- lm(Confidence_Country_Ballots_Diff~Election_Rumor_Placebo_Randomization + Election_Rumor_Randomization, data = dat_final)

create_ols_summary_table(
      models = list(mini_lm_own, mini_lm_county, mini_lm_country),
      title = paste("OLS Regression Results for Smallest Pooled Model"),
      column.labels = paste0("Diff ", c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence")),
      covariate.labels = c(treatment_label, paste("Rumor: ", levels(dat_final$Election_Rumor_Randomization))),
      out.file = "mini_diff.tex",
      label = paste0("tab:mini")
    )

# Check if human writing matters

hitl_lm_own <- lm(Confidence_Own_Ballot_Diff~Election_Rumor_Placebo_Randomization + Human_In_The_Loop, data = dat_final)
hitl_lm_county <- lm(Confidence_County_Ballots_Diff~Election_Rumor_Placebo_Randomization + Human_In_The_Loop, data = dat_final)
hitl_lm_country <- lm(Confidence_Country_Ballots_Diff~Election_Rumor_Placebo_Randomization + Human_In_The_Loop, data = dat_final)

create_ols_summary_table(
      models = list(hitl_lm_own, hitl_lm_county, hitl_lm_country),
      title = paste("OLS Regression Results for HITL Test Model"),
      column.labels = paste0("Diff ", c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence")),
      # covariate.labels = c(treatment_label, paste("Rumor: ", levels(dat_final$Election_Rumor_Randomization))),
      out.file = "hitl_test_diff.tex",
      label = paste0("tab:hitl_test")
    )

rumors <- c("Voter Fraud", "Voter Rolls", "Hacking", "Blue Shift", "Voting Machines")
# mini_lm_cisa <- lm(Rumor_Diff~Election_Rumor_Placebo_Randomization + Election_Rumor_Randomization, data = dat_final)
# mini_lm_cisa <- lm(Rumor_Diff~Election_Rumor_Placebo_Randomization,
#   data = dat_final %>% dplyr::filter(Election_Rumor_Randomization==rumors[5]))
# summary(mini_lm_cisa)

# Run Main Regressions

# Run models for each rumor
rumor_models <- lapply(seq_along(levels(dat_final$Election_Rumor_Randomization)), run_rumor_models)
names(rumor_models) <- levels(dat_final$Election_Rumor_Randomization)

cisa_rumor_models <- lapply(seq_along(levels(dat_final$Election_Rumor_Randomization)), run_rumor_models, var_labels = c("Rumor_Diff", "Rumor_Diff_Recontact"))
names(cisa_rumor_models) <- levels(dat_final$Election_Rumor_Randomization)


# Create tables for individual rumor models
rumor_names <- levels(dat_final$Election_Rumor_Randomization)
create_rumor_tables(rumor_models, rumor_names)


cisa_rumor_names <- paste0("CISA ", levels(dat_final$Election_Rumor_Randomization))
create_rumor_tables(cisa_rumor_models, cisa_rumor_names)

# =============================================================================
# RUN ALL MODELS TWICE: WEIGHTED AND UNWEIGHTED
# =============================================================================

for (weight_type in c("weighted", "unweighted")) {

  # Set which design to use
  svy_design <- if (weight_type == "weighted") svy_design_weighted else svy_design_unweighted
  output_suffix <- if (weight_type == "weighted") "_pate" else "_sate"

  cat(sprintf("\n=== Running %s models ===\n", weight_type))

  # Run pooled models
  pooled_models <- lapply(post_y_vars_labels, function(outcome) {
    predictors <- c(paste0(sub("Post_", "Pre_", outcome)), "Election_Rumor_Placebo_Randomization", "Election_Rumor_Randomization", variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(pooled_models) <- paste0("Pooled_", post_y_vars_labels)

  # Run recontact pooled models
  pooled_followup_models <- lapply(recontact_y_var_labels, function(outcome) {
    predictors <- c(paste0(sub("Recontact_", "Pre_", outcome)), "Election_Rumor_Placebo_Randomization", "Election_Rumor_Randomization", variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(pooled_followup_models) <- paste0("Pooled_", recontact_y_var_labels)

  # Save models to appropriately named variables
  if (weight_type == "weighted") {
    pooled_models_weighted <- pooled_models
    pooled_followup_models_weighted <- pooled_followup_models
  } else {
    pooled_models_unweighted <- pooled_models
    pooled_followup_models_unweighted <- pooled_followup_models
  }
}

# Default to weighted pooled models for downstream plotting code
pooled_models <- pooled_models_weighted
pooled_followup_models <- pooled_followup_models_weighted


# Create tables for pooled models
key_covars <- c(
    pre_y_vars_labels %>% gsub("_", " ", .),
    treatment_label,
    "Rumor: Voter Rolls", "Rumor: Hacking", "Rumor: Blue Shift", "Rumor: Voting Machines"
    )
covar_labels <- c(
    key_covars,
    covariate_labels)

# Weighted table (PATE)
pooled_table_weighted <- create_ols_summary_table(
  models = pooled_models_weighted,
  title = "OLS Regression Results for Pooled Models (Weighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("pooled_models_table_weighted%s.tex", output_label)),
    label = "tab:pooled_models",
    longtable = FALSE
)

# Unweighted table (SATE)
pooled_table_unweighted <- create_ols_summary_table(
  models = pooled_models_unweighted,
  title = "OLS Regression Results for Pooled Models (Unweighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("pooled_models_table_unweighted%s.tex", output_label)),
    label = "tab:pooled_models_unweighted",
    longtable = FALSE
)
cat("Pooled models tables (weighted and unweighted) created.\n")


# =============================================================================
# CISA TABLES - WEIGHTED AND UNWEIGHTED
# =============================================================================

for (weight_type in c("weighted", "unweighted")) {

  output_suffix <- if (weight_type == "weighted") "_weighted" else "_unweighted"
  cat(sprintf("\n=== Running %s CISA models ===\n", weight_type))

  # CISA rumor models
  cisa_rumor_models <- lapply(rumors, function(rumor){
    dat_rumor <- dat_final %>% dplyr::filter(Election_Rumor_Randomization==rumor)
    if (weight_type == "weighted") {
      svy_design_rumor <- svydesign(data = dat_rumor, weights = ~weight, id = ~1)
    } else {
      svy_design_rumor <- svydesign(data = dat_rumor, weights = NULL, id = ~1)
    }
    predictors <- c("Election_Rumor_Placebo_Randomization", variables_in_model_labels)
    run_regression_for_table("Rumor_Diff", predictors, svy_design_rumor, family = gaussian())
  })
  names(cisa_rumor_models) <- rumors

  # CISA rumor followup models
  cisa_rumor_followup_models <- lapply(rumors, function(rumor){
    dat_rumor <- dat_final %>% dplyr::filter(Election_Rumor_Randomization==rumor & !is.na(weight_recontact))
    if (weight_type == "weighted") {
      svy_design_rumor <- svydesign(data = dat_rumor, weights = ~weight_recontact, id = ~1)
    } else {
      svy_design_rumor <- svydesign(data = dat_rumor, weights = NULL, id = ~1)
    }
    predictors <- c("Election_Rumor_Placebo_Randomization", variables_in_model_labels)
    run_regression_for_table("Rumor_Diff_Recontact", predictors, svy_design_rumor, family = gaussian())
  })
  names(cisa_rumor_followup_models) <- paste0("Recontact ", rumors)

  # Save to appropriately named variables
  if (weight_type == "weighted") {
    cisa_rumor_models_weighted <- cisa_rumor_models
    cisa_rumor_followup_models_weighted <- cisa_rumor_followup_models
  } else {
    cisa_rumor_models_unweighted <- cisa_rumor_models
    cisa_rumor_followup_models_unweighted <- cisa_rumor_followup_models
  }
}

# Default to weighted rumor models for plotting code
cisa_rumor_models <- cisa_rumor_models_weighted
cisa_rumor_followup_models <- cisa_rumor_followup_models_weighted

# Create CISA tables
cisa_rumors_table_weighted <- create_ols_summary_table(
  models = cisa_rumor_models_weighted,
  title = "OLS Regression Results for Individual Rumor CISA Models (Weighted)",
  column.labels = rumors,
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("cisa_rumor_models_table_weighted%s.tex", output_label)),
    label = "tab:cisa_rumors",
    longtable = FALSE
)

cisa_rumors_table_unweighted <- create_ols_summary_table(
  models = cisa_rumor_models_unweighted,
  title = "OLS Regression Results for Individual Rumor CISA Models (Unweighted)",
  column.labels = rumors,
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("cisa_rumor_models_table_unweighted%s.tex", output_label)),
    label = "tab:cisa_rumors_unweighted",
    longtable = FALSE
)

cisa_rumors_followup_table_weighted <- create_ols_summary_table(
  models = cisa_rumor_followup_models_weighted,
  title = "OLS Regression Results for Individual CISA Recontact Models (Weighted)",
  column.labels = rumors,
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("cisa_rumors_followup_models_table_weighted%s.tex", output_label)),
    label = "tab:cisa_rumors_followup",
    longtable = FALSE
)

cisa_rumors_followup_table_unweighted <- create_ols_summary_table(
  models = cisa_rumor_followup_models_unweighted,
  title = "OLS Regression Results for Individual CISA Recontact Models (Unweighted)",
  column.labels = rumors,
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("cisa_rumors_followup_models_table_unweighted%s.tex", output_label)),
    label = "tab:cisa_rumors_followup_unweighted",
    longtable = FALSE
)
cat("CISA tables (weighted and unweighted) created.\n")

# =============================================================================
# HITL INTERACTION MODELS AND RECONTACT - WEIGHTED AND UNWEIGHTED
# =============================================================================

for (weight_type in c("weighted", "unweighted")) {

  svy_design <- if (weight_type == "weighted") svy_design_weighted else svy_design_unweighted
  output_suffix <- if (weight_type == "weighted") "_weighted" else "_unweighted"

  cat(sprintf("\n=== Running %s HITL and recontact models ===\n", weight_type))

  # Run pooled models with human in the loop interaction
  hitl_var_label <- variable_labels[[hitl_var]]
  hitl_int_pooled_models <- lapply(post_y_vars_labels, function(outcome) {
    predictors <- c(paste0(sub("Post_", "Pre_", outcome)), "Election_Rumor_Placebo_Randomization",
    paste0("Election_Rumor_Placebo_Randomization * ", hitl_var_label),
    variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(hitl_int_pooled_models) <- paste0("Pooled_", post_y_vars_labels)

  # Run recontact pooled models
  recontact_pooled_models <- lapply(recontact_y_var_labels, function(outcome) {
    predictors <- c(paste0(sub("Recontact_", "Pre_", outcome)), "Election_Rumor_Placebo_Randomization", "Election_Rumor_Randomization", variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(recontact_pooled_models) <- paste0("Recontact_", recontact_y_var_labels)

  # Save to appropriately named variables
  if (weight_type == "weighted") {
    hitl_int_pooled_models_weighted <- hitl_int_pooled_models
    recontact_pooled_models_weighted <- recontact_pooled_models
  } else {
    hitl_int_pooled_models_unweighted <- hitl_int_pooled_models
    recontact_pooled_models_unweighted <- recontact_pooled_models
  }
}

# Create tables for HITL models
hitl_int_key_covars <- c(
    pre_y_vars_labels %>% gsub("_", " ", .),
    treatment_label,
    "Rumor: Voter Rolls", "Rumor: Hacking", "Rumor: Blue Shift", "Rumor: Voting Machines"
    )
hitl_int_covar_labels <- c(
    hitl_int_key_covars,
    covariate_labels)

hitl_int_pooled_table_weighted <- create_ols_summary_table(
  models = hitl_int_pooled_models_weighted,
  title = "OLS Regression Results for Pooled Models with HITL Interaction (Weighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  out.file = file.path(data_dir, sprintf("hitl_int_pooled_models_table_weighted%s.tex", output_label)),
    label = "tab:hitl_int_pooled_models",
    longtable = FALSE
)

hitl_int_pooled_table_unweighted <- create_ols_summary_table(
  models = hitl_int_pooled_models_unweighted,
  title = "OLS Regression Results for Pooled Models with HITL Interaction (Unweighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  out.file = file.path(data_dir, sprintf("hitl_int_pooled_models_table_unweighted%s.tex", output_label)),
    label = "tab:hitl_int_pooled_models_unweighted",
    longtable = FALSE
)

# Create tables for recontact models
recontact_pooled_table_weighted <- create_ols_summary_table(
  models = recontact_pooled_models_weighted,
  title = "OLS Regression Results for Pooled Recontact Models (Weighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("recontact_pooled_models_table_weighted%s.tex", output_label)),
    label = "tab:recontact_pooled_models",
    longtable = FALSE
)

recontact_pooled_table_unweighted <- create_ols_summary_table(
  models = recontact_pooled_models_unweighted,
  title = "OLS Regression Results for Pooled Recontact Models (Unweighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("recontact_pooled_models_table_unweighted%s.tex", output_label)),
    label = "tab:recontact_pooled_models_unweighted",
    longtable = FALSE
)
cat("HITL and recontact tables (weighted and unweighted) created.\n")


# =============================================================================
# OTHER CISA MODELS - WEIGHTED AND UNWEIGHTED
# =============================================================================

for (weight_type in c("weighted", "unweighted")) {

  svy_design <- if (weight_type == "weighted") svy_design_weighted else svy_design_unweighted
  output_suffix <- if (weight_type == "weighted") "_weighted" else "_unweighted"

  cat(sprintf("\n=== Running %s other CISA models ===\n", weight_type))

  # Run CISA models
  cisa_models <- lapply(recontact_cisa_diff_y_var_labels, function(outcome) {
    predictors <- c(
      "Election_Rumor_Placebo_Randomization",
      "Election_Rumor_Randomization",
      variables_in_model_labels
      )
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(cisa_models) <- recontact_cisa_diff_y_var_labels

  # Run CISA models with interactions
  int_cisa_models <- lapply(recontact_cisa_diff_y_var_labels, function(outcome) {
    predictors <- c(
      "Election_Rumor_Placebo_Randomization",
      "Election_Rumor_Randomization",
      paste0("Election_Rumor_Placebo_Randomization * ", "Ideology"),
      variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(int_cisa_models) <- recontact_cisa_diff_y_var_labels

  # Save to appropriately named variables
  if (weight_type == "weighted") {
    cisa_models_weighted <- cisa_models
    int_cisa_models_weighted <- int_cisa_models
  } else {
    cisa_models_unweighted <- cisa_models
    int_cisa_models_unweighted <- int_cisa_models
  }
}

# Default to weighted CISA pooled models for plotting code
cisa_models <- cisa_models_weighted

# Setup covariate labels
key_covars <- c(
  treatment_label,
  "Rumor: Voter Rolls", "Rumor: Hacking", "Rumor: Blue Shift", "Rumor: Voting Machines"
  )
covar_labels <- c(
    key_covars,
    covariate_labels
    )

# Create tables for CISA models
cisa_table_weighted <- create_ols_summary_table(
  models = cisa_models_weighted,
  title = "OLS Regression Results for CISA Models (Weighted)",
  column.labels = stringr::str_replace_all(recontact_cisa_diff_y_var_labels, "_", " "),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("cisa_models_table_weighted%s.tex", output_label)),
  label = "tab:cisa_models",
    longtable = FALSE
)

cisa_table_unweighted <- create_ols_summary_table(
  models = cisa_models_unweighted,
  title = "OLS Regression Results for CISA Models (Unweighted)",
  column.labels = stringr::str_replace_all(recontact_cisa_diff_y_var_labels, "_", " "),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("cisa_models_table_unweighted%s.tex", output_label)),
  label = "tab:cisa_models_unweighted",
    longtable = FALSE
)

# Create tables for CISA models with interactions
int_cisa_table_weighted <- create_ols_summary_table(
  models = int_cisa_models_weighted,
  title = "OLS Regression Results for CISA Models with Ideology Interaction (Weighted)",
  column.labels = stringr::str_replace_all(recontact_cisa_diff_y_var_labels, "_", " "),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("int_cisa_models_table_weighted%s.tex", output_label)),
  label = "tab:int_cisa_models",
    longtable = FALSE
)

int_cisa_table_unweighted <- create_ols_summary_table(
  models = int_cisa_models_unweighted,
  title = "OLS Regression Results for CISA Models with Ideology Interaction (Unweighted)",
  column.labels = stringr::str_replace_all(recontact_cisa_diff_y_var_labels, "_", " "),
  covariate.labels = covar_labels,
  out.file = file.path(data_dir, sprintf("int_cisa_models_table_unweighted%s.tex", output_label)),
  label = "tab:int_cisa_models_unweighted",
    longtable = FALSE
)
cat("Other CISA tables (weighted and unweighted) created.\n")


# =============================================================================
# MOTIVATED AND HITL MODELS - WEIGHTED AND UNWEIGHTED
# =============================================================================

for (weight_type in c("weighted", "unweighted")) {

  svy_design <- if (weight_type == "weighted") svy_design_weighted else svy_design_unweighted
  output_suffix <- if (weight_type == "weighted") "_weighted" else "_unweighted"

  cat(sprintf("\n=== Running %s motivated and HITL models ===\n", weight_type))

  # Motivation to debunk regressions
  motivated_models <- lapply(motivated_y_vars_recontact_labels, function(outcome) {
    predictors <- c("Election_Rumor_Placebo_Randomization", "Election_Rumor_Randomization", variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(motivated_models) <- motivated_y_vars_recontact_labels

  # Human in the loop models
  hitl_models <- lapply(post_y_vars_labels, function(outcome) {
    predictors <- c(paste0(sub("Post_", "Pre_", outcome)),
                    "Election_Rumor_Placebo_Randomization",
                    hitl_var_label,
                    variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(hitl_models) <- post_y_vars_labels

  # Save to appropriately named variables
  if (weight_type == "weighted") {
    motivated_models_weighted <- motivated_models
    hitl_models_weighted <- hitl_models
  } else {
    motivated_models_unweighted <- motivated_models
    hitl_models_unweighted <- hitl_models
  }
}

# Create tables for motivated models
motivated_table_weighted <- create_ols_summary_table(
  models = motivated_models_weighted,
  title = "OLS Regression Results for Debunking Motivation Models (Weighted)",
  column.labels = stringr::str_replace_all(motivated_y_vars_recontact_labels, "_", " "),
  covariate.labels = c(
    treatment_label,
    "Rumor: Voter Rolls", "Rumor: Hacking", "Rumor: Blue Shift", "Rumor: Voting Machines",
    covariate_labels),
  out.file = file.path(data_dir, sprintf("motivated_models_table_weighted%s.tex", output_label)),
    label = "tab:motivated_models",
    longtable = FALSE
)

motivated_table_unweighted <- create_ols_summary_table(
  models = motivated_models_unweighted,
  title = "OLS Regression Results for Debunking Motivation Models (Unweighted)",
  column.labels = stringr::str_replace_all(motivated_y_vars_recontact_labels, "_", " "),
  covariate.labels = c(
    treatment_label,
    "Rumor: Voter Rolls", "Rumor: Hacking", "Rumor: Blue Shift", "Rumor: Voting Machines",
    covariate_labels),
  out.file = file.path(data_dir, sprintf("motivated_models_table_unweighted%s.tex", output_label)),
    label = "tab:motivated_models_unweighted",
    longtable = FALSE
)

# Create tables for HITL models
hitl_table_weighted <- create_ols_summary_table(
  models = hitl_models_weighted,
  title = "OLS Regression Results with Human In the Loop Status (Weighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  out.file = file.path(data_dir, sprintf("hitl_models_table_weighted%s.tex", output_label)),
    label = "tab:hitl_models",
    longtable = FALSE
)

hitl_table_unweighted <- create_ols_summary_table(
  models = hitl_models_unweighted,
  title = "OLS Regression Results with Human In the Loop Status (Unweighted)",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
  out.file = file.path(data_dir, sprintf("hitl_models_table_unweighted%s.tex", output_label)),
    label = "tab:hitl_models_unweighted",
    longtable = FALSE
)
cat("Motivated and HITL tables (weighted and unweighted) created.\n")

#### RANDOM ASSORTED STATISTICS IN PAPER ####
t.test(
  dat_final$Post_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  dat_final$Pre_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  na.rm=TRUE, paired=TRUE)

# calculate cohen's d for pre/post confidence in country ballots (placebo only)
cohen.d(
  dat_final$Post_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  dat_final$Pre_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dat_final$Post_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Treatment"],
  dat_final$Pre_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Treatment"],
  na.rm=TRUE, paired=TRUE
)

party <- "Republican"

t.test(
  dat_final$Post_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  dat_final$Pre_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE)

t.test(
  dat_final$Post_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  dat_final$Pre_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE)

cohen.d(
  dat_final$Post_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  dat_final$Pre_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dat_final$Post_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  dat_final$Pre_Confidence_Country_Ballots[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE
)

# CISA questions
t.test(
  dat_final$Rumor_Recontact[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  dat_final$Rumor[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  na.rm=TRUE, paired=TRUE)

# calculate cohen's d for pre/post confidence in relevant cisa questions (placebo only)
cohen.d(
  dat_final$Rumor_Recontact[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  dat_final$Rumor[dat_final$Election_Rumor_Placebo_Randomization=="Placebo"],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dat_final$Rumor_Recontact[dat_final$Election_Rumor_Placebo_Randomization=="Treatment"],
  dat_final$Rumor[dat_final$Election_Rumor_Placebo_Randomization=="Treatment"],
  na.rm=TRUE, paired=TRUE
)

party <- "Democrat"

t.test(
  dat_final$Rumor_Recontact[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  dat_final$Rumor[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE)

t.test(
  dat_final$Rumor_Recontact[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  dat_final$Rumor[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE)

cohen.d(
  dat_final$Rumor_Recontact[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  dat_final$Rumor[dat_final$Election_Rumor_Placebo_Randomization=="Placebo" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dat_final$Rumor_Recontact[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  dat_final$Rumor[dat_final$Election_Rumor_Placebo_Randomization=="Treatment" & dat_final$Party_Identification==party],
  na.rm=TRUE, paired=TRUE
)

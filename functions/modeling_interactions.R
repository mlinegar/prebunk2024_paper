# =============================================================================
# Interaction Models - Generate regressions for all interaction variables
# =============================================================================
# This file runs the same regression specifications but iterates over
# different interaction variables (Party ID, Ideology, Conspiracy Score, etc.)
# =============================================================================

cat("Generating interaction models for all specified variables...\n")

post_outcomes_for_interactions <- c(post_y_vars_labels, "Rumor_Post")
recontact_outcomes_for_interactions <- c(recontact_y_var_labels, "Rumor_Recontact")

map_outcome_to_pre <- function(outcome_label) {
  if (outcome_label %in% c("Rumor_Post", "Rumor_Recontact")) {
    return("Rumor")
  }
  if (startsWith(outcome_label, "Post_")) {
    return(sub("Post_", "Pre_", outcome_label))
  }
  if (startsWith(outcome_label, "Recontact_")) {
    return(sub("Recontact_", "Pre_", outcome_label))
  }
  stop(sprintf("Cannot map outcome %s to pretreatment variable", outcome_label))
}

# Loop through each interaction variable
for (int_var_name in int_vars) {
  cat(sprintf("\n--- Running models with %s interactions ---\n", int_var_name))

  # Create safe filename suffix
  suffix <- tolower(gsub("_", "", int_var_name))

  # Run both weighted and unweighted versions
  for (weight_type in c("weighted", "unweighted")) {

    svy_design <- if (weight_type == "weighted") svy_design_weighted else svy_design_unweighted
    output_suffix <- if (weight_type == "weighted") "_weighted" else "_unweighted"

    cat(sprintf("  Running %s models...\n", weight_type))

    # Pooled models with interaction
    int_pooled_models <- lapply(post_outcomes_for_interactions, function(outcome) {
      pre_var <- map_outcome_to_pre(outcome)
      predictors <- c(
        pre_var,
        "Election_Rumor_Placebo_Randomization",
        "Election_Rumor_Randomization",
        paste0("Election_Rumor_Placebo_Randomization * ", int_var_name),
        variables_in_model_labels
      )
      run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
    })
    names(int_pooled_models) <- paste0("Pooled_", post_outcomes_for_interactions)

    # Recontact models with interaction
    int_recontact_pooled_models <- lapply(recontact_outcomes_for_interactions, function(outcome) {
      pre_var <- map_outcome_to_pre(outcome)
      predictors <- c(
        pre_var,
        "Election_Rumor_Placebo_Randomization",
        "Election_Rumor_Randomization",
        paste0("Election_Rumor_Placebo_Randomization * ", int_var_name),
        variables_in_model_labels
      )
      run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
    })
    names(int_recontact_pooled_models) <- paste0("Recontact_", recontact_outcomes_for_interactions)

    # Save to appropriately named variables
    if (weight_type == "weighted") {
      assign(paste0("int_pooled_models_", suffix, "_weighted"), int_pooled_models, envir = .GlobalEnv)
      assign(paste0("int_recontact_pooled_models_", suffix, "_weighted"),
             int_recontact_pooled_models, envir = .GlobalEnv)
    } else {
      assign(paste0("int_pooled_models_", suffix, "_unweighted"), int_pooled_models, envir = .GlobalEnv)
      assign(paste0("int_recontact_pooled_models_", suffix, "_unweighted"),
             int_recontact_pooled_models, envir = .GlobalEnv)
    }

    # Create tables
    rumor_baseline <- if ("Rumor_Post" %in% post_outcomes_for_interactions) "Rumor" else character(0)
    int_key_covars <- c(
      rumor_baseline,
      pre_y_vars_labels %>% gsub("_", " ", .),
      treatment_label,
      "Rumor: Voter Rolls", "Rumor: Hacking", "Rumor: Blue Shift", "Rumor: Voting Machines"
    )
    int_covar_labels <- c(int_key_covars, covariate_labels)

    weight_title <- if (weight_type == "weighted") "(Weighted)" else "(Unweighted)"

    int_pooled_table <- create_ols_summary_table(
      models = int_pooled_models,
      title = sprintf("OLS Regression Results - %s Interactions %s", int_var_name, weight_title),
      column.labels = get_column_labels(post_outcomes_for_interactions),
      covariate.labels = int_covar_labels,
      out.file = resolve_writing_path(sprintf("int_pooled_models_%s%s.tex", suffix, output_suffix), "tables"),
      label = sprintf("tab:int_pooled_%s", suffix),
      longtable = FALSE
    )

    int_recontact_pooled_table <- create_ols_summary_table(
      models = int_recontact_pooled_models,
      title = sprintf("Recontact OLS Regression - %s Interactions %s", int_var_name, weight_title),
      column.labels = get_column_labels(recontact_outcomes_for_interactions),
      covariate.labels = int_covar_labels,
      out.file = resolve_writing_path(sprintf("int_recontact_pooled_%s%s.tex", suffix, output_suffix), "tables"),
      label = sprintf("tab:int_recontact_%s", suffix),
      longtable = FALSE
    )
  }

  cat(sprintf("✓ Completed weighted and unweighted models for %s\n", int_var_name))
}

cat("\n=============================================================================\n")
cat("All interaction models generated!\n")
cat("Tables created for:", paste(int_vars, collapse = ", "), "\n")
cat("=============================================================================\n")

# =============================================================================
# Sensitivity Analysis: Models with Different Attention Check Exclusions
# =============================================================================
# This file runs all main models on three samples:
# 1. All respondents (no exclusions) - dat_final
# 2. Exclude if failed both checks - dat_attentive_moderate
# 3. Exclude if failed either check - dat_attentive_strict
# =============================================================================

cat("\n=============================================================================\n")
cat("SENSITIVITY ANALYSIS: ATTENTION CHECK EXCLUSIONS\n")
cat("=============================================================================\n")

# Check if datasets exist (created in balance_and_checks.R)
if (!exists("dat_attentive_moderate") || !exists("dat_attentive_strict")) {
  stop("Attention check datasets not found. Please run balance_and_checks.R first.")
}

cat("\nSample definitions:\n")
cat(sprintf("Sample 1 (All respondents): N = %d\n", nrow(dat_final)))
cat(sprintf("Sample 2 (Exclude if failed both): N = %d (%.1f%%)\n",
            nrow(dat_attentive_moderate), 100 * nrow(dat_attentive_moderate) / nrow(dat_final)))
cat(sprintf("Sample 3 (Exclude if failed either): N = %d (%.1f%%)\n\n",
            nrow(dat_attentive_strict), 100 * nrow(dat_attentive_strict) / nrow(dat_final)))

cat("DEBUG: Checking dat_final column names:\n")
cat(sprintf("DEBUG: dat_final has columns (first 30): %s\n", paste(head(names(dat_final), 30), collapse=", ")))
cat(sprintf("DEBUG: Does dat_final have 'Post_All_Rumors'? %s\n", "Post_All_Rumors" %in% names(dat_final)))
cat(sprintf("DEBUG: Does dat_final have 'cisa_fake_2'? %s\n", "cisa_fake_2" %in% names(dat_final)))

# =============================================================================
# CREATE SURVEY DESIGNS FOR ALL THREE SAMPLES
# =============================================================================

# Sample 1: All respondents
cat(sprintf("DEBUG: About to create svy_design_all. dat_final columns (140-145): %s\n",
            paste(names(dat_final)[140:145], collapse=", ")))
svy_design_all_weighted <- svydesign(data = dat_final, weights = ~weight, id = ~1)
cat(sprintf("DEBUG: Created svy_design_all. Design columns (140-145): %s\n",
            paste(names(svy_design_all_weighted$variables)[140:145], collapse=", ")))
cat(sprintf("DEBUG: Does design have 'Post_All_Rumors'? %s\n",
            "Post_All_Rumors" %in% names(svy_design_all_weighted$variables)))

# Sample 2: Moderate exclusion (failed both)
svy_design_moderate_weighted <- svydesign(data = dat_attentive_moderate, weights = ~weight, id = ~1)

# Sample 3: Strict exclusion (failed either)
svy_design_strict_weighted <- svydesign(data = dat_attentive_strict, weights = ~weight, id = ~1)

cat("Created survey designs for all samples.\n\n")

# =============================================================================
# FUNCTION TO RUN MODELS ON A GIVEN SAMPLE
# =============================================================================

run_models_for_sample <- function(svy_design, sample_name) {
  cat(sprintf("Running models for %s...\n", sample_name))

  # Pooled models (weighted only)
  pooled <- lapply(post_y_vars_labels, function(outcome) {
    predictors <- c(paste0(sub("Post_", "Pre_", outcome)),
                    "Election_Rumor_Placebo_Randomization",
                    "Election_Rumor_Randomization",
                    variables_in_model_labels)
    run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
  })
  names(pooled) <- paste0("Pooled_", post_y_vars_labels)

  # Recontact models (if data available)
  recontact <- NULL
  if ("ballotcount_scale_recontact" %in% names(svy_design$variables)) {
    recontact <- lapply(recontact_y_var_labels, function(outcome) {
      predictors <- c(paste0(sub("Recontact_", "Pre_", outcome)),
                      "Election_Rumor_Placebo_Randomization",
                      "Election_Rumor_Randomization",
                      variables_in_model_labels)
      run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
    })
    names(recontact) <- paste0("Recontact_", recontact_y_var_labels)
  }

  # CISA models (All Rumors and All Facts)
  # NOTE: CISA questions were not asked post-treatment in wave 1
  # They were only asked at baseline and recontact
  # Skip CISA models for sensitivity analysis since all values are NA
  cat(sprintf("\nNOTE: Skipping CISA models for %s - no post-treatment CISA data in wave 1\n", sample_name))

  cisa <- list()

  return(list(pooled = pooled, recontact = recontact, cisa = cisa))
}

# =============================================================================
# RUN MODELS ON ALL THREE SAMPLES
# =============================================================================

cat("--- Running models on all samples ---\n\n")

# Sample 1: All respondents
models_all <- run_models_for_sample(svy_design_all_weighted, "Sample 1 (all respondents)")

# Sample 2: Moderate exclusion
models_moderate <- run_models_for_sample(svy_design_moderate_weighted, "Sample 2 (exclude if failed both)")

# Sample 3: Strict exclusion
models_strict <- run_models_for_sample(svy_design_strict_weighted, "Sample 3 (exclude if failed either)")

cat("\nCompleted all models.\n\n")

# =============================================================================
# EXTRACT TREATMENT EFFECTS FROM ALL SAMPLES
# =============================================================================

cat("--- Extracting treatment effects ---\n")

# Function to extract treatment effect coefficient
extract_treatment_coef <- function(model, var_name = "Election_Rumor_Placebo_RandomizationTreatment") {
  cat(sprintf("DEBUG extract_treatment_coef: Starting extraction for var_name='%s'\n", var_name))

  if (is.null(model)) {
    cat("DEBUG extract_treatment_coef: Model is NULL, returning NA\n")
    return(data.frame(coefficient = NA, se = NA, p_value = NA))
  }

  coef_summary <- summary(model)$coefficients
  cat(sprintf("DEBUG extract_treatment_coef: Got coefficient summary with %d rows\n", nrow(coef_summary)))
  cat(sprintf("DEBUG extract_treatment_coef: Rownames: %s\n", paste(head(rownames(coef_summary), 10), collapse=", ")))
  cat(sprintf("DEBUG extract_treatment_coef: Is '%s' in rownames? %s\n", var_name, var_name %in% rownames(coef_summary)))

  if (var_name %in% rownames(coef_summary)) {
    # Print the full row from coefficient summary
    cat(sprintf("DEBUG extract_treatment_coef: Full coefficient row:\n"))
    print(coef_summary[var_name, ])

    result <- data.frame(
      coefficient = coef_summary[var_name, "Estimate"],
      se = coef_summary[var_name, "Std. Error"],
      p_value = coef_summary[var_name, "Pr(>|t|)"]
    )
    cat(sprintf("DEBUG extract_treatment_coef: Found coefficient! coef=%.4f, se=%.4f, p=%.4f\n",
                result$coefficient, result$se, result$p_value))
    return(result)
  }

  cat(sprintf("DEBUG extract_treatment_coef: Variable '%s' NOT found in rownames, returning NA\n", var_name))
  return(data.frame(coefficient = NA, se = NA, p_value = NA))
}

# =============================================================================
# BUILD COMPARISON DATA FRAMES
# =============================================================================

# POOLED MODELS
comparison_pooled <- data.frame(
  outcome = c("Own Ballot", "County Ballots", "Country Ballots"),

  # Sample 1: All respondents
  all_coef = sapply(models_all$pooled[1:3], function(m) extract_treatment_coef(m)$coefficient),
  all_se = sapply(models_all$pooled[1:3], function(m) extract_treatment_coef(m)$se),
  all_p = sapply(models_all$pooled[1:3], function(m) extract_treatment_coef(m)$p_value),

  # Sample 2: Exclude if failed both
  moderate_coef = sapply(models_moderate$pooled[1:3], function(m) extract_treatment_coef(m)$coefficient),
  moderate_se = sapply(models_moderate$pooled[1:3], function(m) extract_treatment_coef(m)$se),
  moderate_p = sapply(models_moderate$pooled[1:3], function(m) extract_treatment_coef(m)$p_value),

  # Sample 3: Exclude if failed either
  strict_coef = sapply(models_strict$pooled[1:3], function(m) extract_treatment_coef(m)$coefficient),
  strict_se = sapply(models_strict$pooled[1:3], function(m) extract_treatment_coef(m)$se),
  strict_p = sapply(models_strict$pooled[1:3], function(m) extract_treatment_coef(m)$p_value)
)

cat("\nPooled Models - Treatment Effect Comparison:\n")
print(comparison_pooled, digits = 3)

# RECONTACT MODELS (if available)
if (!is.null(models_moderate$recontact) && !is.null(models_all$recontact)) {
  comparison_recontact <- data.frame(
    outcome = c("Own Ballot", "County Ballots", "Country Ballots"),

    all_coef = sapply(models_all$recontact[1:3], function(m) extract_treatment_coef(m)$coefficient),
    all_se = sapply(models_all$recontact[1:3], function(m) extract_treatment_coef(m)$se),
    all_p = sapply(models_all$recontact[1:3], function(m) extract_treatment_coef(m)$p_value),

    moderate_coef = sapply(models_moderate$recontact[1:3], function(m) extract_treatment_coef(m)$coefficient),
    moderate_se = sapply(models_moderate$recontact[1:3], function(m) extract_treatment_coef(m)$se),
    moderate_p = sapply(models_moderate$recontact[1:3], function(m) extract_treatment_coef(m)$p_value),

    strict_coef = sapply(models_strict$recontact[1:3], function(m) extract_treatment_coef(m)$coefficient),
    strict_se = sapply(models_strict$recontact[1:3], function(m) extract_treatment_coef(m)$se),
    strict_p = sapply(models_strict$recontact[1:3], function(m) extract_treatment_coef(m)$p_value)
  )

  cat("\nRecontact Models - Treatment Effect Comparison:\n")
  print(comparison_recontact, digits = 3)
}

# CISA MODELS - Skip since no post-treatment data exists
cat("\nNOTE: Skipping CISA Models comparison - no post-treatment CISA data in wave 1\n")
cat("CISA questions were only asked at baseline and recontact.\n")

# Save comparison CSVs
write.csv(comparison_pooled, file.path(data_dir, "sensitivity_pooled_comparison.csv"), row.names = FALSE)
if (exists("comparison_recontact")) {
  write.csv(comparison_recontact, file.path(data_dir, "sensitivity_recontact_comparison.csv"), row.names = FALSE)
}
# Skip CISA comparison - no data
# write.csv(comparison_cisa, file.path(data_dir, "sensitivity_cisa_comparison.csv"), row.names = FALSE)

# =============================================================================
# CREATE DENSE COMPARISON TABLES (LATEX)
# =============================================================================

cat("\n--- Creating dense comparison tables ---\n")

# Function to create dense comparison table
create_dense_comparison_table <- function(comparison_df, model_type, title, label) {

  # Format: Outcome | All (coef) | Moderate (coef) | Strict (coef)
  table_data <- comparison_df %>%
    mutate(
      Outcome = outcome,
      `All Respondents` = sprintf("%.3f%s (%.3f)",
                                  all_coef,
                                  ifelse(all_p < 0.001, "***",
                                        ifelse(all_p < 0.01, "**",
                                              ifelse(all_p < 0.05, "*", ""))),
                                  all_se),
      `Exclude Both Failed` = sprintf("%.3f%s (%.3f)",
                                      moderate_coef,
                                      ifelse(moderate_p < 0.001, "***",
                                            ifelse(moderate_p < 0.01, "**",
                                                  ifelse(moderate_p < 0.05, "*", ""))),
                                      moderate_se),
      `Exclude Either Failed` = sprintf("%.3f%s (%.3f)",
                                        strict_coef,
                                        ifelse(strict_p < 0.001, "***",
                                              ifelse(strict_p < 0.01, "**",
                                                    ifelse(strict_p < 0.05, "*", ""))),
                                        strict_se)
    ) %>%
    select(Outcome, `All Respondents`, `Exclude Both Failed`, `Exclude Either Failed`)

  # Create LaTeX table
  filename <- file.path(data_dir, sprintf("sensitivity_%s_table.tex", tolower(gsub(" ", "_", model_type))))

  stargazer(table_data,
            summary = FALSE,
            title = title,
            label = label,
            rownames = FALSE,
            notes = c("Treatment effect coefficients with standard errors in parentheses.",
                     sprintf("All: N=%d. Exclude both: N=%d. Exclude either: N=%d.",
                            nrow(dat_final), nrow(dat_attentive_moderate), nrow(dat_attentive_strict)),
                     "*** p$<$0.001, ** p$<$0.01, * p$<$0.05"),
            notes.align = "l",
            out = filename)

  # Post-process to fix stargazer's asterisk conversion
  cat(sprintf("DEBUG: Post-processing %s to fix asterisks\n", filename))
  tex_content <- readLines(filename)
  asterisk_count_before <- sum(grepl("\\\\textasteriskcentered", tex_content))
  cat(sprintf("DEBUG: Found %d instances of \\textasteriskcentered\n", asterisk_count_before))

  tex_content <- gsub("\\\\textasteriskcentered", "$^{*}$", tex_content)

  asterisk_count_after <- sum(grepl("\\\\textasteriskcentered", tex_content))
  cat(sprintf("DEBUG: After replacement: %d instances remain\n", asterisk_count_after))

  writeLines(tex_content, filename)

  cat(sprintf("Created %s\n", filename))

  return(table_data)
}

# Create dense tables for each model type
pooled_table <- create_dense_comparison_table(
  comparison_pooled,
  "pooled",
  "Sensitivity Analysis: Pooled Models by Attention Check Exclusion",
  "tab:sensitivity_pooled"
)

if (exists("comparison_recontact")) {
  recontact_table <- create_dense_comparison_table(
    comparison_recontact,
    "recontact",
    "Sensitivity Analysis: Recontact Models by Attention Check Exclusion",
    "tab:sensitivity_recontact"
  )
}

# Skip CISA table - no post-treatment data
# cisa_table <- create_dense_comparison_table(
#   comparison_cisa,
#   "cisa",
#   "Sensitivity Analysis: CISA Rumor Models by Attention Check Exclusion",
#   "tab:sensitivity_cisa"
# )

# =============================================================================
# STATISTICAL TESTS FOR DIFFERENCES
# =============================================================================

cat("\n--- Testing for significant differences ---\n")

# Combine all comparisons (skip CISA - no data)
all_comparisons <- rbind(
  cbind(Model = "Pooled", comparison_pooled),
  if (exists("comparison_recontact")) cbind(Model = "Recontact", comparison_recontact) else NULL
)

# Test if moderate vs all is different
test_moderate_vs_all <- all_comparisons %>%
  mutate(
    coef_diff = moderate_coef - all_coef,
    se_pooled = sqrt(all_se^2 + moderate_se^2),
    z_stat = coef_diff / se_pooled,
    p_value = 2 * (1 - pnorm(abs(z_stat))),
    significant = p_value < 0.05
  ) %>%
  select(Model, outcome, coef_diff, se_pooled, p_value, significant)

cat("\nModerate exclusion (both failed) vs. All respondents:\n")
print(test_moderate_vs_all, digits = 3)

# Test if strict vs all is different
test_strict_vs_all <- all_comparisons %>%
  mutate(
    coef_diff = strict_coef - all_coef,
    se_pooled = sqrt(all_se^2 + strict_se^2),
    z_stat = coef_diff / se_pooled,
    p_value = 2 * (1 - pnorm(abs(z_stat))),
    significant = p_value < 0.05
  ) %>%
  select(Model, outcome, coef_diff, se_pooled, p_value, significant)

cat("\nStrict exclusion (either failed) vs. All respondents:\n")
print(test_strict_vs_all, digits = 3)

# Save test results
write.csv(test_moderate_vs_all, file.path(data_dir, "sensitivity_test_moderate_vs_all.csv"), row.names = FALSE)
write.csv(test_strict_vs_all, file.path(data_dir, "sensitivity_test_strict_vs_all.csv"), row.names = FALSE)

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat(sprintf("\n=============================================================================\n"))
cat(sprintf("SENSITIVITY ANALYSIS SUMMARY\n"))
cat(sprintf("=============================================================================\n"))

n_tests <- nrow(all_comparisons)
n_sig_moderate <- sum(test_moderate_vs_all$significant, na.rm = TRUE)
n_sig_strict <- sum(test_strict_vs_all$significant, na.rm = TRUE)

cat(sprintf("Total models compared: %d\n", n_tests))
cat(sprintf("\nModerate exclusion (failed both) vs All:\n"))
cat(sprintf("  Significantly different: %d/%d (%.1f%%)\n",
            n_sig_moderate, n_tests, 100 * n_sig_moderate / n_tests))

cat(sprintf("\nStrict exclusion (failed either) vs All:\n"))
cat(sprintf("  Significantly different: %d/%d (%.1f%%)\n",
            n_sig_strict, n_tests, 100 * n_sig_strict / n_tests))

if (n_sig_moderate == 0 && n_sig_strict == 0) {
  cat("\nConclusion: Results are ROBUST across all exclusion criteria.\n")
  cat("Treatment effects remain substantively and statistically similar\n")
  cat("regardless of attention check exclusion decisions.\n")
} else {
  cat(sprintf("\nNote: Some differences detected. Review specific models:\n"))
  if (n_sig_moderate > 0) {
    cat("\nModerate exclusion differences:\n")
    print(test_moderate_vs_all %>% filter(significant) %>% select(Model, outcome, coef_diff, p_value))
  }
  if (n_sig_strict > 0) {
    cat("\nStrict exclusion differences:\n")
    print(test_strict_vs_all %>% filter(significant) %>% select(Model, outcome, coef_diff, p_value))
  }
}

cat("\n=============================================================================\n")
cat("Files created:\n")
cat("- sensitivity_pooled_table.tex (dense 3-column comparison)\n")
if (exists("comparison_recontact")) cat("- sensitivity_recontact_table.tex (dense 3-column comparison)\n")
cat("NOTE: sensitivity_cisa_table.tex NOT created - no post-treatment CISA data in wave 1\n")
cat("- sensitivity_pooled_comparison.csv\n")
if (exists("comparison_recontact")) cat("- sensitivity_recontact_comparison.csv\n")
cat("- sensitivity_test_moderate_vs_all.csv\n")
cat("- sensitivity_test_strict_vs_all.csv\n")
cat("\n")

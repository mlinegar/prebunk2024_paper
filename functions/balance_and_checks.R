# =============================================================================
# Balance Tests, Randomization Inference, and Attention Checks
# =============================================================================
# This file implements:
# 1. Covariate balance tests (joint Wald test with permutation inference)
# 2. Differential attrition tests (Studentized permutation tests)
# 3. Attention check analysis and exclusion criteria
# =============================================================================

cat("\n=============================================================================\n")
cat("BALANCE TESTS AND RANDOMIZATION CHECKS\n")
cat("=============================================================================\n")

# Load required packages for robust SEs
if (!require("sandwich")) install.packages("sandwich")
library(sandwich)

# Set output directory for generated tables and lightweight diagnostics.
output_dir <- if (exists("get_writing_path", mode = "function")) {
  get_writing_path("tables")
} else {
  file.path(data_dir, "writing_draft", "tables")
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Store computationally expensive permutation results separately so quick
# replication runs can reuse them.
permutation_cache_dir <- file.path(data_dir, "data", "cache")
if (!dir.exists(permutation_cache_dir)) {
  dir.create(permutation_cache_dir, recursive = TRUE)
}

# =============================================================================
# 1. ATTENTION CHECKS AND EXCLUSION CRITERIA
# =============================================================================

cat("\n--- Attention Checks and Exclusion Criteria ---\n")

# Attention check variables (from data_processing.R):
# attention_1_correct = attention_1 == 3
# attention_2_correct = attention_2 == 2

# Calculate attention check performance
attention_summary <- dat_final %>%
  summarise(
    N_total = n(),
    attention_1_pass = sum(attention_1_correct, na.rm = TRUE),
    attention_1_fail = sum(!attention_1_correct, na.rm = TRUE),
    attention_1_pct_pass = mean(attention_1_correct, na.rm = TRUE) * 100,
    attention_2_pass = sum(attention_2_correct, na.rm = TRUE),
    attention_2_fail = sum(!attention_2_correct, na.rm = TRUE),
    attention_2_pct_pass = mean(attention_2_correct, na.rm = TRUE) * 100,
    both_pass = sum(attention_1_correct & attention_2_correct, na.rm = TRUE),
    either_fail = sum(!attention_1_correct | !attention_2_correct, na.rm = TRUE),
    both_fail = sum(!attention_1_correct & !attention_2_correct, na.rm = TRUE)
  )

cat("\nAttention Check Summary:\n")
print(attention_summary)

# By treatment condition
attention_by_treatment <- dat_final %>%
  group_by(Election_Rumor_Placebo_Randomization) %>%
  summarise(
    N = n(),
    attention_1_pct_pass = mean(attention_1_correct, na.rm = TRUE) * 100,
    attention_2_pct_pass = mean(attention_2_correct, na.rm = TRUE) * 100,
    both_pass_pct = mean(attention_1_correct & attention_2_correct, na.rm = TRUE) * 100
  )

cat("\nAttention Check by Treatment Condition:\n")
print(attention_by_treatment)

# Test for differential attention check failure
attention_chi_sq_1 <- chisq.test(table(dat_final$Election_Rumor_Placebo_Randomization,
                                        dat_final$attention_1_correct))
attention_chi_sq_2 <- chisq.test(table(dat_final$Election_Rumor_Placebo_Randomization,
                                        dat_final$attention_2_correct))

cat("\nTest for differential attention check performance by treatment:\n")
cat(sprintf("Attention Check 1: Chi-square = %.3f, p = %.4f\n",
            attention_chi_sq_1$statistic, attention_chi_sq_1$p.value))
cat(sprintf("Attention Check 2: Chi-square = %.3f, p = %.4f\n",
            attention_chi_sq_2$statistic, attention_chi_sq_2$p.value))

# EXCLUSION CRITERIA (from preregistration)
cat("\n--- Exclusion Criteria ---\n")
cat("Per preregistration, exclusion criteria were:\n")
cat("1. Failed both attention checks\n")
cat("2. Incomplete surveys\n")
cat("3. Speeding (< 50% median completion time)\n\n")

# Check how many would be excluded under each criterion
exclusion_analysis <- dat_final %>%
  summarise(
    N_total = n(),
    fail_both_attention = sum(!attention_1_correct & !attention_2_correct, na.rm = TRUE),
    excluded_any = fail_both_attention
  )

cat("Exclusion Analysis:\n")
cat(sprintf("Total N: %d\n", exclusion_analysis$N_total))
cat(sprintf("Failed both attention checks: %d (%.2f%%)\n",
            exclusion_analysis$fail_both_attention,
            100 * exclusion_analysis$fail_both_attention / exclusion_analysis$N_total))
cat(sprintf("Currently excluded: 0 (no exclusions applied)\n\n"))

# Create datasets for sensitivity analysis with different exclusion criteria
# Sample 1: All respondents (no exclusions) - dat_final
# Sample 2: Exclude respondents who failed both checks - dat_attentive_moderate
# Sample 3: Exclude respondents who failed either check - dat_attentive_strict
# Sample 4: Exclude respondents who failed attention check 1 - dat_attentive_check1
# Sample 5: Exclude respondents who failed attention check 2 - dat_attentive_check2

dat_attentive_moderate <- dat_final %>%
  filter(!((!attention_1_correct) & (!attention_2_correct)))  # Exclude only those who failed BOTH

dat_attentive_strict <- dat_final %>%
  filter(attention_1_correct & attention_2_correct)  # Exclude if failed either

dat_attentive_check1 <- dat_final %>%
  filter(attention_1_correct)  # Exclude if failed check 1

dat_attentive_check2 <- dat_final %>%
  filter(attention_2_correct)  # Exclude if failed check 2

cat("\nSample definitions for sensitivity analysis:\n")
cat(sprintf("Sample 1 (All respondents): N = %d\n", nrow(dat_final)))
cat(sprintf("Sample 2 (Exclude if failed both checks): N = %d (%.1f%% of original)\n",
            nrow(dat_attentive_moderate), 100 * nrow(dat_attentive_moderate) / nrow(dat_final)))
cat(sprintf("Sample 3 (Exclude if failed either check): N = %d (%.1f%% of original)\n",
            nrow(dat_attentive_strict), 100 * nrow(dat_attentive_strict) / nrow(dat_final)))
cat(sprintf("Sample 4 (Exclude if failed check 1): N = %d (%.1f%% of original)\n",
            nrow(dat_attentive_check1), 100 * nrow(dat_attentive_check1) / nrow(dat_final)))
cat(sprintf("Sample 5 (Exclude if failed check 2): N = %d (%.1f%% of original)\n\n",
            nrow(dat_attentive_check2), 100 * nrow(dat_attentive_check2) / nrow(dat_final)))


# Save attention check summary to file
write.csv(attention_summary, file.path(output_dir, "attention_check_summary.csv"), row.names = FALSE)
write.csv(attention_by_treatment, file.path(output_dir, "attention_check_by_treatment.csv"), row.names = FALSE)

# Create overall attention check summary table
overall_summary_data <- data.frame(
  Sample = c("All respondents",
             "Passed attention check 1",
             "Passed attention check 2",
             "Passed both checks",
             "Failed at least one check",
             "Failed both checks"),
  N = c(nrow(dat_final),
        attention_summary$attention_1_pass,
        attention_summary$attention_2_pass,
        attention_summary$both_pass,
        attention_summary$either_fail,
        attention_summary$both_fail),
  Percent = sprintf("%.1f", c(100,
              attention_summary$attention_1_pct_pass,
              attention_summary$attention_2_pct_pass,
              attention_summary$both_pass / nrow(dat_final) * 100,
              attention_summary$either_fail / nrow(dat_final) * 100,
              attention_summary$both_fail / nrow(dat_final) * 100))
)

stargazer(overall_summary_data,
          summary = FALSE,
          title = "Overall Attention Check Performance",
          label = "tab:attention_checks_overall",
          rownames = FALSE,
          notes = c("Attention check 1 was administered pre-treatment (politics interest question).",
                    "Attention check 2 was administered post-treatment (voter registration question)."),
          notes.align = "l",
          type = "latex",
          out = file.path(output_dir, "attention_check_overall.tex"))

# Create LaTeX table for attention checks by treatment
attention_table_data <- attention_by_treatment %>%
  mutate(
    Treatment = Election_Rumor_Placebo_Randomization,
    `N` = N,
    `Attention 1 Pass (%)` = sprintf("%.1f%%", attention_1_pct_pass),
    `Attention 2 Pass (%)` = sprintf("%.1f%%", attention_2_pct_pass),
    `Both Pass (%)` = sprintf("%.1f%%", both_pass_pct)
  ) %>%
  select(Treatment, N, `Attention 1 Pass (%)`, `Attention 2 Pass (%)`, `Both Pass (%)`)

chi_sq_note <- c("Pass rates for two attention checks embedded in the survey.",
                sprintf("Chi-square tests: Check 1: $\\chi^2$ = %.2f, p = %.3f; Check 2: $\\chi^2$ = %.2f, p = %.3f.",
                       attention_chi_sq_1$statistic, attention_chi_sq_1$p.value,
                       attention_chi_sq_2$statistic, attention_chi_sq_2$p.value))

stargazer(attention_table_data,
          summary = FALSE,
          title = "Attention Check Performance by Treatment Condition",
          label = "tab:attention_checks",
          rownames = FALSE,
          notes = chi_sq_note,
          notes.align = "l",
          type = "latex",
          out = file.path(output_dir, "attention_check_summary.tex"))

cat("Attention check table saved to attention_check_summary.tex\n")

# =============================================================================
# 2. COVARIATE BALANCE TESTS
# =============================================================================

cat("\n--- Covariate Balance Tests ---\n")

# Define covariates to test for balance
# These should match your preregistration
balance_vars_all <- c(
  "Age_Group", "Gender", "Race_Ethnicity", "Education_Level",
  "Party_Identification", "Ideology", "Urban_Rural",
  "Political_Interest", "Conspiracy_Score", "Populism_Score"
)

# Keep only variables that exist in the dataset
balance_vars <- balance_vars_all[balance_vars_all %in% names(dat_final)]

if (length(balance_vars) < length(balance_vars_all)) {
  missing_vars <- setdiff(balance_vars_all, balance_vars)
  cat(sprintf("\nNote: The following variables are not in the dataset and will be excluded: %s\n",
              paste(missing_vars, collapse = ", ")))
}

# Separate continuous and categorical variables
continuous_vars <- balance_vars[sapply(dat_final[balance_vars], is.numeric)]
categorical_vars <- balance_vars[sapply(dat_final[balance_vars], function(x) is.factor(x) | is.character(x))]

cat(sprintf("\nContinuous variables: %s\n", paste(continuous_vars, collapse = ", ")))
cat(sprintf("Categorical variables: %s\n\n", paste(categorical_vars, collapse = ", ")))

# =============================================================================
# INDIVIDUAL BALANCE TESTS (for descriptive purposes)
# =============================================================================

cat("Running individual balance tests (for descriptive table)...\n")

test_balance_individual <- function(var, data) {
  if (is.numeric(data[[var]])) {
    # For continuous variables: t-test and means
    test_result <- t.test(reformulate("Election_Rumor_Placebo_Randomization", var), data = data)
    return(data.frame(
      variable = var,
      level = NA,
      type = "continuous",
      test = "t-test",
      statistic = test_result$statistic,
      p_value = test_result$p.value,
      treatment_value = sprintf("%.2f", mean(data[[var]][data$Election_Rumor_Placebo_Randomization == "Treatment"], na.rm = TRUE)),
      control_value = sprintf("%.2f", mean(data[[var]][data$Election_Rumor_Placebo_Randomization == "Placebo"], na.rm = TRUE)),
      stringsAsFactors = FALSE
    ))
  } else {
    # For categorical variables: chi-square test and proportions
    test_result <- chisq.test(table(data$Election_Rumor_Placebo_Randomization, data[[var]]))

    # Calculate proportions for each level
    prop_table <- prop.table(table(data$Election_Rumor_Placebo_Randomization, data[[var]]), margin = 1)

    treatment_props <- prop_table["Treatment", ]
    control_props <- prop_table["Placebo", ]

    # Create one row per level
    level_names <- names(treatment_props)
    results <- lapply(seq_along(level_names), function(i) {
      level <- level_names[i]
      data.frame(
        variable = if (i == 1) var else "",  # Only show variable name on first row
        level = level,
        type = if (i == 1) "categorical" else "",
        test = if (i == 1) "chi-square" else "",
        statistic = test_result$statistic,  # Show on all rows - applies to whole variable
        p_value = test_result$p.value,      # Show on all rows - applies to whole variable
        treatment_value = sprintf("%.1f%%", treatment_props[level] * 100),
        control_value = sprintf("%.1f%%", control_props[level] * 100),
        stringsAsFactors = FALSE
      )
    })

    return(do.call(rbind, results))
  }
}

balance_results_individual <- lapply(balance_vars, function(v) {
  tryCatch(test_balance_individual(v, dat_final),
           error = function(e) {
             cat(sprintf("  Error processing variable '%s': %s\n", v, e$message))
             cat(sprintf("    Variable class: %s\n", class(dat_final[[v]])))
             cat(sprintf("    Variable levels (if factor): %s\n",
                         paste(levels(dat_final[[v]]), collapse = ", ")))
             return(NULL)  # Return NULL instead of error row
           })
})

# Remove NULL entries (failed variables) and combine
balance_results_individual <- balance_results_individual[!sapply(balance_results_individual, is.null)]
balance_table_individual <- do.call(rbind, balance_results_individual)

cat("\nIndividual Balance Test Results:\n")
print(balance_table_individual, row.names = FALSE)

# Save individual balance table
write.csv(balance_table_individual, file.path(output_dir, "balance_tests_individual.csv"), row.names = FALSE)

# =============================================================================
# JOINT WALD TEST (PRIMARY BALANCE TEST)
# =============================================================================

cat("\n--- Joint Wald Test for Covariate Balance ---\n")

# Create treatment indicator (1 = Treatment, 0 = Placebo)
dat_final$Z <- as.numeric(dat_final$Election_Rumor_Placebo_Randomization == "Treatment")

# Build formula using original variables (R will handle dummy encoding automatically)
# Z ~ continuous_vars + categorical_vars
balance_formula <- as.formula(paste("Z ~", paste(balance_vars, collapse = " + ")))

cat(sprintf("\nRegression formula: %s\n", deparse(balance_formula)))
cat(sprintf("Total covariates: %d (%d continuous + %d categorical)\n",
            length(balance_vars),
            length(continuous_vars),
            length(categorical_vars)))

# Fit the regression (lm will automatically create dummies for categorical variables)
balance_fit <- lm(balance_formula, data = dat_final, singular.ok = FALSE)

# Calculate heteroskedasticity-robust Wald statistic
# Null hypothesis: all slope coefficients = 0 (i.e., R * beta = 0)
# where beta is the vector of coefficients including intercept
# and R is a matrix selecting only the slope coefficients

Rbeta_hat <- coef(balance_fit)[-1]  # Exclude intercept
RVR <- vcovHC(balance_fit, type = "HC0")[-1, -1]  # Robust variance-covariance matrix (exclude intercept)

# Wald statistic (Wooldridge 2010, equation 4.13)
W_obs <- as.numeric(t(Rbeta_hat) %*% solve(RVR, Rbeta_hat))

cat(sprintf("\nObserved Wald statistic: %.3f\n", W_obs))

# =============================================================================
# PERMUTATION TEST FOR WALD STATISTIC
# =============================================================================

cat("\nRunning permutation test for Wald statistic...\n")

set.seed(12345)  # For reproducibility
n_sims <- 10000

W_sims <- numeric(n_sims)

for (i in 1:n_sims) {
  # Randomly reassign treatment
  Z_sim <- sample(dat_final$Z)

  # Fit regression with permuted treatment
  dat_sim <- dat_final
  dat_sim$Z <- Z_sim
  fit_sim <- lm(balance_formula, data = dat_sim, singular.ok = FALSE)

  # Calculate Wald statistic for permuted data
  Rbeta_hat_sim <- coef(fit_sim)[-1]
  RVR_sim <- vcovHC(fit_sim, type = "HC0")[-1, -1]
  W_sims[i] <- as.numeric(t(Rbeta_hat_sim) %*% solve(RVR_sim, Rbeta_hat_sim))

  if (i %% 1000 == 0) cat(sprintf("  Completed %d/%d permutations\n", i, n_sims))
}

# Calculate p-value
p_wald <- mean(W_sims >= W_obs)

cat(sprintf("\nPermutation test results:\n"))
cat(sprintf("  Observed Wald statistic: %.3f\n", W_obs))
cat(sprintf("  Permutation p-value: %.4f\n", p_wald))

# Save Wald test results
wald_results <- data.frame(
  test = "Joint Wald Test (All Covariates)",
  observed_statistic = W_obs,
  permutation_p_value = p_wald,
  n_permutations = n_sims
)

write.csv(wald_results, file.path(permutation_cache_dir, "balance_wald_test.csv"), row.names = FALSE)

# =============================================================================
# 3. DIFFERENTIAL ATTRITION TESTS
# =============================================================================

cat("\n--- Differential Attrition Tests ---\n")

# Check if we have recontact data
if ("weight_recontact" %in% names(dat_final)) {

  # Create attrition indicator (1 = attrited, 0 = stayed)
  dat_final$attrited <- as.numeric(is.na(dat_final$weight_recontact))

  cat(sprintf("\nTotal attrition: %d / %d (%.1f%%)\n",
              sum(dat_final$attrited), nrow(dat_final),
              100 * mean(dat_final$attrited)))

  # Attrition by treatment
  attrition_by_treatment <- dat_final %>%
    group_by(Election_Rumor_Placebo_Randomization) %>%
    summarise(
      N = n(),
      N_attrited = sum(attrited),
      attrition_rate = mean(attrited)
    )

  cat("\nAttrition by treatment:\n")
  print(attrition_by_treatment)

  # =============================================================================
  # TEST 2: Comparison of attrition rates (Studentized permutation test)
  # =============================================================================

  cat("\n--- Test 2: Differential Attrition Rate (Studentized Permutation Test) ---\n")

  # Observed t-statistic
  attrition_ttest_obs <- t.test(attrited ~ Election_Rumor_Placebo_Randomization,
                                 data = dat_final,
                                 var.equal = FALSE)
  t_obs_attrition <- attrition_ttest_obs$statistic

  cat(sprintf("Observed t-statistic: %.3f\n", t_obs_attrition))

  # Studentized permutation test
  set.seed(12345)
  n_sims_attrition <- 10000
  t_sims_attrition <- numeric(n_sims_attrition)

  for (i in 1:n_sims_attrition) {
    dat_sim <- dat_final
    dat_sim$Election_Rumor_Placebo_Randomization <- sample(dat_final$Election_Rumor_Placebo_Randomization)

    ttest_sim <- t.test(attrited ~ Election_Rumor_Placebo_Randomization,
                        data = dat_sim,
                        var.equal = FALSE)
    t_sims_attrition[i] <- ttest_sim$statistic

    if (i %% 1000 == 0) cat(sprintf("  Completed %d/%d permutations\n", i, n_sims_attrition))
  }

  # Two-tailed p-value
  p_attrition_rate <- mean(abs(t_sims_attrition) >= abs(t_obs_attrition))

  cat(sprintf("\nStudentized permutation test results:\n"))
  cat(sprintf("  Observed t-statistic: %.3f\n", t_obs_attrition))
  cat(sprintf("  Permutation p-value: %.4f\n\n", p_attrition_rate))

  # =============================================================================
  # TEST 3: Comparison of attrition patterns (Studentized permutation test)
  # =============================================================================

  cat("\n--- Test 3: Differential Attrition Pattern (Studentized Permutation Test) ---\n")
  cat("Testing if treatment-covariate interactions predict attrition\n\n")

  # Build formula with treatment, covariates, and interactions
  # attrited ~ treatment + cov1 + cov2 + ... + treatment*cov1 + treatment*cov2 + ...

  # For this, we need to use only continuous covariates or create dummies for categorical
  # Using continuous covariates for simplicity
  if (length(continuous_vars) > 0) {

    # Create interaction terms
    interaction_formula <- paste("attrited ~ Election_Rumor_Placebo_Randomization *",
                                 paste0("(", paste(continuous_vars, collapse = " + "), ")"))

    cat(sprintf("Regression formula: %s\n\n", interaction_formula))

    # Fit the model
    attrition_fit_obs <- lm(as.formula(interaction_formula), data = dat_final)

    # Get indices of interaction coefficients
    # These are all coefficients that contain ":" (interaction symbol)
    coef_names <- names(coef(attrition_fit_obs))
    interaction_indices <- grep(":", coef_names)

    cat(sprintf("Number of interaction terms: %d\n", length(interaction_indices)))

    if (length(interaction_indices) > 0) {
      # F-test for interaction coefficients using robust SE
      # We'll construct the hypothesis matrix R
      k <- length(coef(attrition_fit_obs))
      q <- length(interaction_indices)

      R <- matrix(0, nrow = q, ncol = k)
      for (i in 1:q) {
        R[i, interaction_indices[i]] <- 1
      }

      # Calculate F-statistic with robust SE
      beta_hat <- coef(attrition_fit_obs)
      V_robust <- vcovHC(attrition_fit_obs, type = "HC0")

      Rbeta <- R %*% beta_hat
      RVRR <- R %*% V_robust %*% t(R)

      F_obs_pattern <- as.numeric(t(Rbeta) %*% solve(RVRR, Rbeta) / q)

      cat(sprintf("Observed F-statistic: %.3f\n", F_obs_pattern))

      # Studentized permutation test
      F_sims_pattern <- numeric(n_sims_attrition)

      cat("\nRunning permutation test...\n")
      for (i in 1:n_sims_attrition) {
        dat_sim <- dat_final
        dat_sim$Election_Rumor_Placebo_Randomization <- sample(dat_final$Election_Rumor_Placebo_Randomization)

        fit_sim <- lm(as.formula(interaction_formula), data = dat_sim)
        beta_hat_sim <- coef(fit_sim)
        V_robust_sim <- vcovHC(fit_sim, type = "HC0")

        Rbeta_sim <- R %*% beta_hat_sim
        RVRR_sim <- R %*% V_robust_sim %*% t(R)

        F_sims_pattern[i] <- as.numeric(t(Rbeta_sim) %*% solve(RVRR_sim, Rbeta_sim) / q)

        if (i %% 1000 == 0) cat(sprintf("  Completed %d/%d permutations\n", i, n_sims_attrition))
      }

      # Calculate p-value
      p_attrition_pattern <- mean(F_sims_pattern >= F_obs_pattern)

      cat(sprintf("\nStudentized permutation test results:\n"))
      cat(sprintf("  Observed F-statistic: %.3f\n", F_obs_pattern))
      cat(sprintf("  Permutation p-value: %.4f\n", p_attrition_pattern))


    } else {
      cat("No interaction terms to test.\n")
      F_obs_pattern <- NA
      p_attrition_pattern <- NA
    }

  } else {
    cat("No continuous covariates available for interaction test.\n")
    F_obs_pattern <- NA
    p_attrition_pattern <- NA
  }

  # Save attrition test results
  attrition_results <- data.frame(
    test = c("Differential Attrition Rate", "Differential Attrition Pattern"),
    observed_statistic = c(t_obs_attrition,
                          ifelse(exists("F_obs_pattern"), F_obs_pattern, NA)),
    permutation_p_value = c(p_attrition_rate,
                           ifelse(exists("p_attrition_pattern"), p_attrition_pattern, NA)),
    n_permutations = n_sims_attrition
  )

  write.csv(attrition_results, file.path(permutation_cache_dir, "attrition_tests.csv"), row.names = FALSE)

} else {
  cat("\nNo recontact data found - skipping attrition tests.\n")
  attrition_results <- data.frame(
    test = c("Differential Attrition Rate", "Differential Attrition Pattern"),
    observed_statistic = c(NA, NA),
    permutation_p_value = c(NA, NA),
    n_permutations = NA
  )
}

# =============================================================================
# CREATE LATEX TABLES
# =============================================================================

cat("\n--- Creating LaTeX tables ---\n")

# Individual balance tests table
balance_table_latex <- balance_table_individual %>%
  mutate(
    `p-value` = ifelse(is.na(p_value), "",
                               ifelse(p_value < 0.001, "$<$0.001", sprintf("%.3f", p_value))),
    Statistic = ifelse(is.na(statistic), "", sprintf("%.2f", statistic))
  ) %>%
  select(variable, level, test, Statistic, `p-value`,
         treatment_value, control_value)

stargazer(balance_table_latex,
          summary = FALSE,
          title = "Individual Covariate Balance Tests",
          label = "tab:balance_individual",
          rownames = FALSE,
          notes = c("Test statistics and p-values for each covariate.",
                   "Continuous variables: t-test. Categorical variables: chi-square test.",
                   "Values show means (continuous) or proportions (categorical)."),
          notes.align = "l",
          out = file.path(output_dir, "balance_tests_table.tex"))

# Joint Wald test table
wald_table <- wald_results %>%
  mutate(
    statistic_formatted = sprintf("%.2f", observed_statistic),
    p_value_formatted = ifelse(permutation_p_value < 0.001, "< 0.001",
                               sprintf("%.4f", permutation_p_value))
  ) %>%
  select(test, statistic_formatted, p_value_formatted, n_permutations)

stargazer(wald_table,
          summary = FALSE,
          title = "Joint Wald Test for Covariate Balance",
          label = "tab:balance_wald",
          rownames = FALSE,
          notes = "Columns show the test statistic, p-value, and number of permutations. Test regresses treatment indicator on all baseline covariates jointly, computing heteroskedasticity-robust Wald statistic. P-value obtained via permutation test with 10,000 simulations under the null hypothesis of no relationship between treatment assignment and covariates.",
          notes.align = "l",
          out = file.path(output_dir, "balance_wald_table.tex"))

# Attrition tests table (if applicable)
if (exists("attrition_results") && !all(is.na(attrition_results$observed_statistic))) {
  attrition_table <- attrition_results %>%
    mutate(
      Statistic = ifelse(is.na(observed_statistic), "-", sprintf("%.2f", observed_statistic)),
      `p-value` = ifelse(is.na(permutation_p_value), "-",
                                 ifelse(permutation_p_value < 0.001, "$<$0.001",
                                       sprintf("%.4f", permutation_p_value)))
    ) %>%
    select(test, Statistic, `p-value`)

  stargazer(attrition_table,
            summary = FALSE,
            title = "Differential Attrition Tests",
            label = "tab:attrition",
            rownames = FALSE,
            notes = c("Studentized permutation tests (10,000 simulations) for differential attrition.",
                     "Rate test: Compares attrition rates across treatment arms (t-test).",
                     "Pattern test: Tests if treatment-covariate interactions predict attrition (F-test)."),
            notes.align = "l",
            out = file.path(output_dir, "attrition_tests_table.tex"))
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("Balance tests and randomization checks complete!\n")
cat("=============================================================================\n")
cat("\nFiles created:\n")
cat("- attention_check_summary.csv\n")
cat("- attention_check_summary.tex\n")
cat("- attention_check_by_treatment.csv\n")
cat("- balance_tests_individual.csv\n")
cat("- balance_tests_table.tex\n")
cat("- balance_wald_test.csv (joint Wald test - primary balance test)\n")
cat("- balance_wald_table.tex\n")
if (exists("attrition_results") && !all(is.na(attrition_results$observed_statistic))) {
  cat("- attrition_tests.csv\n")
  cat("- attrition_tests_table.tex\n")
}
cat("\nDatasets created for sensitivity analysis:\n")
cat(sprintf("- dat_final (N = %d, all respondents)\n", nrow(dat_final)))
cat(sprintf("- dat_attentive_moderate (N = %d, exclude if failed both checks)\n", nrow(dat_attentive_moderate)))
cat(sprintf("- dat_attentive_strict (N = %d, exclude if failed either check)\n", nrow(dat_attentive_strict)))
cat("\n")

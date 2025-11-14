# =============================================================================
# Balance Tests - Permutation Tests Only
# =============================================================================
# This file runs only the computationally expensive permutation tests.
# Results are saved to CSV files that are loaded by balance_and_checks_quick.R
#
# Run this file when:
# - First time setup
# - Data has changed
# - You want to update permutation test results
#
# Note: This file expects dat_final to be loaded and balance_vars to be defined.
# =============================================================================

cat("\n=============================================================================\n")
cat("RUNNING PERMUTATION TESTS FOR BALANCE AND ATTRITION\n")
cat("=============================================================================\n")
cat("This may take several minutes...\n\n")

# Load required packages
if (!require("sandwich")) install.packages("sandwich")
library(sandwich)

# Set output directory
output_dir <- file.path(data_dir, "Election Myths Stories/data")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =============================================================================
# SETUP: Define variables needed for tests
# =============================================================================

# Define covariates to test for balance (should match quick version)
balance_vars_all <- c(
  "Age_Group", "Gender", "Race_Ethnicity", "Education_Level",
  "Party_Identification", "Ideology", "Urban_Rural",
  "Political_Interest", "Conspiracy_Score", "Populism_Score"
)

# Keep only variables that exist in the dataset
balance_vars <- balance_vars_all[balance_vars_all %in% names(dat_final)]

# Separate continuous and categorical variables
continuous_vars <- balance_vars[sapply(dat_final[balance_vars], is.numeric)]
categorical_vars <- balance_vars[sapply(dat_final[balance_vars], function(x) is.factor(x) | is.character(x))]

# Create treatment indicator
dat_final$Z <- as.numeric(dat_final$Election_Rumor_Placebo_Randomization == "Treatment")

# Build formula
balance_formula <- as.formula(paste("Z ~", paste(balance_vars, collapse = " + ")))

# Fit the regression
balance_fit <- lm(balance_formula, data = dat_final, singular.ok = FALSE)

# Calculate observed Wald statistic
Rbeta_hat <- coef(balance_fit)[-1]  # Exclude intercept
RVR <- vcovHC(balance_fit, type = "HC0")[-1, -1]  # Robust variance-covariance matrix
W_obs <- as.numeric(t(Rbeta_hat) %*% solve(RVR, Rbeta_hat))

cat(sprintf("\nObserved Wald statistic: %.3f\n", W_obs))

# =============================================================================
# PERMUTATION TEST FOR WALD STATISTIC
# =============================================================================

cat("\n--- Running permutation test for Wald statistic ---\n")
cat("This will take a few minutes...\n")

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

write.csv(wald_results, file.path(output_dir, "balance_wald_test.csv"), row.names = FALSE)
cat(sprintf("\nWald test results saved to: %s\n", file.path(output_dir, "balance_wald_test.csv")))

# =============================================================================
# DIFFERENTIAL ATTRITION PERMUTATION TESTS
# =============================================================================

cat("\n--- Running permutation tests for differential attrition ---\n")

# Check if we have recontact data
if ("weight_recontact" %in% names(dat_final)) {

  # Create attrition indicator
  dat_final$attrited <- as.numeric(is.na(dat_final$weight_recontact))

  cat(sprintf("\nTotal attrition: %d / %d (%.1f%%)\n",
              sum(dat_final$attrited), nrow(dat_final),
              100 * mean(dat_final$attrited)))

  # =============================================================================
  # TEST 1: Differential Attrition Rate (Studentized permutation test)
  # =============================================================================

  cat("\n--- Test 1: Differential Attrition Rate ---\n")
  cat("This will take a few minutes...\n")

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

  cat(sprintf("\nPermutation test results:\n"))
  cat(sprintf("  Observed t-statistic: %.3f\n", t_obs_attrition))
  cat(sprintf("  Permutation p-value: %.4f\n", p_attrition_rate))

  # =============================================================================
  # TEST 2: Differential Attrition Pattern (Studentized permutation test)
  # =============================================================================

  cat("\n--- Test 2: Differential Attrition Pattern ---\n")
  cat("Testing if treatment-covariate interactions predict attrition\n")

  # Only run if we have continuous covariates
  if (length(continuous_vars) > 0) {

    cat("This will take a few minutes...\n")

    # Create interaction formula
    interaction_formula <- paste("attrited ~ Election_Rumor_Placebo_Randomization *",
                                 paste0("(", paste(continuous_vars, collapse = " + "), ")"))

    cat(sprintf("Formula: %s\n", interaction_formula))

    # Fit the model
    attrition_fit_obs <- lm(as.formula(interaction_formula), data = dat_final)

    # Get indices of interaction coefficients
    coef_names <- names(coef(attrition_fit_obs))
    interaction_indices <- grep(":", coef_names)

    cat(sprintf("Number of interaction terms: %d\n", length(interaction_indices)))

    if (length(interaction_indices) > 0) {
      # F-test for interaction coefficients using robust SE
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

      cat(sprintf("\nPermutation test results:\n"))
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

  write.csv(attrition_results, file.path(output_dir, "attrition_tests.csv"), row.names = FALSE)
  cat(sprintf("\nAttrition test results saved to: %s\n", file.path(output_dir, "attrition_tests.csv")))

} else {
  cat("\nNo recontact data found - skipping attrition permutation tests.\n")
  attrition_results <- data.frame(
    test = c("Differential Attrition Rate", "Differential Attrition Pattern"),
    observed_statistic = c(NA, NA),
    permutation_p_value = c(NA, NA),
    n_permutations = NA
  )
  write.csv(attrition_results, file.path(output_dir, "attrition_tests.csv"), row.names = FALSE)
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("Permutation tests complete!\n")
cat("=============================================================================\n")
cat("\nFiles created:\n")
cat(sprintf("- %s\n", file.path(output_dir, "balance_wald_test.csv")))
cat(sprintf("- %s\n", file.path(output_dir, "attrition_tests.csv")))
cat("\nThese files will be loaded automatically by balance_and_checks_quick.R\n")
cat("\n")

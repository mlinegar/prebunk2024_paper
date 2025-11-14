# =============================================================================
# Modeling Helper Functions
# =============================================================================
# Helper functions for running regression models across different outcomes
# =============================================================================

#' Run a set of models for specified outcomes
#'
#' @param outcomes Vector of outcome variable names (labeled versions)
#' @param predictor_fn Function that takes an outcome and returns predictors
#' @param design Survey design object
#' @param family Family for regression (default: gaussian())
#' @param model_name_prefix Prefix for naming models
#' @return Named list of model objects
run_outcome_models <- function(outcomes, predictor_fn, design,
                               family = gaussian(),
                               model_name_prefix = "") {
  models <- lapply(outcomes, function(outcome) {
    predictors <- predictor_fn(outcome)
    run_regression_for_table(outcome, predictors, design, family = family)
  })

  if (model_name_prefix != "") {
    names(models) <- paste0(model_name_prefix, "_", outcomes)
  } else {
    names(models) <- outcomes
  }

  return(models)
}

#' Create predictor list for standard pooled models
#'
#' @param outcome Outcome variable (labeled)
#' @param prefix_to_remove Prefix to remove from outcome to get pre-treatment var
#' @param prefix_to_add Prefix to add to get pre-treatment var
#' @param include_rumor Include rumor randomization
#' @param covariates Vector of covariate labels
#' @return Vector of predictor variable names
get_pooled_predictors <- function(outcome,
                                   prefix_to_remove = "Post_",
                                   prefix_to_add = "Pre_",
                                   include_rumor = TRUE,
                                   covariates = variables_in_model_labels) {
  pre_var <- paste0(sub(prefix_to_remove, prefix_to_add, outcome))

  predictors <- c(
    pre_var,
    "Election_Rumor_Placebo_Randomization"
  )

  if (include_rumor) {
    predictors <- c(predictors, "Election_Rumor_Randomization")
  }

  predictors <- c(predictors, covariates)

  return(predictors)
}

#' Create predictor list for interaction models
#'
#' @param outcome Outcome variable (labeled)
#' @param int_var_label Label of interaction variable
#' @param prefix_to_remove Prefix to remove from outcome
#' @param prefix_to_add Prefix to add to get pre-treatment var
#' @param covariates Vector of covariate labels
#' @return Vector of predictor variable names including interactions
get_interaction_predictors <- function(outcome,
                                       int_var_label,
                                       prefix_to_remove = "Post_",
                                       prefix_to_add = "Pre_",
                                       covariates = variables_in_model_labels) {
  pre_var <- paste0(sub(prefix_to_remove, prefix_to_add, outcome))

  predictors <- c(
    pre_var,
    "Election_Rumor_Placebo_Randomization",
    "Election_Rumor_Randomization",
    paste0("Election_Rumor_Placebo_Randomization * Election_Rumor_Randomization * ",
           int_var_label),
    covariates
  )

  return(predictors)
}

#' Get human-readable column labels from outcome variables
#'
#' @param outcomes Vector of outcome variable names (labeled)
#' @return Vector of nice column labels
get_column_labels <- function(outcomes) {
  labels <- sapply(outcomes, function(x) {
    # Remove common prefixes and make readable
    x <- gsub("Post_|Pre_|Recontact_|Confidence_", "", x)
    x <- gsub("_", " ", x)
    x
  })
  return(unname(labels))
}

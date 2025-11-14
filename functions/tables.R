# =============================================================================
# Table Generation Functions
# =============================================================================
# This file contains all table generation and summary statistics code
# for the prebunking analysis.
# Extracted from prebunk.R (lines 335-462, 1600-1985)
# =============================================================================

#### SUMMARY STATISTICS ####
dt <- as.data.table(dat_final)
cor(dt[!is.na(weight_recontact),weight], dt[!is.na(weight_recontact), weight_recontact])

# For weighted statistics
summary_stats_weighted <- generate_summary_stats(dat_final,
                                                 vars_to_summarize = all_var_labels,
                                                 weight_var = weight,
                                                 group_var = Election_Rumor_Randomization,
                                                 placebo_var = Election_Rumor_Placebo_Randomization,
                                                 include_factors = FALSE)

# For unweighted statistics
summary_stats_unweighted <- generate_summary_stats(dat_final,
                                                   vars_to_summarize = all_var_labels,
                                                   weight_var = NULL,
                                                   group_var = Election_Rumor_Randomization,
                                                   placebo_var = Election_Rumor_Placebo_Randomization,
                                                   include_factors = FALSE)

# For weighted statistics (factors)
summary_stats_weighted_factor <- generate_summary_stats(dat_final,
                                                 vars_to_summarize = all_var_labels,
                                                 weight_var = weight,
                                                 group_var = Election_Rumor_Randomization,
                                                 placebo_var = Election_Rumor_Placebo_Randomization,
                                                 include_factors = TRUE)

# For unweighted statistics (factors)
summary_stats_unweighted_factor <- generate_summary_stats(dat_final,
                                                   vars_to_summarize = all_var_labels,
                                                   weight_var = NULL,
                                                   group_var = Election_Rumor_Randomization,
                                                   placebo_var = Election_Rumor_Placebo_Randomization,
                                                   include_factors = TRUE)

# make table comparing pre and post, weighted and unweighted summary statistics
post_unweighted_factor <- generate_summary_stats(
  dat_final %>% dplyr::filter(!is.na(weight_recontact)),
  vars_to_summarize = all_var_labels,
  weight_var = NULL,
  group_var = Election_Rumor_Randomization,
  placebo_var = Election_Rumor_Placebo_Randomization,
  include_factors = TRUE
)

post_weighted_factor <- generate_summary_stats(
  dat_final %>% dplyr::filter(!is.na(weight_recontact)),
  vars_to_summarize = all_var_labels,
  weight_var = weight_recontact,
  group_var = Election_Rumor_Randomization,
  placebo_var = Election_Rumor_Placebo_Randomization,
  include_factors = TRUE
)



# Create separate tables for each group
groups <- unique(summary_stats_unweighted$Group)
for (group in groups) {
  group_stats <- summary_stats_unweighted[Group == group]
  latex_table <- create_latex_table(group_stats,
                                    filename = paste0("summary_statistics_", tolower(group), ".tex"),
                                    title = paste("Summary Statistics -", stringr::str_replace_all(group, "_", " ")),
                                    note = "")
}

groups <- unique(summary_stats_weighted$Group)
for (group in groups) {
  group_stats <- summary_stats_weighted[Group == group]
  latex_table <- create_latex_table(group_stats,
                                    filename = paste0("summary_statistics_", tolower(group), "_weighted", ".tex"),
                                    title = paste("Summary Statistics -", stringr::str_replace_all(group, "_", " "), "(Weighted)"),
                                    note = "")
}

# Factor variables
factor_var_labels <- all_var_labels[sapply(as.data.table(dat_final)[, ..all_var_labels], is.factor)]

# Generating factor summaries
for (var in factor_var_labels) {
  print(var)
  factor_summary_stats <- generate_factor_summary(dat_final, var,
                                                  group_var = "Election_Rumor_Randomization",
                                                  placebo_var = "Election_Rumor_Placebo_Randomization")
  latex_table <- create_latex_table(factor_summary_stats,
                                    filename = paste0("summary_statistics_", tolower(var), ".tex"),
                                    title = paste("Summary Statistics -", stringr::str_replace_all(var, "_", " ")),
                                    note = "")
}


# Factor variables
factor_var_labels <- all_var_labels[sapply(as.data.table(dat_final)[, ..all_var_labels], is.factor)]

# Generating original and followup weighted factor summaries
for (var in factor_var_labels) {
  print(var)

# Generate summaries for different scenarios
pre_unweighted <- generate_factor_summary(dat_final, var,
                                          placebo_var = "Election_Rumor_Placebo_Randomization")
pre_unweighted[, Scenario := "Pre-treatment Unweighted"]

pre_weighted <- generate_factor_summary(dat_final, var,
                                        placebo_var = "Election_Rumor_Placebo_Randomization",
                                        weight_var = "weight")
pre_weighted[, Scenario := "Pre-treatment Weighted"]

post_unweighted <- generate_factor_summary(dat_final %>% dplyr::filter(!is.na(weight_recontact)), var,
                                           placebo_var = "Election_Rumor_Placebo_Randomization")
post_unweighted[, Scenario := "Follow-up Unweighted"]

post_weighted <- generate_factor_summary(dat_final %>% dplyr::filter(!is.na(weight_recontact)), var,
                                         placebo_var = "Election_Rumor_Placebo_Randomization",
                                         weight_var = "weight_recontact")
post_weighted[, Scenario := "Follow-up Weighted"]

# Combine all summaries
combined_summary <- rbindlist(list(pre_unweighted, pre_weighted, post_unweighted, post_weighted), use.names = TRUE, fill = TRUE)

# Create latex table
latex_table <- create_latex_table(combined_summary,
                                  filename = paste0("cross_wave_factor_summary_statistics_", tolower(var), ".tex"),
                                  title = paste("Cross-Wave Summary Statistics -", stringr::str_replace_all(var, "_", " ")),
                                  note = "")

}

#### PROPORTIONS TABLES ####

dt <- as.data.table(dat_final)

props_all <- dt[,.(
  Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
  Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
  Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
  Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
  Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
  Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
), by = .(Election_Rumor_Placebo_Randomization)][
  order(Election_Rumor_Placebo_Randomization)]
props_all[, Party_Identification := "Any"]

props <- dt[,.(
  Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
  Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
  Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
  Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
  Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
  Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
), by = .(Election_Rumor_Placebo_Randomization, Party_Identification)]

props_full <- rbindlist(list(props, props_all), use.names = TRUE)

props_full[,`:=`(
  Election_Confidence_Difference = Pre_Election_Confidence-Post_Election_Confidence,
  Rumor_Difference = Pre_Rumor_Confidence-Post_Rumor_Confidence
  )]
props_full <- props_full[order(as.character(Party_Identification), Election_Rumor_Placebo_Randomization)]

props_all_wtd <- dt[,.(
  Pre_Election_Confidence = weighted.mean(Pre_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
  Post_Election_Confidence = weighted.mean(Post_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
  Recontact_Election_Confidence = weighted.mean(Recontact_Confidence_Country_Ballots>5, weight_recontact, na.rm = TRUE),
  Pre_Rumor_Confidence = weighted.mean(Rumor>5, weight, na.rm = TRUE),
  Post_Rumor_Confidence = weighted.mean(Rumor_Post>5, weight, na.rm = TRUE),
  Recontact_Rumor_Confidence = weighted.mean(Rumor_Recontact>5, weight_recontact, na.rm = TRUE)
), by = .(Election_Rumor_Placebo_Randomization)][
  order(Election_Rumor_Placebo_Randomization)]
props_all_wtd[, Party_Identification := "Any"]

props_wtd <- dt[,.(
  Pre_Election_Confidence = weighted.mean(Pre_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
  Post_Election_Confidence = weighted.mean(Post_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
  Recontact_Election_Confidence = weighted.mean(Recontact_Confidence_Country_Ballots>5, weight_recontact, na.rm = TRUE),
  Pre_Rumor_Confidence = weighted.mean(Rumor>5, weight, na.rm = TRUE),
  Post_Rumor_Confidence = weighted.mean(Rumor_Post>5, weight, na.rm = TRUE),
  Recontact_Rumor_Confidence = weighted.mean(Rumor_Recontact>5, weight_recontact, na.rm = TRUE)
), by = .(Election_Rumor_Placebo_Randomization, Party_Identification)]

props_full_wtd <- rbindlist(list(props_wtd, props_all_wtd), use.names = TRUE)

props_full_wtd[,`:=`(
  Election_Confidence_Difference = Pre_Election_Confidence-Post_Election_Confidence,
  Rumor_Difference = Pre_Rumor_Confidence-Post_Rumor_Confidence
  )]
props_full_wtd <- props_full_wtd[order(as.character(Party_Identification), Election_Rumor_Placebo_Randomization)]

# Function to calculate CI
calculate_ci <- function(p, n) {
  se <- sqrt(p * (1 - p) / n)
  margin <- 1.96 * se
  list(lower = pmax(0, p - margin), upper = pmin(1, p + margin))
}

# Calculate sample sizes
sample_sizes <- dt[, .(
  n_election = sum(!is.na(Pre_Confidence_Country_Ballots)),
  n_rumor = sum(!is.na(Rumor))
), by = .(Election_Rumor_Placebo_Randomization, Party_Identification)]

sample_sizes_all <- dt[, .(
  n_election = sum(!is.na(Pre_Confidence_Country_Ballots)),
  n_rumor = sum(!is.na(Rumor))
), by = .(Election_Rumor_Placebo_Randomization)]
sample_sizes_all[, Party_Identification := "Any"]

sample_sizes <- rbindlist(list(sample_sizes, sample_sizes_all), use.names = TRUE)

# Merge sample sizes with props_full
props_full <- merge(props_full, sample_sizes, by = c("Election_Rumor_Placebo_Randomization", "Party_Identification"))
props_full_wtd <- merge(props_full_wtd, sample_sizes, by = c("Election_Rumor_Placebo_Randomization", "Party_Identification"))

# Calculate CIs
ci_columns <- c("Pre_Election_Confidence", "Post_Election_Confidence", "Recontact_Election_Confidence",
                "Pre_Rumor_Confidence", "Post_Rumor_Confidence", "Recontact_Rumor_Confidence")

for (col in ci_columns) {
  ci_lower <- paste0(col, "_CI_Lower")
  ci_upper <- paste0(col, "_CI_Upper")
  n_col <- ifelse(grepl("Election", col), "n_election", "n_rumor")

  props_full[, c(ci_lower, ci_upper) := calculate_ci(get(col), get(n_col))]
  props_full_wtd[, c(ci_lower, ci_upper) := calculate_ci(get(col), get(n_col))]
}

# By rumor analysis
props_all_rumor <- dt[,.(
  Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
  Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
  Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
  Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
  Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
  Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
), by = .(Election_Rumor_Placebo_Randomization, Election_Rumor_Randomization)][
  order(Election_Rumor_Randomization, Election_Rumor_Placebo_Randomization)]
props_all_rumor[, Party_Identification := "Any"]

props_rumor <- dt[,.(
  Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
  Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
  Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
  Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
  Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
  Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
), by = .(Election_Rumor_Placebo_Randomization, Party_Identification, Election_Rumor_Randomization)]

props_full_rumor <- rbindlist(list(props_rumor, props_all_rumor), use.names = TRUE)

props_full_rumor[,`:=`(
  Election_Confidence_Difference = Pre_Election_Confidence-Post_Election_Confidence,
  Rumor_Difference = Pre_Rumor_Confidence-Post_Rumor_Confidence
  )]

# Create output tables for export
election_table <- props_full[Party_Identification == "Any", .(
  Treatment = Election_Rumor_Placebo_Randomization,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)][order(Treatment)]

rumor_table <- props_full[Party_Identification == "Any", .(
  Treatment = Election_Rumor_Placebo_Randomization,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)][order(Treatment)]

election_table_rumor <- props_full_rumor[Party_Identification == "Any", .(
  Rumor = Election_Rumor_Randomization,
  Treatment = Election_Rumor_Placebo_Randomization,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)][order(Rumor, Treatment)]

rumor_table_rumor <- props_full_rumor[Party_Identification == "Any", .(
  Rumor = Election_Rumor_Randomization,
  Treatment = Election_Rumor_Placebo_Randomization,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)][order(Rumor, Treatment)]

election_table_party <- props_full[Party_Identification != "Any", .(
  Party = Party_Identification,
  Treatment = Election_Rumor_Placebo_Randomization,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)][order(Party, Treatment)]

rumor_table_party <- props_full[Party_Identification != "Any", .(
  Party = Party_Identification,
  Treatment = Election_Rumor_Placebo_Randomization,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)][order(Party, Treatment)]

election_table_rumor_party <- props_full_rumor[Party_Identification != "Any", .(
  Rumor = Election_Rumor_Randomization,
  Treatment = Election_Rumor_Placebo_Randomization,
  Party = Party_Identification,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)][order(Rumor, Party, Treatment)]

rumor_table_rumor_party <- props_full_rumor[Party_Identification != "Any", .(
  Rumor = Election_Rumor_Randomization,
  Treatment = Election_Rumor_Placebo_Randomization,
  Party = Party_Identification,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)][order(Rumor, Party, Treatment)]

calculate_weighted_stats <- function(var_name, outcome_label, timepoint_label, design) {
  var_numeric <- as.numeric(design$variables[[var_name]])
  design_tmp <- update(design, temp_value = var_numeric)
  formula_tmp <- ~temp_value

  mean_by <- svyby(formula_tmp,
                   ~Election_Rumor_Placebo_Randomization,
                   design_tmp,
                   svymean,
                   na.rm = TRUE,
                   vartype = c("se"))

  var_by <- svyby(formula_tmp,
                  ~Election_Rumor_Placebo_Randomization,
                  design_tmp,
                  svyvar,
                  na.rm = TRUE)

  stats_by <- data.table(
    Outcome = outcome_label,
    Timepoint = timepoint_label,
    Group = mean_by$Election_Rumor_Placebo_Randomization,
    Mean = as.numeric(mean_by$temp_value),
    SD = sqrt(as.numeric(var_by$temp_value)),
    CI_Lower = as.numeric(mean_by$temp_value) - 1.96 * as.numeric(mean_by$se),
    CI_Upper = as.numeric(mean_by$temp_value) + 1.96 * as.numeric(mean_by$se)
  )

  mean_overall <- svymean(formula_tmp, design_tmp, na.rm = TRUE)
  var_overall <- svyvar(formula_tmp, design_tmp, na.rm = TRUE)

  overall_row <- data.table(
    Outcome = outcome_label,
    Timepoint = timepoint_label,
    Group = "Overall",
    Mean = as.numeric(coef(mean_overall)),
    SD = sqrt(as.numeric(coef(var_overall))),
    CI_Lower = as.numeric(coef(mean_overall)) - 1.96 * as.numeric(SE(mean_overall)),
    CI_Upper = as.numeric(coef(mean_overall)) + 1.96 * as.numeric(SE(mean_overall))
  )

  rbindlist(list(stats_by, overall_row), use.names = TRUE)
}

rumor_timepoints <- data.table(
  var = c("Rumor", "Rumor_Post", "Rumor_Recontact"),
  label = c("Rumor Confidence", "Rumor Confidence", "Rumor Confidence"),
  time = c("Pre", "Post", "Recontact")
)

rumor_stats <- rbindlist(lapply(1:nrow(rumor_timepoints), function(i) {
  calculate_weighted_stats(rumor_timepoints$var[i],
                           rumor_timepoints$label[i],
                           rumor_timepoints$time[i],
                           svy_design_weighted)
}), use.names = TRUE)

rumor_stats[, Group := factor(Group, levels = c("Treatment", "Placebo", "Overall"))]
setorder(rumor_stats, Outcome, Timepoint, Group)

create_latex_table(
  rumor_stats,
  filename = "pre_post_rumor_confidence_stats.tex",
  title = "Weighted Means, Standard Deviations, and 95\\% CIs for Rumor Confidence",
  label = "pre_post_rumor_confidence_stats",
  note = "Estimates use survey weights. Confidence intervals are mean plus or minus 1.96 multiplied by the standard error."
)

election_timepoints <- data.table(
  var = c("Pre_Confidence_Country_Ballots", "Post_Confidence_Country_Ballots", "Recontact_Confidence_Country_Ballots"),
  label = c("National Ballot Confidence", "National Ballot Confidence", "National Ballot Confidence"),
  time = c("Pre", "Post", "Recontact")
)

election_stats <- rbindlist(lapply(1:nrow(election_timepoints), function(i) {
  calculate_weighted_stats(election_timepoints$var[i],
                           election_timepoints$label[i],
                           election_timepoints$time[i],
                           svy_design_weighted)
}), use.names = TRUE)

election_stats[, Group := factor(Group, levels = c("Treatment", "Placebo", "Overall"))]
setorder(election_stats, Outcome, Timepoint, Group)

create_latex_table(
  election_stats,
  filename = "pre_post_election_confidence_stats.tex",
  title = "Weighted Means, Standard Deviations, and 95\\% CIs for National Ballot Confidence",
  label = "pre_post_election_confidence_stats",
  note = "Estimates use survey weights. Confidence intervals are mean plus or minus 1.96 multiplied by the standard error."
)

# Function to format percentages
format_pct <- function(x) sprintf("%.1f%%", x * 100)

# Apply formatting to all tables
election_table[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]
election_table_rumor[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table_rumor[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]
election_table_party[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table_party[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]
election_table_rumor_party[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table_rumor_party[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]

# Create LaTeX tables
election_latex <- xtable(election_table,
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact",
                         size = "small",
                         label = "tab:prop_election")

rumor_latex <- xtable(rumor_table,
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact",
                      size = "small",
                      label = "tab:prop_rumor")

election_rumor_latex <- xtable(election_table_rumor,
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor",
                         size = "small",
                         label = "tab:prop_election_rumor")

rumor_indiv_rumor_latex <- xtable(rumor_table_rumor,
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor",
                      size = "small",
                      label = "tab:prop_indiv_rumor")

election_party_latex <- xtable(election_table_party,
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact, by party identification",
                         size = "small",
                         label = "tab:prop_election_party")

rumor_party_latex <- xtable(rumor_table_party,
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact, by party identification",
                      size = "small",
                      label = "tab:prop_rumor_party")

election_rumor_party_latex <- xtable(election_table_rumor_party,
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor and party identification",
                         size = "small",
                         label = "tab:prop_election_indiv_rumor_party")

rumor_indiv_rumor_party_latex <- xtable(rumor_table_rumor_party,
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor and party identification",
                      size = "small",
                      label = "tab:prop_rumor_indiv_rumor_party")



# Print LaTeX code
table_output_dir <- file.path(data_dir, "writing_draft", "tables")
dir.create(table_output_dir, showWarnings = FALSE, recursive = TRUE)
print(election_latex, file=file.path(table_output_dir,'election_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_latex, file=file.path(table_output_dir,'rumor_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")
print(election_rumor_latex, file=file.path(table_output_dir,'election_rumor_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_indiv_rumor_latex, file=file.path(table_output_dir,'rumor_indiv_rumor_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")
print(election_party_latex, file=file.path(table_output_dir,'election_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_party_latex, file=file.path(table_output_dir,'rumor_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")
print(election_rumor_party_latex, file=file.path(table_output_dir,'election_rumor_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_indiv_rumor_party_latex, file=file.path(table_output_dir,'rumor_indiv_rumor_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")

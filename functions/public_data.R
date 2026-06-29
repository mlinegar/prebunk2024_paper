# =============================================================================
# Public Replication Data Helpers
# =============================================================================
# Defines the minimum respondent-level columns needed to reproduce the reported
# analyses from the processed data object, plus type restoration for CSV loading.
# =============================================================================

public_replication_filename <- "prebunk_public_replication.csv"
public_codebook_filename <- "prebunk_public_replication_codebook.csv"

public_replication_path <- function(filename = public_replication_filename) {
  file.path(data_dir, "data", "public", filename)
}

public_replication_columns <- function() {
  unique(c(
    variables_in_model_labels,
    "Populism_Bin",
    "Conspiracy_Bin",
    "Election_Rumor_Randomization",
    "Election_Rumor_Placebo_Randomization",
    "Human_In_The_Loop",
    "Article_Recalled_Recontact",
    pre_y_vars_labels,
    post_y_vars_labels,
    diff_y_vars_labels,
    recontact_y_var_labels,
    recontact_diff_y_var_labels,
    "Rumor",
    "Rumor_Post",
    "Rumor_Recontact",
    cisa_pre_y_vars_labels,
    cisa_post_y_vars_labels,
    cisa_recontact_y_vars_labels,
    recontact_cisa_diff_y_var_labels,
    motivated_y_vars_recontact_labels,
    "attention_1_correct",
    "attention_2_correct",
    "weight",
    "weight_recontact"
  ))
}

public_factor_levels <- function() {
  list(
    Age_Group = c("Under 30", "30-44", "45-64", "65+"),
    Gender = c("Male", "Female"),
    Race_Ethnicity = c("White", "Black", "Hispanic", "Other"),
    Education_Level = c("HS or less", "Some college", "College grad", "Postgrad"),
    Party_Identification = c("Democrat", "Independent", "Republican"),
    Ideology = c("Liberal", "Moderate", "Conservative"),
    Region = c("Northeast", "Midwest", "South", "West"),
    Urban_Rural = c("City", "Suburb", "Town", "Rural area"),
    Political_Interest = c(
      "Pol Interest: Most of the time",
      "Pol Interest: Some of the time",
      "Pol Interest: Only now and then",
      "Pol Interest: Hardly at all"
    ),
    Populism_Bin = c("6-10", "11-15", "16-20", "21-25", "26-30"),
    Conspiracy_Bin = c("6-10", "11-15", "16-20", "21-25", "26-30"),
    Election_Rumor_Randomization = c(
      "Voter Fraud",
      "Voter Rolls",
      "Hacking",
      "Blue Shift",
      "Voting Machines"
    ),
    Election_Rumor_Placebo_Randomization = c("Placebo", "Treatment"),
    Human_In_The_Loop = c("Human In The Loop", "Only AI")
  )
}

public_logical_columns <- function() {
  c("Article_Recalled_Recontact", "attention_1_correct", "attention_2_correct")
}

public_numeric_columns <- function() {
  setdiff(
    public_replication_columns(),
    c(names(public_factor_levels()), public_logical_columns())
  )
}

restore_public_replication_types <- function(dat) {
  dat <- as.data.frame(dat)

  missing_cols <- setdiff(public_replication_columns(), names(dat))
  if (length(missing_cols) > 0) {
    stop(
      "Public replication data are missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  for (var in public_numeric_columns()) {
    dat[[var]] <- as.numeric(dat[[var]])
  }

  for (var in public_logical_columns()) {
    dat[[var]] <- as.logical(dat[[var]])
  }

  for (var in names(public_factor_levels())) {
    dat[[var]] <- factor(dat[[var]], levels = public_factor_levels()[[var]])
  }

  dat
}

prepare_public_replication_data <- function(dat) {
  cols <- public_replication_columns()
  missing_cols <- setdiff(cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      "Processed data are missing required public columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  public_dat <- as.data.frame(dat)[, cols, drop = FALSE]
  for (var in public_numeric_columns()) {
    public_dat[[var]] <- as.numeric(haven::zap_labels(public_dat[[var]]))
  }
  restore_public_replication_types(public_dat)
}

build_public_codebook <- function() {
  descriptions <- c(
    Age_Group = "Age group.",
    Gender = "Binary gender category.",
    Race_Ethnicity = "Race/ethnicity category.",
    Education_Level = "Highest education category.",
    Party_Identification = "Three-category party identification.",
    Ideology = "Three-category ideology.",
    Region = "U.S. Census region.",
    Urban_Rural = "Residential area type.",
    Political_Interest = "Self-reported interest in political news.",
    Populism_Score = "Aggregate populism score; higher values indicate stronger populist attitudes.",
    Conspiracy_Score = "Aggregate conspiracy-belief score; higher values indicate stronger conspiracy beliefs.",
    MIST_Correct = "Number of MIST-8 misinformation susceptibility items answered correctly.",
    Populism_Bin = "Binned aggregate populism score.",
    Conspiracy_Bin = "Binned aggregate conspiracy-belief score.",
    Election_Rumor_Randomization = "Randomized election-rumor assignment.",
    Election_Rumor_Placebo_Randomization = "Treatment assignment: prebunking article or placebo article.",
    Human_In_The_Loop = "Whether the assigned prebunking article used additional human refinement.",
    Article_Recalled_Recontact = "Whether the respondent recalled the assigned article in the recontact survey.",
    Pre_Confidence_Own_Ballot = "Pre-treatment confidence that own ballot would be counted accurately.",
    Pre_Confidence_County_Ballots = "Pre-treatment confidence that county ballots would be counted accurately.",
    Pre_Confidence_Country_Ballots = "Pre-treatment confidence that ballots nationally would be counted accurately.",
    Post_Confidence_Own_Ballot = "Immediate post-treatment confidence that own ballot would be counted accurately.",
    Post_Confidence_County_Ballots = "Immediate post-treatment confidence that county ballots would be counted accurately.",
    Post_Confidence_Country_Ballots = "Immediate post-treatment confidence that ballots nationally would be counted accurately.",
    Confidence_Own_Ballot_Diff = "Post-treatment minus pre-treatment confidence in own ballot counting.",
    Confidence_County_Ballots_Diff = "Post-treatment minus pre-treatment confidence in county ballot counting.",
    Confidence_Country_Ballots_Diff = "Post-treatment minus pre-treatment confidence in national ballot counting.",
    Recontact_Confidence_Own_Ballot = "Recontact confidence that own ballot would be counted accurately.",
    Recontact_Confidence_County_Ballots = "Recontact confidence that county ballots would be counted accurately.",
    Recontact_Confidence_Country_Ballots = "Recontact confidence that ballots nationally would be counted accurately.",
    Recontact_Confidence_Own_Ballot_Diff = "Recontact minus pre-treatment confidence in own ballot counting.",
    Recontact_Confidence_County_Ballots_Diff = "Recontact minus pre-treatment confidence in county ballot counting.",
    Recontact_Confidence_Country_Ballots_Diff = "Recontact minus pre-treatment confidence in national ballot counting.",
    Rumor = "Pre-treatment belief/confidence in the assigned election rumor.",
    Rumor_Post = "Immediate post-treatment belief/confidence in the assigned election rumor.",
    Rumor_Diff = "Immediate post-treatment minus pre-treatment belief/confidence in the assigned election rumor.",
    Rumor_Recontact = "Recontact belief/confidence in the assigned election rumor.",
    Rumor_Diff_Recontact = "Recontact minus pre-treatment belief/confidence in the assigned election rumor.",
    Pre_All_Rumors = "Pre-treatment average belief/confidence across all election rumors.",
    Post_All_Rumors = "Immediate post-treatment average belief/confidence across all election rumors; not asked in wave 1 and retained as missing for model compatibility.",
    Recontact_All_Rumors = "Recontact average belief/confidence across all election rumors.",
    All_Rumors_Diff_Recontact = "Recontact minus pre-treatment average belief/confidence across all election rumors.",
    Pre_All_Facts = "Pre-treatment average belief/confidence across true election-security statements.",
    Post_All_Facts = "Immediate post-treatment average belief/confidence across true election-security statements; not asked in wave 1 and retained as missing for model compatibility.",
    Recontact_All_Facts = "Recontact average belief/confidence across true election-security statements.",
    All_Facts_Diff_Recontact = "Recontact minus pre-treatment average belief/confidence across true election-security statements.",
    Tried_To_Change_Friends_Mind = "Recontact item measuring whether the respondent tried to change a friend or family member's mind.",
    Motivated_To_Debunk = "Recontact motivation to debunk election misinformation.",
    Equipped_To_Push_Back = "Recontact perceived ability to push back against election misinformation.",
    Willing_To_Argue_Against = "Recontact willingness to argue against election misinformation.",
    attention_1_correct = "Indicator for passing the first attention check.",
    attention_2_correct = "Indicator for passing the second attention check.",
    weight = "Main-wave survey weight.",
    weight_recontact = "Recontact survey weight; missing for respondents not observed in the recontact wave."
  )

  factor_levels <- public_factor_levels()
  logical_cols <- public_logical_columns()
  numeric_cols <- public_numeric_columns()
  cols <- public_replication_columns()

  data.frame(
    variable = cols,
    type = dplyr::case_when(
      cols %in% names(factor_levels) ~ "categorical",
      cols %in% logical_cols ~ "logical",
      cols %in% numeric_cols ~ "numeric",
      TRUE ~ "unknown"
    ),
    values = vapply(cols, function(var) {
      if (var %in% names(factor_levels)) {
        paste(factor_levels[[var]], collapse = "; ")
      } else if (var %in% logical_cols) {
        "TRUE; FALSE"
      } else {
        ""
      }
    }, character(1)),
    description = unname(descriptions[cols]),
    stringsAsFactors = FALSE
  )
}

write_public_replication_data <- function(dat,
                                          data_file = public_replication_path(),
                                          codebook_file = public_replication_path(public_codebook_filename)) {
  public_dat <- prepare_public_replication_data(dat)

  dir.create(dirname(data_file), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(public_dat, data_file, na = "")
  data.table::fwrite(build_public_codebook(), codebook_file, na = "")

  cat(sprintf("Public replication data written to: %s\n", data_file))
  cat(sprintf("Public replication codebook written to: %s\n", codebook_file))
  cat(sprintf("Rows: %d; columns: %d\n", nrow(public_dat), ncol(public_dat)))

  invisible(public_dat)
}

load_public_replication_data <- function(data_file = public_replication_path()) {
  if (!file.exists(data_file)) {
    stop(
      "Public replication dataset not found: ", data_file,
      "\nRun R/create_public_replication_data.R from a local checkout with raw data, ",
      "or place the published Data S1 CSV at this path."
    )
  }

  dat <- data.table::fread(data_file, na.strings = c("", "NA", "NaN"))
  restore_public_replication_types(dat)
}

initialize_public_replication_data <- function(dat) {
  dat_final <<- restore_public_replication_types(dat)

  svy_design_weighted <<- svydesign(data = dat_final, weights = ~weight, id = ~1)
  svy_design_unweighted <<- svydesign(data = dat_final, weights = NULL, id = ~1)

  covariate_labels <<- c()
  for (var in variables_in_model_labels) {
    var_levels <- levels(dat_final[[var]])[-1]
    if (!is.null(var_levels)) {
      covariate_labels <<- c(covariate_labels, var_levels)
    } else {
      covariate_labels <<- c(covariate_labels, gsub("_", " ", var))
    }
  }

  invisible(dat_final)
}

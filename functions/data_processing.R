# =============================================================================
# Data Processing Functions
# =============================================================================
# This file contains all data loading, transformation, and preparation code
# for the prebunking analysis.
# Extracted from prebunk.R (lines 38-333)
# =============================================================================

#### Prepare data ####
caltech_elections_august24 <- read_sav(file.path(data_dir, data_file))
caltech_elections_august24_recontact <- read_sav(file.path(data_dir, recontact_file))

#### DATA PROCESSING ####
# Process the data
dat <- caltech_elections_august24 %>%
  mutate(
    # Handle skipped values (8 for most variables, 998 for some scale variables)
    across(matches("^(conspiracy|information|populism|attention|mist)_\\d+$"), ~handle_skipped(., 8)),
    across(matches("^(ballotco|cisa)\\w+_scale$"), ~handle_skipped(., 998)),
    across(matches("^newsint$"), ~handle_skipped(., 7)),
    across(matches("^urbancity$"), ~handle_skipped(., 5)),

    # Flip scales for variables where low values indicate agreement
    across(matches("^conspiracy_\\d+$"), ~flip_scale(., rating_min, rating_max)),
    across(matches("^populism_\\d+$"), ~flip_scale(., rating_min, rating_max)),

    # Recode MIST questions to correct/incorrect
    correct_mist_1 = mist_1 == 1,
    correct_mist_2 = mist_2 == 1,
    correct_mist_3 = mist_3 == 1,
    correct_mist_4 = mist_4 == 1,
    correct_mist_5 = mist_5 == 2,
    correct_mist_6 = mist_6 == 2,
    correct_mist_7 = mist_7 == 2,
    correct_mist_8 = mist_8 == 2,
    cisa_rel = case_when(
      electionrumor_rand == 1 ~ cisa1_scale,
      electionrumor_rand == 2 ~ cisa2_scale,
      electionrumor_rand == 3 ~ cisa3_scale,
      electionrumor_rand == 4 ~ cisa4_scale,
      electionrumor_rand == 5 ~ cisa5_scale
    ),
    # Recode information questions
    across(matches("^information_\\d+$"), ~factor(5 - ., levels = 1:4, labels = c("Never", "Rarely", "Sometimes", "Often"))),

    # Calculate MIST, populism, and conspiracy scores
    populism = rowSums(select(., matches("^populism_\\d+$")), na.rm = TRUE),
    conspiracy = rowSums(select(., matches("^conspiracy_\\d+$")), na.rm = TRUE),
    # mist = rowSums(select(., matches("^correct_mist_\\d+$")), na.rm = TRUE),

    # Recode attention check questions
    attention_1_correct = attention_1 == 3,
    attention_2_correct = attention_2 == 2,

    # Recode demographic variables
    gender2 = case_when(
      gender4 %in% c(1, 2) ~ gender4,
      TRUE ~ NA_real_
    ),

    urbancity4 = case_when(
      urbancity %in% 1:4 ~ urbancity,
      TRUE ~ NA_real_
    ),

    newsint4 = case_when(
      newsint %in% 1:4 ~ newsint,
      TRUE ~ NA_real_
    ),

    pid3 = case_when(
      pid3 %in% 1:2 ~ pid3,
      pid3 %in% 3:5 ~ 3,
      TRUE ~ NA_real_
    ),

    ideo3 = case_when(
      ideo3 %in% 1:3 ~ ideo3,
      TRUE ~ NA_real_
    ),

    # Convert variables to factors and label them
    across(c(gender, gender4, urbancity, age4, race, race4, educ, educ4, pid3, pid7, ideo3, ideo5, region, newsint,
             electionrumor_rand, electionrumor_placebo_rand, electionrumor_pipe), as.factor),

    # Add labels to rumor variables
    electionrumor_rand = fct_recode(electionrumor_rand,
                                    "Voter Fraud" = "1",
                                    "Voter Rolls" = "2",
                                    "Hacking" = "3",
                                    "Blue Shift" = "4",
                                    "Voting Machines" = "5"),
    electionrumor_pipe = fct_recode(electionrumor_pipe,
                                    "Voter Fraud" = "1",
                                    "Voter Rolls" = "2",
                                    "Hacking" = "3",
                                    "Blue Shift" = "4",
                                    "Voting Machines" = "5",
                                    "Placebo" = "6"),
    human_in_the_loop = case_when(electionrumor_rand == "Voter Fraud" ~ "Human In The Loop",
                                  electionrumor_rand != "Voter Fraud" ~ "Only AI"
                                    ),
    # Recode and relevel electionrumor_placebo_rand
    electionrumor_placebo_rand = fct_recode(electionrumor_placebo_rand, "Treatment" = "1", "Placebo" = "2"),
    electionrumor_placebo_rand = fct_relevel(electionrumor_placebo_rand, "Placebo"),

    gender = fct_recode(gender, "Male" = "1", "Female" = "2"),
    age4 = fct_recode(age4, "Under 30" = "1", "30-44" = "2", "45-64" = "3", "65+" = "4"),
    age4 = fct_relevel(age4, "Under 30", "30-44", "45-64", "65+"),
    race4 = fct_recode(race4, "White" = "1", "Black" = "2", "Hispanic" = "3", "Other" = "4"),
    educ4 = fct_recode(educ4, "HS or less" = "1", "Some college" = "2", "College grad" = "3", "Postgrad" = "4"),
    educ4 = fct_relevel(educ4, "HS or less", "Some college", "College grad", "Postgrad"),
    pid3 = fct_recode(pid3, "Democrat" = "1", "Republican" = "2", "Independent" = "3"),
    pid3 = fct_relevel(pid3, "Democrat", "Independent", "Republican"),
    ideo3 = fct_recode(ideo3, "Liberal" = "1", "Moderate" = "2", "Conservative" = "3"),
    ideo3 = fct_relevel(ideo3, "Liberal", "Moderate", "Conservative"),
    region = fct_recode(region, "Northeast" = "1", "Midwest" = "2", "South" = "3", "West" = "4"),
    urbancity = fct_recode(urbancity, "City" = "1", "Suburb" = "2", "Town" = "3", "Rural area" = "4"),
    newsint = fct_recode(newsint, "Pol Interest: Most of the time" = "1", "Pol Interest: Some of the time" = "2",
                         "Pol Interest: Only now and then" = "3", "Pol Interest: Hardly at all" = "4")
  )


dat_recontact <- caltech_elections_august24_recontact %>%
  mutate(
    across(matches("^(ballotco|cisa|election)"), ~handle_skipped(., c(-1, 998))),
    across(matches("^(article_recall)"), ~handle_skipped(., c(-1, 6))),
    # Recode MIST questions to correct/incorrect
    correct_mist_1 = mist_1 == 1,
    correct_mist_2 = mist_2 == 1,
    correct_mist_3 = mist_3 == 1,
    correct_mist_4 = mist_4 == 1,
    correct_mist_5 = mist_5 == 2,
    correct_mist_6 = mist_6 == 2,
    correct_mist_7 = mist_7 == 2,
    correct_mist_8 = mist_8 == 2,
    # to do: recode CISA questions, do CISA differences
    # to match whether it matches their assigned treatment
    # e.g.: cisa1_scale matches electionrumor_rand==1
    cisa_rel_recontact = case_when(
      electionrumor_rand == 1 ~ cisa1_scale_recontact,
      electionrumor_rand == 2 ~ cisa2_scale_recontact,
      electionrumor_rand == 3 ~ cisa3_scale_recontact,
      electionrumor_rand == 4 ~ cisa4_scale_recontact,
      electionrumor_rand == 5 ~ cisa5_scale_recontact
    ),
    # similar for article_recall_recontact
    article_recalled = article_recall_recontact == electionrumor_rand,
    across(matches("^(conversations_)"), ~handle_skipped(., -1)),
    across(matches("^(changemind)"), ~handle_skipped(., c(-1, 3))),
    across(matches("^(conversations_)"), ~factor(., levels = 1:2, labels = c("Selected", "Not selected")))
  )

# Create bins for populism and conspiracy scores
populism_bins_and_labels <- create_bins_and_labels(rating_min, rating_max, total_populism_questions)
conspiracy_bins_and_labels <- create_bins_and_labels(rating_min, rating_max, total_conspiracy_questions)

dat <- dat %>%
  mutate(
    populism_bin = cut(populism, breaks = populism_bins_and_labels$bins, labels = populism_bins_and_labels$labels),
    conspiracy_bin = cut(conspiracy, breaks = conspiracy_bins_and_labels$bins, labels = conspiracy_bins_and_labels$labels),
    mist = rowSums(select(., matches("^correct_mist_\\d+$")), na.rm = TRUE),
    ballotcount_diff = ballotcount_2_scale - ballotcount_scale,
    ballotcounty_diff = ballotcounty_2_scale - ballotcounty_scale,
    ballotcountry_diff = ballotcountry_2_scale - ballotcountry_scale,
    # CISA pre-treatment (baseline)
    cisa_fake = rowSums(select(., matches("^cisa[1-5]_scale$")), na.rm = TRUE) / 5,
    cisa_true = rowSums(select(., matches("^cisa[6-9]_scale$|^cisa_10_scale$")), na.rm = TRUE) / 5,
    # NOTE: CISA post-treatment questions were NOT asked in wave 1 immediate followup
    # They were only asked at baseline and in the recontact survey
    # Setting these to NA since the variables don't exist
    cisa_fake_2 = NA_real_,
    cisa_true_2 = NA_real_,
    cisa_rel_diff = prebunking4_scale - cisa_rel
  )

dat_recontact <- dat_recontact %>%
  mutate(
    cisa_fake_recontact = rowSums(select(., matches("^cisa[1-5]_scale_recontact$")), na.rm = TRUE)/5,
    cisa_true_recontact = rowSums(select(., matches("^cisa[6-9]_scale_recontact$|^cisa_10_scale_recontact$")), na.rm = TRUE)/5
  )

#### MERGE RECONTACT DATA ####
dat_final <- copy(dat)

dat_final <- dat_final %>%
  full_join(dat_recontact, by = "ID", suffix = c("", "_2"))

dat_final <- dat_final %>%
  mutate(
    ballotcount_diff_recontact = ballotcount_scale_recontact - ballotcount_scale,
    ballotcounty_diff_recontact = ballotcounty_scale_recontact - ballotcounty_scale,
    ballotcountry_diff_recontact = ballotcountry_scale_recontact - ballotcountry_scale,
    cisa_rel_diff_recontact = cisa_rel_recontact - cisa_rel,
    cisa_fake_diff_recontact = cisa_fake_recontact - cisa_fake,
    cisa_true_diff_recontact = cisa_true_recontact - cisa_true
  )


#### FILTERING ####
# Remove rows with missing values in any of the variables used in the model
complete_cases <- complete.cases(dat_final[,variables_in_model])

# Rename the variables in dat_final
dat_final <- rename_variables(dat_final, variable_labels)

write_full_processed_data <- !tolower(Sys.getenv("PREBUNK_WRITE_FULL_DATA", unset = "true")) %in% c("false", "0", "no")
if (write_full_processed_data) {
  fwrite(dat_final, file.path(data_dir, "prebunk_full.csv"))
}

# Create weighted and unweighted survey designs
svy_design_weighted <- svydesign(data = dat_final, weights = ~weight, id = ~1)
svy_design_unweighted <- svydesign(data = dat_final, weights = NULL, id = ~1)


covariate_labels <- c()
for (var in variables_in_model_labels) {
  var_levels <- levels(dat_final[[var]])[-1]
  if (!is.null(var_levels)){
    covariate_labels <- c(covariate_labels, var_levels)
    } else {
    covariate_labels <- c(covariate_labels, gsub("_", " ", var))
    }
}

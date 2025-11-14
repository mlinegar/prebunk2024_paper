# =============================================================================
# Plotting Functions
# =============================================================================
# This file contains all plotting and visualization code for the prebunking analysis.
# Extracted from prebunk.R (lines 877-1514, 2033-2136)
# =============================================================================

#### PLOT GROUP LEVEL TREATMENT EFFECTS ####

# For probability density plot
plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density_collapsed.pdf",
  weight_var = weight
)
save_plot_to_writing("confidence_country_change_density_collapsed.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density.pdf",
  weight_var = weight
)
save_plot_to_writing("confidence_country_change_density.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  by_var = Party_Identification,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density_party.pdf",
  weight_var = weight
)
save_plot_to_writing("confidence_country_change_density_party.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  by_var = Conspiracy_Bin,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and Conspiracy Beliefs",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density_conspiracy_bin.pdf",
  weight_var = weight
)
save_plot_to_writing("confidence_country_change_density_conspiracy_bin.pdf", width = 12, height = 8)

# Repeat for relevant CISA questions

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  # rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "cisa_change_density_collapsed.pdf",
  weight_var = weight
)
save_plot_to_writing("cisa_change_density_collapsed.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "cisa_change_density.pdf",
  weight_var = weight
)
save_plot_to_writing("cisa_change_density.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff,
  by_var = Party_Identification,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "cisa_change_density_party.pdf",
  weight_var = weight
)
save_plot_to_writing("cisa_change_density_party.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  # rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Followup - Pre)",
  output_file = "cisa_change_density_collapsed_recontact.pdf",
  weight_var = weight_recontact
)
save_plot_to_writing("cisa_change_density_collapsed_recontact.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Followup - Pre)",
  output_file = "cisa_change_density_recontact.pdf",
  weight_var = weight_recontact
)
save_plot_to_writing("cisa_change_density_recontact.pdf", width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff_Recontact,
  by_var = Party_Identification,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Followup - Pre)",
  output_file = "cisa_change_density_party_recontact.pdf",
  weight_var = weight_recontact
)
save_plot_to_writing("cisa_change_density_party_recontact.pdf", width = 12, height = 8)

#### PLOT PRE POST ####
pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Election_Rumor_Placebo_Randomization,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs. Post-Treatment",
  subtitle = "",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
save_plot_to_writing("pre_post_cisa_confidence.pdf", width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Human_In_The_Loop,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs. Post-Treatment",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
save_plot_to_writing("pre_post_cisa_confidence_hitl.pdf", width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs. Post-Treatment",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
save_plot_to_writing("pre_post_cisa_confidence_rumor.pdf", width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
save_plot_to_writing("pre_post_cisa_confidence_party.pdf", width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
save_plot_to_writing("pre_post_cisa_confidence_party_rumor.pdf", width = 12, height = 14)



pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment vs Followup",
  subtitle = "",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_cisa_confidence_recontact.pdf", width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment vs Followup",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_cisa_confidence_rumor_recontact.pdf", width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment vs Followup",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_cisa_confidence_party_recontact.pdf", width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment vs Followup",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_cisa_confidence_party_rumor_recontact.pdf", width = 12, height = 14)



pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Election_Rumor_Placebo_Randomization,
  # rumor_var = Election_Rumor_Randomization,
  title = "Confidence in National Election: Pre-Treatment vs. Post-Treatment",
  subtitle = "",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_confidence_country_ballots.pdf", width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in National Election: Pre-Treatment vs. Post-Treatment",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_confidence_country_ballots_rumor.pdf", width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  # rumor_var = Election_Rumor_Randomization,
  title = "Confidence in National Election: Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_confidence_country_ballots_party.pdf", width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in National Election: Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_confidence_country_ballots_rumor_party.pdf", width = 12, height = 14)


#### PLOT PRE POST DIFF PLOTS ####

pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  title = "Confidence in National Election: Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Treatment Status",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_confidence_country_ballots.pdf", width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in National Election: Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_confidence_country_ballots_rumor.pdf", width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  title = "Confidence in National Election: Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_confidence_country_ballots_party.pdf", width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in National Election: Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_confidence_country_ballots_rumor_party.pdf", width = 12, height = 14)



pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Treatment Status",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_cisa_confidence.pdf", width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_cisa_confidence_rumor.pdf", width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_cisa_confidence_party.pdf", width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor:
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
save_plot_to_writing("pre_post_diff_cisa_confidence_rumor_party.pdf", width = 12, height = 14)



pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Treatment Status",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_diff_cisa_confidence_recontact.pdf", width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_diff_cisa_confidence_rumor_recontact.pdf", width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor: Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_diff_cisa_confidence_party_recontact.pdf", width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("pre_post_diff_cisa_confidence_rumor_party_recontact.pdf", width = 12, height = 14)

#### TIME SERIES PLOTS ####


create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_country.pdf", width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  rumor_var = Election_Rumor_Randomization,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_country_rumor.pdf", width = 12, height = 8, units = "in")


create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_country_party.pdf", width = 12, height = 8, units = "in")


create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_country_rumor_party.pdf", width = 12, height = 8, units = "in")



create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_cisa.pdf", width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status and Rumor",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_cisa_rumor.pdf", width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status and Party Identification",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_cisa_party.pdf", width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Election_Rumor_Placebo_Randomization,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  by_var = Party_Identification,
  rumor_var = Election_Rumor_Randomization,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status, Rumor, and Party Identification",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
save_plot_to_writing("time_series_plot_cisa_rumor_party.pdf", width = 12, height = 8, units = "in")

#### COEFFICIENT PLOTS ####

# Prepare named model lists for weighted and unweighted outputs
pooled_labels <- c("Own Ballot", "County Ballots", "National Ballots")
pooled_followup_labels <- paste0(pooled_labels, " (Followup)")
cisa_labels <- c("Confidence", "Confidence (Recontact)",
                 "Confidence (Recontact)", "Confidence (Recontact)")

pooled_models_weighted_named <- setNames(pooled_models_weighted, pooled_labels)
pooled_followup_models_weighted_named <- setNames(pooled_followup_models_weighted, pooled_followup_labels)
pooled_models_unweighted_named <- setNames(pooled_models_unweighted, pooled_labels)
pooled_followup_models_unweighted_named <- setNames(pooled_followup_models_unweighted, pooled_followup_labels)

cisa_models_weighted_named <- setNames(cisa_models_weighted, cisa_labels)
cisa_models_unweighted_named <- setNames(cisa_models_unweighted, cisa_labels)

cisa_rumor_models_weighted_named <- setNames(cisa_rumor_models_weighted, rumors)
cisa_rumor_followup_models_weighted_named <- setNames(cisa_rumor_followup_models_weighted, rumors)
cisa_rumor_models_unweighted_named <- setNames(cisa_rumor_models_unweighted, rumors)
cisa_rumor_followup_models_unweighted_named <- setNames(cisa_rumor_followup_models_unweighted, rumors)

# Keep weighted models as the default objects used later in the script
pooled_models <- pooled_models_weighted_named
pooled_followup_models <- pooled_followup_models_weighted_named
cisa_models <- cisa_models_weighted_named
cisa_rumor_models <- cisa_rumor_models_weighted_named
cisa_rumor_followup_models <- cisa_rumor_followup_models_weighted_named

subtitle_with_refs <- function(is_weighted, refs = NULL) {
  base <- "Measured Immediately Post-Treatment and After One Week"
  if (is_weighted) {
    return(base)
  }
  if (is.null(refs) || refs == "") {
    return(paste0(base, " | Unweighted (SATE)"))
  }
  paste0(base, " | Unweighted (SATE); compare with ", refs)
}

refs_all <- "cisa_models_table_unweighted.tex, pooled_models_table_unweighted.tex, and recontact_pooled_models_table_unweighted.tex"
refs_main <- "cisa_models_table_unweighted.tex and recontact_pooled_models_table_unweighted.tex"
refs_election <- "pooled_models_table_unweighted.tex and recontact_pooled_models_table_unweighted.tex"
refs_rumors <- "cisa_rumor_models_table_unweighted.tex and cisa_rumors_followup_models_table_unweighted.tex"

for (weight_type in c("weighted", "unweighted")) {
  is_weighted <- weight_type == "weighted"
  suffix <- if (is_weighted) "" else "_unweighted"

  pooled_models_current <- if (is_weighted) pooled_models_weighted_named else pooled_models_unweighted_named
  pooled_followup_models_current <- if (is_weighted) pooled_followup_models_weighted_named else pooled_followup_models_unweighted_named
  cisa_models_current <- if (is_weighted) cisa_models_weighted_named else cisa_models_unweighted_named
  cisa_rumor_models_current <- if (is_weighted) cisa_rumor_models_weighted_named else cisa_rumor_models_unweighted_named
  cisa_rumor_followup_models_current <- if (is_weighted) cisa_rumor_followup_models_weighted_named else cisa_rumor_followup_models_unweighted_named

  all_models <- list(
    "National Ballots" = list(
      "Confidence" = pooled_models_current[[3]],
      "Confidence (Recontact)" = pooled_followup_models_current[[3]]
    ),
    "County Ballots" = list(
      "Confidence" = pooled_models_current[[2]],
      "Confidence (Recontact)" = pooled_followup_models_current[[2]]
    ),
    "Own Ballot" = list(
      "Confidence" = pooled_models_current[[1]],
      "Confidence (Recontact)" = pooled_followup_models_current[[1]]
    ),
    "Assigned Rumor (Pooled)" = cisa_models_current[1:2],
    "All Rumors (Recontact)" = cisa_models_current[3],
    "Election Facts (Recontact)" = cisa_models_current[4],
    "Voter Fraud" = list(
      "Confidence" = cisa_rumor_models_current[[1]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[[1]]
    ),
    "Voter Rolls" = list(
      "Confidence" = cisa_rumor_models_current[[2]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[[2]]
    ),
    "Hacking" = list(
      "Confidence" = cisa_rumor_models_current[[3]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[[3]]
    ),
    "Blue Shift" = list(
      "Confidence" = cisa_rumor_models_current[[4]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[[4]]
    ),
    "Voting Machines" = list(
      "Confidence" = cisa_rumor_models_current[[5]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[[5]]
    )
  )

  cisa_plot <- plot_coefficients(
    all_models,
    title = "Treatment Effect Estimates for All OLS Models",
    subtitle = subtitle_with_refs(is_weighted, refs_all),
    y_label = "Model"
  )

  print(cisa_plot)
  save_plot_to_writing(
    paste0("treatment_effects_plot_all", suffix, ".pdf"),
    cisa_plot,
    width = 15,
    height = 12,
    units = "in"
  )

  individual_rumors <- list(
    "Voter Fraud" = list(
      "Confidence" = cisa_rumor_models_current[["Voter Fraud"]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[["Voter Fraud"]]
    ),
    "Voter Rolls" = list(
      "Confidence" = cisa_rumor_models_current[["Voter Rolls"]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[["Voter Rolls"]]
    ),
    "Hacking" = list(
      "Confidence" = cisa_rumor_models_current[["Hacking"]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[["Hacking"]]
    ),
    "Blue Shift" = list(
      "Confidence" = cisa_rumor_models_current[["Blue Shift"]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[["Blue Shift"]]
    ),
    "Voting Machines" = list(
      "Confidence" = cisa_rumor_models_current[["Voting Machines"]],
      "Confidence (Recontact)" = cisa_rumor_followup_models_current[["Voting Machines"]]
    )
  )

  individual_rumors_plot <- plot_coefficients(
    individual_rumors,
    title = "Treatment Effect Estimates
    Confidence that: Election Rumor is True",
    subtitle = subtitle_with_refs(is_weighted, refs_rumors),
    y_label = "Rumor Model",
    debug = TRUE
  )

  print(individual_rumors_plot)
  save_plot_to_writing(
    paste0("treatment_effects_plot_rumors", suffix, ".pdf"),
    individual_rumors_plot,
    width = 15,
    height = 12,
    units = "in"
  )

  main_models <- list(
    "Assigned Rumor (Pooled)" = list(
      "Confidence" = cisa_models_current[1],
      "Confidence (Recontact)" = cisa_models_current[2]
    ),
    "National Ballots" = list(
      "Confidence" = pooled_models_current[[3]],
      "Confidence (Recontact)" = pooled_followup_models_current[[3]]
    )
  )

  main_models_plot <- plot_coefficients(
    main_models,
    title = "Treatment Effect Estimates
  Confidence that: Election Rumors are True, Ballots are Accurately Counted Nationally",
    subtitle = subtitle_with_refs(is_weighted, refs_main),
    y_label = "Model",
    debug = TRUE
  )

  print(main_models_plot)
  save_plot_to_writing(
    paste0("treatment_effects_plot_main", suffix, ".pdf"),
    main_models_plot,
    width = 15,
    height = 12,
    units = "in"
  )

  election_models <- list(
    "National Ballots" = list(
      "Confidence" = pooled_models_current[[3]],
      "Confidence (Recontact)" = pooled_followup_models_current[[3]]
    ),
    "County Ballots" = list(
      "Confidence" = pooled_models_current[[2]],
      "Confidence (Recontact)" = pooled_followup_models_current[[2]]
    ),
    "Own Ballot" = list(
      "Confidence" = pooled_models_current[[1]],
      "Confidence (Recontact)" = pooled_followup_models_current[[1]]
    )
  )

  election_models_plot <- plot_coefficients(
    election_models,
    title = "Treatment Effect Estimates\nBallot Confidence Outcomes",
    subtitle = subtitle_with_refs(is_weighted, refs_election),
    y_label = "Model",
    debug = TRUE
  )

  print(election_models_plot)
  save_plot_to_writing(
    paste0("treatment_effects_plot_election", suffix, ".pdf"),
    election_models_plot,
    width = 15,
    height = 12,
    units = "in"
  )
}

# =============================================================================
# INTERACTION EFFECT PLOTS
# =============================================================================
# Plot heterogeneous treatment effects showing how effects vary by
# Party ID, Ideology, Conspiracy Score, and other moderators
# These plots show BOTH main treatment effects (reference group ATE) AND
# combined effects for each subgroup (main + interaction)

cat("\n=== Generating interaction effect plots ===\n")

# Source the interaction plotting functions
source(file.path(project_root, "functions", "plotting_interactions.R"))

figure_dir <- get_writing_path("figures")
table_dir <- get_writing_path("tables")

# Generate all individual interaction plots (one per moderator)
# This creates: interaction_effects_partyidentification_weighted.pdf, etc.
all_int_plots <- create_all_interaction_plots(
  int_vars = int_vars,
  weight_type = "weighted",
  output_dir = figure_dir,
  width = 10,
  height = 8
)

party_fig_path <- file.path(figure_dir, "interaction_effects_summary_weighted.pdf")
party_table_path <- file.path(table_dir, "interaction_effects_summary_weighted.tex")
conspiracy_fig_path <- file.path(figure_dir, "interaction_effects_conspiracy_weighted.pdf")
conspiracy_table_path <- file.path(table_dir, "interaction_effects_conspiracy_weighted.tex")
rumor_fig_path <- file.path(figure_dir, "interaction_effects_rumor_summary_weighted.pdf")
rumor_table_path <- file.path(table_dir, "interaction_effects_rumor_summary_weighted.tex")

# Generate summary plots for main text showing key moderators
# Party ID & Ideology (Figure 5)
create_interaction_summary_plot(
  focus_moderators = c("Party_Identification", "Ideology"),
  weight_type = "weighted",
  output_file = party_fig_path,
  width = 12,
  height = 10,
  table_file = party_table_path,
  table_caption = "Estimated treatment effects on national ballot confidence, by party identification and ideology (weighted models).",
  table_label = "tab:interaction_summary_weighted"
)
# Party & Ideology interactions for rumor belief (Figure X)
create_interaction_summary_plot(
  focus_moderators = c("Party_Identification", "Ideology"),
  focus_outcome = "Rumor_Post",
  weight_type = "weighted",
  output_file = rumor_fig_path,
  width = 12,
  height = 10,
  table_file = rumor_table_path,
  table_caption = "Estimated treatment effects on rumor confidence, by party identification and ideology (weighted models).",
  table_label = "tab:interaction_summary_rumor_weighted"
)
# Conspiracy Score (separate appendix figure/table)
create_interaction_summary_plot(
  focus_moderators = c("Conspiracy_Score"),
  weight_type = "weighted",
  output_file = conspiracy_fig_path,
  width = 10,
  height = 6,
  table_file = conspiracy_table_path,
  table_caption = "Estimated treatment effects on national ballot confidence, by conspiracy score (weighted models).",
  table_label = "tab:interaction_conspiracy_weighted"
)
cat("✓ Interaction plots generated successfully\n")

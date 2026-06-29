# =============================================================================
# Interaction Effect Plotting Functions
# =============================================================================
# Functions to visualize heterogeneous treatment effects from interaction models
# Extracts both main treatment effects and interaction terms to show
# treatment effects within each subgroup
# =============================================================================

library(broom)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(tidyr)
library(forcats)
library(forcats)

clean_interaction_display_name <- function(x) {
  x <- str_replace_all(x, "_", " ")
  x <- str_replace_all(x, "([a-z])([A-Z])", "\\1 \\2")
  x <- str_squish(x)

  case_when(
    str_detect(x, regex("^party identification$", ignore_case = TRUE)) ~ "Party Identification",
    str_detect(x, regex("^political interest$", ignore_case = TRUE)) ~ "Political Interest",
    str_detect(x, regex("^mist correct$", ignore_case = TRUE)) ~ "MIST-8 Score",
    str_detect(x, regex("^populism score$", ignore_case = TRUE)) ~ "Populism Score",
    str_detect(x, regex("^conspiracy score$", ignore_case = TRUE)) ~ "Conspiracy Belief",
    str_detect(x, regex("^human in the loop$", ignore_case = TRUE)) ~ "Human-in-the-Loop Status",
    TRUE ~ tools::toTitleCase(tolower(x))
  )
}

clean_interaction_outcome_name <- function(x) {
  case_when(
    x == "Post_Confidence_Country_Ballots" ~ "National Ballot\nConfidence",
    x == "Post_Confidence_County_Ballots" ~ "County Ballot\nConfidence",
    x == "Post_Confidence_Own_Ballot" ~ "Own Ballot\nConfidence",
    x == "Rumor_Post" ~ "Rumor\nConfidence",
    x == "Rumor_Recontact" ~ "Rumor Confidence\n(Recontact)",
    TRUE ~ str_replace_all(x, "_", " ") %>% str_wrap(24)
  )
}

clean_interaction_group_name <- function(x) {
  x <- str_replace_all(x, "_", " ")
  x <- str_replace(x, regex("^Pol Interest:\\s*", ignore_case = TRUE), "")
  x <- str_replace(x, regex("^Political Interest:\\s*", ignore_case = TRUE), "")
  str_squish(x)
}

#' Plot Interaction Coefficients
#'
#' Creates coefficient plots showing how treatment effects vary by moderator.
#' Extracts BOTH the main treatment effect (baseline/reference group) AND
#' the interaction terms to show complete picture of heterogeneous effects.
#'
#' @param model_list List of regression models with interaction terms
#' @param interaction_var_name Name of the moderating variable (e.g., "Party_Identification")
#' @param treatment_var_name Name of the treatment variable (defaults to value from config.R)
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param show_combined If TRUE, calculate and show combined effects (main + interaction) for each group
#' @param point_size Size of coefficient points
#' @param text_size Base text size
#' @return ggplot object
plot_interaction_coefficients <- function(model_list,
                                          interaction_var_name,
                                          treatment_var_name = NULL,
                                          title = "Heterogeneous Treatment Effects",
                                          subtitle = NULL,
                                          show_combined = TRUE,
                                          point_size = NULL,
                                          text_size = NULL) {

  defaults <- get_coef_plot_defaults()
  font_sizes <- get_plot_font_sizes()
  if (is.null(point_size)) point_size <- defaults$point_size
  if (is.null(text_size)) text_size <- defaults$text_size
  axis_title_size <- max(18, font_sizes$axis_title * 0.72)
  axis_text_size <- max(14, font_sizes$axis_text * 0.66)
  title_size <- max(22, font_sizes$title * 0.80)
  subtitle_size <- max(16, font_sizes$subtitle * 0.82)
  legend_title_size <- max(16, font_sizes$legend_title * 0.75)
  legend_text_size <- max(15, font_sizes$legend_text * 0.72)
  strip_text_size <- max(17, font_sizes$strip_text * 0.68)
  wrap_width_current <- defaults$wrap_width
  interaction_display_name <- clean_interaction_display_name(interaction_var_name)
  plot_subtitle <- if (is.null(subtitle)) {
    paste("Treatment Effects by", interaction_display_name)
  } else {
    subtitle
  }
  title <- format_plot_title_lines(title)
  plot_subtitle <- format_plot_title_lines(plot_subtitle)

  # Use treatment variable name from config if not specified
  if (is.null(treatment_var_name)) {
    if (exists("treatment_var_name", envir = .GlobalEnv)) {
      treatment_var_name <- get("treatment_var_name", envir = .GlobalEnv)
    } else {
      stop("treatment_var_name not found in config.R and not specified. Please add to config.R.")
    }
  }

  # Extract interaction coefficients from a single model
  extract_interaction_coefs <- function(model, model_name) {
    if (is.null(model) || !inherits(model, "lm")) {
      return(NULL)
    }

    coef_data <- tidy(model)

    # Find main treatment effect (baseline subgroup)
    main_effect <- coef_data %>%
      filter(term == treatment_var_name) %>%
      mutate(
        outcome = model_name,
        group = "Treatment Effect",
        effect_type = "Main Effect",
        term_clean = "Treatment Effect"
      )

    # Find interaction terms
    interaction_pattern <- paste0(treatment_var_name, ":", interaction_var_name)
    interaction_effects <- coef_data %>%
      filter(str_detect(term, fixed(interaction_pattern))) %>%
      mutate(
        outcome = model_name,
        group = str_remove(term, paste0(".*", interaction_var_name)),
        effect_type = "Interaction",
        term_clean = str_remove(term, paste0(treatment_var_name, ":"))
      )

    # Combine
    all_effects <- bind_rows(main_effect, interaction_effects)

    if (nrow(all_effects) == 0) {
      return(NULL)
    }

    # Calculate combined effects if requested
    if (show_combined && nrow(main_effect) > 0 && nrow(interaction_effects) > 0) {
      main_est <- main_effect$estimate[1]
      main_se <- main_effect$std.error[1]

      combined_effects <- interaction_effects %>%
        mutate(
          combined_estimate = estimate + main_est,
          # Approximate SE for sum (assumes independence, conservative)
          combined_se = sqrt(std.error^2 + main_se^2),
          effect_type = "Combined (Main + Interaction)"
        )

      all_effects <- bind_rows(all_effects, combined_effects)
    }

    # Add significance and clean labels
    all_effects <- all_effects %>%
      mutate(
        estimate_to_plot = if_else(effect_type == "Combined (Main + Interaction)",
                                    combined_estimate, estimate),
        se_to_plot = if_else(effect_type == "Combined (Main + Interaction)",
                             combined_se, std.error),
        ci_lower = estimate_to_plot - 1.96 * se_to_plot,
        ci_upper = estimate_to_plot + 1.96 * se_to_plot,
        significant = case_when(
          ci_lower > 0 ~ "Positive",
          ci_upper < 0 ~ "Negative",
          TRUE ~ "Not Significant"
        )
      )

    return(all_effects)
  }

  # Process all models
  all_coefs <- map_dfr(names(model_list), function(model_name) {
    extract_interaction_coefs(model_list[[model_name]], model_name)
  })

  if (is.null(all_coefs) || nrow(all_coefs) == 0) {
    warning("No interaction coefficients found in models")
    return(NULL)
  }

  wrap_width_current <- defaults$wrap_width
  label_count <- length(unique(all_coefs$term))
  if (label_count > 8) {
    axis_text_size <- axis_text_size * 0.9
    wrap_width_current <- max(22, wrap_width_current - 4)
  }

  # Create clean labels for plotting
  all_coefs <- all_coefs %>%
    mutate(
      # Clean up outcome names
      outcome_clean = str_remove(outcome, "Pooled_|Recontact_") %>%
        clean_interaction_outcome_name(),
      # Clean up group names
      group_label = if_else(
        effect_type == "Main Effect",
        "Treatment Effect",
        clean_interaction_group_name(group)
      ),
      group_label = format_coef_labels(group_label, wrap_width = wrap_width_current),
      term_clean = format_coef_labels(clean_interaction_group_name(term_clean),
                                      wrap_width = wrap_width_current),
      # Order by outcome and effect type
      plot_order = paste(outcome, effect_type, group)
    )

  # Create separate plots for each effect type if showing combined
  if (show_combined) {
    # Plot combined effects (most interpretable)
    plot_data <- all_coefs %>%
      filter(effect_type %in% c("Main Effect", "Combined (Main + Interaction)")) %>%
      mutate(
        group_label = if_else(
          effect_type == "Main Effect",
          "Treatment Effect",
          clean_interaction_group_name(group)
        ),
        group_label = format_coef_labels(group_label, wrap_width = wrap_width_current)
      )

    p <- ggplot(plot_data, aes(x = estimate_to_plot, y = group_label,
                                color = significant)) +
      geom_vline(
        xintercept = 0,
        linetype = defaults$vline_linetype,
        color = defaults$vline_color,
        size = 0.6
      ) +
      geom_point(size = point_size) +
      geom_errorbarh(
        aes(xmin = ci_lower, xmax = ci_upper),
        height = defaults$error_bar_height,
        size = 0.6
      ) +
      facet_wrap(~ outcome_clean, ncol = 2, scales = "free_y") +
      labs(
        x = "Treatment Effect Estimate",
        y = NULL,
        title = title,
        subtitle = plot_subtitle,
        color = "Significance"
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
      scale_color_manual(values = defaults$color_values) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title = element_text(face = "bold", size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        plot.title = element_text(face = "bold", hjust = 0, size = title_size),
        plot.subtitle = element_text(hjust = 0, size = subtitle_size),
        strip.text = element_text(face = "bold", size = strip_text_size),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        panel.spacing = unit(1.4, "lines"),
        plot.title.position = "plot",
        plot.margin = margin(18, 36, 18, 24)
      )

  } else {
    # Plot raw coefficients (main effects and interactions separately)
    p <- ggplot(all_coefs, aes(x = estimate_to_plot, y = term_clean,
                                color = significant, shape = effect_type)) +
      geom_vline(
        xintercept = 0,
        linetype = defaults$vline_linetype,
        color = defaults$vline_color,
        size = 0.6
      ) +
      geom_point(size = point_size) +
      geom_errorbarh(
        aes(xmin = ci_lower, xmax = ci_upper),
        height = defaults$error_bar_height,
        size = 0.6
      ) +
      facet_wrap(~ outcome_clean, ncol = 2, scales = "free_y") +
      labs(
        x = "Coefficient Estimate",
        y = NULL,
        title = title,
        subtitle = plot_subtitle,
        color = "Significance",
        shape = "Effect Type"
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
      scale_color_manual(values = defaults$color_values) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", size = axis_title_size),
        axis.text = element_text(size = axis_text_size),
        plot.title = element_text(face = "bold", hjust = 0, size = title_size),
        plot.subtitle = element_text(hjust = 0, size = subtitle_size),
        strip.text = element_text(face = "bold", size = strip_text_size),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = legend_title_size),
        legend.text = element_text(size = legend_text_size),
        panel.spacing = unit(1.4, "lines"),
        plot.title.position = "plot",
        plot.margin = margin(18, 36, 18, 24)
      )
  }

  return(p)
}


#' Plot Interaction Coefficients Across Multiple Moderators
#'
#' Creates a comparison plot showing interaction effects across different
#' moderating variables (e.g., comparing Party ID vs Ideology vs Conspiracy)
#'
#' @param model_lists Named list of model lists, one per moderator
#' @param outcome_to_plot Which outcome to focus on (e.g., "Post_Confidence_Country_Ballots")
#' @param treatment_var_name Name of the treatment variable (defaults to value from config.R)
#' @return ggplot object
plot_interaction_comparison <- function(model_lists,
                                       outcome_to_plot,
                                       treatment_var_name = NULL,
                                       title = "Interaction Effects Across Moderators",
                                       point_size = NULL,
                                       text_size = NULL) {

  defaults <- get_coef_plot_defaults()
  font_sizes <- get_plot_font_sizes()
  if (is.null(point_size)) point_size <- defaults$point_size
  if (is.null(text_size)) text_size <- defaults$text_size
  axis_title_size <- max(font_sizes$axis_title, text_size)
  axis_text_size <- max(font_sizes$axis_text, text_size * 0.9)
  title_size <- max(font_sizes$title - 2, text_size * 1.25)
  subtitle_size <- max(font_sizes$subtitle - 4, text_size * 0.9)
  legend_title_size <- max(font_sizes$legend_title, text_size * 0.85)
  legend_text_size <- max(font_sizes$legend_text, text_size * 0.8)
  strip_text_size <- max(font_sizes$strip_text, text_size)
  title <- format_plot_title_lines(title)

  # Use treatment variable name from config if not specified
  if (is.null(treatment_var_name)) {
    if (exists("treatment_var_name", envir = .GlobalEnv)) {
      treatment_var_name <- get("treatment_var_name", envir = .GlobalEnv)
    } else {
      stop("treatment_var_name not found in config.R. Please add to config.R.")
    }
  }

  # Extract interaction coefficients for specified outcome
  extract_for_outcome <- function(model_list, moderator_name, outcome_name) {
    model <- model_list[[paste0("Pooled_", outcome_name)]]
    if (is.null(model)) return(NULL)

    coef_data <- tidy(model)

    # Get interaction terms
    interactions <- coef_data %>%
      filter(str_detect(term, paste0(treatment_var_name, ":"))) %>%
      # Exclude rumor interactions, focus on moderator interactions
      filter(!str_detect(term, "Election_Rumor_Randomization")) %>%
      mutate(
        moderator = moderator_name,
        group = str_extract(term, "[^:]+$"),
        ci_lower = estimate - 1.96 * std.error,
        ci_upper = estimate + 1.96 * std.error,
        significant = case_when(
          ci_lower > 0 ~ "Positive",
          ci_upper < 0 ~ "Negative",
          TRUE ~ "Not Significant"
        ),
        group_clean = format_coef_labels(str_replace_all(group, "_", " "),
                                         wrap_width = defaults$wrap_width)
      )

    return(interactions)
  }

  # Process all moderators
  all_interactions <- map_dfr(names(model_lists), function(mod_name) {
    extract_for_outcome(model_lists[[mod_name]], mod_name, outcome_to_plot)
  })

  if (is.null(all_interactions) || nrow(all_interactions) == 0) {
    warning("No interaction coefficients found")
    return(NULL)
  }

  if (length(unique(all_interactions$group_clean)) > 6) {
    axis_text_size <- axis_text_size * 0.9
  }

  # Plot
  p <- ggplot(all_interactions, aes(x = estimate, y = group_clean, color = significant)) +
    geom_vline(
      xintercept = 0,
      linetype = defaults$vline_linetype,
      color = defaults$vline_color,
      size = 0.6
    ) +
    geom_point(size = point_size) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = defaults$error_bar_height,
      size = 0.6
    ) +
    facet_wrap(~ moderator, ncol = 1, scales = "free_y") +
    labs(
      x = "Interaction Coefficient",
      y = NULL,
      title = title,
      subtitle = format_plot_title_lines(paste("Outcome:", str_replace_all(outcome_to_plot, "_", " "))),
      color = "Significance"
    ) +
    scale_color_manual(values = defaults$color_values) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", size = axis_title_size),
      axis.text = element_text(size = axis_text_size),
      plot.title = element_text(face = "bold", hjust = 0, size = title_size),
      plot.subtitle = element_text(hjust = 0, size = subtitle_size),
      strip.text = element_text(face = "bold", size = strip_text_size),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = legend_title_size),
      legend.text = element_text(size = legend_text_size),
      plot.margin = margin(18, 18, 18, 18)
    )

  return(p)
}


#' Create All Interaction Plots
#'
#' Wrapper function to generate all needed interaction plots from
#' the global environment where interaction models are stored
#'
#' @param int_vars Vector of interaction variable names (from config)
#' @param weight_type "weighted" or "unweighted"
#' @param output_dir Directory to save plots
#' @param width Plot width in inches
#' @param height Plot height in inches
create_all_interaction_plots <- function(int_vars,
                                        weight_type = "weighted",
                                        output_dir = ".",
                                        width = 10,
                                        height = 8) {

  cat(sprintf("\n=== Generating %s interaction plots ===\n", weight_type))

  plot_list <- list()

  for (int_var_name in int_vars) {
    cat(sprintf("  Creating plot for %s...\n", int_var_name))

    # Create safe filename suffix (same logic as modeling_interactions.R)
    suffix <- tolower(gsub("_", "", int_var_name))

    # Get the model list from global environment
    model_var_name <- paste0("int_pooled_models_", suffix, "_", weight_type)

    if (!exists(model_var_name, envir = .GlobalEnv)) {
      cat(sprintf("    Warning: %s not found in environment, skipping\n", model_var_name))
      next
    }

    model_list <- get(model_var_name, envir = .GlobalEnv)

    # Create the plot
    plot_obj <- plot_interaction_coefficients(
      model_list = model_list,
      interaction_var_name = int_var_name,
      title = sprintf("Heterogeneous Treatment Effects\nby %s",
                      clean_interaction_display_name(int_var_name)),
      subtitle = paste0("(", tools::toTitleCase(weight_type), " Models)"),
      show_combined = TRUE
    )

    if (!is.null(plot_obj)) {
      # Save plot
      filename <- file.path(
        output_dir,
        sprintf("interaction_effects_%s_%s.pdf", suffix, weight_type)
      )
      ggsave(filename, plot_obj, width = width, height = height)
      cat(sprintf("    ✓ Saved: %s\n", filename))

      # Store in list for return
      plot_list[[int_var_name]] <- plot_obj
    }
  }

  cat(sprintf("✓ Generated %d interaction plots\n", length(plot_list)))

  return(invisible(plot_list))
}


#' Create Interaction Summary Plot
#'
#' Creates a single comprehensive plot showing key interaction effects
#' for main text (e.g., Party ID, Ideology, Conspiracy Score for national ballot confidence)
#'
#' @param focus_moderators Vector of moderator names to include (defaults to first 3 in int_vars)
#' @param focus_outcome Which outcome to plot (defaults to national ballot confidence)
#' @param weight_type "weighted" or "unweighted"
create_interaction_summary_plot <- function(focus_moderators = NULL,
                                            focus_outcome = NULL,
                                            weight_type = "weighted",
                                            output_file = "interaction_effects_summary.pdf",
                                            width = 12,
                                            height = 10,
                                            table_file = NULL,
                                            table_caption = NULL,
                                            table_label = NULL) {

  cat("\n=== Creating interaction summary plot ===\n")
  defaults <- get_coef_plot_defaults()
  font_sizes <- get_plot_font_sizes()
  text_size <- defaults$text_size
  axis_title_size <- max(font_sizes$axis_title, text_size)
  axis_text_y_size <- max(font_sizes$axis_text, text_size * 0.9)
  axis_text_x_size <- max(font_sizes$axis_text, text_size * 0.85)
  title_size <- max(font_sizes$title - 4, text_size * 1.1)
  subtitle_size <- max(font_sizes$subtitle - 2, text_size * 0.82)
  strip_text_size <- max(font_sizes$strip_text, text_size)
  legend_title_size <- max(font_sizes$legend_title, text_size * 0.85)
  legend_text_size <- max(font_sizes$legend_text, text_size * 0.8)

  # Use defaults from config if not specified
  if (is.null(focus_moderators)) {
    # Use int_vars_main_text from config.R
    if (exists("int_vars_main_text", envir = .GlobalEnv)) {
      focus_moderators <- get("int_vars_main_text", envir = .GlobalEnv)
      cat(sprintf("  Using moderators from int_vars_main_text: %s\n",
                  paste(focus_moderators, collapse = ", ")))
    } else {
      stop("int_vars_main_text not found in config.R. Please add it or specify focus_moderators explicitly.")
    }
  }

  if (is.null(focus_outcome)) {
    # Use primary_outcome from config.R
    if (exists("primary_outcome", envir = .GlobalEnv)) {
      focus_outcome <- get("primary_outcome", envir = .GlobalEnv)
      cat(sprintf("  Using primary outcome from config.R: %s\n", focus_outcome))
    } else {
      stop("primary_outcome not found in config.R. Please add it or specify focus_outcome explicitly.")
    }
  }

  all_coefs <- list()
  controls_text <- NULL
  primary_outcome_value <- if (exists("primary_outcome", envir = .GlobalEnv)) get("primary_outcome", envir = .GlobalEnv) else NULL

  outcome_display <- dplyr::case_when(
    focus_outcome == "Post_Confidence_Country_Ballots" ~ "National Ballot Confidence",
    focus_outcome == "Post_Confidence_County_Ballots" ~ "County Ballot Confidence",
    focus_outcome == "Post_Confidence_Own_Ballot" ~ "Own Ballot Confidence",
    focus_outcome == "Rumor_Post" ~ "Rumor Confidence",
    focus_outcome == "Rumor_Recontact" ~ "Rumor Confidence (Recontact)",
    TRUE ~ stringr::str_replace_all(focus_outcome, "_", " ")
  )
  axis_x_label <- sprintf("Treatment Effect on %s", outcome_display)
  plot_title <- if (length(focus_moderators) == 1 && identical(focus_moderators, "Conspiracy_Score")) {
    "Heterogeneous Treatment Effects\nBy Conspiracy Belief"
  } else {
    sprintf("Heterogeneous Treatment Effects\n%s", outcome_display)
  }
  default_caption <- sprintf("Estimated treatment effects on %s by moderator (weighted models).", tolower(outcome_display))
  x_limits <- if (focus_outcome %in% c("Rumor_Post", "Rumor_Recontact")) c(-1.25, 0.75) else c(-1, 1)

  if (exists("variables_in_model_labels", envir = .GlobalEnv)) {
    controls_raw <- get("variables_in_model_labels", envir = .GlobalEnv)
    if (exists("format_coefficient_labels", envir = .GlobalEnv)) {
      controls_pretty <- format_coefficient_labels(controls_raw)
    } else {
      controls_pretty <- controls_raw
    }
    controls_text <- paste(controls_pretty, collapse = ", ")
  }

  for (mod_name in focus_moderators) {
    # Create safe filename suffix (same logic as modeling_interactions.R)
    suffix <- tolower(gsub("_", "", mod_name))
    model_var_name <- paste0("int_pooled_models_", suffix, "_", weight_type)

    if (!exists(model_var_name, envir = .GlobalEnv)) {
      warning(sprintf("%s not found, skipping", model_var_name))
      next
    }

    model_list <- get(model_var_name, envir = .GlobalEnv)
    model <- model_list[[paste0("Pooled_", focus_outcome)]]

    if (is.null(model)) next

    coef_data <- tidy(model)

    # Get main effect
    main_effect <- coef_data %>%
      filter(term == "Election_Rumor_Placebo_RandomizationTreatment") %>%
      mutate(
        moderator = mod_name,
        group = "Reference",
        is_reference = TRUE
      )

    # Get interactions (excluding rumor interactions)
    int_effects <- coef_data %>%
      filter(str_detect(term, "Election_Rumor_Placebo_RandomizationTreatment:")) %>%
      filter(!str_detect(term, "Election_Rumor_Randomization")) %>%
      mutate(
        moderator = mod_name,
        group = str_remove(term, ".*:") %>% str_remove(paste0("^", mod_name)),
        is_reference = FALSE,
        # Calculate combined effect
        combined_est = estimate + main_effect$estimate[1],
        combined_se = sqrt(std.error^2 + main_effect$std.error[1]^2)
      )

    all_coefs[[mod_name]] <- bind_rows(
      main_effect %>% mutate(combined_est = estimate, combined_se = std.error),
      int_effects
    )
  }

  if (length(all_coefs) == 0) {
    warning("No coefficients extracted")
    return(NULL)
  }

  model_label <- if (identical(weight_type, "weighted")) {
    "Survey-weighted"
  } else {
    tools::toTitleCase(weight_type)
  }
  subtitle_text <- paste(
    sprintf("%s OLS; baseline: Democrat/Liberal or moderator = 0.",
            model_label),
    "Controls: baseline outcome, rumor FE, demographics, attitudes.",
    sep = "\n"
  )

  summary_wrap_width <- if (length(focus_moderators) == 1) 22 else defaults$wrap_width

  plot_data <- bind_rows(all_coefs) %>%
    mutate(
      ci_lower = combined_est - 1.96 * combined_se,
      ci_upper = combined_est + 1.96 * combined_se,
      significant = case_when(
        ci_lower > 0 ~ "Positive",
        ci_upper < 0 ~ "Negative",
        TRUE ~ "Not Significant"
      ),
      moderator_clean = str_replace_all(moderator, "_", " "),
      derived_group = case_when(
        is_reference ~ "Treatment Effect",
        group %in% c("", NA) ~ paste0("Treatment × ", str_replace_all(moderator, "_", " "), " (per 1-unit increase)"),
        TRUE ~ paste0("Treatment × ", str_replace_all(group, "_", " "))
      ),
      derived_group = format_coef_labels(derived_group, wrap_width = summary_wrap_width),
      derived_group = fct_inorder(derived_group)
    ) %>%
    group_by(moderator_clean) %>%
    mutate(
      derived_group = factor(derived_group,
        levels = c("Treatment Effect", setdiff(unique(derived_group), "Treatment Effect"))
      ),
      derived_group_plot = fct_rev(derived_group)
    ) %>%
    ungroup()

  compact_summary <- dplyr::n_distinct(plot_data$moderator_clean) == 1
  max_rows <- plot_data %>%
    count(moderator_clean) %>%
    pull(n) %>%
    {if (length(.) == 0) 0 else max(.)}
  if (max_rows > 6) {
    axis_text_y_size <- axis_text_y_size * 0.9
    axis_text_x_size <- axis_text_x_size * 0.95
  }
  if (compact_summary) {
    axis_title_size <- axis_title_size * 0.9
    axis_text_y_size <- axis_text_y_size * 0.9
    axis_text_x_size <- axis_text_x_size * 0.95
    title_size <- title_size * 0.95
    subtitle_size <- subtitle_size * 0.95
    strip_text_size <- strip_text_size * 0.95
    legend_title_size <- legend_title_size * 0.95
    legend_text_size <- legend_text_size * 0.95
  }

  plot_title <- format_plot_title_lines(plot_title, max_width = 60)
  subtitle_text <- format_plot_title_lines(subtitle_text, max_width = 80)
  axis_x_label <- format_plot_title_lines(axis_x_label, max_width = if (compact_summary) 30 else 28)

  p <- ggplot(plot_data, aes(x = combined_est, y = derived_group_plot, color = significant)) +
    geom_vline(
      xintercept = 0,
      linetype = defaults$vline_linetype,
      color = defaults$vline_color,
      size = 0.6
    ) +
    geom_point(size = defaults$point_size) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = defaults$error_bar_height,
      size = 0.6
    ) +
    facet_wrap(~ moderator_clean, ncol = 1, scales = "free_y") +
    labs(
      x = axis_x_label,
      y = NULL,
      title = plot_title,
      subtitle = subtitle_text,
      color = "Significance"
    ) +
    scale_color_manual(
      values = defaults$color_values,
      guide = guide_legend(override.aes = list(size = defaults$point_size))
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x = element_text(face = "bold", size = axis_title_size, margin = margin(t = 10)),
      axis.text.y = element_text(size = axis_text_y_size),
      axis.text.x = element_text(size = axis_text_x_size),
      plot.title = element_text(face = "bold", hjust = 0, size = title_size, margin = margin(b = 6)),
      plot.subtitle = element_text(hjust = 0, size = subtitle_size, margin = margin(b = 15)),
      strip.text = element_text(face = "bold", size = strip_text_size, margin = margin(b = 5)),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = legend_title_size),
      legend.text = element_text(size = legend_text_size),
      panel.spacing = unit(1.5, "lines"),
      plot.title.position = "plot",
      plot.margin = margin(14, 24, 12, 18)
    ) +
  coord_cartesian(xlim = x_limits, clip = "off")

  if (!is.null(table_file)) {
    caption_value <- if (is.null(table_caption)) {
      default_caption
    } else {
      table_caption
    }

    table_data <- plot_data %>%
      arrange(moderator_clean, as.integer(derived_group)) %>%
      mutate(
        Moderator = moderator_clean,
        Group = as.character(derived_group),
        Estimate = sprintf("%.3f", combined_est),
        `CI Lower` = sprintf("%.3f", ci_lower),
        `CI Upper` = sprintf("%.3f", ci_upper),
        Significance = case_when(
          significant == "Positive" ~ "Positive ($p < 0.05$)",
          significant == "Negative" ~ "Negative ($p < 0.05$)",
          TRUE ~ "Not significant"
        )
      ) %>%
      select(Moderator, Group, Estimate, `CI Lower`, `CI Upper`, Significance) %>%
      group_by(Moderator) %>%
      mutate(Moderator = if_else(row_number() == 1, Moderator, "")) %>%
      ungroup()

    xtab <- xtable::xtable(
      table_data,
      caption = caption_value,
      label = table_label,
      align = c("l", "l", "l", "r", "r", "r", "l")
    )

    print(
      xtab,
      include.rownames = FALSE,
      sanitize.text.function = identity,
      file = table_file
    )
    cat(sprintf("✓ Saved summary table: %s\n", table_file))
  }

  ggsave(output_file, p, width = width, height = height)
  cat(sprintf("✓ Saved summary plot: %s\n", output_file))

  invisible(p)
}

cat("✓ Interaction plotting functions loaded\n")
cat("  - plot_interaction_coefficients(): Plot interactions for single moderator\n")
cat("  - plot_interaction_comparison(): Compare interactions across moderators\n")
cat("  - create_all_interaction_plots(): Generate all interaction plots automatically\n")
cat("  - create_interaction_summary_plot(): Create main text summary plot\n")

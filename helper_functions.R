
# Function to handle skipped values
handle_skipped <- function(x, skip_values) {
  x[x %in% skip_values] <- NA
  return(x)
}

# Function to flip scales (works for 1-5, 1-10, or any other scale)
flip_scale <- function(x, min_val, max_val) {
  return(max_val + min_val - x)
}

# source: 
# https://www.alexstephenson.me/post/2022-04-02-weighted-variance-in-r/
weighted.average <- function(x, w){
    ## Sum of the weights 
    sum.w <- sum(w, na.rm = T)
    ## Sum of the weighted $x_i$ 
    xw <- sum(w*x, na.rm = T)
    
    ## Return the weighted average 
    return(xw/sum.w)
}

weighted.se.mean <- function(x, w, na.rm = T){
    ## Remove NAs 
    if (na.rm) {
      i <- !is.na(x)
        w <- w[i]
        x <- x[i]
    }
    
    ## Calculate effective N and correction factor
    n_eff <- (sum(w))^2/(sum(w^2))
    correction = n_eff/(n_eff-1)
    
    ## Get weighted variance 
    numerator = sum(w*(x-weighted.average(x,w))^2)
    denominator = sum(w)
    
    ## get weighted standard error of the mean 
    se_x = sqrt((correction * (numerator/denominator))/n_eff)
    return(se_x)
}

# Function to create bins and labels
create_bins_and_labels <- function(rating_min, rating_max, total_questions) {
  bins <- seq(rating_min + rating_max - 1, rating_max * total_questions, by = rating_max)
  n_bins <- length(bins)
  bin_labels <- data.frame(start = bins[1:(n_bins-1)] + 1, end = bins[2:n_bins]) %>%
    mutate(label = paste(start, end, sep = "-")) %>% 
    pull(label)
  return(list(bins = bins, labels = bin_labels))
}

# -----------------------------------------------------------------------------
# Project path helpers for saving outputs inside writing_draft
# -----------------------------------------------------------------------------
is_absolute_path <- function(path) {
  grepl("^(?:[A-Za-z]:)?/", path)
}

get_project_root <- function() {
  if (exists("data_dir", envir = .GlobalEnv)) {
    get("data_dir", envir = .GlobalEnv)
  } else {
    getwd()
  }
}

ensure_dir_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

get_writing_path <- function(...) {
  ensure_dir_exists(do.call(file.path, c(list(get_project_root(), "writing_draft"), list(...))))
}

resolve_writing_path <- function(filename, subdir) {
  if (dirname(filename) == "." && !is_absolute_path(filename)) {
    file.path(get_writing_path(subdir), filename)
  } else if (is_absolute_path(filename)) {
    ensure_dir_exists(dirname(filename))
    filename
  } else {
    full_path <- file.path(get_project_root(), filename)
    ensure_dir_exists(dirname(full_path))
    full_path
  }
}

save_plot_to_writing <- function(filename, plot = NULL, ..., copy_to_root = FALSE) {
  target_path <- resolve_writing_path(filename, "figures")
  args <- list(...)
  if (!is.null(plot)) {
    args <- c(list(filename = target_path, plot = plot), args)
  } else {
    args <- c(list(filename = target_path), args)
  }
  do.call(ggplot2::ggsave, args)
  if (isTRUE(copy_to_root) && dirname(filename) == "." && !is_absolute_path(filename)) {
    file.copy(target_path, file.path(get_project_root(), filename), overwrite = TRUE)
  }
  invisible(target_path)
}

copy_writing_table_alias <- function(source_filename, alias_filename, alias_label = NULL) {
  source_path <- resolve_writing_path(source_filename, "tables")
  alias_path <- resolve_writing_path(alias_filename, "tables")
  if (!file.exists(source_path)) {
    warning(sprintf("Cannot create table alias; missing source: %s", source_path))
    return(invisible(FALSE))
  }
  file.copy(source_path, alias_path, overwrite = TRUE)
  if (!is.null(alias_label)) {
    alias_lines <- readLines(alias_path, warn = FALSE)
    alias_lines <- sub("\\\\label\\{[^}]+\\}", paste0("\\\\label{", alias_label, "}"), alias_lines)
    writeLines(alias_lines, alias_path)
  }
  cat(sprintf("Table alias created: %s -> %s\n", alias_filename, source_filename))
  invisible(TRUE)
}

# Preserve original ggsave for direct access if needed
if (!exists("ggsave_original", envir = .GlobalEnv)) {
  assign("ggsave_original", ggplot2::ggsave, envir = .GlobalEnv)
}

# Override ggsave so all future calls default to writing_draft/figures
ggsave <- function(filename, plot = ggplot2::last_plot(), ..., copy_to_root = FALSE) {
  save_plot_to_writing(filename = filename, plot = plot, ..., copy_to_root = copy_to_root)
}

# -----------------------------------------------------------------------------
# Shared font sizing for all ggplot outputs
# -----------------------------------------------------------------------------
# Title/subtitle/axis/etc sizes are specified in typographic points because
# element_text() expects that unit. Annotation sizes map to geom_text size units.
plot_font_defaults <- list(
  title = 31,
  subtitle = 23,
  axis_title = 29,
  axis_text = 25,
  legend_title = 27,
  legend_text = 25,
  strip_text = 29,
  caption = 21,
  annotation_large = 10,
  annotation_medium = 8,
  annotation_small = 6.6
)

get_plot_font_sizes <- function() {
  if (exists("plot_font_overrides", envir = .GlobalEnv)) {
    return(modifyList(plot_font_defaults, get("plot_font_overrides", envir = .GlobalEnv)))
  }
  plot_font_defaults
}

format_plot_title_lines <- function(title, max_width = 50) {
  if (is.null(title) || is.na(title) || identical(title, "")) {
    return(title)
  }

  wrap_line <- function(line) {
    if (identical(line, "")) return(line)
    line <- trimws(line)
    if (nchar(line) <= max_width) {
      return(line)
    }
    paste(strwrap(line, width = max_width), collapse = "\n")
  }

  lines <- unlist(strsplit(title, "\n", fixed = TRUE))
  lines <- trimws(lines)
  wrapped_lines <- vapply(lines, wrap_line, character(1), USE.NAMES = FALSE)
  paste(wrapped_lines, collapse = "\n")
}


# Prepare data and create survey design
rename_variables <- function(data, label_list) {
  new_names <- sapply(names(data), function(x) {
    if (x %in% names(label_list)) label_list[[x]] else x
  })
  setNames(data, new_names)
}

# Formatting helpers for coefficient labels
format_single_label <- function(label) {
  if (is.null(label) || is.na(label) || identical(label, "")) {
    return(label)
  }

  if (identical(label, "(Intercept)")) {
    return("Intercept")
  }

  # Use configured treatment label when available
  treatment_display <- if (exists("treatment_label", envir = .GlobalEnv)) {
    get("treatment_label", envir = .GlobalEnv)
  } else {
    "Treatment (vs. Placebo)"
  }

  special_cases <- list(
    "Election_Rumor_Placebo_RandomizationTreatment" = treatment_display,
    "Election_Rumor_Placebo_Randomization" = treatment_display,
    "Rumor" = "Baseline Rumor Confidence",
    "Human_In_The_LoopOnly AI" = "Inoculation Style: Only AI",
    "Election_Rumor_RandomizationVoter Rolls" = "Rumor Assignment: Voter Rolls",
    "Election_Rumor_RandomizationHacking" = "Rumor Assignment: Hacking",
    "Election_Rumor_RandomizationBlue Shift" = "Rumor Assignment: Blue Shift",
    "Election_Rumor_RandomizationVoting Machines" = "Rumor Assignment: Voting Machines",
    "Age_Group30-44" = "Age: 30-44",
    "Age_Group45-64" = "Age: 45-64",
    "Age_Group65+" = "Age: 65+"
  )

  if (label %in% names(special_cases)) {
    return(special_cases[[label]])
  }

  cleaned <- gsub("_", " ", label, fixed = TRUE)
  cleaned <- gsub("([[:lower:]])([[:upper:]])", "\\1 \\2", cleaned, perl = TRUE)
  cleaned <- gsub("\\s+", " ", cleaned)
  cleaned <- trimws(cleaned)

  cleaned <- sub("^Urban Rural", "Urbanicity", cleaned)
  cleaned <- sub("^Human In The Loop", "Human-Assisted Article", cleaned)
  cleaned <- sub("^Political Interest Pol Interest", "Political Interest", cleaned)
  cleaned <- sub("^Election Rumor", "Rumor Assignment", cleaned)

  label_overrides <- c(
    "Pre Confidence Own Ballot" = "Pre: Own Ballot",
    "Pre Confidence County Ballots" = "Pre: County Ballots",
    "Pre Confidence Country Ballots" = "Pre: National Ballots",
    "Post Confidence Own Ballot" = "Post: Own Ballot",
    "Post Confidence County Ballots" = "Post: County Ballots",
    "Post Confidence Country Ballots" = "Post: National Ballots",
    "Recontact Confidence Own Ballot" = "Recontact: Own Ballot",
    "Recontact Confidence County Ballots" = "Recontact: County Ballots",
    "Recontact Confidence Country Ballots" = "Recontact: National Ballots",
    "Pre All Rumors" = "Pre: All Rumors",
    "Pre All Facts" = "Pre: All Facts",
    "All Rumors Diff Recontact" = "Recontact Diff: All Rumors",
    "All Facts Diff Recontact" = "Recontact Diff: All Facts",
    "Election Rumor Randomization" = "Rumor Assignment",
    "Human In The Loop" = "Human-Assisted Article",
    "Urban Rural" = "Urbanicity",
    "Political Interest" = "Political Interest",
    "Education Level Some college" = "Education Level: Some College",
    "Education Level College grad" = "Education Level: College Graduate",
    "Education Level Postgrad" = "Education Level: Postgraduate"
  )

  if (cleaned %in% names(label_overrides)) {
    cleaned <- label_overrides[[cleaned]]
  }

  colon_prefixes <- c(
    "Age Group", "Gender", "Race Ethnicity", "Education Level",
    "Party Identification", "Ideology", "Region", "Urbanicity",
    "Political Interest", "Populism Score", "Conspiracy Score",
    "MIST Correct", "Human-Assisted Article", "Rumor Assignment",
    "Pre Confidence Own Ballot", "Pre Confidence County Ballots", "Pre Confidence Country Ballots",
    "Post Confidence Own Ballot", "Post Confidence County Ballots", "Post Confidence Country Ballots",
    "Recontact Confidence Own Ballot", "Recontact Confidence County Ballots", "Recontact Confidence Country Ballots",
    "All Rumors", "All Facts"
  )

  for (prefix in colon_prefixes) {
    if (startsWith(cleaned, paste0(prefix, " ")) &&
        !startsWith(cleaned, paste0(prefix, ": "))) {
      cleaned <- sub(paste0("^", prefix, "\\s+"), paste0(prefix, ": "), cleaned)
      break
    }
    if (startsWith(cleaned, prefix) && nchar(cleaned) > nchar(prefix) &&
        grepl(paste0("^", prefix, "[A-Z]"), cleaned)) {
      cleaned <- sub(prefix, paste0(prefix, ": "), cleaned, fixed = TRUE)
      break
    }
  }

  cleaned <- sub("^Urbanicity\\s*\\:?", "Urbanicity:", cleaned)
  cleaned <- sub("^Human-Assisted Article\\s*\\:?", "Human-Assisted Article:", cleaned)
  cleaned <- sub("^Political Interest: Pol Interest:?\\s*", "Political Interest: ", cleaned)
  cleaned <- sub("Some of the time", "Some of the Time", cleaned, ignore.case = TRUE)
  cleaned <- sub("Only now and then", "Only Now and Then", cleaned, ignore.case = TRUE)
  cleaned <- sub("Hardly at all", "Hardly at All", cleaned, ignore.case = TRUE)
  cleaned <- sub("^Race Ethnicity:", "Race/Ethnicity:", cleaned)
  cleaned <- sub("College grad", "College Graduate", cleaned, ignore.case = TRUE)
  cleaned <- sub("Some college", "Some College", cleaned, ignore.case = TRUE)
  cleaned <- sub("Postgrad", "Postgraduate", cleaned, ignore.case = TRUE)
  cleaned <- sub("Rural area", "Rural Area", cleaned, ignore.case = TRUE)
  cleaned <- sub("Graduateuate", "Graduate", cleaned, fixed = TRUE)
  cleaned <- sub("Postgraduateuate", "Postgraduate", cleaned, fixed = TRUE)

  cleaned
}

format_coefficient_label <- function(label) {
  if (is.null(label) || is.na(label) || identical(label, "")) {
    return(label)
  }

  label_no_us <- gsub("_", " ", label, fixed = TRUE)
  if (grepl("^Political InterestPol Interest:", label_no_us)) {
    return(format_single_label(label))
  }

  if (grepl(":", label, fixed = TRUE)) {
    parts <- strsplit(label, ":", fixed = TRUE)[[1]]
    left_raw <- parts[1]
    right_raw <- if (length(parts) > 1) paste(parts[-1], collapse = ":") else ""

    left_formatted <- format_single_label(left_raw)
    if (exists("treatment_var_name", envir = .GlobalEnv) &&
        identical(left_raw, get("treatment_var_name", envir = .GlobalEnv))) {
      left_formatted <- format_single_label("Election_Rumor_Placebo_RandomizationTreatment")
    }

    right_formatted <- format_single_label(right_raw)
    return(sprintf("%s $\\times$ %s", left_formatted, right_formatted))
  }

  format_single_label(label)
}

format_coefficient_labels <- function(labels) {
  if (is.null(labels)) {
    return(NULL)
  }
  vapply(labels, format_coefficient_label, character(1), USE.NAMES = FALSE)
}

sanitize_table_text <- function(text) {
  if (is.null(text)) {
    return(text)
  }
  cleaned <- gsub("_", " ", text, fixed = TRUE)
  cleaned <- gsub("\\s+", " ", cleaned)
  trimws(cleaned)
}

# Function to run logistic regression with optional interactions
run_regression <- function(outcome, predictors, design, family=quasibinomial(link='logit'), interactions = NULL) {
  # 
  # Create the main formula
  main_formula <- paste(predictors, collapse = " + ")
  
  # Add interactions if specified
  if (!is.null(interactions)) {
    interaction_terms <- paste(interactions, collapse = " * ")
    formula <- as.formula(paste(outcome, "~", main_formula, "+", interaction_terms))
  } else {
    formula <- as.formula(paste(outcome, "~", main_formula))
  }
  
  # Run the model
  model <- svyglm(formula, design = design, family = family)
  return(model)
}


# Updated regression function
run_regression_for_table <- function(outcome, predictors, design, family=gaussian()) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- svyglm(formula, design = design, family = family)
  return(model)
}

# https://github.com/labreumaia/longtable.stargazer/blob/master/longtable.stargazer.R
##### Function to make stargazer compatible with longtable #####
#### Lucas de Abreu Maia ####
#### Department of Political Science ####
#### UCSD ####
#### lucasamaia.com ####
#### labreumaia@gmail.com ####

## Description
# This function simply makes stargazer compatible with the longtable 
# LaTeX environment
## Arguments
# ... - any argument to be passed to stargazer.
# float - logical. Whether or not the output of stargazer should be in 
# a float environment. This is useful if you want your table to have a 
# title and label associated with it.
# longtable.float - logical. Whether or not you want the longtable 
# itself to be  within a float environment.
# longtable.head - logical. If you want column headers to appear at 
# the top of every page.
# filename - character. An optional file path for the printed output. 
# If abcent, the output is printed to the console.

longtable.stargazer = function(..., float = T, longtable.float = F, 
  longtable.head = T, filename = NULL){
  # Capturing stargazer to hack it
  require(stargazer)
  res = capture.output(
    stargazer(..., float = float)
  )
  # Changing tabulare environment for longtable
    res = gsub("tabular", "longtable", res)
  # removing floating environment
  if(float == T & longtable.float == F){
    res[grep("table", res)[1]] = res[grep("longtable", res)[1]]
    # Removing extra longtable commands
    res = res[-grep("longtable", res)[2]]
    res = res[-length(res)]
  }
  # Adding page headings
  if(longtable.head == T){
    res = c(res[1:which(res == "\\hline \\\\[-1.8ex] ")[1] - 1], "\\endhead", res[which(res == "\\hline \\\\[-1.8ex] ")[1]:length(res)])
  }
  # Exporting
  cat(res, sep = "\n")
  # Exporting
  if(!is.null(filename)){
    cat(res, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ", 
      collapse = ""))
  }else{
    cat(res, sep = "\n")
  }
}


create_ols_summary_table <- function(models,
                                     title = "OLS Regression Results",
                                     column.labels = NULL,
                                     dep.var.labels = NULL,
                                     covariate.labels = NULL,
                                     out.file = NULL,
                                     label="",
                                     omit = NULL,
                                     longtable = FALSE,
                                     ci = FALSE) {

  if (!is.null(out.file)) {
    out.file <- resolve_writing_path(basename(out.file), "tables")
  }
  
  # Extract coefficients and standard errors
  extract_coef_se <- function(model) {
    coef_summary <- summary(model)$coefficients
    coef <- coef_summary[, "Estimate"]
    se <- coef_summary[, "Std. Error"]
    return(list(coef = coef, se = se))
  }
  
  model_data_raw <- lapply(models, extract_coef_se)
  all_coef_names <- unique(unlist(lapply(model_data_raw, function(x) names(x$coef))))
  align_vector <- function(vec) {
    res <- setNames(rep(NA_real_, length(all_coef_names)), all_coef_names)
    matching <- intersect(names(vec), all_coef_names)
    res[matching] <- vec[matching]
    return(res)
  }
  model_data <- lapply(model_data_raw, function(x) {
    list(
      coef = align_vector(x$coef),
      se = align_vector(x$se)
    )
  })
  default_labels <- format_coefficient_labels(all_coef_names)

  if (is.null(covariate.labels) || length(covariate.labels) == 0) {
    covariate.labels <- default_labels
  } else {
    formatted_inputs <- format_coefficient_labels(covariate.labels)
    covariate.labels <- default_labels
    idx <- seq_along(formatted_inputs)
    covariate.labels[idx] <- formatted_inputs
  }
  covariate.labels <- format_coefficient_labels(covariate.labels)
  
  title <- sanitize_table_text(title)
  column.labels <- format_coefficient_labels(column.labels)
  dep.var.labels <- format_coefficient_labels(dep.var.labels)
  
  if (longtable==FALSE){
    # Create the LaTeX table
    stargazer_output <- stargazer(
      models,
      title = title,
      column.labels = column.labels,
      dep.var.labels = dep.var.labels,
      dep.var.labels.include = FALSE,
      model.names = FALSE,
      model.numbers = FALSE,
      dep.var.caption = "",
      covariate.labels = covariate.labels,
      coef = lapply(model_data, function(x) x$coef),
      se = lapply(model_data, function(x) x$se),
      type = "latex",
      style = "default",
      font.size = "footnotesize",
      single.row = TRUE,
      no.space = TRUE,
      table.placement = "h",
      digits = 3,
      column.sep.width = "-10pt",
      star.cutoffs = c(0.05, 0.01, 0.001),
      ci = ci,
      header = FALSE,
      label = label,
      omit = omit,
      out = out.file
    )    
  } else {
    # Create the LaTeX table
    stargazer_output <- longtable.stargazer(
      models,
      title = title,
      column.labels = column.labels,
      dep.var.labels = dep.var.labels,
      dep.var.labels.include = FALSE,
      model.names = FALSE,
      model.numbers = FALSE,
      dep.var.caption = "",
      covariate.labels = covariate.labels,
      coef = lapply(model_data, function(x) x$coef),
      se = lapply(model_data, function(x) x$se),
      type = "latex",
      style = "default",
      font.size = "scriptsize",
      single.row = FALSE,
      no.space = TRUE,
      table.placement = "!htbp",
      digits = 3,
      star.cutoffs = c(0.05, 0.01, 0.001),
      ci = ci,
      header = FALSE,
      label = label,
      omit = omit,
      float.env = "sidewaystable",  # Use sidewaystable environment
      out = out.file
    )
    
  }
  
  if (!is.null(out.file)) {
    cat("LaTeX table has been written to", out.file, "\n")
  }
  
  return(stargazer_output)
}

generate_summary_stats <- function(dt, vars_to_summarize, weight_var = NULL, 
                                   group_var = NULL, 
                                   placebo_var = "Election_Rumor_Placebo_Randomization",
                                   include_factors = FALSE) {
  
  dt <- as.data.table(dt)
  weight_var <- enquo(weight_var)
  group_var <- enquo(group_var)
  placebo_var <- enquo(placebo_var)
  
  weight_var_name <- if (!quo_is_null(weight_var)) quo_name(weight_var) else NULL
  group_var_name <- if (!quo_is_null(group_var)) quo_name(group_var) else NULL
  placebo_var_name <- quo_name(placebo_var)
  
  summary_stats <- lapply(vars_to_summarize, function(var) {
    if (is.factor(dt[[var]]) || is.character(dt[[var]])) {
      if (!include_factors) return(NULL)
      
      stats <- dt[, .(
        N = as.integer(.N),
        Levels = as.double(length(unique(get(var)))),
        MostFrequent = names(which.max(table(get(var)))),
        Proportion = as.double(max(prop.table(table(get(var)))))
      ), by = c(group_var_name, placebo_var_name)]
    } else {
      if (is.null(weight_var_name)) {
        stats <- dt[, .(
          N = as.integer(sum(!is.na(get(var)))),
          Mean = as.double(mean(get(var), na.rm = TRUE)),
          Var = as.double(var(get(var), na.rm = TRUE)),
          Min = as.double(min(get(var), na.rm = TRUE)),
          Q10 = as.double(quantile(get(var), probs = 0.1, na.rm = TRUE)),
          Q25 = as.double(quantile(get(var), probs = 0.25, na.rm = TRUE)),
          Median = as.double(median(get(var), na.rm = TRUE)),
          Q75 = as.double(quantile(get(var), probs = 0.75, na.rm = TRUE)),
          Q90 = as.double(quantile(get(var), probs = 0.9, na.rm = TRUE)),
          Max = as.double(max(get(var), na.rm = TRUE))
        ), by = c(group_var_name, placebo_var_name)]
      } else {
        stats <- dt[, .(
          N = as.integer(sum(!is.na(get(var)))),
          Mean = as.double(tryCatch(weighted.mean(get(var), w = get(weight_var_name), na.rm = TRUE), error = function(e) NA_real_)),
          Var = as.double(tryCatch(wtd.var(get(var), weights = get(weight_var_name), na.rm = TRUE), error = function(e) NA_real_)),
          Min = as.double(min(get(var), na.rm = TRUE)),
          Q10 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.1, na.rm = TRUE), error = function(e) NA_real_)),
          Q25 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.25, na.rm = TRUE), error = function(e) NA_real_)),
          Median = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.5, na.rm = TRUE), error = function(e) NA_real_)),
          Q75 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.75, na.rm = TRUE), error = function(e) NA_real_)),
          Q90 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.9, na.rm = TRUE), error = function(e) NA_real_)),
          Max = as.double(max(get(var), na.rm = TRUE))
        ), by = c(group_var_name, placebo_var_name)]
      }
    }
    
    if (!is.null(stats)) {
      stats[, Variable := var]
    }
    
    return(stats)
  })
  
  # Remove NULL entries and ensure all columns are of the same type
  summary_stats <- Filter(Negate(is.null), summary_stats)
  
  # Identify all unique columns across all data.tables
  all_columns <- unique(unlist(lapply(summary_stats, names)))
  
  # Ensure each data.table has all columns, filling with NA where necessary
  summary_stats <- lapply(summary_stats, function(dt) {
    for (col in all_columns) {
      if (!(col %in% names(dt))) {
        dt[, (col) := NA_real_]
      }
    }
    # Ensure all columns (except character and factor columns) are double
    for (col in names(dt)) {
      if (!is.character(dt[[col]]) && !is.factor(dt[[col]])) {
        set(dt, j = col, value = as.double(dt[[col]]))
      }
    }
    return(dt[, ..all_columns])
  })
  
  summary_table <- rbindlist(summary_stats, fill = TRUE, use.names = TRUE)
  
  if (!is.null(group_var_name)) {
    setnames(summary_table, group_var_name, "Group")
    if (is.factor(dt[[group_var_name]])) {
      summary_table[, Group := factor(Group, levels = levels(dt[[group_var_name]]), labels = levels(dt[[group_var_name]]))]
    }
  } else {
    summary_table[, Group := "All"]
  }
  
  setnames(summary_table, placebo_var_name, "Placebo")
  if (is.factor(dt[[placebo_var_name]])) {
    summary_table[, Placebo := factor(Placebo, levels = levels(dt[[placebo_var_name]]), labels = levels(dt[[placebo_var_name]]))]
  }
  
  # Calculate total N for each variable
  total_N <- summary_table[, .(Total_N = sum(N)), by = .(Variable, Placebo)]
  
  # Add "All" group with total N
  all_stats <- summary_table[, lapply(.SD, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else if (is.integer(x)) sum(x, na.rm = TRUE) else x[1]), 
                             by = .(Variable, Placebo), 
                             .SDcols = setdiff(names(summary_table), c("Group", "Variable", "Placebo", "N"))]
  all_stats[, Group := "All"]
  
  # Merge total N into all_stats
  all_stats <- merge(all_stats, total_N, by = c("Variable", "Placebo"))
  setnames(all_stats, "Total_N", "N")
  
  summary_table <- rbindlist(list(summary_table, all_stats), use.names = TRUE, fill = TRUE)
  
  # Determine columns dynamically based on the data
  all_cols <- names(summary_table)
  fixed_cols <- c("Group", "Placebo", "Variable")
  stat_cols <- setdiff(all_cols, fixed_cols)
  
  setcolorder(summary_table, c(fixed_cols, stat_cols))

  summary_table[, `:=`(Group = str_replace_all(Group, "_", " "), 
                       Placebo = str_replace_all(Placebo, "_", " "), 
                       Variable = str_replace_all(Variable, "_", " "))]
  
  return(summary_table)
}

generate_factor_summary <- function(dt, factor_vars, group_var, placebo_var) {
  dt <- as.data.table(dt)
  
  summary_list <- lapply(factor_vars, function(var) {
    full <- data.table()
    levels_group <- levels(dt[[group_var]])
    
    for (lvl in levels_group) {
      sub <- dt[get(group_var) == lvl]
      temp <- sub[, .N, by = c(placebo_var, var)]
      condition_total <- sub[, .(cond_N = .N), by = placebo_var]
      
      temp <- merge(temp, condition_total, by = placebo_var)
      temp[, Proportion := N / cond_N]
      
      temp_wide <- dcast(temp, as.formula(paste(placebo_var, "~", var)), value.var = "Proportion")
      temp_wide[, Group := lvl]
      full <- rbindlist(list(full, temp_wide), use.names = TRUE, fill = TRUE)
    }
    
    # Rename columns
    setnames(full, old = placebo_var, new = "Treatment Status")
    setnames(full, old = "Group", new = "Assigned Rumor")
    
    full[, Variable := var]
    return(full)
  })
  
  factor_summary_table <- rbindlist(summary_list, fill = TRUE, use.names = TRUE)
  
  # Reorder columns
  desired_order <- c("Treatment Status", "Assigned Rumor", "Variable")
  remaining_cols <- setdiff(names(factor_summary_table), desired_order)
  final_order <- c(desired_order, remaining_cols)
  
  setcolorder(factor_summary_table, final_order)
  
  return(factor_summary_table)
}

generate_factor_summary <- function(dt, factor_vars, placebo_var, group_var=NULL, weight_var = NULL) {
  require(data.table)
  require(Hmisc)
  
  dt <- as.data.table(dt)
  
  summary_list <- lapply(factor_vars, function(var) {
    full <- data.table()
    if (is.null(group_var)) {
      levels_group <- "All"
    } else {
      levels_group <- levels(dt[[group_var]])
    }
    
    for (lvl in levels_group) {
      if (is.null(group_var)) {
        sub <- dt
      } else {
        sub <- dt[get(group_var) == lvl]
      }
      
      if (is.null(weight_var)) {
        temp <- sub[, .N, by = c(placebo_var, var)]
        condition_total <- sub[, .(cond_N = .N), by = placebo_var]
        temp <- merge(temp, condition_total, by = placebo_var)
        temp[, Proportion := N / cond_N]
      } else {
        temp <- sub[, .(N = sum(get(weight_var))), by = c(placebo_var, var)]
        condition_total <- sub[, .(cond_N = sum(get(weight_var))), by = placebo_var]
        temp <- merge(temp, condition_total, by = placebo_var)
        temp[, Proportion := N / cond_N]
      }
      
      temp_wide <- dcast(temp, as.formula(paste(placebo_var, "~", var)), value.var = "Proportion")
      temp_wide[, Group := lvl]
      full <- rbindlist(list(full, temp_wide), use.names = TRUE, fill = TRUE)
    }
    
    # Rename columns
    setnames(full, old = placebo_var, new = "Treatment Status")
    if (!is.null(group_var)) {
      setnames(full, old = "Group", new = "Assigned Rumor")
    } else {
      full[, Group := NULL]
    }
    
    full[, Variable := var]
    return(full)
  })
  
  factor_summary_table <- rbindlist(summary_list, fill = TRUE, use.names = TRUE)
  
  # Reorder columns
  if (!is.null(group_var)) {
    desired_order <- c("Treatment Status", "Assigned Rumor", "Variable")
  } else {
    desired_order <- c("Treatment Status", "Variable")
  }
  remaining_cols <- setdiff(names(factor_summary_table), desired_order)
  final_order <- c(desired_order, remaining_cols)
  
  setcolorder(factor_summary_table, final_order)
  
  return(factor_summary_table)
}


create_latex_table <- function(dt, filename, title = NULL, caption = NULL, label = NULL, note = NULL, digits = 2) {
  if (is.null(caption)) {
    caption <- title
  }
  if (is.null(label)) {
    label <- paste0("tab:", gsub("\\s+", "_", tolower(filename)))
  }
  
  # Format numeric columns
  numeric_cols <- sapply(dt, is.numeric)
  dt_formatted <- copy(dt)
  dt_formatted[, (names(numeric_cols)[numeric_cols]) := lapply(.SD, function(x) sprintf(paste0("%.", digits, "f"), x)), 
               .SDcols = names(numeric_cols)[numeric_cols]]
  # Format character and factor columns
    char_factor_cols <- sapply(dt, function(x) is.character(x) || is.factor(x))
    dt_formatted[, (names(char_factor_cols)[char_factor_cols]) := lapply(.SD, function(x) gsub("_", " ", x)), 
                 .SDcols = names(char_factor_cols)[char_factor_cols]]
  
  # Format 'N' column separately
  if ("N" %in% names(dt_formatted)) {
    dt_formatted[, N := format(as.numeric(N), big.mark = ",")]
  }

  colnames(dt_formatted) <- gsub("_", " ", colnames(dt_formatted), fixed = TRUE)
  
  latex_table <- kable(dt_formatted, format = "latex", booktabs = TRUE, 
                       caption = caption,
                       label = label,
                      #  align = c("l", "l", "l", rep("r", ncol(dt_formatted) - 3)),
                      align = "l",
                       escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    # column_spec(1:3, bold = TRUE) %>%
    row_spec(0, bold = TRUE)
  
  if (!is.null(note)) {
    latex_table <- latex_table %>% 
      footnote(general = note, general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = FALSE)
  }

  table_path <- resolve_writing_path(filename, "tables")
  save_kable(latex_table, file = table_path)
  
  cat("Table saved to:", table_path, "\n")
  
  return(latex_table)
}


calculate_stats <- function(data, variable, groups) {
  data %>%
    group_by(across(all_of(groups))) %>%
    dplyr::summarise(
      mean = mean({{variable}}, na.rm = TRUE),
      sd = sd({{variable}}, na.rm = TRUE),
      skewness = skewness({{variable}}, na.rm = TRUE),
      kurtosis = kurtosis({{variable}}, na.rm = TRUE),
      q10 = quantile({{variable}}, 0.10, na.rm = TRUE),
      q25 = quantile({{variable}}, 0.25, na.rm = TRUE),
      median = median({{variable}}, na.rm = TRUE),
      q75 = quantile({{variable}}, 0.75, na.rm = TRUE),
      q90 = quantile({{variable}}, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
}

calculate_treatment_effects <- function(data, variable, treatment_var, by_var = NULL, rumor_var = NULL) {
  # Convert arguments to quosures (quoted expressions)
  variable <- enquo(variable)
  treatment_var <- enquo(treatment_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  
  # Prepare grouping variables
  grouping_vars <- quos(!!treatment_var)
  if (!quo_is_null(by_var)) grouping_vars <- c(grouping_vars, by_var)
  if (!quo_is_null(rumor_var)) grouping_vars <- c(grouping_vars, rumor_var)
  
  # Calculate basic statistics for each group
  stats <- data %>%
    group_by(!!!grouping_vars) %>%
    dplyr::summarise(
      mean = mean(!!variable, na.rm = TRUE),
      sd = sd(!!variable, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Calculate treatment effects
  effects <- stats %>%
    group_by(across(-c(!!treatment_var, mean, sd, n))) %>%
    dplyr::summarise(
      mean_diff = diff(mean),
      pooled_sd = sqrt(sum((n - 1) * sd^2) / sum(n - 2)),
      cohens_d = mean_diff / pooled_sd,
      t_stat = mean_diff / (pooled_sd * sqrt(sum(1/n))),
      df = sum(n) - 2,
      p_value = 2 * pt(-abs(t_stat), df),
      treatment_var = quo_name(treatment_var),  # Add this line
      .groups = "drop"
    )
  
  return(effects)
}


plot_cumulative_density <- function(data, variable, by_var, treatment_var, rumor_var,
                                    title = "Cumulative Distribution of Changes",
                                    subtitle = "By Treatment Status",
                                    x_label = "Change",
                                    y_label = "Cumulative Proportion",
                                    output_file = NULL,
                                    weight_var = NULL) {
  
  # Ensure variables are quoted
  by_var <- enquo(by_var)
  treatment_var <- enquo(treatment_var)
  rumor_var <- enquo(rumor_var)
  variable <- enquo(variable)
  weight_var <- enquo(weight_var)
  font_sizes <- get_plot_font_sizes()
  title <- format_plot_title_lines(title)
  subtitle <- format_plot_title_lines(subtitle)
  
  # Calculate statistics
  stats <- data %>%
    group_by(!!treatment_var, !!rumor_var, !!by_var) %>%
    dplyr::summarise(
      q10 = Hmisc::wtd.quantile(!!variable, weights = !!weight_var, probs = 0.10, na.rm = TRUE),
      median = Hmisc::wtd.quantile(!!variable, weights = !!weight_var, probs = 0.50, na.rm = TRUE),
      q90 = Hmisc::wtd.quantile(!!variable, weights = !!weight_var, probs = 0.90, na.rm = TRUE),
      # q10 = quantile(!!variable, 0.10, na.rm = TRUE),
      # median = median(!!variable, na.rm = TRUE),
      # q90 = quantile(!!variable, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create the plot
  plot <- data %>%
    group_by(!!treatment_var, !!rumor_var, !!by_var) %>%
    arrange(!!variable) %>%
    mutate(cum_prop = row_number() / n()) %>%
    ggplot(aes(x = !!variable, y = cum_prop, 
               color = !!treatment_var,
               group = !!treatment_var)) +
    stat_ecdf(geom = "step") +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         color = "Treatment") +
    theme_minimal() +
    # scale_color_brewer(palette = "Set4") +
    scale_color_manual(values = wes_palette("Royal1", 2)) +
    scale_y_continuous(labels = percent_format()) +
    facet_grid(rows = vars(!!rumor_var), cols = vars(!!by_var)) +
    geom_text_repel(data = stats, 
                    aes(x = median, y = 0.6,
                        label = sprintf("%s\n10th %%: %.2f\n50th %%: %.2f\n90th %%: %.2f", 
                                        !!treatment_var, q10, median, q90),
                        color = !!treatment_var),
                    size = font_sizes$annotation_small, show.legend = FALSE,
                    box.padding = 1.5, point.padding = 1.5,
                    min.segment.length = 0, seed = 123) +
    coord_cartesian(clip = "off") + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = font_sizes$axis_text),
      axis.text.y = element_text(size = font_sizes$axis_text),
      legend.position = "bottom",
      legend.title = element_text(size = font_sizes$legend_title, face = "bold"),
      legend.text = element_text(size = font_sizes$legend_text),
      plot.title = element_text(size = font_sizes$title, face = "bold", hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = font_sizes$subtitle, hjust = 0),
      axis.title = element_text(size = font_sizes$axis_title),
      strip.text = element_text(size = font_sizes$strip_text, face = "bold")
    )
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    save_plot_to_writing(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}




plot_probability_density <- function(data, variable, by_var = NULL, treatment_var, rumor_var = NULL,
                                     title = "Distribution of Changes",
                                     subtitle = "By Treatment Status",
                                     x_label = "Change",
                                     y_label = "Count",
                                     output_file = NULL,
                                     na.rm=TRUE,
                                     weight_var = NULL) {
  
  # Ensure variables are quoted
  by_var <- enquo(by_var)
  treatment_var <- enquo(treatment_var)
  rumor_var <- enquo(rumor_var)
  variable <- enquo(variable)
  weight_var <- enquo(weight_var)
  font_sizes <- get_plot_font_sizes()
  title <- format_plot_title_lines(title)
  subtitle <- format_plot_title_lines(subtitle)
  
  data <- data %>% 
    dplyr::filter(!is.na(!!variable)) %>%
    {if (!quo_is_null(by_var)) dplyr::filter(., !is.na(!!by_var)) else .} %>%
    {if (!quo_is_null(weight_var)) dplyr::filter(., !is.na(!!weight_var)) else .} %>%
    droplevels()
  # Pre-calculate statistics
  stats <- data %>%
    group_by(!!treatment_var, !!rumor_var, !!by_var) %>%
    dplyr::summarise(
      count = n(),
      mean = Hmisc::wtd.mean(!!variable, na.rm = TRUE),
      sd = sd(!!variable, na.rm = TRUE),
      se = sd / sqrt(count),
      skewness = moments::skewness(!!variable, na.rm = TRUE),
      kurtosis = moments::kurtosis(!!variable, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate treatment effects (assuming this function exists)
  effects <- calculate_treatment_effects(data, !!variable, !!treatment_var, !!by_var, !!rumor_var)
  
  bar_position <- position_dodge2(preserve = "single")
  # Create the plot
  plot <- ggplot(data, aes(x = !!variable,
                           fill = !!treatment_var,
                           group = !!treatment_var)) +
    geom_histogram(aes(y = after_stat(count)), 
                   alpha = 0.7, 
                   position = bar_position,
                   bins = 30) +
    geom_errorbar(stat = "bin",
                  aes(y = after_stat(count),
                      ymin = after_stat(count - 1.96 * sqrt(count)),
                      ymax = after_stat(count + 1.96 * sqrt(count))),
                  position = bar_position,
                  color = "grey30",
                  width = 0.25) +
    # geom_rug(alpha = 0.2) +
    geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         fill = "Treatment") +
    theme_minimal() +
    scale_fill_manual(values = wes_palette("Royal1", 2)) +
    facet_grid(rows = vars(!!rumor_var), cols = vars(!!by_var)) +
    geom_text_repel(data = stats, 
                    aes(x = mean, y = Inf,
                        label = sprintf("%s\nMean: %.2f\nSD: %.2f\nSE: %.2f\nSkew: %.2f\nKurt: %.2f", 
                                        !!treatment_var, mean, sd, se, skewness, kurtosis),
                        color = !!treatment_var),
                    size = font_sizes$annotation_medium, show.legend = FALSE,
                    box.padding = 0.5, point.padding = 0.5,
                    min.segment.length = 0, seed = 123,
                    vjust = 1) +
                    
    geom_text(data = effects,
              aes(x = -Inf, y = Inf,
                  label = sprintf(
                    "Mean Diff: %.2f\nCohen's d: %.2f\np-value: %.3f",
                    mean_diff, cohens_d, p_value),
                    #  bold if p_value<0.05
                    fontface = ifelse(p_value < 0.05, 2, 1)
                  ),
              hjust = -0.1, vjust = 1.1, size = font_sizes$annotation_medium,
              inherit.aes = FALSE) +
    coord_cartesian(clip = "off") + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = font_sizes$axis_text),
      axis.text.y = element_text(size = font_sizes$axis_text),
      legend.position = "bottom",
      legend.title = element_text(size = font_sizes$legend_title, face = "bold"),
      legend.text = element_text(size = font_sizes$legend_text),
      plot.title = element_text(size = font_sizes$title, face = "bold", hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = font_sizes$subtitle, hjust = 0),
      axis.title = element_text(size = font_sizes$axis_title),
      strip.text = element_text(size = font_sizes$strip_text, face = "bold")
    )
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    save_plot_to_writing(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}


# Function to run models for a specific rumor
run_rumor_models <- function(rumor_value, var_labels=post_y_vars_labels) {
  rumor_label <- levels(dat_final$Election_Rumor_Randomization)[rumor_value]
  rumor_data <- subset(dat_final, Election_Rumor_Randomization == rumor_label)
  rumor_design <- svydesign(data = rumor_data, weights = ~weight, id = ~1)
  
  models <- lapply(var_labels, function(outcome) {
    predictors <- c(paste0(sub("Post_", "Pre_", outcome)), "Election_Rumor_Placebo_Randomization", variables_in_model_labels)
    run_regression_for_table(outcome, predictors, rumor_design, family = gaussian())
  })
  
  names(models) <- var_labels
  return(models)
}


# Function to create tables for individual rumor models
create_rumor_tables <- function(rumor_models, rumor_names, table_label = "") {
  for (i in seq_along(rumor_names)) {
    rumor_table <- create_ols_summary_table(
      models = rumor_models[[i]],
      title = paste("OLS Regression Results for", rumor_names[i]),
      dep.var.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
      covariate.labels = c(
        pre_y_vars_labels %>% gsub("_", " ", .), 
        treatment_label, 
        covariate_labels),
      out.file = paste0("rumor_", i, "_table", table_label, output_label, ".tex"),
      label = paste0("tab:rumor_", i)
    )
    # print(paste("Table for", rumor_names[i], "has been created."))
  }
}

pre_post_plot <- function(data, pre_var, post_var, treatment_var, by_var, rumor_var,
                          title = "Pre vs Post Plot",
                          subtitle = "By Treatment and Group",
                          x_label = "Pre Score",
                          y_label = "Post Score",
                          output_file = NULL,
                          weight_var = NULL) {
  
  # Ensure variables are quoted
  pre_var <- enquo(pre_var)
  post_var <- enquo(post_var)
  treatment_var <- enquo(treatment_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  weight_var <- enquo(weight_var)
  font_sizes <- get_plot_font_sizes()
  title <- format_plot_title_lines(title)
  subtitle <- format_plot_title_lines(subtitle)

  if (!quo_is_null(weight_var)) {
    data <- data %>% filter(!is.na(!!weight_var))
  }
  weight_expr <- if (quo_is_null(weight_var)) rlang::expr(1) else weight_var
  data <- data %>% mutate(`._plot_weight` = !!weight_expr)

  pre_var_name <- quo_name(pre_var)
  post_var_name <- quo_name(post_var)
  treatment_var_name <- quo_name(treatment_var)
  by_var_name <- if (quo_is_null(by_var)) NULL else quo_name(by_var)
  rumor_var_name <- if (quo_is_null(rumor_var)) NULL else quo_name(rumor_var)
  weight_var_name <- if (quo_is_null(weight_var)) NULL else quo_name(weight_var)

  group_quos <- list(treatment_var)
  if (!is.null(by_var_name)) group_quos <- append(group_quos, list(by_var))
  if (!is.null(rumor_var_name)) group_quos <- append(group_quos, list(rumor_var))

  # Function to calculate summary statistics
  calculate_summary_stats <- function(data) {
    data %>%
      dplyr::group_by(!!!group_quos, .add = FALSE) %>%
      dplyr::group_modify(function(.x, .y) {
        weights <- if (is.null(weight_var_name)) rep(1, nrow(.x)) else .x[[weight_var_name]]
        pre_vals <- .x[[pre_var_name]]
        post_vals <- .x[[post_var_name]]

        sd_x <- sqrt(Hmisc::wtd.var(pre_vals, weights = weights, normwt = TRUE, na.rm = TRUE))
        sd_y <- sqrt(Hmisc::wtd.var(post_vals, weights = weights, normwt = TRUE, na.rm = TRUE))
        if (!is.finite(sd_x)) sd_x <- 0
        if (!is.finite(sd_y)) sd_y <- 0

        tibble::tibble(
          mean_x = Hmisc::wtd.mean(pre_vals, weights = weights, na.rm = TRUE),
          mean_y = Hmisc::wtd.mean(post_vals, weights = weights, na.rm = TRUE),
          sd_x = sd_x,
          sd_y = sd_y,
          se_x = weighted.se.mean(pre_vals, w = weights, na.rm = TRUE),
          se_y = weighted.se.mean(post_vals, w = weights, na.rm = TRUE),
          n = nrow(.x)
        )
      }) %>%
      dplyr::ungroup() %>%
      mutate(
        ci_x_lower = mean_x - 1.96 * se_x,
        ci_x_upper = mean_x + 1.96 * se_x,
        ci_y_lower = mean_y - 1.96 * se_y,
        ci_y_upper = mean_y + 1.96 * se_y
      )
  }
  
  # Calculate summary statistics
  summary_stats <- calculate_summary_stats(data)
  
  # Function to calculate effect sizes
  calculate_effect_sizes <- function(data) {
    data %>%
      dplyr::group_by(!!rumor_var, !!by_var) %>%
      dplyr::summarise(
        effect_size = -1 * effsize::cohen.d(
          formula = as.formula(paste("I(", quo_name(post_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$estimate,
        p_value = t.test(
          formula = as.formula(paste("I(", quo_name(post_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$p.value,
        .groups = "drop"
      )
  }

  # Calculate effect sizes
  effect_sizes <- calculate_effect_sizes(data)
  # aggregate data
  agg_data <- data %>%
    dplyr::group_by(!!rumor_var, !!by_var, !!treatment_var, !!pre_var, !!post_var) %>%
    dplyr::summarise(
      n = n(),
      .groups = "drop"
    )
    # print(agg_data)
  # scaleFUN <- function(x) sprintf("%.2f", x)
  has_rumor <- !(quo_is_null(rumor_var) || quo_is_missing(rumor_var))
  has_by <- !(quo_is_null(by_var) || quo_is_missing(by_var))
  has_two_facets <- has_rumor && has_by
  axis_text_size_current <- font_sizes$axis_text
  axis_title_size_current <- font_sizes$axis_title
  strip_text_size_current <- font_sizes$strip_text
  if (!has_rumor & !has_by) {
    annotation_size <- 4.7
  } else if (!(has_rumor && has_by)) {
    annotation_size <- 3.5
  } else {
    annotation_size <- 2.8
  }
  if (has_two_facets) {
    axis_text_size_current <- font_sizes$axis_text * 0.85
    axis_title_size_current <- font_sizes$axis_title * 0.9
    strip_text_size_current <- font_sizes$strip_text * 0.9
  }
  show_annotations <- !has_two_facets
  axis_text_size_current <- if (has_two_facets) font_sizes$axis_text * 0.85 else font_sizes$axis_text
  axis_title_size_current <- if (has_two_facets) font_sizes$axis_title * 0.9 else font_sizes$axis_title
  strip_text_size_current <- if (has_two_facets) font_sizes$strip_text * 0.9 else font_sizes$strip_text
  
  # Create the plot
  plot <- ggplot(agg_data, aes(x = !!pre_var, y = !!post_var, color = !!treatment_var)) +
    geom_point(
      aes(size = n),
      alpha = 0.55,
      position = position_jitter(width = 0.1, height = 0.1),
      show.legend = FALSE
    )

  if (show_annotations) {
    plot <- plot +
      ggrepel::geom_label_repel(
        data = summary_stats,
        aes(x = mean_x, y = mean_y, 
            label = sprintf(
              "%s\nMean (pre, post): (%.2f, %.2f)\nSD (pre, post): (%.2f, %.2f)\n95%% CI pre: [%.2f, %.2f]\n95%% CI post: [%.2f, %.2f]\nn: %d",
              !!treatment_var, mean_x, mean_y, sd_x, sd_y, ci_x_lower, ci_x_upper, ci_y_lower, ci_y_upper, n
            )),
        inherit.aes = FALSE,
        size = annotation_size,
        box.padding = 2.2,
        point.padding = 2.0,
        min.segment.length = 0,
        direction = 'both',
        position = position_nudge_line(
          x = c(2, -3), 
          y = c(1, -3),
          direction = "split"
        )
      ) +
      geom_label(
        data = effect_sizes,
        aes(x = -Inf, y = Inf, 
            label = sprintf("ATT Cohen's d: %.2f\np-value: %.3f", 
                            effect_size, p_value),
            fontface = ifelse(p_value < 0.05, 2, 1)
        ),
        hjust = -0.1, vjust = 1.1, size = annotation_size * 0.82,
        fill = "white",
        label.size = 0,
        label.padding = unit(0.08, "lines"),
        inherit.aes = FALSE
      )
  }

  plot <- plot +
    scale_size_continuous(range = c(4, 12)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 5, linetype = "dotted", color = "grey40") + 
    geom_errorbar(
      data = summary_stats,
      aes(
        x = mean_x,
        ymin = ci_y_lower,
        ymax = ci_y_upper,
        color = !!treatment_var
      ),
      width = 0.1,
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_errorbarh(
      data = summary_stats,
      aes(
        y = mean_y,
        xmin = ci_x_lower,
        xmax = ci_x_upper,
        color = !!treatment_var
      ),
      height = 0.1,
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = summary_stats,
      aes(x = mean_x, y = mean_y, color = !!treatment_var),
      size = 9,
      shape = 21,
      stroke = 1.6,
      fill = "white",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         color = "Treatment") +
    coord_cartesian(xlim = c(0,10), ylim = c(0,10)) +
    facet_grid(vars(!!rumor_var), vars(!!by_var)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = font_sizes$legend_title, face = "bold"),
      legend.text = element_text(size = font_sizes$legend_text),
      plot.title = element_text(size = font_sizes$title, face = "bold", hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = font_sizes$subtitle, hjust = 0),
      axis.title = element_text(size = axis_title_size_current),
      axis.text = element_text(size = axis_text_size_current),
      strip.text = element_text(size = strip_text_size_current, face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    # scale_y_continuous(labels=scales::label_number(accuracy=1)) + 
    # scale_x_continuous(labels=scales::label_number(accuracy=1)) +
    scale_y_continuous(breaks = 0:10, labels = 0:10) + 
    scale_x_continuous(breaks = 0:10, labels = 0:10) +
    scale_color_manual(values = wesanderson::wes_palette("Royal1", 2)) +
    guides(color = guide_legend(override.aes = list(size = 6))) +
    # Add arrows and labels for "persuaded for" and "persuaded against"
    annotate("segment", x = 0, xend = 0, y = 0, yend = 4, 
             arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
             color = "blue") +
    annotate("text", x = 1, y = 5, label = "increased\nconfidence", color = "blue", size = annotation_size) +
    annotate("segment", x = 0, xend = 4, y = 0, yend = 0, 
             arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
             color = "blue") +
    annotate("text", x = 5, y = 1, label = "decreased\nconfidence", color = "blue", size = annotation_size)
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    save_plot_to_writing(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}



pre_post_diff_plot <- function(data, pre_var, diff_var, treatment_var, by_var, rumor_var,
                          title = "Pre vs Diff Plot",
                          subtitle = "By Treatment and Group",
                          x_label = "Pre Score",
                          y_label = "Post-Pre Diff Score",
                          output_file = NULL,
                          weight_var = NULL) {
  
  # Ensure variables are quoted
  pre_var <- enquo(pre_var)
  diff_var <- enquo(diff_var)
  treatment_var <- enquo(treatment_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  weight_var <- enquo(weight_var)
  font_sizes <- get_plot_font_sizes()
  title <- format_plot_title_lines(title)
  subtitle <- format_plot_title_lines(subtitle)

  if (!quo_is_null(weight_var)) {
    data <- data %>% filter(!is.na(!!weight_var))
  }
  
  pre_var_name <- quo_name(pre_var)
  diff_var_name <- quo_name(diff_var)
  treatment_var_name <- quo_name(treatment_var)
  by_var_name <- if (quo_is_null(by_var)) NULL else quo_name(by_var)
  rumor_var_name <- if (quo_is_null(rumor_var)) NULL else quo_name(rumor_var)
  weight_var_name <- if (quo_is_null(weight_var)) NULL else quo_name(weight_var)

  group_quos <- list(treatment_var)
  if (!is.null(by_var_name)) group_quos <- append(group_quos, list(by_var))
  if (!is.null(rumor_var_name)) group_quos <- append(group_quos, list(rumor_var))

  # Function to calculate summary statistics
  calculate_summary_stats <- function(data) {
    data %>%
      dplyr::group_by(!!!group_quos, .add = FALSE) %>%
      dplyr::group_modify(function(.x, .y) {
        weights <- if (is.null(weight_var_name)) rep(1, nrow(.x)) else .x[[weight_var_name]]
        pre_vals <- .x[[pre_var_name]]
        diff_vals <- .x[[diff_var_name]]
        post_vals <- pre_vals + diff_vals

        sd_pre <- sqrt(Hmisc::wtd.var(pre_vals, weights = weights, normwt = TRUE, na.rm = TRUE))
        sd_diff <- sqrt(Hmisc::wtd.var(diff_vals, weights = weights, normwt = TRUE, na.rm = TRUE))
        sd_post <- sqrt(Hmisc::wtd.var(post_vals, weights = weights, normwt = TRUE, na.rm = TRUE))
        if (!is.finite(sd_pre)) sd_pre <- 0
        if (!is.finite(sd_diff)) sd_diff <- 0
        if (!is.finite(sd_post)) sd_post <- 0

        tibble::tibble(
          mean_pre = Hmisc::wtd.mean(pre_vals, weights = weights, na.rm = TRUE),
          mean_diff = Hmisc::wtd.mean(diff_vals, weights = weights, na.rm = TRUE),
          mean_post = Hmisc::wtd.mean(post_vals, weights = weights, na.rm = TRUE),
          sd_pre = sd_pre,
          sd_diff = sd_diff,
          sd_post = sd_post,
          se_pre = weighted.se.mean(pre_vals, w = weights, na.rm = TRUE),
          se_diff = weighted.se.mean(diff_vals, w = weights, na.rm = TRUE),
          se_post = weighted.se.mean(post_vals, w = weights, na.rm = TRUE),
          n = nrow(.x)
        )
      }) %>%
      dplyr::ungroup() %>%
      mutate(
        ci_pre_lower = mean_pre - 1.96 * se_pre,
        ci_pre_upper = mean_pre + 1.96 * se_pre,
        ci_diff_lower = mean_diff - 1.96 * se_diff,
        ci_diff_upper = mean_diff + 1.96 * se_diff,
        ci_post_lower = mean_post - 1.96 * se_post,
        ci_post_upper = mean_post + 1.96 * se_post
      )
  }

  # Calculate summary statistics and place labels in fixed panel corners so
  # ggrepel does not drift into the title/subtitle or stack summaries together.
  label_group_quos <- list()
  if (!is.null(by_var_name)) label_group_quos <- append(label_group_quos, list(by_var))
  if (!is.null(rumor_var_name)) label_group_quos <- append(label_group_quos, list(rumor_var))

  summary_stats <- calculate_summary_stats(data) %>%
    dplyr::group_by(!!!label_group_quos, .add = FALSE) %>%
    dplyr::arrange(dplyr::desc(mean_diff), .by_group = TRUE) %>%
    dplyr::mutate(
      label_rank = dplyr::row_number(),
      label_target_x = dplyr::if_else(label_rank %% 2 == 1, 6.25, 0.75),
      label_target_y = dplyr::if_else(label_rank %% 2 == 1, 9.55, -9.55),
      label_vjust = dplyr::if_else(label_rank %% 2 == 1, 1, 0),
      label_nudge_x = label_target_x - mean_pre,
      label_nudge_y = label_target_y - mean_diff
    ) %>%
    dplyr::ungroup()

  # Function to calculate effect sizes
  calculate_effect_sizes <- function(data) {
    data %>%
      dplyr::group_by(!!rumor_var, !!by_var) %>%
      dplyr::summarise(
        effect_size = -1 * effsize::cohen.d(
          formula = as.formula(paste("I(", quo_name(diff_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$estimate,
        p_value = t.test(
          formula = as.formula(paste("I(", quo_name(diff_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$p.value,
        .groups = "drop"
      )
  }
  
  # Calculate effect sizes
  effect_sizes <- calculate_effect_sizes(data)
  # aggregate data
  agg_data <- data %>%
    dplyr::group_by(!!rumor_var, !!by_var, !!treatment_var, !!pre_var, !!diff_var) %>%
    dplyr::summarise(
      n = n(),
      .groups = "drop"
    )
  # scaleFUN <- function(x) sprintf("%.2f", x)
  has_rumor <- !(quo_is_null(rumor_var) || quo_is_missing(rumor_var))
  has_by <- !(quo_is_null(by_var) || quo_is_missing(by_var))
  has_two_facets <- has_rumor && has_by
  axis_text_size_current <- font_sizes$axis_text
  axis_title_size_current <- font_sizes$axis_title
  strip_text_size_current <- font_sizes$strip_text
  if (!has_rumor & !has_by) {
    annotation_size <- 4.15
  } else if (!(has_rumor && has_by)) {
    annotation_size <- 3.45
  } else {
    annotation_size <- 2.95
  }
  if (has_two_facets) {
    axis_text_size_current <- font_sizes$axis_text * 0.85
    axis_title_size_current <- font_sizes$axis_title * 0.9
    strip_text_size_current <- font_sizes$strip_text * 0.9
  }
  show_annotations <- !has_two_facets
  # Create the plot
  plot <- ggplot(agg_data, aes(x = !!pre_var, y = !!diff_var, color = !!treatment_var)) +
    geom_point(
      aes(size = n),
      alpha = 0.55,
      position = position_jitter(width = 0.1, height = 0.1),
      show.legend = FALSE
    ) +
    scale_size_continuous(range = c(4, 12)) +
    geom_vline(xintercept = 5, linetype = "dotted", color = "grey40") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_errorbar(
      data = summary_stats,
      aes(
        x = mean_pre,
        ymin = ci_diff_lower,
        ymax = ci_diff_upper,
        color = !!treatment_var
      ),
      width = 0.1,
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_errorbarh(
      data = summary_stats,
      aes(
        y = mean_diff,
        xmin = ci_pre_lower,
        xmax = ci_pre_upper,
        color = !!treatment_var
      ),
      height = 0.1,
      linewidth = 0.8,
      inherit.aes = FALSE
    ) +
    geom_point(
      data = summary_stats,
      aes(x = mean_pre, y = mean_diff, color = !!treatment_var),
      size = 9,
      shape = 21,
      stroke = 1.6,
      fill = "white",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         color = "Treatment") +
    coord_cartesian(xlim = c(0,10), ylim = c(-10,10), clip = "off") +
    facet_grid(vars(!!rumor_var), vars(!!by_var)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = font_sizes$legend_title, face = "bold"),
      legend.text = element_text(size = font_sizes$legend_text),
      plot.title = element_text(size = font_sizes$title, face = "bold", hjust = 0, margin = margin(b = 4)),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = font_sizes$subtitle, hjust = 0, margin = margin(b = 10)),
      axis.title = element_text(size = axis_title_size_current),
      axis.text = element_text(size = axis_text_size_current),
      strip.text = element_text(size = strip_text_size_current, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 18, 12, 18)
    ) +
    # scale_y_continuous(labels=scales::label_number(accuracy=1)) + 
    # scale_x_continuous(labels=scales::label_number(accuracy=1)) +
    scale_y_continuous(
      breaks = c(-10, -8, -5, -2, 0, 2, 5, 8, 10),
      labels = c(-10, -8, -5, -2, 0, 2, 5, 8, 10),
      minor_breaks = -10:10
    ) +
    scale_x_continuous(
      breaks = c(0, 2, 5, 8, 10),
      labels = c(0, 2, 5, 8, 10),
      minor_breaks = 0:10
    ) +
    scale_color_manual(values = wesanderson::wes_palette("Royal1", 2)) +
    guides(color = guide_legend(override.aes = list(size = 6)))

  if (show_annotations) {
    plot <- plot +
      geom_segment(
        data = summary_stats,
        aes(
          x = mean_pre,
          y = mean_diff,
          xend = label_target_x,
          yend = label_target_y,
          color = !!treatment_var
        ),
        inherit.aes = FALSE,
        linewidth = 0.45,
        show.legend = FALSE
      ) +
      geom_label(
        data = summary_stats,
        aes(x = label_target_x, y = label_target_y,
            vjust = label_vjust,
            label = sprintf(
              "%s\nMean: pre %.2f, post %.2f, diff %.2f\nSD: pre %.2f, post %.2f, diff %.2f\n95%% CI pre: [%.2f, %.2f]\n95%% CI post: [%.2f, %.2f]\n95%% CI diff: [%.2f, %.2f]\nn: %d",
              !!treatment_var,
              mean_pre, mean_post, mean_diff,
              sd_pre, sd_post, sd_diff,
              ci_pre_lower, ci_pre_upper,
              ci_post_lower, ci_post_upper,
              ci_diff_lower, ci_diff_upper,
              n
            )),
        inherit.aes = FALSE,
        size = annotation_size,
        fill = "white",
        label.size = 0,
        label.padding = unit(0.13, "lines"),
        label.r = unit(0.05, "lines"),
        hjust = 0
      ) +
      geom_label(
        data = effect_sizes,
        aes(x = -Inf, y = Inf, 
            label = sprintf("ATT Cohen's d: %.2f\np-value: %.3f", 
                            effect_size, p_value),
            fontface = ifelse(p_value < 0.05, 2, 1)
        ),
        hjust = -0.1, vjust = 1.1, size = annotation_size * 0.82,
        fill = "white",
        label.size = 0,
        label.padding = unit(0.08, "lines"),
        inherit.aes = FALSE
      )
  }
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    save_plot_to_writing(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}

# Function to create CISA time series plot
create_time_series_plot <- function(data, treatment_var, pre_var, post_var, recontact_var, by_var, rumor_var, ylab="Score", title="Time Series Plot", subtitle = "By Treatment Condition", weight_var = NULL) {
  treatment_var <- enquo(treatment_var)
  pre_var <- enquo(pre_var)
  post_var <- enquo(post_var)
  recontact_var <- enquo(recontact_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  weight_var <- enquo(weight_var)
  font_sizes <- get_plot_font_sizes()

  # Prepare the data
  plot_data <- data %>% 
    dplyr::select(
      !!treatment_var, 
      !!pre_var, 
      !!post_var,
      !!recontact_var,
      !!by_var, 
      !!rumor_var,
      !!weight_var
    )

  if (!quo_is_null(weight_var)) {
    plot_data <- plot_data %>% 
      dplyr::filter(!is.na(!!weight_var))
  }

  plot_data_long <- plot_data %>% 
    pivot_longer(cols = c(!!pre_var, !!post_var, !!recontact_var),
                 names_to = "time", 
                 values_to = "score") %>%
      mutate(time = case_when(
        time == quo_name(pre_var) ~ "Pre",
        time == quo_name(post_var) ~ "Post",
        time == quo_name(recontact_var) ~ "Recontact",
        TRUE ~ time
      ) %>% 
    factor(levels = c("Pre", "Post", "Recontact")))
    #               %>%
    # mutate(time = factor(time, levels = c(!!pre_var, !!post_var, !!recontact_var),
    #                      labels = c("Pre", "Post", "Recontact")))

  # aggregate data
  plot_data_agg <- plot_data_long %>%
    group_by(!!treatment_var, !!by_var, !!rumor_var, time, score) %>%
    summarise(
      n = n(),
      .groups = 'drop'
    ) 
  # Calculate means and standard errors
  dat_summary <- plot_data_long %>%
    group_by(time, !!treatment_var, !!by_var, !!rumor_var) %>%
    summarise(
      # weighted version of mean and se if weight_var is provided
      mean_score = ifelse(quo_is_null(weight_var), mean(score, na.rm = TRUE), weighted.mean(score, w = !!weight_var, na.rm = TRUE)),
      # mean_score = mean(score, na.rm = TRUE),
      se = ifelse(quo_is_null(weight_var), sd(score, na.rm = TRUE) / sqrt(n()), weighted.se.mean(score, w = !!weight_var, na.rm = TRUE)),
      # se = sd(score, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    )
  dodge_width <- 0.2
  # Create the plot
  p <- ggplot() +
    # Individual points
    # geom_jitter(data = plot_data_long, 
    #             aes(x = time, y = score, color = !!treatment_var),
    #             alpha = 0.2, width = 0.2) +
    geom_point(data = plot_data_agg, 
               aes(x = time, y = score, color = !!treatment_var, size = n),
               position = position_jitter(width=0.1, height=0.1)) +
    geom_violinhalf(data = plot_data_long, 
                aes(x = time, y = score, color = !!treatment_var, fill = !!treatment_var),
                alpha = 0.4, position = "identity", flip = TRUE, width = 0.5
                ) +
    # Means and error bars
    geom_line(data = dat_summary,
              aes(x = time, y = mean_score, color = !!treatment_var, group = !!treatment_var),
              # position = position_dodge(width = dodge_width),
              position = position_nudge(x = dodge_width),
              size = 1) +
    geom_point(data = dat_summary,
               aes(x = time, y = mean_score, color = !!treatment_var),
              #  position = position_dodge(width = dodge_width),
              position = position_nudge(x = dodge_width),
               size = 5) +
    geom_errorbar(data = dat_summary,
                  aes(x = time, y = mean_score, 
                      ymin = mean_score - 1.96 * se, 
                      ymax = mean_score + 1.96 * se,
                      color = !!treatment_var),
                  # position = position_dodge(width = dodge_width),
                  position = position_nudge(x = dodge_width),
                  width = 0.3) +
    coord_cartesian(ylim = c(0, 10)) +
    facet_grid(vars(!!rumor_var), vars(!!by_var)) +
    labs(title = title,
        subtitle = subtitle,
         x = "Time",
         y = ylab,
         color = "Treatment") + 
    theme_minimal() + 
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = font_sizes$legend_title, face = "bold"),
      legend.text = element_text(size = font_sizes$legend_text),
      plot.title = element_text(size = font_sizes$title, face = "bold", hjust = 0),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = font_sizes$subtitle, hjust = 0),
      axis.title = element_text(size = font_sizes$axis_title),
      axis.text = element_text(size = font_sizes$axis_text),
      strip.text = element_text(size = font_sizes$strip_text, face = "bold")
    ) +
    guides(fill = "none") +
    # theme(
    #   legend.position = "bottom",
    #   axis.text.x = element_text(angle = 45, hjust = 1),
    #   strip.text = element_text(face = "bold"),
    #   panel.spacing = unit(1, "lines")
    # ) + 
    scale_color_manual(values = wes_palette("Royal1", 2), 
                       labels = c("Control", "Treatment")) + 
    scale_fill_manual(values = wes_palette("Royal1", 2),
                      labels = c("Control", "Treatment"))

  return(p)
}


coef_plot_defaults <- list(
  point_size = 4.2,
  error_bar_height = 0.35,
  text_size = get_plot_font_sizes()$axis_title,
  wrap_width = 30,
  color_values = c(
    "Positive" = "#0072B2",
    "Negative" = "#D55E00",
    "Not Significant" = "gray60"
  ),
  shape_values = c(
    "Post-treatment" = 16,
    "Recontact" = 17
  ),
  vline_color = "gray65",
  vline_linetype = "dashed"
)

get_coef_plot_defaults <- function() {
  if (exists("coef_plot_overrides", envir = .GlobalEnv)) {
    return(modifyList(coef_plot_defaults, get("coef_plot_overrides", envir = .GlobalEnv)))
  }
  coef_plot_defaults
}

format_coef_labels <- function(labels, wrap_width = coef_plot_defaults$wrap_width) {
  if (is.null(labels)) {
    return(NULL)
  }

  vapply(labels, function(label) {
    if (is.null(label) || is.na(label) || identical(label, "")) {
      return(label)
    }

    cleaned <- gsub("_", " ", label, fixed = TRUE)
    cleaned <- gsub("\\.{2,}", " ", cleaned)
    cleaned <- gsub("Treatment\\s*\\.+\\s*", "Treatment × ", cleaned, perl = TRUE)
    cleaned <- gsub("Treatment\\s*[xX×]\\s*", "Treatment × ", cleaned, perl = TRUE)
    cleaned <- gsub("\\s+", " ", cleaned)
    cleaned <- trimws(cleaned)

    wrapped <- strwrap(cleaned, width = wrap_width)
    paste(wrapped, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

plot_coefficients <- function(model_lists, 
                                       var_name = "Election_Rumor_Placebo_RandomizationTreatment",
                                       title = "Estimated Treatment Effects for OLS Models",
                                       subtitle = "Across Different Outcomes",
                                       x_label = "Estimated Treatment Effect",
                                       y_label = "Model",
                                       point_size = NULL,
                                       error_bar_height = NULL,
                                       text_size = NULL,
                                       wrap_width = NULL,
                                       debug = FALSE) {

  defaults <- get_coef_plot_defaults()
  font_sizes <- get_plot_font_sizes()
  if (is.null(point_size)) point_size <- defaults$point_size
  if (is.null(error_bar_height)) error_bar_height <- defaults$error_bar_height
  if (is.null(text_size)) text_size <- defaults$text_size
  if (is.null(wrap_width)) wrap_width <- defaults$wrap_width
  axis_title_size <- max(font_sizes$axis_title, text_size)
  axis_text_size <- max(font_sizes$axis_text, text_size * 0.85)
  title_size <- max(font_sizes$title - 2, text_size * 1.2)
  subtitle_size <- max(font_sizes$subtitle - 4, text_size * 0.9)
  legend_title_size <- max(font_sizes$legend_title, text_size * 0.85)
  legend_text_size <- max(font_sizes$legend_text, text_size * 0.8)
  title <- format_plot_title_lines(title, max_width = 58)
  subtitle <- format_plot_title_lines(subtitle, max_width = 76)
  
  extract_coefs <- function(model, var_name, model_name, is_cisa = FALSE) {
    if (is.null(model) || !inherits(model, "lm")) {
      if(debug) cat("Skipping", model_name, "- not an lm object\n")
      return(NULL)
    }
    
    coef_data <- tidy(model) %>%
      filter(term == var_name) %>%
      select(estimate, std.error) %>%
      mutate(
        model = if (is_cisa) {
          parts <- str_split(model_name, ": ", n = 3)[[1]]
          paste(parts[1:min(2, length(parts))], collapse = ": ")
        } else model_name,
        significance = case_when(
          estimate - 1.96 * std.error > 0 ~ "Positive",
          estimate + 1.96 * std.error < 0 ~ "Negative",
          TRUE ~ "Not Significant"
        ),
        type = if_else(str_detect(model_name, "Recontact|Followup"), "Recontact", "Post-treatment")
      )
    
    if (nrow(coef_data) == 0) {
      if(debug) cat("No coefficients found for", model_name, "\n")
      return(NULL)
    }
    if(debug) cat("Extracted coefficients for", model_name, "\n")
    coef_data
  }
  
  process_model_list <- function(models, prefix = "") {
    if(debug) cat("Processing", prefix, "\n")
    map_dfr(names(models), function(model_name) {
      full_name <- paste(prefix, model_name, sep = if(prefix == "") "" else ": ")
      if (is.list(models[[model_name]]) && !inherits(models[[model_name]], "lm")) {
        if(debug) cat("Recursing into", full_name, "\n")
        process_model_list(models[[model_name]], full_name)
      } else {
        if(debug) cat("Extracting coefficients for", full_name, "\n")
        extract_coefs(models[[model_name]], var_name, full_name, is_cisa = grepl("CISA", model_name))
      }
    })
  }
  
  all_coefs <- process_model_list(model_lists)
  
  if (nrow(all_coefs) == 0) {
    stop("No coefficients found for the specified variable name.")
  }
  
  if(debug) {
    print(str(all_coefs))
    print(head(all_coefs))
  }
  
  all_coefs <- all_coefs %>%
    mutate(model = factor(model, levels = rev(unique(model))))
  
  ggplot(all_coefs, aes(x = estimate, y = model, color = significance, shape = type)) +
    geom_vline(
      xintercept = 0,
      linetype = defaults$vline_linetype,
      color = defaults$vline_color
    ) +
    geom_point(size = point_size) +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                       xmax = estimate + 1.96 * std.error), 
                   height = error_bar_height) +
    labs(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      color = "Significance",
      shape = "Type"
    ) +
    scale_color_manual(values = defaults$color_values) +
    scale_shape_manual(values = defaults$shape_values) +
    guides(
      color = guide_legend(order = 1, nrow = 1, override.aes = list(size = point_size)),
      shape = guide_legend(order = 2, nrow = 1)
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.y = element_text(face = "bold", size = axis_title_size, margin = margin(r = 10)),
      axis.title.x = element_text(face = "bold", size = axis_title_size, margin = margin(t = 12)),
      plot.title = element_text(face = "bold", hjust = 0, size = title_size),
      plot.subtitle = element_text(hjust = 0, size = subtitle_size),
      axis.text = element_text(size = axis_text_size),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.justification = "center",
      legend.title = element_text(face = "bold", size = legend_title_size * 0.9),
      legend.text = element_text(size = legend_text_size * 0.9),
      plot.title.position = "plot",
      plot.margin = margin(18, 30, 18, 18)
    ) +
    coord_cartesian(xlim = c(-1, 1)) +
    scale_y_discrete(labels = function(x) format_coef_labels(x, wrap_width = wrap_width))
}

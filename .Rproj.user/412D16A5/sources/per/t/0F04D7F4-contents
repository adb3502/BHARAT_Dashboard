# data_loader.R

library(tidyverse)
library(lubridate)

# Debug function for data checks
print_debug <- function(data, msg = "") {
  print(paste("DEBUG:", msg))
  print(head(data$participant_id))
  print(table(data$sex, useNA = "always"))
}

# Clean up column names
clean_colnames <- function(x) {
  x %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("-", "_") %>%
    str_to_lower()
}

# Main data loading function
load_bharat_data <- function(data_dir = "data/raw/") {
  # List of all data files
  file_list <- list(
    personal_info = list(
      file = "Personal Information.csv",
      key_cols = c("Participant_ID")
    ),
    mmse = list(
      file = "Annexure-1 MMSE (Mini Mental St.csv",
      key_cols = c("Participant_ID")
    ),
    frailty = list(
      file = "Annexure-2 Simple Frail Questio.csv",
      key_cols = c("Participant_ID")
    ),
    anthropometry = list(
      file = "Anthropometry.csv",
      key_cols = c("Participant_ID")
    ),
    comorbidities = list(
      file = "Co-Morbidities.csv",
      key_cols = c("Participant_ID")
    ),
    covid = list(
      file = "COVID-19 Infection.csv",
      key_cols = c("Participant_ID")
    ),
    dass21 = list(
      file = "DASS 21 Scale.csv",
      key_cols = c("Participant_ID")
    ),
    diet = list(
      file = "Dietary Pattern.csv",
      key_cols = c("Participant_ID")
    ),
    exercise = list(
      file = "Exercise Habits.csv",
      key_cols = c("Participant_ID")
    ),
    food_freq = list(
      file = "Food Frequency List.csv",
      key_cols = c("Participant_ID")
    ),
    habits = list(
      file = "Habits.csv",
      key_cols = c("Participant_ID")
    ),
    exam = list(
      file = "Head-to-toe Examination.csv",
      key_cols = c("Participant_ID")
    ),
    personal_data = list(
      file = "Personal Data.csv",
      key_cols = c("Participant_ID")
    ),
    phys_exam = list(
      file = "Physical Examination.csv",
      key_cols = c("Participant_ID")
    ),
    screening = list(
      file = "Screening test.csv",
      key_cols = c("Participant_ID")
    ),
    systemic = list(
      file = "Systemic Examination.csv",
      key_cols = c("Participant_ID")
    ),
    vitals = list(
      file = "Vitals.csv",
      key_cols = c("Participant_ID")
    )
  )
  
  # Load each dataset
  data_list <- map(names(file_list), function(name) {
    tryCatch({
      # Read the file with tab delimiter
      df <- read_delim(
        file.path(data_dir, file_list[[name]]$file),
        delim = "\t",
        show_col_types = FALSE,
        guess_max = 10000
      )
      
      # Clean column names
      names(df) <- clean_colnames(names(df))
      
      # Ensure participant_id is properly formatted
      df <- df %>%
        rename_with(~"participant_id", matches("^participant.*id$", ignore.case = TRUE))
      
      list(name = name, data = df)
      
    }, error = function(e) {
      warning(sprintf("Error reading %s: %s", name, e$message))
      return(NULL)
    })
  })
  
  # Remove NULL entries
  data_list <- compact(data_list)
  
  # Get all unique participant IDs
  all_ids <- unique(unlist(map(data_list, ~unique(.x$data$participant_id))))
  
  # Create template dataframe
  template <- tibble(participant_id = all_ids)
  
  # Join all datasets
  combined_data <- reduce(
    map(data_list, "data"),
    ~left_join(.x, .y, by = "participant_id"),
    .init = template
  )
  
  # Print data loading summary
  print(sprintf("Loaded %d datasets", length(data_list)))
  print(sprintf("Total participants: %d", length(all_ids)))
  print("Columns available:")
  print(names(combined_data))
  
  # Add BHARAT study age groups
  combined_data <- combined_data %>%
    mutate(
      age_group = case_when(
        age_years < 30 ~ "18-29",
        age_years < 45 ~ "30-44",
        age_years < 60 ~ "45-59",
        age_years < 75 ~ "60-74",
        TRUE ~ "75+"
      ),
      age_group = factor(age_group, 
                         levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
      sex = factor(gender, levels = c("Male", "Female")), # Convert gender to sex
      group_combined = paste(age_group, sex)
    ) %>%
    # Remove redundant gender column after conversion to sex
    select(-gender)
  
  # Add data quality indicators with correct column names
  combined_data <- combined_data %>%
    mutate(
      data_completeness = rowSums(!is.na(select(., -participant_id))) / 
        (ncol(.) - 1) * 100,
      has_vital_signs = !is.na(pulse_rate_bpm) & !is.na(blood_pressure_mmhg),
      has_anthropometry = !is.na(height_cm) & !is.na(weight_kg),
      has_cognitive = !is.na(mmse_total_score),
      has_frailty = !is.na(frailty_total_score)
    )
  
  
  # Print basic data quality summary
  print("\nData Quality Summary:")
  print(sprintf("Average completeness: %.1f%%", 
                mean(combined_data$data_completeness, na.rm = TRUE)))
  print(sprintf("Participants with vital signs: %d", 
                sum(combined_data$has_vital_signs, na.rm = TRUE)))
  print(sprintf("Participants with anthropometry: %d", 
                sum(combined_data$has_anthropometry, na.rm = TRUE)))
  
  return(combined_data)
}

# Function to check data completeness
check_data_completeness <- function(data) {
  completeness <- map_df(data, ~sum(!is.na(.)) / length(.)) %>%
    gather(variable, completeness) %>%
    arrange(desc(completeness))
  
  return(completeness)
}

# Function to get basic summary statistics
get_summary_stats <- function(data, vars = NULL) {
  if(is.null(vars)) {
    vars <- names(select_if(data, is.numeric))
  }
  
  data %>%
    select(all_of(vars)) %>%
    summary()
}

# Function to identify outliers
identify_outliers <- function(data, var, method = "iqr") {
  if(method == "iqr") {
    q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers <- data %>%
      filter(!!sym(var) < lower_bound | !!sym(var) > upper_bound)
    
    return(outliers)
  }
}
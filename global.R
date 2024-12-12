# global.R

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)
library(readxl)
library(scales)
library(viridis)
library(wesanderson)
library(effsize)
library(rstatix)
library(showtext)
library(sysfonts)
library(shinyBS)  

# Add custom fonts
font_add_google("Manrope", "Manrope")
showtext_auto()

# Function to categorize age into standard groups
standardize_age_group <- function(age) {
  case_when(
    age >= 18 & age <= 29 ~ "18-29",
    age >= 30 & age <= 44 ~ "30-44",
    age >= 45 & age <= 59 ~ "45-59",
    age >= 60 & age <= 74 ~ "60-74",
    age >= 75 ~ "75+",
    TRUE ~ NA_character_
  )
}

# Add this function to create age groups
create_age_groups <- function(age) {
  case_when(
    age >= 18 & age < 30 ~ "18-29",
    age >= 30 & age < 45 ~ "30-44",
    age >= 45 & age < 60 ~ "45-59",
    age >= 60 & age < 75 ~ "60-74",
    age >= 75 ~ "75+",
    TRUE ~ NA_character_
  )
}

# Modify the data loading and processing
load_and_process_data <- function() {
  # Read data
  personal_info <- read.csv("personal_information.csv", stringsAsFactors = FALSE)
  blood_test_results <- read.csv("blood_test_results.csv", stringsAsFactors = FALSE)
  blood_test_metadata <- read.csv("blood_test_metadata.csv", stringsAsFactors = FALSE)
  
  # Create age groups in personal info
  personal_info <- personal_info %>%
    mutate(
      age_group = create_age_groups(age_years),
      age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60-74", "75+"))
    )
  
  # Merge personal info with blood test results
  processed_data <- blood_test_results %>%
    left_join(personal_info, by = c("participant_id" = "Participant_ID")) %>%
    mutate(
      sex = gender,
      group_combined = paste(age_group, sex)
    )
  
  return(list(
    data = processed_data,
    metadata = blood_test_metadata
  ))
}

# Load personal information
personal_info <- read_csv("data/Personal Information.csv") %>%
  select(participant_id = Participant_ID, 
         age = age_years, 
         sex = gender) %>%
  # Clean any whitespace in IDs
  mutate(
    participant_id = str_trim(participant_id),
    age_group = standardize_age_group(age),
    sex = factor(sex, levels = c("Male", "Female"))
  )

# Load blood data
blood_data <- read_csv("data/blood_test_results.csv") %>%
  rename_with(~str_replace_all(., "-", "_"))

# Process BHARAT data with demographics
blood_data_with_demo <- blood_data %>%
  left_join(personal_info, by = "participant_id") %>%
  mutate(
    dataset = "BHARAT",
    group_combined = paste(age_group, sex)
  )

# Initialize integrated datasets list
integrated_datasets <- list(
  "BHARAT" = blood_data_with_demo
)

# Metadata handling
blood_metadata <- read_csv("data/blood_test_metadata.csv") %>%
  rename_with(~str_replace_all(., "-", "_"))

# Create named vector for parameter selection
blood_params <- names(blood_data)[!names(blood_data) %in% 
                                   c("participant_id", "month")]
param_choices <- setNames(
  blood_params,
  str_to_title(str_replace_all(blood_params, "_", " "))
)

# Standard age groups
standard_age_groups <- c(
  "18-29" = "18-29",
  "30-44" = "30-44",
  "45-59" = "45-59",
  "60-74" = "60-74",
  "75+" = "75+"
)

# Grouping options
grouping_choices <- c(
  "Age Groups" = "age",
  "Sex" = "sex",
  "Age and Sex Combined" = "combined",
  "Custom Groups" = "custom"
)

# Secondary coloring options
color_by_choices <- c(
  "None" = "none",
  "Age Groups" = "age_group",
  "Sex" = "sex",
  "Dataset" = "dataset"
)

# Color scheme options
color_schemes <- c(
  "Zissou" = "zissou",
  "Darjeeling" = "darjeeling",
  "Royal" = "royal"
)

# Dataset colors
dataset_colors <- c(
  "BHARAT" = "#FF9642",
  "External_1" = "#00A08A",
  "External_2" = "#F2AD00",
  "External_3" = "#5BBCD6",
  "External_4" = "#E6AA68",
  "External_5" = "#A1C181"
)

# Function to get colors based on scheme and groups needed
get_color_palette <- function(scheme, n) {
  colors <- switch(scheme,
                  "zissou" = wes_palette("Zissou1"),
                  "darjeeling" = wes_palette("Darjeeling1"),
                  "royal" = wes_palette("Royal1"),
                  wes_palette("Zissou1")  # default
  )
  
  if(n <= length(colors)) {
    return(colors[1:n])
  } else {
    colorRampPalette(colors)(n)
  }
}

# Function to filter data by selected age groups
filter_by_age_groups <- function(data, selected_groups) {
  data %>%
    filter(age_group %in% selected_groups)
}

# Function to apply custom grouping
apply_custom_group <- function(data, custom_groups) {
  if (length(custom_groups) == 0) {
    data$custom_group <- "No Groups Defined"
    return(data)
  }
  
  # Create cases for case_when
  cases <- lapply(names(custom_groups), function(group_name) {
    quo(age_group %in% !!custom_groups[[group_name]] ~ !!group_name)
  })
  
  # Add default case
  cases$default <- quo(TRUE ~ "Other")
  
  # Apply grouping
  data %>%
    mutate(custom_group = case_when(!!!cases))
}

# Function to calculate volcano plot data
calculate_volcano_data <- function(data, param_list, grouping_var) {
  map_df(param_list, function(param) {
    tryCatch({
      # Prepare data
      test_data <- data %>%
        select(!!sym(grouping_var), !!sym(param)) %>%
        drop_na()
      
      if(n_distinct(test_data[[grouping_var]]) == 2) {
        # For two groups - use t-test and Cohen's d
        t_res <- t.test(as.formula(paste(param, "~", grouping_var)), data = test_data)
        eff_size <- cohens_d(test_data, as.formula(paste(param, "~", grouping_var)))$effsize
        
        tibble(
          parameter = param,
          p_value = t_res$p.value,
          effect_size = eff_size,
          test_type = "t-test"
        )
      } else if(n_distinct(test_data[[grouping_var]]) > 2) {
        # For multiple groups - use ANOVA and eta squared
        aov_res <- aov(as.formula(paste(param, "~", grouping_var)), data = test_data)
        p_val <- summary(aov_res)[[1]]$"Pr(>F)"[1]
        eta_sq <- summary.lm(aov_res)$r.squared
        
        tibble(
          parameter = param,
          p_value = p_val,
          effect_size = eta_sq,
          test_type = "ANOVA"
        )
      }
    }, error = function(e) NULL)
  }) %>%
    mutate(
      significant = p_value < 0.05,
      fdr_p_value = p.adjust(p_value, method = "BH"),
      parameter_label = str_to_title(str_replace_all(parameter, "_", " "))
    )
}

# Function to validate external dataset
validate_dataset <- function(dataset) {
  required_cols <- c("participant_id", "dataset", "age", "sex", "age_group")
  missing_cols <- required_cols[!required_cols %in% names(dataset)]
  
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  if (any(duplicated(dataset$participant_id))) {
    return(list(
      valid = FALSE,
      message = "Dataset contains duplicate participant IDs"
    ))
  }
  
  # Validate age groups
  invalid_age_groups <- setdiff(unique(dataset$age_group), names(standard_age_groups))
  if (length(invalid_age_groups) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Invalid age groups found:", paste(invalid_age_groups, collapse = ", "))
    ))
  }
  
  # Validate sex values
  invalid_sex <- setdiff(unique(dataset$sex), c("Male", "Female"))
  if (length(invalid_sex) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Invalid sex values found:", paste(invalid_sex, collapse = ", "))
    ))
  }
  
  return(list(valid = TRUE, message = "Dataset validation successful"))
}

# Function to process external dataset
process_external_data <- function(data_path, mapping_df, dataset_name) {
  # Read external data
  external_data <- read_csv(data_path, show_col_types = FALSE)
  
  # Get required column mappings
  id_col <- mapping_df$external_column[mapping_df$bharat_column == "participant_id"]
  age_col <- mapping_df$external_column[mapping_df$bharat_column == "age"]
  sex_col <- mapping_df$external_column[mapping_df$bharat_column == "sex"]
  
  # Start with required columns
  processed_data <- tibble(
    participant_id = as.character(external_data[[id_col]]),
    age = as.numeric(external_data[[age_col]])
  )
  
  # Add age_group using standardize_age_group function
  processed_data <- processed_data %>%
    mutate(
      age_group = standardize_age_group(age),
      age_group = factor(age_group, levels = c("18-29", "30-44", "45-59", "60-74", "75+"))
    )
  
  # Process sex with standardization
  sex_values <- external_data[[sex_col]]
  sex_values <- str_trim(sex_values)
  sex_values <- case_when(
    sex_values %in% c("M", "m", "Male", "MALE") ~ "Male",
    sex_values %in% c("F", "f", "Female", "FEMALE") ~ "Female",
    TRUE ~ NA_character_
  )
  
  # Add processed columns
  processed_data <- processed_data %>%
    mutate(
      sex = factor(sex_values, levels = c("Male", "Female")),
      dataset = dataset_name,
      group_combined = paste(age_group, sex)
    )
  
  # Process remaining columns from mapping
  for (i in 1:nrow(mapping_df)) {
    ext_col <- mapping_df$external_column[i]
    bharat_col <- mapping_df$bharat_column[i]
    data_type <- mapping_df$data_type[i]
    
    # Skip already processed columns
    if (bharat_col %in% c("participant_id", "age", "sex", "dataset", "age_group", "group_combined")) {
      next
    }
    
    if (ext_col %in% names(external_data)) {
      tryCatch({
        raw_values <- external_data[[ext_col]]
        
        converted_values <- switch(data_type,
          "numeric" = {
            nums <- str_remove_all(as.character(raw_values), ",")
            as.numeric(nums)
          },
          "character" = as.character(raw_values),
          "factor" = as.factor(raw_values),
          "date" = as.Date(raw_values),
          raw_values
        )
        
        processed_data[[bharat_col]] <- converted_values
        
      }, error = function(e) {
        warning(sprintf(
          "Error converting column %s to %s. Sample values: %s. Error: %s",
          ext_col, data_type, 
          paste(head(raw_values), collapse = ", "),
          e$message
        ))
        processed_data[[bharat_col]] <- NA
      })
    }
  }
  
  # Print debug info
  print("Processed data dimensions:")
  print(dim(processed_data))
  print("Columns in processed data:")
  print(names(processed_data))
  
  return(processed_data)
}

# Function to generate mapping template
generate_mapping_template <- function() {
  bharat_cols <- names(blood_data_with_demo)
  template <- data.frame(
    external_column = rep("", length(bharat_cols)),
    bharat_column = bharat_cols,
    data_type = sapply(blood_data_with_demo, function(x) class(x)[1]),
    description = paste("Enter description for", bharat_cols),
    required = ifelse(bharat_cols %in% c("participant_id"), "Yes", "No")
  )
  return(template)
}

# Modify the prepare_plot_data function
prepare_plot_data <- function(data, selected_datasets, selected_age_groups) {
  filtered_data <- data %>%
    filter(
      dataset %in% selected_datasets,
      age_group %in% selected_age_groups
    )
  return(filtered_data)
}

# Make sure age group selection is properly defined
selectInput("age_groups", 
            "Select Age Groups:",
            choices = c("18-29", "30-44", "45-59", "60-74", "75+"),
            selected = c("18-29", "30-44", "45-59", "60-74", "75+"),
            multiple = TRUE)
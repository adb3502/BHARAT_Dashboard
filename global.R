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
  
  # Create cases for case_when using the order of the custom groups
  cases <- lapply(names(custom_groups), function(group_name) {
    quo(age_group %in% !!custom_groups[[group_name]] ~ !!group_name)
  })
  
  # Add default case
  cases$default <- quo(TRUE ~ "Other")
  
  # Apply grouping and convert to factor with levels in original order
  data %>%
    mutate(
      custom_group = case_when(!!!cases),
      custom_group = factor(custom_group, levels = c(names(custom_groups), "Other"))
    )
}

# Function to calculate volcano plot data
calculate_volcano_data <- function(data, parameters, grouping_var, comparison) {
  # Split comparison into two groups
  groups <- strsplit(comparison, " vs ")[[1]]
  
  # Subset data for selected groups
  subset_data <- data %>%
    filter(!!sym(grouping_var) %in% groups)
  
  # Initialize results dataframe
  results <- data.frame(
    parameter = character(),
    parameter_label = character(),
    effect_size = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # For each parameter
  for(param in parameters) {
    tryCatch({
      # Get data for each group
      group1_data <- subset_data[[param]][subset_data[[grouping_var]] == groups[1]]
      group2_data <- subset_data[[param]][subset_data[[grouping_var]] == groups[2]]
      
      # Remove NA values
      group1_data <- group1_data[!is.na(group1_data)]
      group2_data <- group2_data[!is.na(group2_data)]
      
      # Calculate statistics
      n1 <- length(group1_data)
      n2 <- length(group2_data)
      m1 <- mean(group1_data)
      m2 <- mean(group2_data)
      s1 <- sd(group1_data)
      s2 <- sd(group2_data)
      
      # Pooled standard deviation
      pooled_sd <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
      
      # Effect size (Cohen's d)
      effect_size <- (m2 - m1)/pooled_sd
      
      # Welch's t-test
      t_result <- t.test(group2_data, group1_data, var.equal = FALSE)
      
      # Add to results
      results <- rbind(results, data.frame(
        parameter = param,
        parameter_label = str_to_title(str_replace_all(param, "_", " ")),
        effect_size = effect_size,
        p_value = t_result$p.value,
        comparison = comparison,
        group1 = groups[1],
        group2 = groups[2],
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      warning(paste("Error processing parameter:", param, "-", e$message))
    })
  }
  
  # Add FDR-adjusted p-values
  if(nrow(results) > 0) {
    results <- results %>%
      mutate(
        fdr_p_value = p.adjust(p_value, method = "fdr"),
        significant = fdr_p_value < 0.05,
        log_p = -log10(p_value)
      )
  }
  
  return(results)
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

# Function to generate mapping template with proper metadata
generate_mapping_template <- function() {
  bharat_cols <- names(blood_data_with_demo)
  template <- data.frame(
    external_column = rep("", length(bharat_cols)),
    bharat_column = bharat_cols,
    data_type = sapply(blood_data_with_demo, function(x) class(x)[1]),
    description = paste("Enter description for", bharat_cols),
    required = ifelse(bharat_cols %in% c("participant_id", "age", "sex"), "Yes", "No")
  )
  
  # Ensure required columns appear first
  template <- template %>%
    arrange(desc(required), bharat_column)
    
  return(template)
}


# Make sure age group selection is properly defined
selectInput("age_groups", 
            "Select Age Groups:",
            choices = c("18-29", "30-44", "45-59", "60-74", "75+"),
            selected = c("18-29", "30-44", "45-59", "60-74", "75+"),
            multiple = TRUE)

# Add to global.R

# Function to save datasets
saveDatasets <- function(datasets, dir = "data/saved_datasets") {
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(datasets, file.path(dir, "integrated_datasets.rds"))
}

# Function to load saved datasets
loadSavedDatasets <- function(dir = "data/saved_datasets") {
  rds_file <- file.path(dir, "integrated_datasets.rds")
  if(file.exists(rds_file)) {
    readRDS(rds_file)
  } else {
    list("BHARAT" = blood_data_with_demo)
  }
}


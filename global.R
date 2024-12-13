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

# Data persistence functions
saveDatasets <- function(datasets, dir = "data/saved_datasets") {
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(datasets, file.path(dir, "integrated_datasets.rds"))
}

loadSavedDatasets <- function(dir = "data/saved_datasets") {
  rds_file <- file.path(dir, "integrated_datasets.rds")
  if(file.exists(rds_file)) {
    readRDS(rds_file)
  } else {
    list("BHARAT" = blood_data_with_demo)
  }
}

# Age group functions
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

# Custom grouping function
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

# Template generation function
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

# Color and visualization functions
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

# Data validation and processing functions
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
  
  return(processed_data)
}

# Load data
personal_info <- read_csv("data/Personal Information.csv") %>%
  select(participant_id = Participant_ID, 
         age = age_years, 
         sex = gender) %>%
  mutate(
    participant_id = str_trim(participant_id),
    age_group = standardize_age_group(age),
    sex = factor(sex, levels = c("Male", "Female"))
  )

blood_data <- read_csv("data/blood_test_results.csv") %>%
  rename_with(~str_replace_all(., "-", "_"))

blood_metadata <- read_csv("data/blood_test_metadata.csv") %>%
  rename_with(~str_replace_all(., "-", "_"))

# Process BHARAT data
blood_data_with_demo <- blood_data %>%
  left_join(personal_info, by = "participant_id") %>%
  mutate(
    dataset = "BHARAT",
    group_combined = paste(age_group, sex)
  )

# Define constants and choices
blood_params <- names(blood_data)[!names(blood_data) %in% 
                                   c("participant_id", "month")]
param_choices <- setNames(
  blood_params,
  str_to_title(str_replace_all(blood_params, "_", " "))
)

standard_age_groups <- c(
  "18-29" = "18-29",
  "30-44" = "30-44",
  "45-59" = "45-59",
  "60-74" = "60-74",
  "75+" = "75+"
)

grouping_choices <- c(
  "Age Groups" = "age",
  "Sex" = "sex",
  "Age and Sex Combined" = "combined",
  "Custom Groups" = "custom"
)

color_by_choices <- c(
  "None" = "none",
  "Age Groups" = "age_group",
  "Sex" = "sex",
  "Dataset" = "dataset"
)

color_schemes <- c(
  "Zissou" = "zissou",
  "Darjeeling" = "darjeeling",
  "Royal" = "royal"
)

dataset_colors <- c(
  "BHARAT" = "#FF9642",
  "External_1" = "#00A08A",
  "External_2" = "#F2AD00",
  "External_3" = "#5BBCD6",
  "External_4" = "#E6AA68",
  "External_5" = "#A1C181"
)
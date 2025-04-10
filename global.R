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
library(shinyBS)
library(factoextra)
library(RColorBrewer)

#' Data persistence functions
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

#' Age group functions
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

# Complete apply_custom_group function with strict order preservation
# Simplified apply_custom_group function - no fancy features, just basic functionality
apply_custom_group <- function(data, custom_groups) {
  # Basic input validation
  if (!is.data.frame(data) || nrow(data) == 0) {
    warning("Invalid data input")
    return(data)
  }
  if (!is.list(custom_groups) || length(custom_groups) == 0) {
    warning("Invalid custom groups")
    return(data)
  }
  if (!"age_group" %in% names(data)) {
    warning("Missing age_group column in data")
    return(data)
  }
  
  # Get the order of groups (keys of the list)
  group_names <- names(custom_groups)
  
  # Create an output data frame
  result <- data
  
  # Add a custom_group column (initialized as NA)
  result$custom_group <- NA_character_
  
  # Iterate through each custom group and assign values
  for (i in seq_along(group_names)) {
    group_name <- group_names[i]
    age_ranges <- custom_groups[[group_name]]
    
    # Find rows where age_group is in the current group's ranges
    matches <- result$age_group %in% age_ranges
    
    # Assign the group name to matching rows
    result$custom_group[matches] <- group_name
  }
  
  # Convert custom_group to a factor with levels in the original order
  result$custom_group <- factor(result$custom_group, levels = group_names)
  
  # Add a numeric order column for sorting
  result$custom_group_order <- as.integer(result$custom_group)
  
  # Handle combined groups if sex column exists
  if ("sex" %in% names(result)) {
    # Create combined group
    result$custom_group_combined <- paste(result$custom_group, result$sex)
    
    # Create all possible combinations in order
    combined_levels <- c()
    for (group in group_names) {
      combined_levels <- c(combined_levels, 
                          paste(group, "Male"), 
                          paste(group, "Female"))
    }
    
    # Set as factor with correct order
    result$custom_group_combined <- factor(result$custom_group_combined, 
                                          levels = combined_levels)
    
    # Add numeric order for combined groups
    result$custom_group_combined_order <- as.integer(result$custom_group_combined)
  }
  
  return(result)
}

custom_group_order_rv <- reactiveVal(c())

# Function to save both groups and their order
saveCustomGroupsWithOrder <- function(custom_groups, order_vector, 
                                      dir = "data/saved_custom_groups") {
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(list(groups = custom_groups, order = order_vector), 
          file.path(dir, "custom_groups.rds"))
}

# Function to load groups with their order
loadSavedCustomGroupsWithOrder <- function(dir = "data/saved_custom_groups") {
  rds_file <- file.path(dir, "custom_groups.rds")
  if(file.exists(rds_file)) {
    saved_data <- readRDS(rds_file)
    # Return both components or empty defaults
    list(
      groups = if(!is.null(saved_data$groups)) saved_data$groups else list(),
      order = if(!is.null(saved_data$order)) saved_data$order else c()
    )
  } else {
    list(groups = list(), order = c())
  }
}

#' PCA Analysis Functions
compute_pca <- function(data, selected_params, scale = TRUE) {
  # Remove any non-numeric columns and NA values
  numeric_data <- data %>%
    select(all_of(selected_params)) %>%
    drop_na()
  
  # Standardize the data (z-score standardization)
  standardized_data <- scale(numeric_data, center = TRUE, scale = TRUE)
  
  # Check for near-zero variance
  var_check <- apply(standardized_data, 2, var)
  if(any(var_check < 1e-10)) {
    warning("Some variables have near-zero variance after scaling")
  }
  
  # Perform PCA with standardized data
  pca_result <- prcomp(standardized_data, scale. = FALSE)  # Already scaled
  
  # Calculate variance explained
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  # Get loadings with variable names
  loadings <- as.data.frame(pca_result$rotation)
  
  # Get scores with proper scaling
  scores <- as.data.frame(pca_result$x)
  
  # Calculate variable contributions to each PC
  contributions <- sweep(pca_result$rotation^2, 2, pca_result$sdev^2, "*")
  contributions <- as.data.frame(contributions)
  
  # Calculate quality of representation (cos2)
  cos2 <- sweep(pca_result$rotation^2, 1, rowSums(pca_result$rotation^2), "/")
  cos2 <- as.data.frame(cos2)
  
  return(list(
    pca = pca_result,
    var_explained = var_explained,
    loadings = loadings,
    scores = scores,
    contributions = contributions,
    cos2 = cos2,
    scaling_info = list(
      center = attr(standardized_data, "scaled:center"),
      scale = attr(standardized_data, "scaled:scale")
    )
  ))
}

get_top_loadings <- function(loadings_df, pc, n = 10) {
  loadings_df %>%
    select(!!sym(pc)) %>%
    mutate(
      parameter = rownames(loadings_df),
      abs_loading = abs(!!sym(pc))
    ) %>%
    arrange(desc(abs_loading)) %>%
    slice_head(n = n) %>%
    select(-abs_loading)
}

#' Function to get variable contributions to PCs
get_var_contributions <- function(pca_result, n_vars = 10) {
  contributions <- pca_result$contributions
  
  # For each PC, get top contributing variables
  map_df(names(contributions), function(pc) {
    contributions %>%
      select(!!sym(pc)) %>%
      mutate(
        parameter = rownames(contributions),
        PC = pc,
        contribution = !!sym(pc)
      ) %>%
      arrange(desc(contribution)) %>%
      slice_head(n = n_vars) %>%
      select(-!!sym(pc))
  })
}

#' Function to assess variable quality of representation
assess_var_quality <- function(pca_result) {
  cos2_df <- pca_result$cos2
  
  # Calculate total cos2 across all PCs
  total_cos2 <- rowSums(cos2_df)
  
  tibble(
    parameter = rownames(cos2_df),
    total_cos2 = total_cos2,
    quality = case_when(
      total_cos2 >= 0.8 ~ "High",
      total_cos2 >= 0.5 ~ "Medium",
      TRUE ~ "Low"
    )
  ) %>%
    arrange(desc(total_cos2))
}

#' Data Processing Functions
process_external_data <- function(data_path, mapping_df, dataset_name) {
  tryCatch({
    # Read external data
    external_data <- read_csv(data_path, show_col_types = FALSE)
    
    # Get required column mappings
    id_col <- mapping_df$external_column[mapping_df$bharat_column == "participant_id"]
    age_col <- mapping_df$external_column[mapping_df$bharat_column == "age"]
    sex_col <- mapping_df$external_column[mapping_df$bharat_column == "sex"]
    
    if(any(is.na(c(id_col, age_col, sex_col)))) {
      stop("Missing required column mappings for participant_id, age, or sex")
    }
    
    # Process required columns
    processed_data <- tibble(
      participant_id = as.character(external_data[[id_col]]),
      age = as.numeric(external_data[[age_col]]),
      sex = factor(standardize_sex(external_data[[sex_col]]), levels = c("Male", "Female"))
    ) %>%
      mutate(
        age_group = factor(standardize_age_group(age), 
                          levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
        dataset = dataset_name,
        group_combined = paste(age_group, sex)
      )
    
    # Process remaining columns
    for(i in seq_len(nrow(mapping_df))) {
      ext_col <- mapping_df$external_column[i]
      bharat_col <- mapping_df$bharat_column[i]
      
      if(!bharat_col %in% c("participant_id", "age", "sex") && 
         !is.na(ext_col) && ext_col %in% names(external_data)) {
        processed_data[[bharat_col]] <- convert_column(external_data[[ext_col]], 
                                                     mapping_df$data_type[i])
      }
    }
    
    return(processed_data)
  }, error = function(e) {
    stop(paste("Error processing data:", e$message))
  })
}

#' Generate mapping template function
generate_mapping_template <- function() {
  # Get blood parameter names from the blood data
  blood_params <- names(blood_data)[!names(blood_data) %in% c("participant_id", "month")]
  
  # Create template dataframe
  template <- tibble(
    bharat_column = c(
      "participant_id",  # Required
      "age",            # Required
      "sex",            # Required
      blood_params      # Optional blood parameters
    ),
    external_column = NA_character_,
    data_type = c(
      "character",      # participant_id
      "numeric",        # age
      "factor",         # sex
      rep("numeric", length(blood_params))  # blood parameters
    ),
    required = c(
      "yes",           # participant_id
      "yes",           # age
      "yes",           # sex
      rep("no", length(blood_params))  # blood parameters
    ),
    notes = c(
      "Unique identifier for each participant",
      "Age in years",
      "Male or Female",
      rep("Blood parameter value", length(blood_params))
    )
  )
  
  return(template)
}

#' Helper function to standardize sex values
standardize_sex <- function(sex_values) {
  sex_values <- str_trim(sex_values)
  case_when(
    str_to_lower(sex_values) %in% c("m", "male", "1") ~ "Male",
    str_to_lower(sex_values) %in% c("f", "female", "2") ~ "Female",
    TRUE ~ NA_character_
  )
}

#' Helper function to convert column types
convert_column <- function(values, data_type) {
  tryCatch({
    # Remove any leading/trailing whitespace
    values <- str_trim(values)
    
    # Handle empty or NA values
    values[values == ""] <- NA
    
    # Convert based on data type
    result <- switch(data_type,
           "numeric" = {
             # Remove commas and convert
             clean_vals <- str_remove_all(as.character(values), ",")
             # Convert to numeric, warning=FALSE to suppress warnings
             as.numeric(clean_vals)
           },
           "character" = as.character(values),
           "factor" = as.factor(values),
           "date" = as.Date(values),
           values)
    
    return(result)
    
  }, error = function(e) {
    warning(paste("Error converting column:", e$message))
    rep(NA, length(values))
  })
}

#' Validation function for processed data
validate_dataset <- function(data) {
  tryCatch({
    # Check if data is a dataframe
    if(!is.data.frame(data)) {
      return(list(valid = FALSE, message = "Invalid data format"))
    }
    
    # Check required columns
    required_cols <- c("participant_id", "age", "sex", "age_group", "dataset", "group_combined")
    missing_cols <- setdiff(required_cols, names(data))
    if(length(missing_cols) > 0) {
      return(list(valid = FALSE, 
                 message = paste("Missing required columns:", 
                               paste(missing_cols, collapse = ", "))))
    }
    
    # Check for empty data
    if(nrow(data) == 0) {
      return(list(valid = FALSE, message = "Dataset is empty"))
    }
    
    # Check for missing values in key columns
    key_cols <- c("participant_id", "age", "sex")
    for(col in key_cols) {
      if(any(is.na(data[[col]]))) {
        return(list(valid = FALSE, 
                   message = paste("Missing values found in", col)))
      }
    }
    
    # Check age range
    if(any(data$age < 18, na.rm = TRUE)) {
      return(list(valid = FALSE, message = "Ages below 18 found in dataset"))
    }
    
    # Check sex values
    if(!all(data$sex %in% c("Male", "Female"), na.rm = TRUE)) {
      return(list(valid = FALSE, 
                 message = "Invalid sex values found (should be Male or Female)"))
    }
    
    # All checks passed
    return(list(valid = TRUE, 
               message = "Dataset validation successful"))
    
  }, error = function(e) {
    return(list(valid = FALSE, 
               message = paste("Validation error:", e$message)))
  })
}

#' Visualization Helper Functions
get_color_palette <- function(scheme = NULL, n = 1) {
  # Default to zissou if scheme is NULL, empty, or invalid
  if (is.null(scheme) || nchar(scheme) == 0) {
    scheme <- "zissou"
  }
  
  # Ensure n is at least 1
  n <- max(1, n)
  
  base_colors <- switch(scheme,
                       "zissou" = wes_palette("Zissou1", 5),
                       "darjeeling" = wes_palette("Darjeeling1", 5),
                       "royal" = wes_palette("Royal1", 4),
                       wes_palette("Zissou1", 5))  # default to zissou
  
  # If we need more colors than the base palette provides, interpolate
  if (n > length(base_colors)) {
    colorRampPalette(base_colors)(n)
  } else {
    base_colors[1:n]
  }
}

#' Statistical Analysis Functions
calculate_group_statistics <- function(data, param, group_var) {
  data %>%
    group_by(!!sym(group_var)) %>%
    summarise(
      n = n(),
      mean = mean(!!sym(param), na.rm = TRUE),
      sd = sd(!!sym(param), na.rm = TRUE),
      median = median(!!sym(param), na.rm = TRUE),
      q1 = quantile(!!sym(param), 0.25, na.rm = TRUE),
      q3 = quantile(!!sym(param), 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

# Load and process initial data
personal_info <- read_csv("data/Personal Information.csv") %>%
  select(participant_id = Participant_ID, 
         age = age_years, 
         sex = gender) %>%
  mutate(
    participant_id = str_trim(participant_id),
    age_group = factor(standardize_age_group(age), 
                      levels = c("18-29", "30-44", "45-59", "60-74", "75+")),
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

# Constants and choices
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
  "Custom Age Groups" = "custom",
  "Custom Age Groups and Sex Combined" = "custom_combined"
)

color_by_choices <- c(
  "None" = "none",
  "Age Groups" = "age_group",
  "Custom Age Groups" = "custom_age_group",
  "Sex" = "sex",
  "Dataset" = "dataset",
  "Age (continuous)" = "age"
)

color_schemes <- c(
  "Zissou" = "zissou",
  "Darjeeling" = "darjeeling",
  "Royal" = "royal"
)

dataset_colors <- c(
  "BHARAT" = "#4682B4",  # Steel Blue
  "ICPH" = "#FF8C00",    # Dark Orange
  "External" = "#20B2AA"  # Light Sea Green
)
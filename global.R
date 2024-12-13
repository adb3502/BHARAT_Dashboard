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
library(factoextra)

# Add custom fonts
font_add_google("Manrope", "Manrope")
showtext_auto()

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

#' Custom group functions
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

#' PCA Analysis Functions
compute_pca <- function(data, selected_params, scale = TRUE) {
  # Remove any non-numeric columns and NA values
  numeric_data <- data %>%
    select(all_of(selected_params)) %>%
    drop_na()
  
  # Perform PCA
  pca_result <- prcomp(numeric_data, scale. = scale)
  
  # Calculate variance explained
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  # Get loadings
  loadings <- as.data.frame(pca_result$rotation)
  
  # Get scores
  scores <- as.data.frame(pca_result$x)
  
  return(list(
    pca = pca_result,
    var_explained = var_explained,
    loadings = loadings,
    scores = scores
  ))
}

get_top_loadings <- function(loadings_df, pc, n = 10) {
  loadings_df %>%
    select(!!sym(pc)) %>%
    mutate(parameter = rownames(loadings_df)) %>%
    arrange(desc(abs(!!sym(pc)))) %>%
    slice_head(n = n)
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
      stop("Missing required column mappings")
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

standardize_sex <- function(sex_values) {
  sex_values <- str_trim(sex_values)
  case_when(
    str_to_lower(sex_values) %in% c("m", "male") ~ "Male",
    str_to_lower(sex_values) %in% c("f", "female") ~ "Female",
    TRUE ~ NA_character_
  )
}

convert_column <- function(values, data_type) {
  tryCatch({
    switch(data_type,
           "numeric" = as.numeric(str_remove_all(as.character(values), ",")),
           "character" = as.character(values),
           "factor" = as.factor(values),
           "date" = as.Date(values),
           values)
  }, error = function(e) {
    warning(paste("Error converting column:", e$message))
    NA
  })
}

#' Visualization Helper Functions
get_color_palette <- function(scheme, n) {
  colors <- switch(scheme,
                  "zissou" = wes_palette("Zissou1"),
                  "darjeeling" = wes_palette("Darjeeling1"),
                  "royal" = wes_palette("Royal1"),
                  wes_palette("Zissou1"))
  
  if(n <= length(colors)) {
    colors[1:n]
  } else {
    colorRampPalette(colors)(n)
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
  "Custom Groups" = "custom"
)

color_by_choices <- c(
  "None" = "none",
  "Age Groups" = "age_group",
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
  "BHARAT" = "#FF9642",
  "External_1" = "#00A08A",
  "External_2" = "#F2AD00",
  "External_3" = "#5BBCD6",
  "External_4" = "#E6AA68",
  "External_5" = "#A1C181"
)
# global.R
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

# Add custom fonts
font_add_google("Manrope", "Manrope")
showtext_auto()

# Debug function
print_debug <- function(data, msg = "") {
  print(paste("DEBUG:", msg))
  print(head(data$participant_id))
  print(table(data$sex, useNA = "always"))
}

# Load and process data with corrected sex pattern matching
blood_data <- read_csv("data/blood_test_results.csv") %>%
  rename_with(~str_replace_all(., "-", "_")) %>%
  mutate(
    # First extract the A/B part from IDs like "1A-001"
    sex_code = str_extract(participant_id, "[AB](?=-)"),
    # Then convert to Male/Female
    sex = case_when(
      sex_code == "A" ~ "Male",
      sex_code == "B" ~ "Female",
      TRUE ~ NA_character_
    ),
    # Age grouping
    age_group = case_when(
      str_detect(participant_id, "^1") ~ "18-30",
      str_detect(participant_id, "^[45]") ~ "60+",
      TRUE ~ "Other"
    ),
    group_combined = paste(age_group, sex)
  ) %>%
  filter(age_group != "Other") %>%
  mutate(
    age_group = factor(age_group, levels = c("18-30", "60+")),
    sex = factor(sex, levels = c("Male", "Female")),
    group_combined = factor(group_combined)
  )

# Debug print to verify data loading
print("Initial data verification:")
print_debug(blood_data, "After loading")

# Metadata handling
blood_metadata <- read_csv("data/blood_test_metadata.csv") %>%
  rename_with(~str_replace_all(., "-", "_"))

# Create named vector for parameter selection
blood_params <- names(blood_data)[!names(blood_data) %in% 
                                    c("participant_id", "month", "age_group", "sex", "group_combined", "sex_code")]
param_choices <- setNames(
  blood_params,
  str_to_title(str_replace_all(blood_params, "_", " "))
)

# Grouping options
grouping_choices <- c(
  "Age Groups" = "age",
  "Sex" = "sex",
  "Age and Sex Combined" = "combined"
)

# Secondary coloring options
color_by_choices <- c(
  "None" = "none",
  "Age Groups" = "age_group",
  "Sex" = "sex"
)

# Color scheme options
color_schemes <- c(
  "Zissou" = "zissou",
  "Darjeeling" = "darjeeling",
  "Royal" = "royal"
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
# global.R

# 1. Load Libraries --------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)
library(wesanderson)
library(showtext)
library(sysfonts)
library(rstatix)
library(effsize)
library(scales)
library(lubridate)
library(viridis)

# 2. Define Parameters First ----------------------------------------------
# Clinical parameters
clinical_params <- c(
  "BMI" = "bmi",
  "Height (cm)" = "height_cm",
  "Weight (kg)" = "weight_kg",
  "Pulse Rate" = "pulse_rate_bpm",
  "Blood Pressure" = "blood_pressure_mmhg",
  "Respiratory Rate" = "respiratory_rate_bpm",
  "Oxygen Saturation" = "oxygen_saturation_percent"
)

# Vital parameters
vital_params <- c(
  "Pulse Rate" = "pulse_rate_bpm",
  "Blood Pressure" = "blood_pressure_mmhg",
  "Respiratory Rate" = "respiratory_rate_bpm",
  "Oxygen Saturation" = "oxygen_saturation_percent",
  "Temperature" = "body_temperature_farenheit"
)

# Cognitive parameters
cognitive_params <- c(
  "MMSE Total" = "mmse_total_score",
  "Orientation (Time)" = "orientation_time_score",
  "Orientation (Place)" = "orientation_place_score",
  "Registration" = "registration_score",
  "Attention" = "attention_calculation_score",
  "Recall" = "recall_score"
)

# Physical parameters
physical_params <- c(
  "Frailty Score" = "frailty_total_score",
  "Hand Grip Strength" = "hand_grip_strength",
  "Walking Speed" = "walking_speed_category",
  "Fatigue" = "fatigue_score",
  "Resistance" = "resistance_score",
  "Ambulation" = "ambulation_score"
)

# Grouping choices
grouping_choices <- c(
  "Age Groups" = "age_group",
  "Sex" = "sex",
  "Age and Sex Combined" = "group_combined"
)

# Color schemes
color_schemes <- c(
  "Zissou" = "zissou",
  "Darjeeling" = "darjeeling",
  "Royal" = "royal"
)

# 3. Setup Functions -----------------------------------------------------
# Add custom fonts
font_add_google("Manrope", "Manrope")
showtext_auto()

# Color palette function
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

# 4. Load and Process Data -----------------------------------------------
bharat_data <- tryCatch({
  # Load all data files
  personal_info <- read_delim("data/raw/Personal Information.csv", delim = "\t")
  mmse <- read_delim("data/raw/Annexure-1 MMSE.csv", delim = "\t")
  frailty <- read_delim("data/raw/Annexure-2 Simple Frail Questio.csv", delim = "\t")
  anthropometry <- read_delim("data/raw/Anthropometry.csv", delim = "\t")
  comorbidities <- read_delim("data/raw/Co-Morbidities.csv", delim = "\t")
  covid <- read_delim("data/raw/COVID-19 Infection.csv", delim = "\t")
  dass21 <- read_delim("data/raw/DASS 21 Scale.csv", delim = "\t")
  diet <- read_delim("data/raw/Dietary Pattern.csv", delim = "\t")
  exercise <- read_delim("data/raw/Exercise Habits.csv", delim = "\t")
  food_freq <- read_delim("data/raw/Food Frequency List.csv", delim = "\t")
  habits <- read_delim("data/raw/Habits.csv", delim = "\t")
  exam <- read_delim("data/raw/Head-to-toe Examination.csv", delim = "\t")
  phys_exam <- read_delim("data/raw/Physical Examination.csv", delim = "\t")
  systemic <- read_delim("data/raw/Systemic Examination.csv", delim = "\t")
  vitals <- read_delim("data/raw/Vitals.csv", delim = "\t")
  
  # Join all datasets
  combined_data <- personal_info %>%
    left_join(mmse, by = "Participant_ID") %>%
    left_join(frailty, by = "Participant_ID") %>%
    left_join(anthropometry, by = "Participant_ID") %>%
    left_join(comorbidities, by = "Participant_ID") %>%
    left_join(covid, by = "Participant_ID") %>%
    left_join(dass21, by = "Participant_ID") %>%
    left_join(diet, by = "Participant_ID") %>%
    left_join(exercise, by = "Participant_ID") %>%
    left_join(food_freq, by = "Participant_ID") %>%
    left_join(habits, by = "Participant_ID") %>%
    left_join(exam, by = "Participant_ID") %>%
    left_join(phys_exam, by = "Participant_ID") %>%
    left_join(systemic, by = "Participant_ID") %>%
    left_join(vitals, by = "Participant_ID")
  
  # Clean names and add derived variables
  combined_data %>%
    rename_with(~str_replace_all(., " ", "_")) %>%
    rename_with(~str_to_lower(.)) %>%
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
      sex = factor(gender, levels = c("Male", "Female")),
      group_combined = paste(age_group, sex)
    )
}, error = function(e) {
  stop(paste("Error loading data:", e$message))
})

# 5. Print Debug Info ---------------------------------------------------
print("Data loaded successfully")
print(sprintf("Number of participants: %d", nrow(bharat_data)))
print(sprintf("Number of variables: %d", ncol(bharat_data)))
print("Parameters loaded successfully")
# BHARAT Study Dashboard
# Author: adb
# Last Updated: 2024-11-28

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)
library(wesanderson)
library(showtext)
library(sysfonts)

# Source configuration and utility files
source("app/R/config.R")
source("app/utils/data_loader.R")
source("app/utils/plotting_utils.R")
source("app/utils/statistics.R")

# Load global data
bharat_data <- load_bharat_data(DATA_DIR)

# Run validation checks
validation_results <- validate_data(bharat_data)
if(any(unlist(validation_results) > 0)) {
  warning("Data validation found issues. Check validation_results for details.")
}

# Source UI and server files
source("app/R/ui.R")
source("app/R/server.R")

# Run the application
shinyApp(ui = ui, server = server)
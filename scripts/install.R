# Install required packages for BHARAT Study Dashboard

# List of required packages
packages <- c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "plotly",
  "DT",
  "bslib",
  "wesanderson",
  "showtext",
  "sysfonts",
  "rstatix",
  "effsize",
  "scales",
  "lubridate",
  "viridis"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Installing missing packages:", paste(new_packages, collapse=", "), "\n")
    install.packages(new_packages)
  }
}

# Install packages
tryCatch({
  install_if_missing(packages)
  cat("\nAll required packages installed successfully!\n")
}, error = function(e) {
  cat("\nError installing packages:", e$message, "\n")
})

# Verify installations
missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
  warning("Some packages could not be installed: ", 
          paste(missing_packages, collapse=", "))
} else {
  cat("\nAll package dependencies are satisfied!\n")
}

# Load packages to verify they work
cat("\nVerifying package loading...\n")
for(pkg in packages) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("Successfully loaded:", pkg, "\n")
  }, error = function(e) {
    warning("Error loading package ", pkg, ": ", e$message)
  })
}
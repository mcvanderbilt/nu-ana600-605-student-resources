# -------------------------------------------------------------------------
# Purpose: Install (if needed) and load required R libraries
# Author: Matthew C. Vanderbilt, MSBA
# Created: 31 October 2025
# Updated: 29 January 2026
# -------------------------------------------------------------------------
# STUDENT INSTRUCTIONS:
# 1. ONLY list the libraries you will actually use in your assignment.
# 2. DO NOT list tidyverse core packages when already using tidyverse.
#    These load automatically when you call: library(tidyverse).
# 3. tidyverse core packages (do NOT list these):
#       dplyr, ggplot2, purrr, readr, tibble, tidyr
# 4. Packages already in tidyverse are marked with "***" below.
# 5. This script installs missing packages and loads only those that are
#    not already part of tidyverse.
#
# NOTE: Code comments created by Microsoft Copilot AI tool.
# ------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Create a vector of required libraries. Keep this list short and focused.
# -------------------------------------------------------------------------

libraries <- c(
  "broom",        # model summaries
  "car",          # regression tools
  "corrplot",     # correlation plots
  "dplyr",        # data wrangling ***
  "glue",         # string interpolation
  "ggplot2",      # plotting system ***
  "gridExtra",    # arrange multiple plots
  "httr",         # HTTP requests / APIs
  "jsonlite",     # JSON import/export
  "kableExtra",   # table formatting
  "knitr",        # report generation
  "mosaic",       # introductory statistics tools
  "nortest",      # normality tests
  "openxlsx",     # Excel read/write
  "plotly",       # interactive visualizations
  "purrr",        # functional programming ***
  "random",       # random.org API access
  "readr",        # fast data import ***
  "readxl",       # read Excel files
  "scales",       # formatting helpers
  "supernova",    # ANOVA tools
  "tibble",       # modern data frames ***
  "tidyr",        # data reshaping ***
  "tidyverse",    # data science suite ***
  "tidycensus",   # Census API access
  "truncnorm"     # truncated normal distributions
)

# -------------------------------------------------------------------------
# Define tidyverse core packages. These load automatically with tidyverse.
# -------------------------------------------------------------------------

tidyverse_core <- c(
  "dplyr",
  "ggplot2",
  "purrr",
  "readr",
  "tibble",
  "tidyr"
)

# -------------------------------------------------------------------------
# Warn students if they listed tidyverse packages unnecessarily.
# -------------------------------------------------------------------------

duplicates <- intersect(
  libraries,
  tidyverse_core
)

if (length(duplicates) > 0) {
  message("")
  message("WARNING: Some packages you listed are already in tidyverse.")
  message("These do NOT need to be listed, installed, or loaded:")
  message(paste("  -", duplicates, collapse = "\n"))
  message("")
  message("Please remove these packages to avoid duplication.")
  message("")
}

# -------------------------------------------------------------------------
# Remove tidyverse core packages from the list to avoid duplication.
# -------------------------------------------------------------------------

libraries_to_process <- setdiff(libraries, tidyverse_core)

# -------------------------------------------------------------------------
# Install missing packages. require() checks if a package is installed.
# If not installed, install.packages() is called automatically.
# -------------------------------------------------------------------------

for (pkg in libraries_to_process) {
  if (
    !require(
      pkg,
      character.only = TRUE,
      quietly = TRUE
    )
  ) {
    suppressMessages(install.packages(pkg))
  }
}

# -------------------------------------------------------------------------
# Load all non-tidyverse packages. suppressMessages() keeps output clean.
# -------------------------------------------------------------------------

for (pkg in libraries_to_process) {
  suppressMessages(
    library(
      pkg,
      character.only = TRUE
    )
  )
}

# -------------------------------------------------------------------------
# Load tidyverse last. This loads all tidyverse core packages at once.
# -------------------------------------------------------------------------

suppressMessages(library(tidyverse))

# -------------------------------------------------------------------------
# Clean up temporary objects. gc() frees unused memory.
# -------------------------------------------------------------------------

rm(
  libraries,
  tidyverse_core,
  libraries_to_process,
  duplicates,
  pkg
)

gc()
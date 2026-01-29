# -------------------------------------------------------------------------
# File: load-libraries.r
# Author: Matthew C. Vanderbilt, MSBA
# Created: 29 January 2026
# Updated: 29 January 2026
# -------------------------------------------------------------------------
# PURPOSE:
#   Defines the load_libraries() function. It installs (if needed) and
#   loads required R libraries while preventing duplicate loading of
#   tidyverse core packages.
#
# WHERE TO SAVE THIS FILE (NAME IT "load-libraries.r"):
#   Option 1: Save load-libraries.r in the same folder as your R script.
#   Option 2: Save it in a shared folder for all your custom R functions.
#
# HOW TO CALL THIS FUNCTION:
#   If the file is in the same folder:
#       source("load-libraries.r")
#
#   If the file is in a shared folder, provide the full or relative path:
#       source("C:/R/functions/load-libraries.r")
#       source("../shared/load-libraries.r")
#       source("~/R/functions/load-libraries.r")
#
# HOW TO USE THE FUNCTION:
#   1. Create a vector of required libraries:
#         my_libs <- c("broom", "car", "readxl", "tidyverse")
#   2. Call the function:
#         load_libraries(my_libs)
#   3. Remove the vector if no longer needed and clear memory:
#         rm(my_libs)
#         gc()
#
# NOTES:
#   DO NOT list tidyverse core packages when already using tidyverse.
#   These load automatically when you call: library(tidyverse).
#   tidyverse core packages:
#       dplyr, ggplot2, purrr, readr, tibble, tidyr
# -------------------------------------------------------------------------

load_libraries <- function(libraries) {

  tidyverse_core <- c(
    "dplyr",
    "ggplot2",
    "purrr",
    "readr",
    "tibble",
    "tidyr"
  )

  duplicates <- intersect(libraries, tidyverse_core)

  if (length(duplicates) > 0) {
    message("")
    message("WARNING: Some packages you listed are in tidyverse.")
    message("These do NOT need to be listed, installed, or loaded:")
    message(paste("  -", duplicates, collapse = "\n"))
    message("")
    message("Please remove these packages to avoid duplication.")
    message("")
  }

  libraries_to_process <- setdiff(libraries, tidyverse_core)

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

  for (pkg in libraries_to_process) {
    suppressMessages(
      library(
        pkg,
        character.only = TRUE
      )
    )
  }

  if ("tidyverse" %in% libraries) {
    suppressMessages(library(tidyverse))
  }

  rm(
    tidyverse_core,
    libraries_to_process,
    duplicates
  )

  invisible(TRUE) # Return TRUE invisibly
}
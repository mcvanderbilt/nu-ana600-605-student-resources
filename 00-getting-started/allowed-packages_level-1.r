# -------------------------------------------------------------------------
# Purpose: Install (if needed) and load required R libraries
# Author: Matthew C. Vanderbilt, MSBA
# Created: 31 October 2025
# Updated: 29 January 2026
# -------------------------------------------------------------------------
# STUDENT INSTRUCTIONS:
# This script installs and loads each package on its own line. Only list
# the packages you will use. Do NOT list tidyverse core packages. These
# load automatically when you call library(tidyverse).
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Install each package individually
# quietly = TRUE suppresses startup messages and non-error warnings
# -------------------------------------------------------------------------

if (!require(broom, quietly = TRUE)) install.packages("broom")
if (!require(car, quietly = TRUE)) install.packages("car")
if (!require(corrplot, quietly = TRUE)) install.packages("corrplot")
if (!require(dplyr, quietly = TRUE)) install.packages("dplyr")
if (!require(glue, quietly = TRUE)) install.packages("glue")
if (!require(ggplot2, quietly = TRUE)) install.packages("ggplot2")
if (!require(gridExtra, quietly = TRUE)) install.packages("gridExtra")
if (!require(httr, quietly = TRUE)) install.packages("httr")
if (!require(jsonlite, quietly = TRUE)) install.packages("jsonlite")
if (!require(kableExtra, quietly = TRUE)) install.packages("kableExtra")
if (!require(knitr, quietly = TRUE)) install.packages("knitr")
if (!require(mosaic, quietly = TRUE)) install.packages("mosaic")
if (!require(nortest, quietly = TRUE)) install.packages("nortest")
if (!require(openxlsx, quietly = TRUE)) install.packages("openxlsx")
if (!require(plotly, quietly = TRUE)) install.packages("plotly")
if (!require(purrr, quietly = TRUE)) install.packages("purrr")
if (!require(random, quietly = TRUE)) install.packages("random")
if (!require(readr, quietly = TRUE)) install.packages("readr")
if (!require(readxl, quietly = TRUE)) install.packages("readxl")
if (!require(scales, quietly = TRUE)) install.packages("scales")
if (!require(supernova, quietly = TRUE)) install.packages("supernova")
if (!require(tibble, quietly = TRUE)) install.packages("tibble")
if (!require(tidyr, quietly = TRUE)) install.packages("tidyr")
if (!require(tidyverse, quietly = TRUE)) install.packages("tidyverse")
if (!require(tidycensus, quietly = TRUE)) install.packages("tidycensus")
if (!require(truncnorm, quietly = TRUE)) install.packages("truncnorm")

# -------------------------------------------------------------------------
# Load each package individually
# -------------------------------------------------------------------------

library(broom)
library(car)
library(corrplot)
library(dplyr)
library(glue)
library(ggplot2)
library(gridExtra)
library(httr)
library(jsonlite)
library(kableExtra)
library(knitr)
library(mosaic)
library(nortest)
library(openxlsx)
library(plotly)
library(purrr)
library(random)
library(readr)
library(readxl)
library(scales)
library(supernova)
library(tibble)
library(tidyr)
library(tidyverse)
library(tidycensus)
library(truncnorm)
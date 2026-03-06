# -------------------------------------------------------------------------
# title: Module 1 Lecture Code
# course: ANA600 - Fundamentals of Analytics
# author: Matthew C. Vanderbilt, MSBA
# last updated: 5 March 2026
# -------------------------------------------------------------------------
# PROFESSOR'S NOTE:
# Keep GitHub version updated from course master
# -------------------------------------------------------------------------
# STUDENT NOTE:
# All filePath variables need to be changed to the path on YOUR COMPUTER
# where the datasets sit. The current paths will only resolve to the
# location on the professor's computer
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# MISCELLANEOUS REVIEW ----------------------------------------------------
# -------------------------------------------------------------------------

# install and load necessary package(s); first step in any project
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(nycflights13)) {install.packages("nycflights13")}
library(tidyverse)
library(nycflights13)

# calculation of mean value with exclusion of any NA observations
flights.air_time.M <- mean(flights$air_time, na.rm = TRUE)
flights.air_time.SD <- sd(flights$air_time, na.rm = TRUE)

# base R histogram functionality
hist(
  flights$air_time,
  main = "Distribution of Air Time for NYC Flights 2013",
  xlab = "Air Time (minutes)",
  ylab = "Frequency (count)",
  col = "steelblue",
  border = "lightgrey"
)

# ggplot2 histogram functionality
ggplot(
  flights,
  aes(
    x = air_time
  )
) +
  geom_histogram(
    fill = "steelblue",
    color = "black"
  )

# evaluate dataset structure
str(flights)

# output top and bottom 3 observations to console
head(myData, 3)
tail(myData, 3)

# evaluate carrier flights by month with kable formatting
kable(
  tally(
    carrier ~ month,
    data = flights
  )
)

# create a new dataset from flights
myData <- flights

# add variable for distance in 100 miles
myData$distance100 <- myData$distance / 100

# evaluate new structure of dataset
str(myData)

# calculate the mean air_time; notice it does not resolve because there are
# NA values in the dataset
mean(flights$air_time)

# remove all observations with a NULL in any variable to a new dataset for
# later review
myData.NA <- myData[!complete.cases(myData), ]

# remove all observations with a NULL in any variable from the dataset
myData <- na.omit(myData)

# check if the removed observations equals the observations in myData.NA
nrow(flights) - nrow(myData) # 9,430
nrow(myData.NA) # 9,430

# evaluate aggregation of mean by flight origin
aggregate(
  air_time ~ origin,
  myData,
  FUN = mean
)

# evaluate aggregation of standard deviation by flight origin
aggregate(
  air_time ~ origin,
  myData,
  FUN = sd
)


# -------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS --------------------------------------------------
# -------------------------------------------------------------------------

# Measures of Location ----------------------------------------------------
if (!require(ggplot2)) {install.packages("ggplot2")}
library(ggplot2)

filePath <- "~/GitHub/NU-ANA600-Fundamentals-of-Analytics/07-data/raw/"
setwd(filePath)

dataEd <- read.csv("dataset-gss-2012-subset4.csv")
dataEd <- na.omit(dataEd)

dataEd.income.mean <- mean(dataEd$income)
dataEd.income.medi <- median(dataEd$income)
dataEd.income.sd <- sd(dataEd$income)

dataEd.bins <- round(sqrt(nrow(dataEd)), 0)

ggplot(
  dataEd,
  aes(
    x = income
  )
) +
  geom_histogram(
    bins = dataEd.bins,
    fill = "steelblue",
    color = "black"
  ) +
  geom_vline(
    xintercept = dataEd.income.mean,
    color = "red",
    linewidth = 1.0
  ) +
  geom_vline(
    xintercept = dataEd.income.medi,
    color = "red",
    linewidth = 1.0,
    linetype = "dashed"
  ) +
  labs(
    title = "Histogram of Income",
    subtitle = "dataset-gss-2012-subset4.csv",
    x = "Income",
    y = "Frequency (Count)"
  ) +
  theme_minimal(
    base_size = 14
  )

# Measures of Dispersion --------------------------------------------------
if (!require(ggplot2)) {install.packages("ggplot2")}
library(ggplot2)

filePath <- "~/GitHub/NU-ANA600-Fundamentals-of-Analytics/07-data/raw/"
setwd(filePath)

dataCards <- read.csv("dataset-cardiac-catheterization-2018-subset1.csv")
dataCards <- na.omit(dataCards)

dataCards.age.mean <- mean(dataCards$age)
dataCards.age.medi <- median(dataCards$age)
dataCards.age.sd <- sd(dataCards$age)

dataCards.bins <- round(sqrt(nrow(dataCards)), 0)

ggplot(
  dataCards,
  aes(
    x = age
  )
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = .75*dataCards.bins,
    fill = "steelblue",
    color = "black"
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = dataCards.age.mean,
      sd = dataCards.age.sd
    ),
    color = "red",
    size = 1
  ) +
  geom_vline(
    xintercept = dataCards.age.mean,
    color = "darkblue",
    linewidth = 1.0
  ) +
  geom_vline(
    xintercept = dataCards.age.medi,
    color = "darkblue",
    linewidth = 1.0,
    linetype = "dashed"
  ) +
  labs(
    title = "Histogram of Age",
    subtitle = "",
    x = "Age",
    y = "Density (Percent)"
  ) +
  theme_minimal(
    base_size = 14
  )


# -------------------------------------------------------------------------
# WORKING WITH DATA -------------------------------------------------------
# -------------------------------------------------------------------------

## DATASET: nycflights13 --------------------------------------------------
### Install & Load Packages
#### needed once per computer
if (!require(tidyverse)) {install.packages("tidyverse")} # ggplot & dplyr
if (!require(nycflights13)) {install.packages("nycflights13")}

#### needed once per instance
library(tidyverse)
library(nycflights13)

### View Data Frame Structure
#### view the data structure - variables, data types, count, sample data
str(flights)

### Identify Observations & Variables
nrow(flights)
ncol(flights)

head(flights, 5)
tail(flights, 5)

### Calculate Descriptive Statistics for Flight Air Time
favstats(flights$air_time)

## DATASET: fueleconomy ---------------------------------------------------
if (!require(tidyverses)) {install.packages("tidyverses")}
if (!require(mosaic)) {install.packages("mosaic")}
if (!require(kableExtra)) {install.packages("kableExtra")}
if (!require(fueleconomy)) {install.packages("fueleconomy")}
library(tidyverse)
library(mosaic)
library(kableExtra)
library(fueleconomy)

### Question: How many observations are in fueleconomy::vehicles?
nrow(vehicles)

### Question: How many variables are in fueleconomy::vehicles?
ncol(vehicles)

### Question: What stored data type is vehicles$id?
### Question: What information / variable type is vehicles$id?
str(vehicles)

### Crosstab data with formatted ascii table
kable(
  tally(
    fuel ~ cyl,
    data = vehicles
  )
)

## DATASET: hate_crimes_datasd.csv ----------------------------------------
if (!require(tidyverses)) {install.packages("tidyverses")}
if (!require(mosaic)) {install.packages("mosaic")}
library(tidyverse)
library(mosaic)

filePath <- "~/GitHub/NU-ANA600-Fundamentals-of-Analytics/07-data/raw/"
setwd(filePath)

dataHate <- read.csv("hate_crimes_datasd.csv")
dataHate <- na.omit(dataHate)

### Question: What will be the result of this code?
dataHate.arranged <- arrange(
  dataHate,
  number_of_suspects
)

### Question: What will be the result of this code?
sort(dataHate$number_of_suspects)

### Aggregated Data
tally(dataHate$number_of_suspects)

## CREATE A VARIABLE ------------------------------------------------------
### DATASET: nycflights13 -------------------------------------------------
#### Install & Load Packages
if (!require(tidyverse)) {install.packages("tidyverse")} # ggplot & dplyr
if (!require(mosaic)) {install.packages("mosaic")} # favstats
if (!require(nycflights13)) {install.packages("nycflights13")}
library(tidyverse)
library(mosaic)
library(nycflights13)

#### Create a variable using a case statement
dataFlights <- flights |>
  mutate(
    month_name = case_when(
      month ==  1 ~ "JAN",
      month ==  2 ~ "FEB",
      month ==  3 ~ "MAR",
      month ==  4 ~ "APR",
      month ==  5 ~ "MAY",
      month ==  6 ~ "JUN",
      month ==  7 ~ "JUL",
      month ==  8 ~ "AUG",
      month ==  9 ~ "SEP",
      month == 10 ~ "OCT",
      month == 11 ~ "NOV",
      month == 12 ~ "DEC",
      .default = "#Err"
    )
  )

str(dataFlights)

kable(tally(dataFlights$month_name ~ dataFlights$month))

#### Null values and filtering data
##### Question: What will be the result of the code below?
mean(dataFlights$air_time)

dataFlights <- filter(
  dataFlights,
  air_time != "NA"
)

##### Question: Now what will be the result of the code below?
mean(dataFlights$air_time)

#### Question: How many observations were removed with filter?
nrow(flights)
nrow(dataFlights)
paste("Observations Removed:", nrow(flights) - nrow(dataFlights))

#### Question: How many observations are removed with this code?
dataFlights.omit <- na.omit(flights)
nrow(flights)
nrow(dataFlights.omit)
paste("Observations Removed:", nrow(flights) - nrow(dataFlights.omit))

#### Evaluate descriptive statistics
summary(dataFlights)
favstats(dataFlights$air_time)

#### Aggregate data
aggregate(
  air_time ~ origin,
  dataFlights,
  FUN = mean
)

aggregate(
  air_time ~ origin,
  dataFlights,
  FUN = sd
)

#### Aggregate combined data using dplyr
dataFlights.air_time.stat <- dataFlights |>
  group_by(origin) |>
  summarise(
    air_time.M = mean(air_time),
    air_time.SD = sd(air_time),
    .groups = "drop" # prevent other grouping issues
  )

dataFlights.air_time.stat

#### Question: What is the mean value for dep_delay at EWR?
dataFlights.dep_delay.ewr <- dataFlights |>
  filter(origin == "EWR") |>
  summarise(
    dep_delay.M = mean(dep_delay, na.rm = TRUE)
  )

dataFlights.dep_delay.ewr

# alt
dataFlights.EWR <- filter(
  dataFlights,
  origin == "EWR"
)

mean(dataFlights.EWR$dep_delay)
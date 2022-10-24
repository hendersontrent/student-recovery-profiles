#-------------------------------------------
# This script aims to set up everything
# necessary to run the project
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

# Load packages

library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(data.table)
library(readxl)
library(janitor)
library(tidyLPA)
library(broom)

# Turn off scientific notation

options(scipen = 999)

# Load in new data file

new_data <- read_excel("data/HDR Study 1_REQ only.xlsx") %>%
  clean_names() %>%
  rename(d1 = 14,
         d2 = 18)

# Load in old data file

old_data <- read_excel("data/JBlower.MOrgPsych.v1.0.xlsx") %>%
  clean_names()

# Run preprocessing and cleaning

source("processing/masters_cleaning.R")

# Create an output folder if none exists:

if(!dir.exists('output')) dir.create('output')

#-------------------------------------------
# This script aims to set up everything
# necessary to run the project
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

# Load packages

library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(tidyLPA)
library(Cairo)
library(broom)
library(sjPlot)
library(caTools)
library(e1071)

# Turn off scientific notation

options(scipen = 999)

# Load logistic regression output function

source("R/logit_output_generator.R")
source("R/stat_production.R")

# Run preprocessing and cleaning

source("processing/cleaning.R")

# Load in excel file

d <- read_excel("data/JBlower.MOrgPsych.v1.0.xlsx") %>%
  clean_names()

# Create an output folder if none exists:

if(!dir.exists('output')) dir.create('output')

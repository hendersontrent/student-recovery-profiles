#-------------------------------------------
# This script aims to run analysis to
# understand differences in certain variables
# between the profile classes
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

# Load LPA outputs

source("analysis/lpa.R")

#------------------------ ANALYSIS ---------------------------------

# Specify models

m1 <- lm(age ~ Class, data = d3)
m2 <- lm(employhrs ~ Class, data = d3)
m3 <- lm(job_demands ~ Class, data = d3)
m4 <- lm(job_control ~ Class, data = d3)
m5 <- lm(consis_interest ~ Class, data = d3)
m6 <- lm(pers_effort ~ Class, data = d3)

#------------------------ GET OUTPUTS ------------------------------

# Output table

tab_model(m1, m2, m3, m4, m5, m6, p.style = "stars")

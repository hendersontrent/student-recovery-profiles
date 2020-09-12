#-------------------------------------------
# This script aims to run a series of linear
# models
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

# Load LPA outputs

source("analysis/lpa.R")

#------------------------ MODELLING --------------------------------

out_m1 <- lm(engagement_vigour ~ Class + gender + age + employhrs, data = d3)
out_m2 <- lm(engagement_dedication ~ Class + gender + age + employhrs, data = d3)
out_m3 <- lm(engagement_absorption ~ Class + gender + age + employhrs, data = d3)
out_m4 <- lm(burnout_exhaustion ~ Class + gender + age + employhrs, data = d3)
out_m5 <- lm(burnout_cynicism ~ Class + gender + age + employhrs, data = d3)
out_m6 <- lm(burnout_profefficacy ~ Class + gender + age + employhrs, data = d3)
out_m7 <- lm(mental_health ~ Class + gender + age + employhrs, data = d3)
out_m8 <- lm(panas_pos ~ Class + gender + age + employhrs, data = d3)
out_m9 <- lm(panas_neg ~ Class + gender + age + employhrs, data = d3)
out_m10 <- lm(swl ~ Class + gender + age + employhrs, data = d3)

#------------------------ GET OUTPUTS ------------------------------

adj_pval <- 0.05 / 10 # Bonferroni correction for inflated error rate due to 10 models

# Output tables

tab_model(out_m1, out_m2, out_m3, out_m4, out_m5, p.style = "stars", p.threshold = c(adj_pval, .001, .0001))
tab_model(out_m6, out_m7, out_m8, out_m9, out_m10, p.style = "stars", p.threshold = c(adj_pval, .001, .0001))

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

#------------------------ MODELLING --------------------------------

out_m1 <- lm(engagement_vigour ~ Class + gender + age + employhrs, data = d3)
out_m2 <- lm(engagement_dedication ~ Class + gender + age + employhrs, data = d3)
out_m3 <- lm(engagement_absorption ~ Class + gender + age + employhrs, data = d3)
out_m4 <- lm(burnout_exhaustion ~ Class + gender + age + employhrs, data = d3) # Significant
out_m5 <- lm(burnout_cynicism ~ Class + gender + age + employhrs, data = d3) # Significant
out_m6 <- lm(burnout_profefficacy ~ Class + gender + age + employhrs, data = d3)
out_m7 <- lm(mental_health ~ Class + gender + age + employhrs, data = d3) # Significant
out_m8 <- lm(panas_pos ~ Class + gender + age + employhrs, data = d3)
out_m9 <- lm(panas_neg ~ Class + gender + age + employhrs, data = d3) # Significant
out_m10 <- lm(swl ~ Class + gender + age + employhrs, data = d3) # Significant

#------------------------ GET OUTPUTS ------------------------------

# Output tables

tab_model(out_m1, out_m2, out_m3, out_m4, out_m5, p.style = "asterisk")
tab_model(out_m6, out_m7, out_m8, out_m9, out_m10, p.style = "asterisk")

# Model outputs

adj_pval <- 0.05 / 10 # Bonferroni correction for inflated error rate

# Produce outputs for each model

m1_clean <- stat_production(out_m1) %>% mutate("Response variable" = "Engagement (Vigour)")
m2_clean <- stat_production(out_m2) %>% mutate("Response variable" = "Engagement (Dedication)")
m3_clean <- stat_production(out_m3) %>% mutate("Response variable" = "Engagement (Absorption)")
m4_clean <- stat_production(out_m4) %>% mutate("Response variable" = "Burnout (Exhaustion)")
m5_clean <- stat_production(out_m5) %>% mutate("Response variable" = "Burnout (Cynicism)")
m6_clean <- stat_production(out_m6) %>% mutate("Response variable" = "Burnout (Professional Efficacy)")
m7_clean <- stat_production(out_m7) %>% mutate("Response variable" = "Mental Health")
m8_clean <- stat_production(out_m8) %>% mutate("Response variable" = "PANAS (Positive Affect)")
m9_clean <- stat_production(out_m9) %>% mutate("Response variable" = "PANAS (Negative Affect")
m10_clean <- stat_production(out_m10) %>% mutate("Response variable" = "Satisfaction with Life")

all_models <- bind_rows(m1_clean, m2_clean, m3_clean, m4_clean, m5_clean, 
                        m6_clean, m7_clean, m8_clean, m9_clean, m10_clean) %>%
  dplyr::select(-c(1)) %>%
  mutate("Explanatory variable" = "Recovery Profile")

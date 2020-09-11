#-------------------------------------------
# This script aims to build an LPA model
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

#------------------------- MODEL SPECIFICATION ---------------------

m1 <- d1[1:nrow(d1), ] %>%
  select(detachment, relaxation, mastery, control) %>%
  single_imputation() %>%
  estimate_profiles(1:6,
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying"))

#------------------------- OUTPUTS ---------------------------------

#-----------------
# FIT INDICES
#-----------------

m1 %>%
  compare_solutions(statistics = c("AIC", "BIC"))

#-----------------
# DEFAULT LPA PLOT
#-----------------

# Plot LPA

m1 %>%
  plot_profiles()

#-----------------
# FIT STATISTICS
#-----------------

# Get fit statistics

get_fit(m1)

#-----------------
# COLLECT OUTPUTS
#-----------------

# Get outputs in a dataframe

lpa_outputs <- get_data(m1)

# Filter to just Model 1/Classes 2 as this model had the best BIC value

lpa_outputs_filt <- lpa_outputs %>%
  filter(model_number == "1" & classes_number == 2)

# Join back in to main dataset

d2 <- d1 %>%
  inner_join(lpa_outputs_filt, by = c("detachment" = "detachment", "relaxation" = "relaxation",
                                      "mastery" = "mastery", "control" = "control")) %>%
  filter(Probability > 0.8) %>%
  mutate(Class = as.factor(Class)) %>%
  mutate(gender = as.factor(gender))

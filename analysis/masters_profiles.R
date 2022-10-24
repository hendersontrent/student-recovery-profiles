#-----------------------------------------
# This script aims to build an LPA model
# for Masters dataset
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 24 October 2022
#-----------------------------------------

#------------------------- MODEL SPECIFICATION ---------------------

m1 <- old_data[1:nrow(old_data), ] %>%
  select(detachment, relaxation, mastery, control) %>%
  single_imputation() %>%
  estimate_profiles(1:4,
                    variances = c("equal", "varying", "equal", "varying"),
                    covariances = c("equal", "varying", "zero", "zero"))

#------------------------- OUTPUTS ---------------------------------

#-----------------
# FIT INDICES
#-----------------

m1 %>%
  compare_solutions(statistics = c("AIC", "BIC", "AWE", "CLC", "KIC"))

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

# Filter to just best model

lpa_outputs_filt <- lpa_outputs %>%
  filter(model_number == "2" & classes_number == 3)

# Join back in to main dataset

d2 <- old_data %>%
  mutate(id = row_number()) %>%
  inner_join(lpa_outputs_filt, by = c("detachment" = "detachment", "relaxation" = "relaxation",
                                      "mastery" = "mastery", "control" = "control", "id" = "id")) %>%
  group_by(id) %>%
  slice(which.max(Probability)) %>%
  mutate(Class = case_when(
          Class == "1" ~ "Low Recoverers",
          Class == "2" ~ "High Recoverers",
          TRUE         ~ "Low Detachers"),
         Class = as.factor(Class))

#------------------------
# ANALYSIS OF CLASSES
#------------------------

# Calculate summary stats

summary_stats <- d2 %>%
  dplyr::select(c(detachment, relaxation, mastery, control, Class)) %>%
  gather(key = metric, value = value, 2:5) %>%
  mutate(metric = str_to_title(metric)) %>%
  group_by(Class, metric) %>%
  summarise(mean = round(mean(value), digits = 2),
            sd = round(sd(value), digits = 2)) %>%
  ungroup()

# Draw plot

p <- d2 %>%
  dplyr::select(c(detachment, relaxation, mastery, control, Class)) %>%
  gather(key = metric, value = value, 2:5) %>%
  mutate(metric = str_to_title(metric)) %>%
  ggplot() +
  geom_density(aes(x = value, fill = Class), colour = "black", alpha = 0.5) +
  geom_text(data = summary_stats[summary_stats$Class == "Low Recoverers", ], aes(x = 1, y = 1.75, hjust = "left", label = paste0(Class, ": ", "M = ", mean, ", SD = ", sd)), colour = "#1b9e77") +
  geom_text(data = summary_stats[summary_stats$Class == "High Recoverers", ], aes(x = 1, y = 2, hjust = "left", label = paste0(Class, ": ", "M = ", mean, ", SD = ", sd)), colour = "#d95f02") +
  geom_text(data = summary_stats[summary_stats$Class == "Low Detachers", ], aes(x = 1, y = 2.25, hjust = "left", label = paste0(Class, ": ", "M = ", mean, ", SD = ", sd)), colour = "#7570b3") +
  labs(title = "Distribution of recovery experiences by recovery profile",
       x = "Value",
       y = "Density",
       fill = "Recovery Profile") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3")) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~metric)

print(p)
ggsave("output/profile-distribution-plot-masters.png")

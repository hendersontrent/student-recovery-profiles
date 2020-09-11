#-------------------------------------------------
# This script aims to produce a function for
# producing bulk linear model outputs
#-------------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

stat_production <- function(stat_model){
  stat_model <- tidy(stat_model) %>%
    mutate(Significance = case_when(
      p.value < adj_pval ~ "Significant effect",
      TRUE               ~ "Non-significant effect")) %>%
    filter(!grepl("Intercept", term)) %>%
    mutate(p.value = round(p.value, digits = 3),
           estimate = round(estimate, digits = 2),
           std.error = round(std.error, digits = 2),
           statistic = round(statistic, digits = 2)) %>%
    rename("p-value" = p.value) %>%
    rename("Coefficient point estimate" = estimate) %>%
    rename("SE" = std.error) %>%
    rename("t" = statistic) %>%
    rename("Explanatory variable" = term)
}

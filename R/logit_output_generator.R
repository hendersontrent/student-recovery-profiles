#-------------------------------------------------
# This script aims to produce a function for
# producing bulk logistic regression outputs
#-------------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

# Get confidence intervals using profile likelihood (more reliable than Wald)

logit_output_generator <- function(x){
  
  the_conf <- as.data.frame(confint(x)) %>%
    rename(lower_log_odds = "2.5 %") %>%
    rename(upper_log_odds = "97.5 %")
  
  the_conf <- setDT(the_conf, keep.rownames = TRUE)[]
  
  final_conf <- the_conf %>%
    rename(measure = rn)
  
  # Get model test summary statistics and join together
  
  the_summary <- as.data.frame(summary.glm(x)$coefficients) %>%
    clean_names()
  
  the_summary <- setDT(the_summary, keep.rownames = TRUE)[]
  
  logit_outputs <- the_summary %>%
    rename(measure = rn) %>%
    rename(estimate_log_odds = estimate) %>%
    left_join(final_conf, by = c("measure" = "measure")) %>%
    rename(p_value = pr_z) %>%
    mutate(sig_indicator = case_when(
      p_value <= 0.05 ~ "Significant",
      TRUE            ~ "Non-significant"))
  
}

#-------------------------------------------
# This script aims to run a logistic
# regression model
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

# Model build

m1_logit <- glm(Class ~ job_demands + workload + job_control + consis_interest + pers_effort,
                family = binomial(link = "logit"),
                data = d2)

#------------------------ DIAGNOSTICS & ASSUMPTIONS ----------------

# Check outliers

model.data <- augment(m1_logit) %>% 
  mutate(index = 1:n())

model.data %>% 
  top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Class), size = 2) +
  theme_bw()

check<- model.data %>% 
  filter(abs(.std.resid) > 3) # No significant outliers

# Check multicollinearity

car::vif(m1_logit) # VIF values are well below 5 suggesting no multicollinearity

# Check linearity of continuous predictors and logit of response variable

probabilities <- predict(m1_logit, type = "response")

d4 <- d3 %>%
  dplyr::select(c(job_demands, workload, job_control, consis_interest, pers_effort))

mydata <- d4 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "lm") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#------------------------ GET OUTPUTS ------------------------------

# Get outputs

tab_model(m1_logit, p.style = "asterisk")
summary(m1_logit)

# Retrieve and exponentiate coefficients

logit_outputs <- logit_output_generator(m1_logit)

logit_outputs <- logit_outputs %>%
  mutate(odds_calc = exp(estimate_log_odds),
         lower_odds = exp(lower_log_odds),
         upper_odds = exp(upper_log_odds))

chart_data <- logit_outputs %>%
  mutate(measure = case_when(
    measure == "job_demands"     ~ "Job Demands",
    measure == "workload"        ~ "Workload",
    measure == "job_control"     ~ "Job Control",
    measure == "consis_interest" ~ "Grit",
    measure == "pers_effort"     ~ "Perseverance of Effort",
    TRUE                         ~ measure)) %>%
  filter(measure != "(Intercept)") %>%
  mutate(measure = as.factor(measure))

#------------------------ DATA VISUALISATION -----------------------

# Specify colour palette

the_palette <- c("Significant" = "#FEB06A",
                 "Non-significant" = "#189AB4")

# Measurement to help with graph labelling

all_measures <- (length(chart_data$measure)+0.5)

# Build plot

p <- chart_data %>%
  ggplot(aes(y = measure, x = lower_odds, xend = upper_odds, yend = measure)) +
  geom_segment(aes(colour = sig_indicator, group = measure), size = 5) +
  geom_point(aes(x = odds_calc, y = measure), colour = "#A0E7E5", size = 4) +
  geom_vline(xintercept = 1, colour = "#05445E", lty = 2) +
  annotate("text", x = 0.70, y = all_measures, label  = "Decreased odds of Profile 2", 
           colour = "#05445E", hjust = 0.5, fontface = "bold", size = 3) +
  annotate("text", x = 1.5, y = all_measures, label  = "Increased odds of Profile 2", 
           colour = "#05445E", hjust = 0.5, fontface = "bold", size = 3) +
  labs(title = "Logistic regression output for predictors of LPA-generated recovery profiles",
       subtitle = "LPA model returned 2 recovery profiles (classes)",
       x = "Odds",
       y = "Measure",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  scale_colour_manual(values = the_palette)
print(p)

# Export plot

CairoPNG("output/logit.png", 800, 600)
print(p)
dev.off()

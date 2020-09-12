#-------------------------------------------
# This script aims to prep the data ready
# for analysis
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

#------------------------ PREP DATA --------------------------------

d1 <- d %>%
  mutate(ci1 = case_when(
         ci1 == 1 ~ 5,
         ci1 == 2 ~ 4,
         ci1 == 3 ~ 3,
         ci1 == 4 ~ 2,
         ci1 == 5 ~ 1,
         TRUE    ~ 999)) %>%
  mutate(ci2 = case_when(
         ci2 == 1 ~ 5,
         ci2 == 2 ~ 4,
         ci2 == 3 ~ 3,
         ci2 == 4 ~ 2,
         ci2 == 5 ~ 1,
         TRUE    ~ 999)) %>%
  mutate(ci3 = case_when(
         ci3 == 1 ~ 5,
         ci3 == 2 ~ 4,
         ci3 == 3 ~ 3,
         ci3 == 4 ~ 2,
         ci3 == 5 ~ 1,
         TRUE    ~ 999)) %>%
  mutate(pe4 = case_when(
         pe4 == 1 ~ 5,
         pe4 == 2 ~ 4,
         pe4 == 3 ~ 3,
         pe4 == 4 ~ 2,
         pe4 == 5 ~ 1,
         TRUE     ~ 999)) %>%
  filter(ci1 != 999) %>%
  filter(ci2 != 999) %>%
  filter(ci3 != 999) %>%
  filter(pe4 != 999)

d2 <- d1 %>%
  mutate(detachment = (d1 + d2 + d3 + d4)/4,
         relaxation = (r1 + r2 + r3 + r4)/4,
         mastery = (m1 + m2 + m3 + m4)/4,
         control = (c1 + c2 + c3 + c4)/4,
         consis_interest = ci1 + ci2 + ci3 + ci4,
         pers_effort = pe1 + pe2 + pe3 + pe4,
         job_demands = jd1 + jd2 + jd3 + jd4,
         workload = qwi1 + qwi2 + qwi3 + qwi4 + qwi5,
         job_control = jc1 + jc2 + jc3 + jc4 + jc5 + jc6 + jc7 + jc8 + jc9 + jc10,
         engagement_vigour = vig1 + vig2 + vig3,
         engagement_dedication = ded1 + ded2 + ded3,
         engagement_absorption = abs1 + abs2 + abs3,
         burnout_exhaustion = exh1 + exh2 + exh3 + exh4 + exh5,
         burnout_cynicism = cyn1 + cyn2 + cyn3 + cyn4,
         burnout_profefficacy = eff1 + eff2 + eff3 + eff4 + eff5 + eff6,
         mental_health = (mhi1 + mhi2 + mhi3 + mhi4 + mhi5)*4,
         panas_pos = p1 + p2 + p3 + p4 + p5,
         panas_neg = n1 + n2 + n3 + n4 + n5,
         swl = swl1 + swl2 + swl3 + swl4 + swl5) %>%
  dplyr::select(c(id, gender, age, marital, kids, program, load, degree, employ, employhrs,
                  detachment, relaxation, mastery, control, consis_interest, pers_effort,
                  job_demands, workload, job_control, engagement_vigour, engagement_dedication, 
                  engagement_absorption, burnout_exhaustion, burnout_cynicism, burnout_profefficacy,
                  mental_health, panas_pos, panas_neg, swl)) %>%
  drop_na()

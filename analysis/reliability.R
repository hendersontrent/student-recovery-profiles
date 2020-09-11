#-------------------------------------------
# This script aims to run reliability
# diagnostics using Cronbach's Alpha
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 11 September 2020
#-------------------------------------------

#------------------- JOB DEMANDS -----------------------------------

demands <- d %>%
  dplyr::select(c(jd1, jd2, jd3, jd4))

m <- alpha(demands)
demands_alpha <- data.frame(construct = "Job Demands",
                            alpha = m$total$raw_alpha)

#------------------- WORKLOAD -----------------------------------

workload <- d %>%
  dplyr::select(c(qwi1, qwi2, qwi3, qwi4, qwi5))

m1 <- alpha(workload)
workload_alpha <- data.frame(construct = "Workload",
                            alpha = m1$total$raw_alpha)

#------------------- JOB CONTROL -----------------------------------

job_control <- d %>%
  dplyr::select(c(jc1, jc2, jc3, jc4, jc5, jc6, jc7, jc8, jc9, jc10))

m2 <- alpha(job_control)
job_control_alpha <- data.frame(construct = "Job Control",
                             alpha = m2$total$raw_alpha)

#------------------- CONSISTENCY OF INTEREST -----------------------

consis_int <- d %>%
  dplyr::select(c(ci1, ci2, ci3, ci4))

m3 <- alpha(consis_int) # DOUBLE CHECK REVERSE SCORING HERE!!!
consis_alpha <- data.frame(construct = "Consistency of Interest",
                             alpha = m3$total$raw_alpha)

#------------------- PERSISTENCY OF EFFORT -------------------------

persis_effort <- d %>%
  dplyr::select(c(pe1, pe2, pe3, pe4))

m4 <- alpha(persis_effort) # DOUBLE CHECK REVERSE SCORING HERE!!!
persis_alpha <- data.frame(construct = "Persistency of Effort",
                             alpha = m4$total$raw_alpha)

#------------------- DETACHMENT ------------------------------------

detachment <- d %>%
  dplyr::select(c(d1, d2, d3, d4))

m5 <- alpha(detachment)
detach_alpha <- data.frame(construct = "Detachment",
                             alpha = m5$total$raw_alpha)

#------------------- RELAXATION ------------------------------------

relaxation <- d %>%
  dplyr::select(c(r1, r2, r3, r4))

m6 <- alpha(relaxation)
relax_alpha <- data.frame(construct = "Relaxation",
                             alpha = m6$total$raw_alpha)

#------------------- MASTERY ---------------------------------------

mastery <- d %>%
  dplyr::select(c(m1, m2, m3, m4))

m7 <- alpha(mastery)
mastery_alpha <- data.frame(construct = "Mastery",
                             alpha = m7$total$raw_alpha)

#------------------- CONTROL ---------------------------------------

control <- d %>%
  dplyr::select(c(c1, c2, c3, c4))

m8 <- alpha(control)
control_alpha <- data.frame(construct = "Control",
                             alpha = m8$total$raw_alpha)

#------------------- MERGE -----------------------------------------

alphas <- bind_rows(demands_alpha, workload_alpha, job_control_alpha, 
                    consis_alpha, persis_alpha, detach_alpha, 
                    relax_alpha, mastery_alpha, control_alpha) %>%
  mutate(alpha = round(alpha, digits = 3))

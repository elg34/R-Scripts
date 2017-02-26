rm(list = ls())
library(mediation)
data(jobs)

# # # # DESCRIPTION OF DATA SET
# Data :: efficacy of a job training intervention on unemployed workers. 1,801 unemployed workers
# received a pre-screening questionnaire and were then randomly assigned to treatment (workshops)
# and control groups.
# IV/X         treat: whether the ppts were in treatment or control group 
# Mediator/M   job_seek: A continuous scale (1-5) measuring the level of job-search self-efficacy
# DV/Y         depress2: Measure of depressive symptoms post-treatment.
# Other vars   econ_hard: economic hardship pre-treatment, sex, age, educ: education, [...]

# # # # MEDIATION ANALYSIS
# Is the effect of treatment on depressive symptoms mediated by job-search self-efficacy?

# Linear Regression: Effect of Treatment on Mediator (factoring out econ_hard, gender and age)
b <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
# Linear Regression: Effect of Treatment on Outcome (factoring out the mediator, econ_hard, gender and age)
c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)

# Mediation (quasi-Bayesian approximation):
# roughly equals LSEM method (Baron and Kenny, 1986) if b & c are normal linear regressions!
mediation_bayes <- mediate(b, c, sims=50, treat="treat", mediator="job_seek")
summary(mediation_bayes)
plot(mediation_bayes)

# Mediation (nonparametric bootstrap):
mediation_bootstrap <- mediate(b, c, boot=TRUE, sims=50, treat="treat", mediator="job_seek")
summary(mediation_bootstrap)
plot(mediation_bootstrap)

# # Interpretation of output: Mediation significant if ACME significant!
# Total Effect = a total effect of X on Y (Y~X). 
# The direct effect (ADE) = a direct effect of X on Y after taking into account M (Y~X+M). 
# The mediation effect (ACME) is the total effect minus the direct effect


library(ggplot2)
library(lme4)
library(stringr)
library(tidyverse)

# import data ==================================================================
regr_df <- read_csv('2160_regressions.csv')

# set up reporting variable (i.e. parent/self) for adversities and depression
# variables
regr_df$report_lvl_dep <- "child"
regr_df$report_lvl_adv <- "child"

# parent reported depression
regr_df$report_lvl_dep[!str_detect(regr_df$y, "cbcl.*")] <- "parent"

# parent reported adversities !!! 
# NEED TO CHECK THE PARENT_STRESS VARIABLES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# From comparing the results here with the results Vera sent me it seems that 
# self and parent on these variables are swapped around!
# Currently I am assuming the variable names are correct.
regr_df$report_lvl_adv[regr_df$x1 == 'ce_z'] <- "parent"
regr_df$report_lvl_adv[regr_df$x1 == 'ltd_z'] <- "parent"
regr_df$report_lvl_adv[regr_df$x1 == 'parent0_5stress_z'] <- "parent"
regr_df$report_lvl_adv[regr_df$x1 == 'parent6_11stress_z'] <- "parent"
regr_df$report_lvl_adv[regr_df$x1 == 'pprisks_z'] <- "parent"

# recode sample data to character labels to make it clearer
regr_df$sample[regr_df$sample == 2] <- "mixed"
regr_df$sample[regr_df$sample == 1] <- "female"
regr_df$sample[regr_df$sample == 0] <- "male"

# examining data ===============================================================
# distribution of regression coefficients
hist(regr_df$b, breaks = 100) # normal

# distribution of p values
hist(regr_df$p, breaks = 100) # almost uniform with a slight peak close to 0

# regression coefficients with error bars, ordered
regr_df <- regr_df[order(regr_df$b),]
regr_df$id <- 1:nrow(regr_df)
ggplot(data = regr_df, aes(x = id, y = b)) + 
  geom_errorbar(aes(ymin = b - (1.96 * se), ymax = b + (1.96 * se)), 
                colour='orange') + 
  geom_point(colour='black') + 
  coord_flip()

# distribution of regression coefficients per y
ggplot(data = regr_df, aes(x = y,y = b, colour = y)) + 
  geom_hline(aes(yintercept = 0)) + 
  geom_jitter(alpha = .5) +
  coord_flip() + 
  theme(legend.position = "none")
# CIDI has a much wider range than all the others, this might be because it
# comes from a logistic regression. Have to see if we can actually use those
# effect sizes in our current approach

# distribution of regression coefficients per x1
ggplot(data = regr_df, aes(x = x1,y = b, colour = x1)) + 
  geom_hline(aes(yintercept = 0)) + 
  geom_jitter(alpha = .5) +
  coord_flip() + 
  theme(legend.position = "none")

# distribution of regression coefficients per x2
ggplot(data = regr_df, aes(x = x2,y = b, colour = x2)) + 
  geom_hline(aes(yintercept = 0)) + 
  geom_jitter(alpha = .5) +
  coord_flip() + 
  theme(legend.position = "none")

# analysis =====================================================================
# For now, do analyses on two subsets: including and excluding CIDI.
regr_df_nc <- filter(regr_df, y != "cidi")

# look at average coefficient through single level intercept only regression
fit_lm <- lm(b ~ 1, data = regr_df)
fit_lm_nc <- lm(b ~ 1, data = regr_df_nc)
summary(fit_lm)
summary(fit_lm_nc)

# decompose variance by variable categories (y, x1, & x2) by fitting
# cross-classified mixed model
fit_lmm <- lmer(b ~ (1 | y) + (1 | x1) + (1 | x2), data = regr_df)
fit_lmm_nc <- lmer(b ~ (1 | y) + (1 | x1) + (1 | x2), data = regr_df_nc)
summary(fit_lmm)
summary(fit_lmm_nc)

# examine variance components (i.e. proportion of variance by variable cat)
var_lmm <- as.data.frame(VarCorr(fit_lmm))
var_lmm <- var_lmm[c(1,3,2,4),]
rbind(var_lmm$grp, round(var_lmm$vcov / sum(var_lmm$vcov), 3))

var_lmm_nc <- as.data.frame(VarCorr(fit_lmm_nc))
rbind(var_lmm_nc$grp, round(var_lmm_nc$vcov / sum(var_lmm_nc$vcov), 3))

# how much variance by sample (i.e. sex) and reporting level (parent / self)?
fit_lmm_sample <- lmer(b ~ sample + (1 | y) + (1 | x1) + (1 | x2), 
                       data = regr_df)
fit_lmm_dep <- lmer(b ~ report_lvl_dep + (1 | y) + (1 | x1) + (1 | x2), 
                    data = regr_df)
fit_lmm_adv <- lmer(b ~ report_lvl_adv + (1 | y) + (1 | x1) + (1 | x2), 
                    data = regr_df)

summary(fit_lmm_sample)
car::Anova(fit_lmm_sample, type = 2)
summary(fit_lmm_dep)
summary(fit_lmm_adv)

fit_lmm_allpredictors <- lmer(b ~ sample * report_lvl_dep * report_lvl_adv + 
                                (1 | y) + (1 | x1) + (1 | x2), 
                              data = regr_df)

summary(fit_lmm_allpredictors)
car::Anova(fit_lmm_allpredictors, type = 3)

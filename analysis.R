---
  title             : "Justification is good; transparency is better: Applying the multilevel multivariate meta-analysis method"
shorttitle        : "The dynamical signature of anhedonia in MDD"

author: 
  - name          : "Vera E. Heininga"
affiliation   : "1"
corresponding : yes
address       : "Tiensestraat 102 - bus 3713, 3000 Leuven"
email         : "vera.heininga@kuleuven.be"
- name          : "B. Stuijfzand"
affiliation   : "2"
- name          : "J.A.C.J. Bastiaansen"
affiliation   : "3"
- name          : "A.J. Oldehinkel"
affiliation   : "3"
- name          : "W. Vanpaemel"
affiliation   : "1"
- name          : "F. Teurlinckx"
affiliation   : "1"
- name          : "R. Artner"
affiliation   : "1"
- name          : "A. Mason"
affiliation   : "4"    
- name          : "M. Munafò"
affiliation   : "5"   

affiliation:
  - id            : "1"
institution   : "Research group of Quantitative Psychology and Individual Differences, KU Leuven, Belgium"
- id            : "2"
institution   : "Statscape - Data Science and Statistics"
- id            : "3"
institution   : "University Medical Center Groningen"
- id            : "4"
institution   : "University of Western Australia"
- id            : "5"
institution   : "University of Bristol"


abstract: |
  Would not it be great if we could not only report a finding, but also how robust this finding is over all the alternative analysis options that are present in that dataset? Also, transparently discussing findings along these different alternatives could provide others the opportunity to question analytical choices, and to come up with and test other alternatives (Simmons, Nelson en Simonsohn, 2011). Transparently discussing findings along different alternatives provides not only a robustness check but also provides other with the opportunity to question analytical choices, and to come up with and test other alternatives (Heininga et al., 2015; Silberzahn et al., 2017). With the aim to facilitate those researchers who would like to be more transparent in their analytical decisions, the present study illustrates what can be gained by adopting the multilevel multivariate meta-analysis approach.

keywords          : "Transparancy; Multiverse"

bibliography      : ["references.bib"]

figurelist        : no
tablelist         : no
footnotelist      : no
lineno            : no
figsintext        : no

documentclass     : "apa6"
mask              : no
class             : "man"
output            : papaja::apa6_pdf
classoption: landscape
---

```{r load_packages, include = FALSE}
devtools::install_github("crsh/papaja")
# install.packages("stringi", repos="http://cran.rstudio.com/", dependencies=TRUE)

if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(lme4, stringr, tidyverse)
```

```{r Data_Import, include = FALSE}
regr_df <- read.csv("i:/# SyncFiles/Paper ideas/Multiplicity paper/2160_regressions.csv", header = TRUE, dec = ".",sep = "\t")
```

```{r Prep_work, include = FALSE}
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
```

# Examining data

```{r Examining_Data}

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
# comes from a logistic regression. Logistic regression coefficients cannot be
# compared between models, so this might not be appropriate here either. For 
# now, cidi is included, but this is something that needs a bit more thinking.

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
```

# Analysis

```{r}
# analysis =====================================================================
# look at average coefficient through single level intercept only regression
fit_lm <- lm(b ~ 1, data = regr_df)
summary(fit_lm)

# decompose variance by variable categories (y, x1, & x2) by fitting
# cross-classified mixed model
fit_lmm <- lmer(b ~ (1 | y) + (1 | x1) + (1 | x2), data = regr_df)
summary(fit_lmm)

# examine variance components (i.e. proportion of variance by variable cat)
var_lmm <- as.data.frame(VarCorr(fit_lmm))
var_lmm <- var_lmm[c(1,3,2,4),]
rbind(var_lmm$grp, round(var_lmm$vcov / sum(var_lmm$vcov), 3))

# do sample (female/male/mixed) and reporting level (parent/self) affect
# results? Note the potential issue with parent_stress and self_stress - i.e.
# the reporting variable on adversities might not be correct at the moment. This
# should be checked.
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
```
 

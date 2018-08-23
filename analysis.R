---
title             : "Justification is good, transparency is better: Applying the multilevel meta-analysis method"
shorttitle        : "Justification is good, transparency is better"

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

author_note: |
  This is work in progress.

abstract: |
  Would not it be great if we could not only report a finding, but also how robust this finding is over all the alternative analysis options that are present in that dataset? Also, transparently discussing findings along these different alternatives could provide others the opportunity to question analytical choices, and to come up with and test other alternatives (Simmons, Nelson en Simonsohn, 2011). Transparently discussing findings along different alternatives provides not only a robustness check but also provides other with the opportunity to question analytical choices, and to come up with and test other alternatives (Heininga et al., 2015; Silberzahn et al., 2017). With the aim to facilitate those researchers who would like to be more transparent in their analytical decisions, the present study illustrates what can be gained by adopting the multilevel multivariate meta-analysis approach.

keywords          : "Type 1 error; Transparancy; Choice overload; Comparable measures available"

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
regr_df <- read.csv("i:/# SyncFiles/Multiplicity paper/2160_regressions.csv", header = TRUE, dec = ".",sep = ";")
```

# Introduction

## The concept: A multi-level meta-variable analysis tool

Multilevel models, also known as hierarchical linear models, linear mixed models, or mixed-effect models, are statistical models designed to handle clustered or grouped data.

Across study findings, meta-analytical techniques can detect and correct for the variation in variable choice by means of a weighed, single metric. Despite that these meta-analyses could also be used on results from a single paper, there are interdependencies between variables, and no techniques have been proposed to address this problem within studies.
Using techniques common in multilevel modelling and meta-analytical literature, we established an overall effect size for these outcomes, including an uncertainty interval, and disentangle the dependencies in this effect that arise due to the repeated use of the same variables in different analysis permutations. We subsequently used regression techniques to identify how the different informants and subsample may cause this effect to vary.

Using this approach, we aimed to provide a multi-level meta-variable analysis tool that can: a) give new insights into the robustness of the measures, and b) provide formal testing procedures into which subset of variables is the most predictive of the outcome variable. 

# Methods
Data was downloaded from Heininga et al 2015 (see S1 Dataset; Table 1 in Heininga et al 2015). 

## Participants
The data for this study comes from the TRacking Adolescents' Individual Lives Survey (TRAILS). TRAILS is an ongoing, multidisciplinary research on the psychological, social and physical development of adolescents and young adults (Esther Nederhof, Belsky, Ormel, & Oldehinkel, 2012). Approximately 2700 young people participate, since their tenth or eleventh year (Esther Nederhof, Jorg, et al., 2012). These participants have been examined biennially to triennially the past 15 years, through a multitude of different measure types such as questionnaires, interviews, tests and/or physical measurements. In addition to information on the participant, also family members, teachers and partners have provided information on the psychosocial functioning of the TRAILS participants.

At T1, 2230 (pre-)adolescents were enrolled in the study (response rate 76%, mean age 11.1, SD = 0.6, 51% girls) (de Winter et al., 2005), of whom 96% (N = 2149, mean age 13.6, SD = 0.5, 51% girls) participated at T2. The response rates at T3 and T4 were, respectively, 81% (N = 1816, mean age 16.3, SD = 0.7, 52% girls) and 83% (N = 1881, mean age 19.1, SD = 0.6, 52% girls). The data collection included, among many other things, self-report and parent-report questionnaires, a psychiatric diagnostic interview, and a life stress interview.

The study was approved by the Dutch Central Committee on Research Involving Human Subjects (CCMO). Participants were treated in compliance with APA ethical standards, and all measurements were carried out with their adequate understanding and written consent.

## Measures
### Depressive symptoms 
#### CBCL11
Parent reported depressive symptoms by means of the 13-item DSM-oriented Affective Problem Scale (Thomas M Achenbach, Dumenci, & Rescorla, 2010) of the Child Behavior Checklist (CBCL; T. M. Achenbach & Rescorla, 2001), when participant  was aged 11. Symptoms were assessed over the last six months, with possible answers 0 = not true, 1 = somewhat true or sometimes true, and 2 = very true or often true. Cronbachs alpha is α = .68
#### CBCL13
Parent reported depressive symptoms by means of the 13-item DSM-oriented Affective Problem Scale (Thomas M Achenbach, Dumenci, & Rescorla, 2010) of the Child Behavior Checklist (CBCL; T. M. Achenbach & Rescorla, 2001), when participant was aged 13. Symptoms were assessed over the last six months, with possible answers 0 = not true, 1 = somewhat true or sometimes true, and 2 = very true or often true. Cronbachs alpha is α = .73
#### CBCL16
Parent reported depressive symptoms by means of the 13-item DSM-oriented Affective Problem Scale (Thomas M Achenbach, Dumenci, & Rescorla, 2010) of the Child Behavior Checklist (CBCL; T. M. Achenbach & Rescorla, 2001), when participant  was aged 16. Symptoms were assessed over the last six months, with possible answers 0 = not true, 1 = somewhat true or sometimes true, and 2 = very true or often true. Cronbachs alpha is α = .76
#### CBCLmean
Mean of CBCL11, CBCL13, and CBCL16.

#### YSR11
Participants reported on depressive symptoms by its self-report variant, the 13-item Affective Problem Scale of the Youth Self-Report (YSR; Ormel et al., 2012), when aged 11. Symptoms were assessed over the last six months, with possible answers 0 = not true, 1 = somewhat true or sometimes true, and 2 = very true or often true. Cronbachs alpha is α = .77
#### YSR13
Participants reported on depressive symptoms by its self-report variant, the 13-item Affective Problem Scale of the Youth Self-Report (YSR; Ormel et al., 2012), when aged 13. Symptoms were assessed over the last six months, with possible answers 0 = not true, 1 = somewhat true or sometimes true, and 2 = very true or often true. Cronbachs alpha is α = .72
#### YSR16
Participants reported on depressive symptoms by its self-report variant, the 13-item Affective Problem Scale of the Youth Self-Report (YSR; Ormel et al., 2012), when aged 11. Symptoms were assessed over the last six months, with possible answers 0 = not true, 1 = somewhat true or sometimes true, and 2 = very true or often true. Cronbachs alpha is α = .78
#### YSRmean
Mean of YSR11, YSR13, YSR16.

#### CIDI MDD
Participants were interviewed using the World Health Organization Composite International Diagnostic Interview (WHO CIDI; version 3.0; Kessler & Ustün, 2004), to assess a lifetime Major Depressive Episode according to the Diagnostic and Statistical Manual of Mental Disorders (DSM-IV; American Psychiatric Association, 2000).

### Childhood adversities
The following descriptions are copied from Heininga et al. (2015)
#### Pre-/perinatal risks
Pre- and perinatal risks were assessed at T1 in an interview with one of the parents, usually the mother, and included questions about maternal prenatal smoking, maternal prenatal alcohol use, birth weight, gestational age, and pregnancy and delivery complications. Maternal prenatal smoking was coded as follows: 0 = no smoking; 1 = 10 cigarettes a day or less; 2 = more than 10 cigarettes a day. Maternal prenatal alcohol use was coded as follows: 0 = no alcohol use, 1 = up to three glasses per week, 2 = four glasses per week or more. Birth weight was coded as 0 = normal birth weight and 1 = either low (< 2,500 g) or high (> 4,500 g) birth weight. Gestational age was recoded into two groups: 0 = normal (between 34 and 42 weeks) and 1 = abnormal (33 weeks or less, or more than 42 weeks). Pregnancy and delivery complications were coded as 0 = no complications; 1 = between one and four complications; 2 = five or more complications. The index of pre- and perinatal risks was composed by adding the scores of these variables.

#### Childhood events
Childhood events were assessed during the T1 parent interview, as partent reported stressful events that occurred in t he life of the participant before T1. The variable was created by summing the following events: severe (physical or mental) disease of father or mother; severe illness (life-threatening or threat of serious permanent effects) of a sibling; death of a household member; parental divorce; and absence from home for three months or longer.

#### Long term difficulty
Long term difficulties were assessed with a questionnaire completed by parents at T2, and reflects the sum score of the following difficulties: chronic disease or handicap of the participant, chronic disease or handicap of a household member, being bullied, long-lasting conflicts with a household member, and long-lasting conflicts with someone else, all experienced before T1.

#### SR stress (0-5)
Self Reported stressfulness of life when the participant was between 0 and 5 years old, retrospectively reported at T2. Participants were asked ‘How many stressful events did you experience in this period?, with answers ranging from 0 = not at all to 10 = very much on a 11-point scale.

#### SR stress (6-11)
Self Reported stressfulness of life when the participant was between 6 and 11 years old, retrospectively reported at T2. Participants were asked ‘How many stressful events did you experience in this period?’, with answers ranging from 0 = not at all to 10 = very much on a 11-point scale.

#### PR stress (0-5)
Parent Reported stressfulness of the participants’ lives when they were between 0 and 5 years old, retrospectively reported at T2. Parents were asked ‘How stressful was your child’s life in this life phase?’, with answers ranging from 0 = not at all to 10 = very much on a 11-point scale.

#### PR stress (6-11)
Parent Reported stressfulness of the participants’ lives when they were between 6 and 11 years old, retrospectively reported at T2. Parents were asked ‘How stressful was your child’s life in this life phase?’, with answers ranging from 0 = not at all to 10 = very much on a 11-point scale.

#### Verbal abuse
Physical abuse was measured at T4 by six questions from the Childhood Trauma Questionnaire (Bernstein & Fink, 1998; α = .84), with answers possibilities 1 = no, never; 2 = yes, one or two times; 3 = sometimes; 4 = often; or 5 = very often.

#### Physical abuse
Physical abuse was measured at T4 by six questions from the Childhood Trauma Questionnaire (Bernstein & Fink, 1998; α = .73), with answers possibilities 1 = no, never; 2 = yes, one or two times; 3 = sometimes; 4 = often; or 5 = very often.

#### Sexual abuse
Sexual abuse was based on a list of five unwanted sexual acts by an adult family member, friend of the family or stranger, ranging from touching to sexual intercourse, the occurrence of which could be rated as 1 = never happened to me, 2 = happened once, or 3 = happened several times. The abuse measures reflect the mean of the ratings.



# Results
# Examining data

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

# Discussion

- Richard: "The smaller the changes of a certain characteristic (e.g. the influence of IV1 on the DV), the more robust that characteristic is and the less important it is which of the many possible models you chose. On the other hand, if you get completely different results for the IV of interest, you should make an argument for which model to choose." How to define the line between "small change" and "big change" in models? 

# Study limitations

# Conclusions

\newpage

# References

\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}



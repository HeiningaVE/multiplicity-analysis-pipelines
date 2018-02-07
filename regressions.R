library(stringr)
library(tidyverse)

# import data ==================================================================
# dataset available from: 
# http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0125383.s002
# and saved into .csv format using SPSS before loading here.
ge_df <- read_csv("data.csv")

# rename genome variables into names that match excel file distributed to me by
# Vera Heininga: "2017 03 29 VH_pval_2160_regressions_VH (long format).xlsx"
# note that for the recessive genotypes "recessive" and "recessive with SNP"
# there are 2 variables each, which have been named accordingly.
ge_df <- 
  ge_df %>% 
  rename(
    dom_snp = HTTLPR_TriDom,
    dom = HTTLPR_BiDom,
    add_snp = HTTLPR_TriAdd,
    add = HTTLPR_BiAdd,
    rec_snp_1 = HTTLPR_TriCoDom_SS,
    rec_snp_2 = HTTLPR_TriCoDom_LS,
    rec_1 = HTTLPR_BiCoDom_SS,
    rec_2 = HTTLPR_BiCoDom_LS
  )

# all variable names to lowercase for ease
names(ge_df) <- tolower(names(ge_df))

# recode all SPSS defined missings to NA's
ge_df$pprisks[ge_df$pprisks == 98] <- NA
ge_df$ce[ge_df$ce == 98] <- NA
ge_df$ltd[ge_df$ltd == 98] <- NA

# recode all adversaties into z-scores
ge_df$pprisks_z <- scale(ge_df$pprisks)
ge_df$ce_z <- scale(ge_df$ce)
ge_df$ltd_z <- scale(ge_df$ltd)
ge_df$parent0_5stress_z <- scale(ge_df$parent0_5stress)
ge_df$parent6_11stress_z <- scale(ge_df$parent6_11stress)
ge_df$self0_5stress_z <- scale(ge_df$self0_5stress)
ge_df$self6_11stress_z <- scale(ge_df$self6_11stress)
ge_df$verbalabuse_z <- scale(ge_df$verbalabuse)
ge_df$physabuse_z <- scale(ge_df$physabuse)
ge_df$sexabuse_z <- scale(ge_df$sexabuse)

# recode all depression scores into z-scores, except CIDI which is binary
ge_df$cbcl1_z <- scale(ge_df$cbcl1)
ge_df$cbcl2_z <- scale(ge_df$cbcl2)
ge_df$cbcl3_z <- scale(ge_df$cbcl3)
ge_df$cbclmean_z <- scale(ge_df$cbclmean)
ge_df$ysr1_z <- scale(ge_df$ysr1)
ge_df$ysr2_z <- scale(ge_df$ysr2)
ge_df$ysr3_z <- scale(ge_df$ysr3)
ge_df$ysrmean_z <- scale(ge_df$ysrmean)

# get model formulas ===========================================================
# get depression variable names (outcome)
depression <- 
  ge_df %>% 
  select(cbcl1_z:ysrmean_z, cidi) %>% 
  names()
       
# get adversity variable names (predictor 1)
adversity <- 
  ge_df %>% 
  select(pprisks_z:sexabuse_z) %>% 
  names()

# get genetic variable names (predictor 2). Note I skip the recessive genotypes,
# will add these later as model specification is slightly different
genotype <- 
  ge_df %>% 
  select(dom_snp:add) %>% 
  names()

# get recessive genotype variable names
genotype_rec <- cbind(c("rec_snp_1", "rec_1"),
                      c("rec_snp_2", "rec_2")
)

n_y <- length(depression) # number of depression outcomes
n_x1 <- length(adversity) # number of adversity predictors
n_x2 <- length(genotype) # number of genotypes (- recessive)
n_x2_rec <- nrow(genotype_rec)

# combine into model formulas
model_fml <- paste(rep(depression, each = n_x1 * n_x2), 
                        "~", 
                        rep(rep(adversity, each = n_x2), n_y), 
                        "*", 
                        rep(genotype, n_y * n_x1)
                  )

model_fml_rec <- paste(rep(depression, each = n_x1 * n_x2_rec), 
                            "~", 
                            rep(rep(adversity, each = n_x2_rec), n_y), 
                            "*", 
                            rep(genotype_rec[,1], n_y * n_x1), 
                            "+",
                            rep(rep(adversity, each = n_x2_rec), n_y), 
                            "*", 
                            rep(genotype_rec[,2], n_y * n_x1)
)

# for each model, specify whether it's a gaussian outcome or binomial
model_type <- rep("gaussian", n_y * n_x1 * n_x2)
model_type[str_detect(model_fml, "cidi.")] <- "binomial"

model_type_rec <- rep("gaussian", n_y * n_x1 * n_x2_rec)
model_type_rec[str_detect(model_fml_rec, "cidi.")] <- "binomial"

# fit models ===================================================================
# create generic functions for model fitting and extracting relevant output
# (i.e. coefficient, SE, p-value, and model df's)

fit_model <- function(fml, type, data) {
  fit <- summary(glm(fml, 
                     family = type, 
                     data = data))
  out <- c(fit$coef[4, c(1, 2, 4)],
           fit$df[2:3]
  )
  return(out)
}

fit_model_rec <- function(fml, type, data) {
  fit <- summary(glm(fml, 
                     family = type, 
                     data = data))
  out <- c(fit$coef[5, c(1, 2, 4)],
           fit$df[2:3], 
           fit$coef[6, c(1, 2, 4)],
           fit$df[2:3]
  )
  return(out)
}

# run models on complete sample
# dominant and additive genotypes 
out_all <- mapply(x = model_fml, 
                  y = model_type, 
                  function (x, y) fit_model(fml = x, 
                                            type = y, 
                                            data = ge_df)
           )

# recessive genotypes
out_all_rec <- mapply(x = model_fml_rec, 
                      y = model_type_rec, 
                      function (x, y) fit_model_rec(fml = x, 
                                                    type = y, 
                                                    data = ge_df)
                )
# shape output to get a row for each recessive * adversity interaction effect
out_all_rec <- t(out_all_rec)
tmp <- matrix(NA, nrow(out_all_rec) * 2, ncol(out_all_rec) / 2)
tmp[seq(1, (nrow(out_all_rec) * 2), 2), ] <- out_all_rec[, 1:5]
tmp[seq(2, (nrow(out_all_rec) * 2), 2), ] <- out_all_rec[, 6:10]
out_all_rec <- tmp

# run models on females
# dominant and additive genotypes
out_fem <- mapply(x = model_fml, 
                  y = model_type, 
                  function (x, y) fit_model(fml = x, 
                                            type = y, 
                                            data = filter(ge_df, sex == 0))
           )

# recessive genotypes
out_fem_rec <- mapply(x = model_fml_rec, 
                      y = model_type_rec, 
                      function (x, y) fit_model_rec(fml = x, 
                                                    type = y, 
                                                    data = filter(ge_df, 
                                                                  sex == 0))
)
# shape output to get a row for each recessive * adversity interaction effect
out_fem_rec <- t(out_fem_rec)
tmp <- matrix(NA, nrow(out_fem_rec) * 2, ncol(out_fem_rec) / 2)
tmp[seq(1, (nrow(out_fem_rec) * 2), 2), ] <- out_fem_rec[, 1:5]
tmp[seq(2, (nrow(out_fem_rec) * 2), 2), ] <- out_fem_rec[, 6:10]
out_fem_rec <- tmp

# model results males
# dominant and additive genotypes
out_mal <- mapply(x = model_fml, 
                  y = model_type, 
                  function (x, y) fit_model(fml = x, 
                                            type = y, 
                                            data = filter(ge_df, sex == 1))
           )

# recessive genotypes
out_mal_rec <- mapply(x = model_fml_rec, 
                      y = model_type_rec, 
                      function (x, y) fit_model_rec(fml = x, 
                                                    type = y, 
                                                    data = filter(ge_df, 
                                                                  sex == 1))
)
# shape output to get a row for each recessive * adversity interaction effect
out_mal_rec <- t(out_mal_rec)
tmp <- matrix(NA, nrow(out_mal_rec) * 2, ncol(out_mal_rec) / 2)
tmp[seq(1, (nrow(out_mal_rec) * 2), 2), ] <- out_mal_rec[, 1:5]
tmp[seq(2, (nrow(out_mal_rec) * 2), 2), ] <- out_mal_rec[, 6:10]
out_mal_rec <- tmp

# get output into a single data frame ==========================================
out <-rbind(cbind(t(out_all), 2),
            cbind(t(out_fem), 1),
            cbind(t(out_mal), 0),
            cbind(out_all_rec, 2),
            cbind(out_fem_rec, 1),
            cbind(out_mal_rec, 0)
       )

out <- as_tibble(out)
names(out) <- c("b", "se", "p", "df_res", "n_coef", "sample")

# get variable names for each model
out$y <- c(rep(sub("(.*) ~.*", "\\1", model_fml), 3),
           rep(rep(sub("(.*) ~.*", "\\1", model_fml_rec), each = 2), 3))

out$x1 <- c(rep(sub(".*~ (.*) \\*.*", "\\1", model_fml), 3),
            rep(rep(sub(".*~ (.*) \\*.*\\*.*", "\\1", model_fml_rec), each = 2), 3))

out$x2 <- c(rep(sub(".*\\* (.*)", "\\1", model_fml), 3),
            rep(rep(sub(".*\\* (.*)", "\\1", model_fml_rec), each = 2), 3))

index <- seq((length(model_fml) * 3) + 1, nrow(out), 2)
out$x2[index] <- paste(str_sub(out$x2[index], 1, -2), "1", sep = "")

# save results =================================================================
write.csv(out, "2160_regressions.csv", row.names=F)
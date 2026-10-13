# MFCC analysis of Turkish r ####
# Janalyn A. Miklas & Matthew C. Kelley ##
# Last Updated 2026 September 30 ##

## Load libraries ##
## Packages for data processing ##
library(tidyverse)
library(dplyr)
## Packages for logistic regression models & emmeans ##
library(lme4)
library(effects)
library(emmeans)
## Packages for creating the Filterbank ##
library(seewave) # for mel conversions #
library(MALDIquant) # for closest.match #
## Packages for plotting ##
library(ggplot2)
library(ggeffects)
library(GGally)

## Set working directory ##
setwd("~/Desktop/Dissertation/miklas_mfa/mfa_data")

## Data processing ####
## Upload Data file & determine column names ##
r_MFCC_data = read_csv("2026-09-30_r_MFCC_data.csv")
colnames(r_MFCC_data) = c("Filename", "Word_label", "Segment_label", "prev", "nex", "wd_prev", "wd_nex", "Interval",
                          "MFCC1", "MFCC2", "MFCC3", "MFCC4", "MFCC5", "MFCC6", "MFCC7", 
                          "MFCC8", "MFCC9", "MFCC10", "MFCC11", "MFCC12", "dur", "prev_dur", "nex_dur")

## Get total number of segments ##
r_MFCC_data %>%
  summarise(ntotal = n())
  # 215,045

## Get total number of segments per label ##
r_MFCC_data %>%
  group_by(Segment_label) %>%
  summarise(ntotal = n())

# A tibble: 2 × 2
#Segment_label ntotal
#<chr>          <int>
# 1 ɾ             214871
# 2 ɾː               174

## Mutate the data to indicate word-initial or word-final based on prev, nex, wd_prev, and wd_nex labels ##
r_MFCC_label = r_MFCC_data %>%
  mutate(
    position = as.factor(case_when(
      prev == "#UI#" ~ 'utt_initial',
      nex == "#UF#" ~ 'utt_final',
      wd_prev == "#WI#" & prev == "#PAU#" ~ 'WI_pau',
      wd_nex == "#WF#" & nex == "#PAU#" ~ 'WF_pau',
      wd_prev == "#WI#" & prev != "#PAU#" ~ 'WI', 
      wd_nex == "#WF#" & nex != "#PAU#" ~ 'WF',
      .default = 'medial')
  )) %>%
  relocate(position, .after = wd_nex)

## Print a summary of how many tokens are per position
table(r_MFCC_label$position)
# medial   utt_final utt_initial          WF      WF_pau          WI      WI_pau 
# 145739       20812         752       40041        5082        2337         282 

## Outlier Exclusion ####
## Identifying outliers in duration ##
summary(r_MFCC_label$dur)
plot(density(r_MFCC_label$dur))
quantile(r_MFCC_label$dur, 0.999)
order(r_MFCC_label$dur, decreasing = TRUE) %>% head(., 100)
sd(r_MFCC_label$dur)
mean(r_MFCC_label$dur)

## Exclusion of the outliers 3 or more sds from the mean using Mahalanobis distance ##
r_MFCC_label.sub = r_MFCC_label[abs(scale(r_MFCC_label$dur)) <= 3, ]

## Get summary of total segments after outlier exclusion ##
r_MFCC_label.sub %>%
  summarise(ntotal = n())
  #210,010

## Get summary of how many tokens per position after outlier exclusion ##
table(r_MFCC_label.sub$position)
# medial   utt_final utt_initial          WF      WF_pau          WI      WI_pau 
# 145672       16518         751       40005        4448        2334         282 

## Create a new column in the dataframe that uses contrast encoding for each position
r_MFCC_label.sub$position_2 = r_MFCC_label.sub$position
contrasts(r_MFCC_label.sub$position_2) <- "contr.sum"
levels(r_MFCC_label.sub$position_2) # shows the levels that exist in the column
str(r_MFCC_label.sub$position_2) # confirms that there are the number of contrasts as a string

## Statistical models & Plots ####
## Linear models predicting MFCC value per position ##
## MFCC1 ##
mfcc1 = lm( MFCC1 ~ position_2, data=r_MFCC_label.sub)
summary(mfcc1, cor=FALSE)
knitr::kable(round(coef(summary(mfcc1)), 3), format="latex")

## Pairwise comparison of the levels ##
emmeans(mfcc1, pairwise ~ position_2:position_2)

## Effect plot showing average MFCC1 value for each position ##
plot(ggpredict(mfcc1, "position_2")) + xlab("Segment Position") + ylab("MFCC1") + 
  ggtitle("")

## MFCC2 ##
mfcc2 = lm( MFCC2 ~ position_2, data=r_MFCC_label.sub)
summary(mfcc2, cor=FALSE)
knitr::kable(round(coef(summary(mfcc2)), 3), format="latex")
## Pairwise comparison of the levels ##
emmeans(mfcc2, pairwise ~ position_2:position_2)

## Effect plot showing average MFCC2 value for each position ##
plot(ggpredict(mfcc2, "position_2")) + xlab("Segment Position") + ylab("MFCC2") + ggtitle("")

## MFCC3 ##
mfcc3 = lm( MFCC3 ~ position_2, data=r_MFCC_label.sub)
summary(mfcc3, cor=FALSE)
knitr::kable(round(coef(summary(mfcc3)), 3), format="latex")

## Pairwise comparison of the levels ##
emmeans(mfcc3, pairwise ~ position_2:position_2)

## Effect plot showing average MFCC3 value for each position ##
plot(ggpredict(mfcc3, "position_2")) + xlab("Segment Position") + ylab("MFCC3") + ggtitle("")

## MFCC4 ##
mfcc4 = lm( MFCC4 ~ position_2, data=r_MFCC_label.sub)
summary(mfcc4, cor=FALSE)
knitr::kable(round(coef(summary(mfcc4)), 3), format="latex")
## Pairwise comparison of the levels ##
emmeans(mfcc4, pairwise ~ position_2:position_2)

## Effect plot showing average MFCC4 value for each position ##
plot(ggpredict(mfcc4, "position_2")) + xlab("Segment Position") + ylab("MFCC4") + ggtitle("")

## MFCC 5
mfcc5 = lm( MFCC5 ~ position_2, data=r_MFCC_label.sub)
summary(mfcc5, cor=FALSE)
knitr::kable(round(coef(summary(mfcc5)), 3), format="latex")

## Pairwise comparison of the levels ##
emmeans(mfcc5, pairwise ~ position_2:position_2)

## Effect plot showing average MFCC5 value for each position ##
plot(ggpredict(mfcc5, "position_2")) + xlab("Segment Position") + ylab("MFCC5") + 
  ggtitle("")

## Duration ##
dur = lm( dur ~ position_2, data=r_MFCC_label.sub)
summary(dur, cor=FALSE)
knitr::kable(round(coef(summary(dur)), 3), format="latex")
## Pairwise comparison of the levels ##
emmeans(dur, pairwise ~ position_2:position_2)

## Effect plot showing average duration value for each position ##
plot(ggpredict(dur, "position_2")) + xlab("Segment Position") + ylab("Duration (s)") + ggtitle("")




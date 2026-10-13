# Plotting average spectra using GAM ####
# J.A.Miklas & M.C.Kelley #
# Last updated: 2026 September 30 #

## Load libraries ##
## Packages for data processing ##
library(tidyverse)
library(dplyr)
## Packages for general additive models ##
library(mgcv) 
library(itsadug)
## Packages for creating the Filterbank ##
library(seewave) # for mel conversions #
library(MALDIquant) # for closest.match #
## Packages for plotting ##
library(ggplot2)
library(ggeffects)
library(GGally)

## Set working directory and load data ##
setwd("~/Desktop/Dissertation/miklas_mfa/mfa_data")
r_spec_data = read_csv("2026-09-30_r_spec_data.csv")

## Pre-processing of the data files ####
## Mutate the data to indicate each segment position in the previous or next segment columns, respectfully ##
r_spec_label = r_spec_data %>%
   group_by(Filename, Word_label, Interval) %>%
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

#save(r_spec_label, file = "r_spec_label.Rda", compress = "xz")
#load(file = "r_spec_label.Rda")

# Assessing average spectra by position for Turkish <r> ####
## Plot mean of all data ##
plot(unique(r_spec_label$bin), tapply(r_spec_label$amp, r_spec_label$bin, mean), type = "l")

## Genearlized Additive Models ####
## Fit a gam interaction btwn (bin = time) and position (within smooth (itsadug for plotting), position gets smooth) ##
m_all = bam(amp ~ s(bin, by = position), data=r_spec_label)
#save(m_all, file = "spectra_all.Rda", compress = "xz") #saves bam as a compressed file
#load(file = "spectra_all.rda")
acf_resid(m_all) # looks at autocorrelation residuals

## Variables for dealing with autocorrelation ##
rho = start_value_rho(m_all)
minbin = min(r_spec_label$bin)
r_spec_label$start.value = r_spec_label$bin == minbin

## Model that deals with autocorrelation issues in m_all ##
m_all_2 = bam(amp ~ s(bin, by = position), data=r_spec_label, rho = rho, AR.start = start.value)
#save(m_all_2, file = "spectra_all_2.Rda", compress = "xz") #saves bam as a compressed file
#load(file = "spectra_all_2.rda")
acf_resid(m_all_2) # looks at autocorrelation residuals

## Plots ####
## The following plots use the GAM/BAM model above and visualize using plot_smooth ##
my_colors = rainbow(7) # determines color palate for graphs

## Plots all positions at once, includes legend with labels in the dataframe ##
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", plot_all="position", col = my_colors, rug = FALSE, print.summary=FALSE)

## Plots one position at a time by adding the next line to the first plot, allows for using the legend function to change legend labels ##
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "WI_pau"),  col = my_colors[2], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "utt_final"), add = TRUE, col = my_colors[1], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "medial"), add = TRUE, col = my_colors[3], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "WF_pau"), add = TRUE, col = my_colors[4], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "WF"), add = TRUE, col = my_colors[5], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "WI"), add = TRUE, col = my_colors[6], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "utt_initial"), add = TRUE, col = my_colors[7], rug = FALSE, print.summary=FALSE)
legend(list(x=5750, y=24), lty= 1, cex = .75, col = my_colors[c(1, 4, 5, 3, 7, 2, 6)], legend = c("Utterance Final", "Word Final with pause", "Word Final", "Medial", "Utterance Initial", "Word Initial with pause", "Word Initial"))

## Plots only word-final, word-final with pause, and utterance final ##
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "WF"), col = my_colors[5], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "utt_final"), add = TRUE, col = my_colors[1], rug = FALSE, print.summary=FALSE)
plot_smooth(m_all_2, "bin", xlab = "Frequency (Hz)", ylab = "Intensity (dB)", cond = list(position = "WF_pau"), add = TRUE, col = my_colors[4], rug = FALSE, print.summary=FALSE)
legend(list(x=7450, y=15), lty= 1, cex = 0.5, col = my_colors[c(1, 4, 5)], legend = c("Utterance Final", "Word Final with pause", "Word Final"))

## Difference plots Pairwise comparison analysis for utterance final taps compared to every segment position##
plot_diff(m_all_2, view="bin", comp=list(position=c("utt_final", "WF_pau")), 
          xlab= "Frequency (Hz)", ylab="Intensity (dB)", ylim = c(-30,30), main = "")

plot_diff(m_all_2, view="bin", comp=list(position=c("utt_final", "WF")), 
          xlab= "Frequency (Hz)", ylab="Intensity (dB)", ylim = c(-30,30), main = "")

plot_diff(m_all_2, view="bin", comp=list(position=c("utt_final", "utt_initial")), 
          xlab= "Frequency (Hz)", ylab="Intensity (dB)", ylim = c(-30,30), main = "")

plot_diff(m_all_2, view="bin", comp=list(position=c("utt_final", "medial")), 
          xlab= "Frequency (Hz)", ylab="Intensity (dB)", ylim = c(-30,30), main = "")

plot_diff(m_all_2, view="bin", comp=list(position=c("utt_final", "WI")), 
          xlab= "Frequency (Hz)", ylab="Intensity (dB)", ylim = c(-30,30), main = "")

plot_diff(m_all_2, view="bin", comp=list(position=c("utt_final", "WI_pau")), 
          xlab= "Frequency (Hz)", ylab="Intensity (dB)", ylim = c(-30,30), main = "")



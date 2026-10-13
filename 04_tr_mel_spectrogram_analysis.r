# Plotting average mel filter bank for phonetic interpretation ####
# J.A.Miklas & M.C.Kelley #
# Last updated: 2026 Sept 30 #

## Load libraries ##
## Packages for data processing ##
library(tidyverse)
library(dplyr)
## Packages for plotting ##
library(ggplot2)
library(ggeffects)
library(patchwork)

## Set working directory and load data ##
setwd("~/Desktop/Dissertation/miklas_mfa/mfa_data")

r_mel_data = read_csv("2026-09-30_r_mel_data.csv")

## Pre-processing of the data files ####
## Mutate the data to indicate each segment position in the previous or next segment columns, respectfully ##
r_mel_label = r_mel_data %>%
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

#saved 2026-10-12
#save(r_mel_label, file = "r_mel_label.Rda", compress = "xz")
#load(file = "r_mel_label.Rda")


## Subset the spectral data to only include utterance final segments for plotting our average filter bank ##
r_utt_final = r_mel_label[r_mel_label$nex == "#UF#",]

uf_mfb = data.frame(
  mel = tapply(r_utt_final$mel, r_utt_final$bin, mean), 
  bin = unique(r_utt_final$bin)
)
uf_mfb$number = 1:nrow(uf_mfb)

uf_pivot = uf_mfb %>%
  pivot_wider(
    id_cols = -number, 
    names_from = bin,
    names_prefix =  "mel_",
    values_from = c(mel)
  )

uf_dup = uf_pivot %>%
  slice(rep(1, 4)) %>%
  bind_rows(uf_pivot, .) %>%
  mutate(
    Cosine = factor(c(1, 2, 3, 4, 5))
  )

uf_new = uf_dup %>%
  pivot_longer(
    cols = starts_with("mel_"),
    names_to = "bin",
    names_prefix = "mel_",
    values_to = "mel"
  )

uf_scale <- max(uf_mfb$mel)/2 

uf_new = uf_new %>%
  mutate(
    cos = cos(pi/31*(uf_mfb$number+0.5)*as.numeric(Cosine))*uf_scale + uf_scale,
    bin = as.numeric(bin)
  )


uf_plot = ggplot(uf_new) + 
  geom_segment(aes(x = bin, xend = bin, y = 0, yend = mel), color = "gray") +
  geom_point(aes(x = bin, y = mel), shape = 1, color = "darkgray")+
  geom_line(aes(x = bin, y = cos, color = Cosine, linetype = Cosine), linewidth = .75)+
  geom_point(aes(x = bin, y = cos, color = Cosine), size = 1.5)+
  labs(x = "Mel channel") +
  scale_x_continuous(breaks = c(0,10,20,30),
                     labels = c(0, uf_mfb$bin[c(10,20,30)]))+
  scale_y_continuous( 
    name = "Energy (dB)",
    sec.axis = sec_axis(~ . / uf_scale,
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = c(-1, -0.5, 0, 0.5, 1))
  ) +
  labs(title = "Utterance-Final") +
  theme(panel.grid = element_blank(),panel.background = element_blank())


## Subsets the mel spectrogram data to only include word final (not with pause) segments for plotting average filter bank ##
r_word_final = r_mel_label %>% filter(position == "WF")

wf_mfb = data.frame(
  mel = tapply(r_word_final$mel, r_word_final$bin, mean), 
  bin = unique(r_word_final$bin)
)
wf_mfb$number = 1:nrow(wf_mfb)
wf_pivot = wf_mfb %>%
  pivot_wider(
    id_cols = -number, 
    names_from = bin,
    names_prefix =  "mel_",
    values_from = c(mel)
  )

wf_dup = wf_pivot %>%
  slice(rep(1, 4)) %>%
  bind_rows(wf_pivot, .) %>%
  mutate(
    Cosine = factor(c(1, 2, 3, 4, 5))
  )


wf_new = wf_dup %>%
  pivot_longer(
    cols = starts_with("mel_"),
    names_to = "bin",
    names_prefix = "mel_",
    values_to = "mel"
  )

wf_scale <- max(wf_mfb$mel)/2

wf_new = wf_new %>%
  mutate(
    cos = cos(pi/31*(wf_mfb$number+0.5)*as.numeric(Cosine))*wf_scale + wf_scale,
    bin = as.numeric(bin)
  )

wf_plot = ggplot(wf_new) + 
  geom_segment(aes(x = bin, xend = bin, y = 0, yend = mel), color = "gray") +
  geom_point(aes(x = bin, y = mel), shape = 1, color = "darkgray")+
  geom_line(aes(x = bin, y = cos, color = Cosine, linetype = Cosine), linewidth = .75)+
  geom_point(aes(x = bin, y = cos, color = Cosine), size = 1.5)+
  labs(x = "Mel channel") +
  scale_x_continuous(breaks = c(0,10,20,30),
                     labels = c(0, wf_mfb$bin[c(10,20,30)]))+
  scale_y_continuous( 
    name = "Energy (dB)",
    sec.axis = sec_axis(~ . / wf_scale, 
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = c(-1, -0.5, 0, 0.5, 1))
  ) +
  labs(title = "Word-Final") +
  theme(panel.grid = element_blank(), panel.background = element_blank())


## Subsets the mel spectrogram data to only include word final with pause segments for plotting average filter bank ##
r_word_final_pause = r_mel_label %>% filter(position == "WF_pau")

wfp_mfb = data.frame(
  mel = tapply(r_word_final_pause$mel, r_word_final_pause$bin, mean), 
  bin = unique(r_word_final_pause$bin)
)
wfp_mfb$number = 1:nrow(wfp_mfb)
wfp_pivot = wfp_mfb %>%
  pivot_wider(
    id_cols = -number, 
    names_from = bin,
    names_prefix =  "mel_",
    values_from = c(mel)
  )

wfp_dup = wfp_pivot %>%
  slice(rep(1, 4)) %>%
  bind_rows(wfp_pivot, .) %>%
  mutate(
    Cosine = factor(c(1, 2, 3, 4, 5))
  )

wfp_new = wfp_dup %>%
  pivot_longer(
    cols = starts_with("mel_"),
    names_to = "bin",
    names_prefix = "mel_",
    values_to = "mel"
  )

wfp_scale <- max(wfp_mfb$mel)/2

wfp_new = wfp_new %>%
  mutate(
    cos = cos(pi/31*(wfp_mfb$number+0.5)*as.numeric(Cosine))*wfp_scale + wfp_scale,
    bin = as.numeric(bin)
  )

wfp_plot = ggplot(wfp_new) + 
  geom_segment(aes(x = bin, xend = bin, y = 0, yend = mel), color = "gray") +
  geom_point(aes(x = bin, y = mel), shape = 1, color = "darkgray")+
  geom_line(aes(x = bin, y = cos, color = Cosine, linetype = Cosine), linewidth = .75)+
  geom_point(aes(x = bin, y = cos, color = Cosine), size = 1.5)+
  labs(x = "Mel channel") +
  scale_x_continuous(breaks = c(0,10,20,30),
                     labels = c(0, wfp_mfb$bin[c(10,20,30)]))+
  scale_y_continuous( 
    name = "Energy (dB)",
    sec.axis = sec_axis(~ . / wfp_scale,
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = c(-1, -0.5, 0, 0.5, 1))
  ) +
  labs(title = "Word-Final with Pause") +
  theme(panel.grid = element_blank(), panel.background = element_blank())


## Subsets the mel spectrogram data to include medial segments for plotting average filter bank ##
r_med = r_mel_label %>% filter(position == "medial")

m_mfb = data.frame(
  mel = tapply(r_med$mel, r_med$bin, mean), 
  bin = unique(r_med$bin)
)
m_mfb$number = 1:nrow(m_mfb)

m_pivot = m_mfb %>%
  pivot_wider(
    id_cols = -number, 
    names_from = bin,
    names_prefix =  "mel_",
    values_from = c(mel)
  )

m_dup = m_pivot %>%
  slice(rep(1, 4)) %>%
  bind_rows(m_pivot, .) %>%
  mutate(
   Cosine = factor(c(1, 2, 3, 4, 5))
  )

m_new = m_dup %>%
  pivot_longer(
  cols = starts_with("mel_"),
  names_to = "bin",
  names_prefix = "mel_",
  values_to = "mel"
)

m_scale <- max(m_mfb$mel)/2

m_new = m_new %>%
  mutate(
    cos = cos(pi/31*(m_mfb$number+0.5)*as.numeric(Cosine))*m_scale + m_scale,
    bin = as.numeric(bin)
  )

m_plot = ggplot(m_new) + 
  geom_segment(aes(x = bin, xend = bin, y = 0, yend = mel), color = "gray") +
  geom_point(aes(x = bin, y = mel), shape = 1, color = "darkgray")+
  geom_line(aes(x = bin, y = cos, color = Cosine, linetype = Cosine), linewidth = .75)+
  geom_point(aes(x = bin, y = cos, color = Cosine), size = 1.5)+
  labs(x = "Mel channel") +
  scale_x_continuous(breaks = c(0,10,20,30),
                     labels = c(0, m_mfb$bin[c(10,20,30)]))+
  scale_y_continuous( 
    name = "Energy (dB)",
    sec.axis = sec_axis(~ . / m_scale,
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = c(-1, -0.5, 0, 0.5, 1))
  ) +
  labs(title = "Medial") +
  theme(panel.grid = element_blank(), panel.background = element_blank())


## Subsets the mel spectrogram data to include utterance initial segments for plotting average filter bank ##
r_utt_initial = r_mel_label[r_mel_label$prev == "#UI#",]

ui_mfb = data.frame(
  mel = tapply(r_utt_initial$mel, r_utt_initial$bin, mean), 
  bin = unique(r_utt_initial$bin)
)
ui_mfb$number = 1:nrow(ui_mfb)

ui_pivot = ui_mfb %>%
  pivot_wider(
    id_cols = -number, 
    names_from = bin,
    names_prefix =  "mel_",
    values_from = c(mel)
  )

ui_dup = ui_pivot %>%
  slice(rep(1, 4)) %>%
  bind_rows(ui_pivot, .) %>%
  mutate(
    Cosine = factor(c(1, 2, 3, 4, 5))
  )

ui_new = ui_dup %>%
  pivot_longer(
    cols = starts_with("mel_"),
    names_to = "bin",
    names_prefix = "mel_",
    values_to = "mel"
  )

ui_scale <- max(ui_mfb$mel)/2 

ui_new = ui_new %>%
  mutate(
    cos = cos(pi/31*(ui_mfb$number+0.5)*as.numeric(Cosine))*ui_scale + ui_scale,
    bin = as.numeric(bin)
  )

ui_plot = ggplot(ui_new) + 
  geom_segment(aes(x = bin, xend = bin, y = 0, yend = mel), color = "gray") +
  geom_point(aes(x = bin, y = mel), shape = 1, color = "darkgray")+
  geom_line(aes(x = bin, y = cos, color = Cosine, linetype = Cosine), linewidth = .75)+
  geom_point(aes(x = bin, y = cos, color = Cosine), size = 1.5)+
  labs(x = "Mel channel") +
  scale_x_continuous(breaks = c(0,10,20,30),
                     labels = c(0, ui_mfb$bin[c(10,20,30)]))+
  scale_y_continuous( 
    name = "Energy (dB)",
    sec.axis = sec_axis(~ . / ui_scale,
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = c(-1, -0.5, 0, 0.5, 1))
  ) +
  labs(title = "Utterance-Initial") +
  theme(panel.grid = element_blank(),panel.background = element_blank())

## Subsets the mel spectrogram data to include word initial segments for plotting average filter bank ##
r_wd_initial = r_mel_label %>% filter(position == "WI")

wi_mfb = data.frame(
  mel = tapply(r_wd_initial$mel, r_wd_initial$bin, mean), 
  bin = unique(r_wd_initial$bin)
)
wi_mfb$number = 1:nrow(wi_mfb)

wi_pivot = wi_mfb %>%
  pivot_wider(
    id_cols = -number, 
    names_from = bin,
    names_prefix =  "mel_",
    values_from = c(mel)
  )

wi_dup = wi_pivot %>%
  slice(rep(1, 4)) %>%
  bind_rows(wi_pivot, .) %>%
  mutate(
    Cosine = factor(c(1, 2, 3, 4, 5))
  )

wi_new = wi_dup %>%
  pivot_longer(
    cols = starts_with("mel_"),
    names_to = "bin",
    names_prefix = "mel_",
    values_to = "mel"
  )

wi_scale <- max(wi_mfb$mel)/2 

wi_new = wi_new %>%
  mutate(
    cos = cos(pi/31*(wi_mfb$number+0.5)*as.numeric(Cosine))*wi_scale + wi_scale,
    bin = as.numeric(bin)
  )

wi_plot = ggplot(wi_new) + 
  geom_segment(aes(x = bin, xend = bin, y = 0, yend = mel), color = "gray") +
  geom_point(aes(x = bin, y = mel), shape = 1, color = "darkgray")+
  geom_line(aes(x = bin, y = cos, color = Cosine, linetype = Cosine), linewidth = .75)+
  geom_point(aes(x = bin, y = cos, color = Cosine), size = 1.5)+
  labs(x = "Mel channel") +
  scale_x_continuous(breaks = c(0,10,20,30),
                     labels = c(0, wi_mfb$bin[c(10,20,30)]))+
  scale_y_continuous( 
    name = "Energy (dB)",
    sec.axis = sec_axis(~ . / wi_scale,
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = c(-1, -0.5, 0, 0.5, 1))
  ) +
  labs(title = "Word-Initial") +
  theme(panel.grid = element_blank(),panel.background = element_blank())

## Subsets the mel spectrogram data to include word-initial with pause segments for plotting average filter bank ##
r_wdp_initial = r_mel_label %>% filter(position == "WI_pau")

wip_mfb = data.frame(
  mel = tapply(r_wdp_initial$mel, r_wdp_initial$bin, mean), 
  bin = unique(r_wdp_initial$bin)
)
wip_mfb$number = 1:nrow(wip_mfb)

wip_pivot = wip_mfb %>%
  pivot_wider(
    id_cols = -number, 
    names_from = bin,
    names_prefix =  "mel_",
    values_from = c(mel)
  )

wip_dup = wip_pivot %>%
  slice(rep(1, 4)) %>%
  bind_rows(wip_pivot, .) %>%
  mutate(
    Cosine = factor(c(1, 2, 3, 4, 5))
  )

wip_new = wip_dup %>%
  pivot_longer(
    cols = starts_with("mel_"),
    names_to = "bin",
    names_prefix = "mel_",
    values_to = "mel"
  )

wip_scale <- max(wip_mfb$mel)/2 

wip_new = wip_new %>%
  mutate(
    cos = cos(pi/31*(wip_mfb$number+0.5)*as.numeric(Cosine))*wip_scale + wip_scale,
    bin = as.numeric(bin)
  )

wip_plot = ggplot(wip_new) + 
  geom_segment(aes(x = bin, xend = bin, y = 0, yend = mel), color = "gray") +
  geom_point(aes(x = bin, y = mel), shape = 1, color = "darkgray")+
  geom_line(aes(x = bin, y = cos, color = Cosine, linetype = Cosine), linewidth = .75)+
  geom_point(aes(x = bin, y = cos, color = Cosine), size = 1.5)+
  labs(x = "Mel channel") +
  scale_x_continuous(breaks = c(0,10,20,30),
                     labels = c(0, wip_mfb$bin[c(10,20,30)]))+
  scale_y_continuous( 
    name = "Energy (dB)",
    sec.axis = sec_axis(~ . / wip_scale,
                        breaks = c(0, 0.5, 1, 1.5, 2), 
                        labels = c(-1, -0.5, 0, 0.5, 1))
  ) +
  labs(title = "Word-Initial with Pause") +
  theme(panel.grid = element_blank(),panel.background = element_blank())

## Combines all of the plots into one patchwork plot with a single key ##
layout = '
1111#2222
3333#4444
5555#6666
7777#####
'
ui_plot+
  wi_plot+
  wip_plot+
  m_plot +
  wf_plot +
  wfp_plot +
  uf_plot +
  plot_layout(
    design = layout,
    guides = 'collect'
  )+
  plot_annotation(
    tag_levels = 'a',
    tag_prefix = '(',
    tag_suffix = ')'
  ) +
  theme(legend.position = 'none') &
  theme(plot.tag.position = c(0,1))

# Turkish Data Labeling for MFCC and Spectrum Analyses ####
# J.A.Miklas & M.C.Kelley #
# Last updated: 2026 June 02 #

## Call libraries ##
library(dplyr)
library(tidyverse)
library(readr)

# Define filepath for raw data #
setwd("~/Desktop/Dissertation/miklas_mfa/mfa_data")

#### PART 1 to label spectrum data ####
## Upload Data files from folder path for Spectrum data (from 01_spectrum_analysis.praat) ##
#test_data = read_csv("tr_spectrumresults/common_voice_tr_17341269.csv", col_type = "cccnnnnnnn", locale = locale(encoding = "UTF-8"), na = "na")

spec_data = 
  list.files(path = "tr_spectrumresults",  #lists all the csv files in the specified folder
             pattern = "*.csv",       #change csv here if your files have a different extension
             full.names = TRUE) %>%   #appends full file path
  map_df(~read_csv(., col_type = "cccnnnnnnn", locale = locale(encoding = "UTF-8"), na = "na"))  #map_df applies a function to each element of a list or vector

## Pivot the data wider to have 1 row per segment ##
pivot_spec = spec_data %>%
  pivot_wider(
    id_cols = -int, 
    names_from = bin,
    names_prefix =  "amp_",
    values_from = c(amp)
  )

## Make the utterance boundary and pause labels ##
spec_utt_label = pivot_spec %>% 
  group_by(Filename) %>% #groups by Filename to indicate utterance boundaries (each file is one sentence)
  slice_head(n = -1) %>% #removes the first row of that file name which in the XPF TextGrids is a blank boundary
  slice_tail(n = -1) %>% #removes the last row of that file name which again is a blank boundary
  mutate(
    prev = lag(Segment_label, n=1), #writes the lag row segment label
    prev2 = lag(Segment_label, n=2), #writes the segment label 2 rows previous
    nex = lead(Segment_label, n=1), #writes the lead row segment label
    nex2 = lead(Segment_label, n=2), #writes the segment label 2 rows next
    prev = ifelse(is.na(prev), "#UI#", prev), #indicates utterance initial when the prev column has an NA (Filename boundary); labels #UI#
    nex = ifelse(is.na(nex), "#UF#", nex), #indicates utterance final when the nex column has an NA (Filename boundary); labels #UF#
    prev = ifelse(prev == "", "#PAU#", prev), #remaining blanks within the prev column indicate a pause before the segment; labels #PAU#
    nex = ifelse(nex == "", "#PAU#", nex), #remaining blanks within the nex column indicate a pause after the segment; labels #PAU#
    prev_dur = lag(dur, n=1),       #writes the lag duration; will be used to look at pause duration before the segment
    nex_dur = lead(dur, n=1)       #writes the lead duration; will be used to look at pause duration after the segment
  ) %>%
  relocate(prev, .after = Segment_label) %>%
  relocate(nex, .after = prev)

#Make word boundary labels
spec_word_label = spec_utt_label %>% 
  group_by(Filename, Word_label, Interval) %>% #groups by Word_label to indicate word boundaries
  mutate(
    wd_prev = lag(Segment_label, n=1), #writes the lag row segment label
    wd_nex = lead(Segment_label, n=1), #writes the lead row segment label
    wd_prev = ifelse(is.na(wd_prev), "#WI#", prev), #indicates word initial when the prev column has NA (word boundary); labels #WI#
    wd_nex = ifelse(is.na(wd_nex), "#WF#", nex) #indicates word final when the nex column has NA (word boundary); labels #WF#
  )%>%
  relocate(wd_prev, .after = nex) %>%
  relocate(wd_nex, .after = wd_prev)

#Pivot the data longer to have 110 rows per each segment
pivot_all_spec = spec_word_label %>%
  pivot_longer(
    cols = starts_with("amp_"),
    names_to = "bin",
    names_prefix = "amp_",
    values_to = "amp"
  )

#Subset to only include data if the Segment_label column has ɾ ɾː or r 
r_spec_data = pivot_all_spec %>% filter(Segment_label %in% c("ɾ", "ɾː", "r"))

#Write r_spec_data as a csv
write_csv(r_spec_data, "2026-09-30_r_spec_data.csv")

#### PART 2 to label MFCC data ####
## Upload Data files from folder path for MFCC data (from 02_MFCC_analysis.praat)
#test_data = read_csv("test_MFCCresults/common_voice_tr_17341269.csv", locale = locale(encoding = "UTF-8"), na = "na")

MFCC_data <- 
  list.files(path = "tr_MFCCresults",  #lists all the csv files in the specified folder
             pattern = "*.csv",       #change csv here if your files have a different extension
             full.names = TRUE) %>%   #appends full file path
  map_df(~read_csv(., col_type = "cccnnnnnnnnnnnnnn", locale = locale(encoding = "UTF-8"), na = "na"))  #map_df applies a function to each element of a list or vector


#Make the utterance boundary and pause labels
MFCC_utt_label = MFCC_data %>% 
  group_by(Filename) %>% #groups by Filename to indicate utterance boundaries (each file is one sentence)
  slice_head(n = -1) %>% #removes the first row of that file name which in the XPF TextGrids is a blank boundary
  slice_tail(n = -1) %>% #removes the last row of that file name which again is a blank boundary
  mutate(
    prev = lag(Segment_label, n=1), #writes the lag row segment label
    nex = lead(Segment_label, n=1), #writes the lead row segment label
    prev_dur = lag(dur, n=1),       #writes the lag duration; will be used to look at pause duration before the segment
    nex_dur = lead(dur, n=1),       #writes the lead duration; will be used to look at pause duration after the segment
    prev = ifelse(is.na(prev), "#UI#", prev), #indicates utterance initial when the prev column has an NA (Filename boundary); labels #UI#
    nex = ifelse(is.na(nex), "#UF#", nex), #indicates utterance final when the nex column has an NA (Filename boundary); labels #UF#
    prev = ifelse(prev == "", "#PAU#", prev), #remaining blanks within the prev column indicate a pause before the segment; labels #PAU#
    nex = ifelse(nex == "", "#PAU#", nex) #remaining blanks within the nex column indicate a pause after the segment; labels #PAU#
    )%>%
  relocate(prev, .after = Segment_label) %>%
  relocate(nex, .after = prev)

#Make word boundary labels
MFCC_word_label = MFCC_utt_label %>% 
  group_by(Filename, Word_label, Interval) %>% #groups by Word_label to indicate word boundaries
  mutate(
    wd_prev = lag(Segment_label, n=1), #writes the lag row segment label
    wd_nex = lead(Segment_label, n=1), #writes the lead row segment label
    wd_prev = ifelse(is.na(wd_prev), "#WI#", prev), #indicates word initial when the prev column has NA (word boundary); labels #WI#
    wd_nex = ifelse(is.na(wd_nex), "#WF#", nex) #indicates word final when the nex column has NA (word boundary); labels #WF#
  )%>%
  relocate(wd_prev, .after = nex) %>%
  relocate(wd_nex, .after = wd_prev)

#Subset to only include data if the Segment_label column has ɾ ɾː or r 
r_MFCC_data = MFCC_word_label %>% filter(Segment_label %in% c("ɾ", "ɾː", "r"))

r_MFCC_data %>%
  group_by(Segment_label) %>%
  summarise(ntotal = n())

# A tibble: 2 × 2
# Segment_label ntotal
# <chr>          <int>
# 1 ɾ             214873
# 2 ɾː               174

#Write r_data as a csv
write_csv(r_MFCC_data, "2026-09-30_r_MFCC_data.csv")

#### PART 3 to label mel spectrogram data ####
## Upload Data files from folder path for mel spectrogram data (from 03_melspectrogram_analysis.praat)
#test_data = read_csv("tr_melspectrogramresults/common_voice_tr_17342172.csv", col_type = "cccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn", locale = locale(encoding = "UTF-8"), na = "na")

mel_data = 
  list.files(path = "tr_melspectrogramresults",  #lists all the csv files in the specified folder
             pattern = "*.csv",       #change csv here if your files have a different extension
             full.names = TRUE)[1:100] %>%   #appends full file path
  map_df(~read_csv(., col_type = "cccnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn", locale = locale(encoding = "UTF-8"), na = "na"))  #map_df applies a function to each element of a list or vector

#Make the utterance boundary and pause labels
mel_utt_label = mel_data %>% 
  group_by(Filename) %>% #groups by Filename to indicate utterance boundaries (each file is one sentence)
  slice_head(n = -1) %>% #removes the first row of that file name which in the XPF TextGrids is a blank boundary
  slice_tail(n = -2) %>% #removes the last 2 rows of that file name which again is a blank boundary and the extra blank row
  mutate(
    prev = lag(Segment_label, n=1), #writes the lag row segment label
    prev2 = lag(Segment_label, n=2), #writes the segment label 2 rows previous
    nex = lead(Segment_label, n=1), #writes the lead row segment label
    nex2 = lead(Segment_label, n=2), #writes the segment label 2 rows next
    prev = ifelse(is.na(prev), "#UI#", prev), #indicates utterance initial when the prev column has an NA (Filename boundary); labels #UI#
    nex = ifelse(is.na(nex), "#UF#", nex), #indicates utterance final when the nex column has an NA (Filename boundary); labels #UF#
    prev = ifelse(prev == "", "#PAU#", prev), #remaining blanks within the prev column indicate a pause before the segment; labels #PAU#
    nex = ifelse(nex == "", "#PAU#", nex), #remaining blanks within the nex column indicate a pause after the segment; labels #PAU#
    prev_dur = lag(dur, n=1),       #writes the lag duration; will be used to look at pause duration before the segment
    nex_dur = lead(dur, n=1)       #writes the lead duration; will be used to look at pause duration after the segment
  ) %>%
  relocate(prev, .after = Segment_label) %>%
  relocate(nex, .after = prev)

#Make word boundary labels
mel_word_label = mel_utt_label %>% 
  group_by(Filename, Word_label, Interval) %>% #groups by Word_label to indicate word boundaries
  mutate(
    wd_prev = lag(Segment_label, n=1), #writes the lag row segment label
    wd_nex = lead(Segment_label, n=1), #writes the lead row segment label
    wd_prev = ifelse(is.na(wd_prev), "#WI#", prev), #indicates word initial when the prev column has NA (word boundary); labels #WI#
    wd_nex = ifelse(is.na(wd_nex), "#WF#", nex) #indicates word final when the nex column has NA (word boundary); labels #WF#
  )%>%
  relocate(wd_prev, .after = nex) %>%
  relocate(wd_nex, .after = wd_prev)

#Pivot the data longer to have 31 rows per each segment
pivot_all_mel = mel_word_label %>%
  pivot_longer(
    cols = starts_with("bin_"),
    names_to = "bin",
    names_prefix = "bin_",
    values_to = "mel"
  )

#Subset to only include data if the Segment_label column has ɾ ɾː or r 
r_mel_data = pivot_all_mel %>% filter(Segment_label %in% c("ɾ", "ɾː", "r"))

#Write r_mel_data as a csv
write_csv(r_mel_data, "2026-09-30_r_mel_data.csv")



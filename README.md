# Turkish_rhotic_mfccs
This repository contains the Praat and R scripts used for analyzing MFCCs for the rhotic in Turkish.

To extract the acoustic measurements for analysis, please use the files with the file extention .praat

  * 01_spectrum_analysis.praat extracts long-term average spectra measurements
  * 02_MFCC_analysis.praat extracts mel frequency cepstral coefficient measurements
  * 03_melspectrogram_analysis.praat extracts mel spectrogram measurements, reflective of how MFCCs were calculated in 02_MFCC_analysis.praat
  
For data cleaning and analysis, please use the files with the file extension .r

  * 01_tr_data_labeling.r labels and subsets the data extracted from Praat
  * 02_tr_spectra_analysis.r analyzes the long-term average spectrum data
  * 03_tr_MFCC_analysis.r analyzes the MFCC data
  * 04_tr_mel_spectrogram_analysis.r analyzes the mel spectrogram data

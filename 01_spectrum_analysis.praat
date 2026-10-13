# Extracts amplitude values in spectrum. The user specifies the size of each averaged spectrum amplitude bin,
# e.g. bins of 100 Hz. or 50 Hz., etc. Spectra are calculated dynamically across the duration defined by the textgrid. 
# The number of interval values extracted is equal to numintervals below.
# Writes results to a textfile.
# Christian DiCanio, 2010.
# Revised in 2024 to handle aperiodic speech sounds - extracting entire spectral envelopes in long format
# with filtering.
# Modified by J.A.Miklas & M.C.Kelley, 2026


form data from labelled points
	comment Directory of sound files
	text sound_directory my_corpus
	sentence Sound_file_extension .mp3
	comment Directory of TextGrid files
	text textGrid_directory tr_xpf_textgrids
	sentence TextGrid_file_extension .TextGrid
	positive Analysis_points_time_step 0.005
	positive Record_with_precision 1
	positive Bin_size 100
	positive Resampling_value 16000
	positive Low_cutoff 300
	positive High_cutoff 16000
	positive Filter_smoothing 100
	comment Full path of the resulting text file:
	text resultfile tr_spectrumresults
endform

# If your sound files are in a different format, you can insert that format instead of wav below.

Create Strings as file list: "list", sound_directory$ + "/*" + sound_file_extension$
numberOfFiles = Get number of strings

# Write a row with column titles to the result file:

titleline$ = "Filename,Word_label,Segment_label,Interval,start,end,dur,int,bin,amp'newline$'"

#Go through all the sound files, one by one:

for ifile to numberOfFiles
#for ifile to 100
	filename$ = Get string: ifile
	tablename$ = resultfile$ + "/" + replace$(filename$, "mp3", "csv", 0)
	if not fileReadable (tablename$)
		fileappend "'tablename$'" 'titleline$'
		#A sound file is opened from the listing:
		s_og = Read from file: sound_directory$ + "/" + filename$
		soundname$ = selected$ ("Sound", 1)	
		s_re = Resample: resampling_value, 50
		s_fi = Filter (pass Hann band): low_cutoff, high_cutoff, filter_smoothing
		removeObject(s_og)
		selectObject(s_re)
		#Here, we add all the repeated measures and information we want for every file
		soundres$ = selected$ ("Sound", 1)
		gridfile$ = textGrid_directory$ + "/" + soundname$ + textGrid_file_extension$
		writeInfoLine: gridfile$
		a = fileReadable (gridfile$)
		appendInfoLine: a
		if fileReadable (gridfile$)
			tg = Read from file: gridfile$
			tier = 2
			numberOfIntervals = Get number of intervals: tier
			# Pass through all intervals in the selected tier:
			for interval from 1 to numberOfIntervals
				selectObject(tg)
				label$ = Get label of interval: tier, interval
				# Get the start and end for every interval:
				start = Get start time of interval: tier, interval
				wordintnum = Get interval at time: 1, start
				word$ = Get label of interval: 1, wordintnum
				wordstart = Get start time of interval: 1, wordintnum
				wordend = Get end time of interval: 1, wordintnum
				rnum = Get high interval at time: 1, start
				end = Get end time of interval: tier, interval
				dur = end-start				
				start = start - 0.025
				end = end + 0.025
				
				selectObject(s_fi)
				part = Extract part: start, end, "rectangular", 1.0, "no"
				To Spectrum: "yes"
				spect = selected("Spectrum")
				lt = To Ltas: bin_size
				To Matrix
				mat = selected("Matrix")
				select mat
				colnum = Get number of columns
				for k to colnum
					val = Get value in cell: 1, k
					bin = k*bin_size
					# Save result to csv file:
					resultline$ = "'soundres$','word$','label$','wordintnum','start','end','dur','k','bin','val''newline$'"
					fileappend "'tablename$'" 'resultline$'	
				endfor
				removeObject(part, mat, lt, spect)
			endfor
			removeObject(tg)	
		endif
		removeObject(s_fi, s_re)		
		select Strings list
		# and go on with the next sound file!
	endif
endfor
select all
Remove
writeInfoLine: "Done! :)"
#Script takes a sound file and TextGrid and iterates through each sound segment
#Resamples the sound file then Analyzes to MelSpectrogram 
#Output of the Matrix with the 26 Mel filter bank values and segment duration
#Saved as csv for each file into a specified folder

# Modified by J.A.Miklas & M.C.Kelley, 2026


form data from labeled points
	comment Directory of sound files
	text sound_directory my_corpus
	sentence Sound_file_extension .mp3
	comment Directory of TextGrid files
	text textGrid_directory tr_xpf_textgrids
	sentence TextGrid_file_extension .TextGrid
	positive Analysis_points_time_step 0.01
	positive Record_with_precision 1
	positive Resampling_value 16000
	comment Full path of the resulting text file:
	text resultfile tr_melspectrogramresults
endform

# If your sound files are in a different format, you can insert that format instead of wav below.

Create Strings as file list: "list", sound_directory$ + "/*" + sound_file_extension$
numberOfFiles = Get number of strings

# Write a row with column titles to the result file:
bin$ = ""
maxbins = 31
for i from 1 to maxbins
	bin$ = bin$ + "bin_" + string$(i)
	if i < maxbins
		bin$ = bin$ + ","
	endif
endfor
titleline$ = "Filename,Word_label,Segment_label,Interval,start,end,dur,'bin$''newline$'"

#Go through all the sound files, one by one:

for ifile to numberOfFiles
#for ifile to 5
	filename$ = Get string: ifile
	tablename$ = resultfile$ + "/" + replace$(filename$, "mp3", "csv", 0)
	if not fileReadable (tablename$)
		fileappend "'tablename$'" 'titleline$'
		#A sound file is opened from the listing:
		s_og = Read from file: sound_directory$ + "/" + filename$
		soundname$ = selected$ ("Sound", 1)	
		s_re = Resample: resampling_value, 50
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
				
				selectObject(s_re)
				part = Extract part: start, end, "rectangular", 1.0, "no"
				mel = noprogress To MelSpectrogram: 0.025, 0.01, 100.0, 100.0, 0.0
				nframes = Get number of frames
				midpoint = floor(nframes / 3)
				if midpoint = 0
					midpoint = 1
				endif
				mat = To Matrix: "yes"
				selected("Matrix")
				vec# = Get all values in column: midpoint
					val$ = ""
					for j from 1 to size (vec#)
						val$ = val$ + string$(vec#[j]) 
						if j < size(vec#)
							val$ = val$ + ","
						endif
					endfor
					# Save result to csv file:
					resultline$ = "'soundres$','word$','label$','wordintnum','start','end','dur','val$''newline$'"
					fileappend "'tablename$'" 'resultline$'	
				removeObject(part, mel, mat)
			endfor
			removeObject(tg)	
		endif
		removeObject(s_re)		
		select Strings list
		# and go on with the next sound file!
	endif
endfor
select all
Remove
writeInfoLine: "Done! :)"
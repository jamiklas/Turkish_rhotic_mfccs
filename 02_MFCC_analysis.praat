# This script goes through sound and TextGrid files in a directory,
# opens each pair of Sound and TextGrid, calculates the formant values
# at the midpoint of each labeled interval, and saves results to a text file.
# To make some other or additional analyses, you can modify the script
# yourself... it should be reasonably well commented! ;)
#
# This script is distributed under the GNU General Public License.
# Copyright 4.7.2003 Mietta Lennes
# Modified by J.A.Miklas & M.C.Kelley, 2026

form Analyze cepstrum from labeled segments in files
	comment Directory of sound files
	text sound_directory my_corpus
	sentence Sound_file_extension .mp3
	comment Directory of TextGrid files
	text textGrid_directory tr_xpf_textgrids
	sentence TextGrid_file_extension .TextGrid
	comment Full path of the resulting text file:
	text resultfile tr_MFCCresults
endform

# Here, you make a listing of all the sound files in a directory.
# The example gets file names ending with ".wav" from D:\tmp\

Create Strings as file list: "list", sound_directory$ + "/*" + sound_file_extension$
numberOfFiles = Get number of strings


# Write a row with column titles to the result file:
# (remember to edit this if you add or change the analyses!)

titleline$ = "Filename,Word_label,Segment_label,Interval,MFCC1,MFCC2,MFCC3,MFCC4,MFCC5,MFCC6,MFCC7,MFCC8,MFCC9,MFCC10,MFCC11,MFCC12,dur'newline$'"

# Go through all the sound files, one by one:

for ifile to numberOfFiles
	filename$ = Get string: ifile
	tablename$ = resultfile$ + "/" + replace$(filename$, "mp3", "csv", 0)
	if not fileReadable (tablename$)
		fileappend "'tablename$'" 'titleline$'
		# A sound file is opened from the listing:
		s_og= Read from file: sound_directory$ + "/" + filename$	
		soundname$ = selected$ ("Sound", 1)
		s_re = Resample: 16000, 50	
		removeObject(s_og)
		selectObject(s_re)
		# Starting from here, you can add everything that should be 
		# repeated for every sound file that was opened:
		soundres$ = selected$ ("Sound", 1)
		# Open a TextGrid by the same name:
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
				label$ = Get label of interval: tier, interval
					# Get start and end for every interval:
					start = Get start time of interval: tier, interval
					wordintnum = Get interval at time: 1, start
					word$ = Get label of interval: 1, wordintnum
					wordstart = Get start time of interval: 1, wordintnum
					wordend = Get end time of interval: 1, wordintnum
					rnum = Get high interval at time: 1, start
					end = Get end time of interval: tier, interval
					dur = end - start				
					start = start - 0.025
					end = end + 0.025			
					
					selectObject("Sound " + soundres$)
					part = Extract part: start, end, "rectangular", 1.0, "no"
					#get MFCC
					noprogress To MFCC: 12, 0.025, 0.01, 100, 100, 0
					nframes = Get number of frames
					midpoint = floor(nframes / 3)
					if midpoint = 0
						midpoint = 1
					endif
					mfcc1 = Get value in frame: midpoint, 1
					#pauseScript: "c1: 'mfcc1', 'start', 'end'"
					mfcc2 = Get value in frame: midpoint, 2
					mfcc3 = Get value in frame: midpoint, 3
					mfcc4 = Get value in frame: midpoint, 4
					mfcc5 = Get value in frame: midpoint, 5
					mfcc6 = Get value in frame: midpoint, 6
					mfcc7 = Get value in frame: midpoint, 7
					mfcc8 = Get value in frame: midpoint, 8
					mfcc9 = Get value in frame: midpoint, 9
					mfcc10 = Get value in frame: midpoint, 10
					mfcc11 = Get value in frame: midpoint, 11
					mfcc12 = Get value in frame: midpoint, 12
					Remove
					# Save result to text file:
					resultline$ = "'soundname$','word$','label$','wordintnum','mfcc1','mfcc2','mfcc3','mfcc4','mfcc5','mfcc6','mfcc7','mfcc8','mfcc9','mfcc10','mfcc11','mfcc12','dur''newline$'"
					fileappend "'tablename$'" 'resultline$'
					removeObject(part)
					selectObject(tg)
				endfor
				# Remove the TextGrid object from the object list
				removeObject(tg)
			endif
			# Remove the temporary objects from the object list
			select Sound 'soundres$'
			Remove
			select Strings list
			# and go on with the next sound file!
	endif
endfor

Remove

writeInfoLine: "Done! :)"


#-------------
# This procedure finds the number of a tier that has a given label.

procedure GetTier name$ variable$
        numberOfTiers = Get number of tiers
        itier = 1
        repeat
                tier$ = Get tier name... itier
                itier = itier + 1
        until tier$ = name$ or itier > numberOfTiers
        if tier$ <> name$
                'variable$' = 0
        else
                'variable$' = itier - 1
        endif

	if 'variable$' = 0
		exit The tier called 'name$' is missing from the file 'soundname$'!
	endif

endproc

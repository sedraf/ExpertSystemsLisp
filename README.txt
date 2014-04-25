This is  Tetsuya Idota's, Hafiza Farzami's, and Ross Hoyer's solution to the Expert Systems problem.

Compilation or Compilation like activity:
	In CLISP command is (load 'es)

Running or run like activity:
	to run the program (carreers 'linda) 
		where linda can be any student represented in the student file
	after a successfull run you can use the following functions
		(how '(job = research)) where job and reasearch can be any speciallty and sub specialty
			such as '(location = moutain-state), '(reaserch = ai-research), etc., etc. 
			This returns a list of rules used to get this outcome
		(why '(degree = phd)) where degree and phd can be any specialty and sub specialty
			this returns a list of objectives found out using this euqality
	(fgetframe 'linda) where linda can be any student represented in the student file
		this returns the frame at this point of the program if used before (carreers 'linda) it will return
		the things we know from linda from the student file
		if used after it will include the information found out about linda
	This program requires 2 files to be in the same directory they are:
		students  which holds student information
		rulbase which holes all the rules
	

			
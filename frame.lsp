#|
                ***** FRAMES.LSP *****
This is a copy of Dr. Weiss's FRAMES.LSP with a couple of new functions for this use of it

Frame-based knowledge representation routines from Chapter 22 of Winston and Horn LISP text
(1st edition, Addison-Wesley, 1981). Frames are stored as associations lists on symbol property lists.
Value facets are stored as (value mb md), where mb and md are MYCIN measures of belief and disbelief.

Usage:
Code runs automatically upon loading.
WEISS FUNCTIONS
	(fget frame slot facet)			- retrieve SLOT/FACET information from FRAME
	(fput frame slot facet value)		- add SLOT/FACET/VALUE to FRAME
	(fremove frame slot facet value)	- remove SLOT/FACET/VALUE information from FRAME
	(fgetframe frame)			- return the entire FRAME info
OUR FUNCTIONS
	(fputproved proved student)   - add the facts PROVED to the FRAME of STUDENT
	(fclearframe student)	- NOT USED but usage is to delete all info about a single student


Modifications (John M. Weiss, Ph.D.)
11/25/98 - Removed interactive queries for more info.
04/04/07 - More code cleanups.
|#

; FGET retrieves SLOT/FACET information from the FRAME.
(defun fget (frame slot facet)
    (mapcar #'car
        (cdr (assoc facet
            (cdr (assoc slot
                (cdr (get frame 'frame))
            ))
        ))
    )
)

; FPUT adds SLOT/FACET/VALUE to the FRAME if not already present.
(defun fput (frame slot facet value)
    (cond
        ; value is list (VALUE MB MD), so need EQUAL test
        ((member value (fget frame slot facet) :test #'equal) nil)
        (t (fassoc value (fassoc facet (fassoc slot (fgetframe frame)))) value)
    )
)

; FGETFRAME is called by FPUT to return the FRAME if it already exists,
; or create the FRAME if it does not.
(defun fgetframe (frame)
    (cond
        ((get frame 'frame))
        (t (setf (get frame 'frame) (list frame)))
    )
)

; FASSOC is called by FPUT to build frames. It acts like ASSOC when KEY
; is found on A-LIST, otherwise it appends KEY to the end of A-LIST.
(defun fassoc (key a-list)
    (cond
        ((assoc key (cdr a-list)))
        (t (cadr (rplacd (last a-list) (list (list key)))))
    )
)

; FREMOVE removes SLOT/FACET/VALUE information from the FRAME.
(defun fremove (frame slot facet value)
    (let (slots facets values target)
        (setq slots (fgetframe frame))
        (setq facets (assoc slot (cdr slots)))
        (setq values (assoc facet (cdr facets)))
        ; value is list (VALUE MB MD), so need EQUAL test
        (setq target (assoc value (cdr values) :test #'equal))
        (delete target values)
        (when (null (cdr values)) (delete values facets))
        (when (null (cdr facets)) (delete facets slots))
        (not (null target))     ; return T if removed something, else NIL
    )
)

;Adds proved facts to student frame
(defun fputproved (proved student)
	(let (frame slot tem2 ) 
		;For each fact proved add that to the frame
		(dolist (rule proved)
				;Student Frame
				(setq frame student)
				;what the n part is eg n = y
				(setq slot (nth 0 rule))
				;the y part minus the cf 
				(setq temp2 (caaadr (nth 1 rule)))
				;the y part plus the cf
				(setq value (cons temp2 (cdaadr  (nth 1 rule))))				
				(fput frame slot 'value value)
		)		
	)
)

;NOT USED it clears all info from a student
(defun fclearframe(student)
	(let (frame slot value)
	
	;Go through all slots of the student
	(dolist (temp (cdr (fgetframe student)))
		;set frame student
		(setq frame student)	
		;set slot
		(setq slot  (car temp))
		;for each value in that slot
		(dolist (value (cadadr temp)))
			(setq value (car value))
			(fremove frame slot 'value value)
		)
	)	
)

#|
;=============================================================
; Test the routines:

(dribble "frames.log")

(format t "(fput 'henry 'hobby 'value '(skiing 1 0)) = ~a~%"
    (fput 'henry 'hobby 'value '(skiing 1 0)))
(format t "(fgetframe 'henry) = ~a~%" (fgetframe 'henry))

(format t "(fput 'henry 'hobby 'value '(surfing 1 0)) = ~a~%"
    (fput 'henry 'hobby 'value '(surfing 1 0)))
(format t "(fgetframe 'henry) = ~a~%" (fgetframe 'henry))

; make sure FPUT doesn't add same info twice
(format t "(fput 'henry 'hobby 'value '(skiing 1 0)) = ~a~%"
    (fput 'henry 'hobby 'value '(skiing 1 0)))
(format t "(fgetframe 'henry) = ~a~%" (fgetframe 'henry))

(format t "(fput 'henry 'gpa 'value '(3.6 1 0)) = ~a~%"
    (fput 'henry 'gpa 'value '(3.6 1 0)))
(format t "(fgetframe 'henry) = ~a~%" (fgetframe 'henry))

(format t "(fremove 'henry 'hobby 'value '(skiing 1 0)) = ~a~%"
    (fremove 'henry 'hobby 'value '(skiing 1 0)))
(format t "(fgetframe 'henry) = ~a~%" (fgetframe 'henry))

(format t "(fremove 'henry 'hobby 'value '(surfing 1 0)) = ~a~%"
    (fremove 'henry 'hobby 'value '(surfing 1 0)))
(format t "(fgetframe 'henry) = ~a~%" (fgetframe 'henry))

(format t "(fremove 'henry 'gpa 'value '(3.6 1 0)) = ~a~%"
    (fremove 'henry 'gpa 'value '(3.6 1 0)))
(format t "(fgetframe 'henry) = ~a~%" (fgetframe 'henry))

(dribble)

=============================================================

FRAMES.OUT
----------
(fput 'henry 'hobby 'value '(skiing 1 0)) = (SKIING 1 0)

(fgetframe 'henry) = (HENRY (HOBBY (VALUE ((SKIING 1 0)))))

(fput 'henry 'hobby 'value '(surfing 1 0)) = (SURFING 1 0)

(fgetframe 'henry) = (HENRY (HOBBY (VALUE ((SKIING 1 0)) ((SURFING 1 0)))))

(fput 'henry 'hobby 'value '(skiing 1 0)) = NIL

(fgetframe 'henry) = (HENRY (HOBBY (VALUE ((SKIING 1 0)) ((SURFING 1 0)))))

(fput 'henry 'gpa 'value '(3.6 1 0)) = (3.6 1 0)

(fgetframe 'henry) = (HENRY
    (HOBBY (VALUE ((SKIING 1 0)) ((SURFING 1 0)))) (GPA (VALUE ((3.6 1 0)))))

(fremove 'henry 'hobby 'value '(skiing 1 0)) = T

(fgetframe 'henry) = (HENRY (HOBBY (VALUE ((SURFING 1 0)))) (GPA (VALUE ((3.6 1 0)))))

(fremove 'henry 'hobby 'value '(surfing 1 0)) = T

(fgetframe 'henry) = (HENRY (GPA (VALUE ((3.6 1 0)))))

(fremove 'henry 'gpa 'value '(3.6 1 0)) = T

(fgetframe 'henry) = (HENRY)

=============================================================|#

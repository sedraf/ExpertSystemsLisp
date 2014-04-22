
(defun careers ( student &optional n)

	(fclearframe student)
	(students)
	(initialize student)       ; initialization
	(setf *proved* (diagnose)) ; main process
	
	(fputproved *proved* student)
	
	(if (= (length (list n)) 0)
	 (output student (length *rulesused*))                          ; output
	 (output student n))
)


	

;--------------------------------------------------------------------------
; "main" function

; load I/O functions
(load 'io)
; load main functions
(load 'expert)
(load 'cf)
(load 'frame)

(defvar *students*)
(defvar *rules*)
(defvar *rulesused*)
(defvar *proved*)
(students)
(setq *rules* (io "rulebase"))



(cond
	((NOT (and (NOT (= (length EXT:*ARGS*) 1)) (NOT (=(length EXT:*ARGS*) 2))))
		(let
			(
			(student (parse-integer (first EXT:*ARGS*)))
			(if (=(length EXT:*ARGS*) 2) 
			(recommendations (parse-integer (first EXT:*ARGS*))))						
			(pathStudents "students")
			(pathRules "rulebase")
			)

		; execute
		(if (=(length EXT:*ARGS*) 1) 
		(ES   student)
		(ES   recommendations))
	))
	
	; otherwise print a usage message
	(T
		; Print out a usage message:
		(format t "~%~%How to use our Expert System~%")
		(format t   "----------- -------- -------~%")
		(format t "Usage: (careers student n)~%")
		(format t "       student - Name of the student who wishes career advice e.g., \"Ross Hoyer\"")		
		(format t "       n - optional the maximum number of recommendations e.g., \"2\"~%~%")
	)
)


#|
		**** CF.LSP ****

This file is to calcualte a certainty factor from MB/MD or other CFs of facts.
The value of CF is supposed to be between -1 and 1.

|#

; to get CF from fact
; fact is supposed to be like (HOBBY (VALUE ((PUZZLES 1.0 0.0)))
(defun getcf (fact)
(let (mbmd)
	(cond
		((null fact) ; if fact is NIL, return NIL
			NIL
		)
		((null (cdar (cadadr fact))) ; if fact is in the form (HOBBY (VALUE ((PUZZLES)))), return 0
			0
		)
		(T
			; get MB/MD
			(setf mbmd (cdar (cadadr fact)))
			; return the sum as CF
			(- (car mbmd) (cadr mbmd))
		)
	)
))

; to set CF to fact
; fact is supposed to be like (HOBBY (VALUE ((PUZZLES 1.0 0.0))) or (HOBBY (VALUE ((PUZZLES)))
(defun setcf (fact cf)
	;update CF
	(if (>= cf 0)
		(setf (cdar (cadadr fact)) (list cf 0.0))
		(setf (cdar (cadadr fact)) (list 0.0 (abs cf)))
	)
)

; to combine certainty factors of two rules based on the same evidence
(defun combcf (cf1 cf2)
	(if (>= (* cf1 cf2) 0)
		; if both are the same sign (i.e., both positive or both negative)
		(- (+ cf1 cf2) (* (abs cf1) cf2))
		; otherwise (i.e., either one is positive, or negative)
		(/ (+ cf1 cf2) (- 1 (min (abs cf1) (abs cf2))))
	)
)

; to propagate the certainty factor by a rule (cfr is CF of the rule, cfe is CF of the evidence)
(defun propcf (cfr cfe)
	(if (> cfe 0)
		; if CF of evidence is positive
		(* cfr cfe)
		; otherwise, 0
		0
	)
)

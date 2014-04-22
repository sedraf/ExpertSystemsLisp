#|
                ***** EXPERT.LSP *****

This file is the functions of expert system based on MYCIN.
Rules are fired and a certainty factor is provided in a rage of -1.0 and 1.0.

The original codes were provided by Dr. Weiss. Functions were edited so that they deal with CF.

14/04/18 - diagnose edited
14/04/20 - minor modifications

The following notes come from the originalsource file.
---------------------------------------------------------
"Toy" expert system routines from Chapter 18 of Winston and Horn LISP text
(1st edition, Addison-Wesley, 1981).

This code illustrates both forward and backward chaining expert system approaches.

Usage:
Code runs automatically upon loading.
(initialize)	- initializes global variables
(deduce)	- runs forward chaining expert system
(diagnose)	- runs backward chaining expert system
(how <fact>)	- explains how <fact> was inferred
(why <fact>)	- explains why <fact> was needed

Modifications (John M. Weiss, Ph.D.)
11/25/98 - Rewrote and reformatted much of the archaic Lisp code.
11/25/98 - Removed interactive queries for more info.
04/04/07 - More code cleanups.
|#

;-----------------------------------------------------------------

; global variables
(defvar *asked*)
(defvar *facts*)
(defvar *hypotheses*)
(defvar *rules*)
(defvar *rulesused*)

;-----------------------------------------------------------------
; INITIALISE global variables prior to running diagnose
; initializes *HYPOTHESES* by adding unique THEN-PART of every RULE in *RULES*
; *students* and *rules* must be set before this function.
(defun initialize (person)
	; initialize with NIL
	
	(setf *asked* NIL)
	(setf *rulesused* NIL)
	(setf *proved* NIL)
	
	; initialize *facts*
	(setf *facts* NIL)
	(setq *facts* (cdr (fgetframe person)))
	
	; initialize *hypotheses*
	(setf *hypotheses* NIL)
	(dolist (rule *rules*)  ; for each rule
		; check if rule has a fact in its thenpart which is equivalent to one in *hypotheses*
		(when (notany #'(lambda (hypo) (if (thenp hypo rule) hypo NIL)) *hypotheses*)
			; if there is no equivalent hypo in *HYPOTHESES*, add facts in thenpart
			(setf *hypotheses*
				(append 
					; converted facts in then part
					(apply #'append (mapcar #'(lambda (thenpart) (list (rl2frm thenpart))) (cdadr rule)) )
					; to the list of hypotheses
					*hypotheses*
				)
			)
		)
	)
)

;-----------------------------------------------------------------
#| forward chaining is not used, so just commented out
; forward chaining routines

; DEDUCE calls STEPFORWARD repeatedly until no more progress can be made
(defun deduce ()
	(do
		(progress)				; local variable
		((not (stepforward)) progress)		; termination condition
		(setq progress t)
	)
)

; STEPFORWARD scans down the rule list, trying to fire each rule
(defun stepforward ()
	(let
		((rulelist *rules*))

		; loop through rule list
		(dolist (rule rulelist)
			(when (tryrule rule) (return-from stepforward t))
		)
	)
)
|#

; TRYRULE attempts to fire a RULE with TESTIF and USETHEN.
; returns CF of rule
(defun tryrule (rule)
(let (
	cf-if
	(cf-result 0.0)
)
	; get cf from if part
	(setf cf-if (testif rule))
	; if CF from testif is more than 0.2, fire rule
	(when (>= cf-if 0.2)
		(setf cf-result (usethen rule cf-if))
		(setq *rulesused* (cons rule *rulesused*))
	)
	; return the result
	cf-result
))

; RL2FRM converts the form "(a = b)" to the form "(a (VALUE ((b))))"
(defun rl2frm (rl)
	(list (car rl) (list 'VALUE (list (list (caddr rl)))))
)

; FRM2RL converts the from "(a (VALUE ((b))))" to the form "(a = b)"
(defun frm2rl (frm)
	(list (car frm) '= (caar (cadadr frm)))
)

; SLOT-INCLUDED returns FACT if SLOT is included in FACT with MB/MD
(defun slot-included (slot fact)
	(if (equal slot (car fact))
		; if included
		fact
		; if not
		NIL
	)
)

; CHKINEQ checks one IF part with '>' and '<' (used for TESTIFPART and TESTIFPART+)
; returns CF
(defun chkineq (if-part)
(let (frm)
	(cond ((equal (cadr if-part) '>) ; if the symbol is ">"
			; get the fact with the same slot of if-part
			(setf frm (car (member (car if-part) *facts* :test #'slot-included)))
			
			(cond ((null frm) 0 ) ; if no related fact, return 0
				(T  ; check if frm satisfies if-part
					(if (> (caar (cadadr frm)) (caddr if-part))
						(getcf frm) ; if satisfied, return CF
						0.0         ; otherwise, return 0
					)
				)
			)
		)
		(T  ; if the symbol is "<"
			; get the fact with the same slot of if-part
			(setf frm (car (member (car if-part) *facts* :test #'slot-included)))
			
			(cond ((null frm) 0) ; if no related fact, return 0
				(T  ; check if frm satisfies if-part
					(if (< (caar (cadadr frm)) (caddr if-part))
						(getcf frm) ; if satisfied, return CF
						0.0         ; otherwise, return 0
					)
				)
			)
		)
	)
))

; TESTIFPART checks one IF part (used for TESTIF)
; using RECALL to get the fact with MB/MD.
; returns CF
(defun testifpart (if-part)
(let (frm)
	(cond
		; if the symbol is "="
		((equal (cadr if-part) '=)
			(setf frm (rl2frm if-part))  ; convert the form to "(a (VALUE ((b)))"
			(setf frm (recall frm))      ; if a fact is known, frm is added with MB/MD
			(if (null frm)
				; if unknown, return 0
				0.0
				; otherwise, return CF
				(getcf frm)
			)
		)
		; if the symbol is ">" or "<"
		(T (chkineq if-part) )
	)
))

; TESTIF checks to see whether all the IF parts of a rule match,
; using RECALL to find known facts.
; the result is CF calculated from IF parts
(defun testif (rule)
(let(
	(ifs (cdar rule))
	frm
	cf-result
)
	(cond
		; ifall
		((equal (caar rule) 'ifall)
			(setf cf-result 1.0)
			; loop through IF parts of RULE
			(dolist (if-part ifs)
				; calculate CF and store minimum one
				(setf cf-result (min cf-result (testifpart if-part)))
			)
			; return minimum CF
			cf-result
		)
		; ifsome
		((equal (caar rule) 'ifsome)
			(setf cf-result 0.0)
			; loop through IF parts of RULE
			(dolist (if-part ifs)
				; calculate CF and store maximum one
				(setf cf-result (max cf-result (testifpart if-part)))
			)
			; return maximum CF
			cf-result
		)
		(T
			NIL
		)
	)
))

; VAL-EQUAL returns T if val1 and val2 has the same value in their first place
; it is used in fact-equal
(defun val-equal (val1 val2)
	(equal (caar val1) (caar val2))
)

; FACT-INCLUDED returns T if all corresponding items of facts are the same except for MB/MD
; NOTE: f1 is supposed to have only one value
(defun fact-included (f1 f2)
	(and
		(equal (car f1) (car f2))
		(equal (caadr f1) (caadr f2))
		(not (null
			(member (cadadr f1) (cdadr f2) :test #'val-equal)
		))
	)
)

; RECALL returns FACT with the current MB/MD if already known, FACT with 0/0 otherwise.
; note: MB/MD of FACT are updated in the process
(defun recall (fact)
(let (fact-db mbmd)
	; get the corresponding fact
	(setf fact-db (car (member fact *facts* :test #'fact-included)))
	
	(cond
		((null fact-db) ; if fact-db is nil, return FACT with 0 of MB/MD
			; assign 0/0 to fact
			(setcf fact 0.0)
		)
		(T              ; if fact-db has values, return fact with current MB/MD
			; get the MB/MD from fact-db
			(setf mbmd (cdar (cadadr fact-db)))
			; assign MB/MD to fact
			(setf (cdar (cadadr fact)) mbmd)
		)
	)
	; return fact
	fact
))

; USETHEN attempts to add new facts as a result of firing the RULE with CF,
; by checking all of the THEN parts.
; the result is CF calculated from RULE's CF and CF-IF (cf of if part of RULE)
(defun usethen (rule cf-if)
(let (
	(thens (cdadr rule))
	(cf-rule (cadr (caddr rule)))
	cfbuff
	frmbuff
	(cf-result 0)
)
	; loop through THEN parts of RULE
	(dolist (then-part thens)
		; calculate the CF of then-part (from cf-if and cf-rule) and set it to fact
		(setf cfbuff (* cf-if cf-rule))
		; convert to the form of (a (VALUE ((b))))
		(setf frmbuff (rl2frm then-part))
		
		; set MB/MD from CF
		(setcf frmbuff cfbuff)
		
		; add a new fact and get updated CF
		(setf cf-result (getcf (remember frmbuff)))
	)
	; return CF
	cf-result
))


;-----------------------------------------------------------------
; backward chaining routines

; BEFORE DIAGNOSIS,
;   input list
;		*hypotheses* << input
;		*facts* //maybe?, used in RECALL 
;		*rules* << environment
;		*rulesused* << in processing

; DIAGNOSE tries to VERIFY each hypothesis
; return a list of proved hypotheses
(defun diagnose ()
(let (
		(*proved* NIL) ; list of proved hypothesis
		(cfbuff 0)     ; buffer of CF returned from the verify function
	)
	
	; each hypothesis is examined
	(dolist (hypothesis *hypotheses*)
	
		; get the CF of hypothesis
		(setf cfbuff  (getcf (verify hypothesis)))
		
		; if CF is beyond the limit (0.2), add it to the proved list
		(if (>= cfbuff 0.2)
			(setf *proved* (cons hypothesis *proved*))
		)
	)
	
	; return the hypotheses
	*proved*
))

; VERIFY attempts to deduce a FACT using backward chaining.
; The basic approach is:
;      1) see whether a FACT is already known
;      2) make a list of relevant rules with FACT in THEN section
;           a) try to deduce FACT directly via TRYRULE
;           b) try to deduce FACT indirectly via TRYRULE+
; Note: Code to request more info from the user has been commented out.
; Note2: the value to return is FACT which is attached with an updated certainty factor(-1 to +1)
(defun verify (fact)
(let (relevant1 relevant2 cfbuff)
	(setf cfbuff (getcf (recall fact))) ; try to get fact in the knowledge (MB/MD of FACT are updated)
	(when (>= (abs cfbuff) 0.2)         ; is fact already known? (getcf is in cf.lsp: if unknown, cfbuff gets NIL)
		(return-from verify fact)       ; if already known, return FACT with its current CF
	)
	(setq relevant1 (inthen fact))      ; list rules with fact in then part
	(setq relevant2 relevant1)

	#| This version is not interactive:
	(cond
		((null relevant1)               ; if none, ask human user
			(cond
				((member fact *asked* :test #'equal)
					(return-from verify nil)
				)
				((and
					(format t "Is this true [yes/no]: ~a~%" fact)
					(eq (read) 'yes)
				 )
					(remember fact)
					(return-from verify t)
				)
				(t
					(setq *asked* (cons fact *asked*))
					(return-from verify nil)
				)
			)
		)
	)
	|#

	; see if fact is directly deducible
	(dolist (relevant relevant1)
		(setf cfbuff (tryrule relevant))    ; get CF by firing rules based on existing knowledge
		(when (>= cfbuff 0.2)               ; if CF has a value high enough to be credible
			(setcf fact cfbuff)                 ; update CF
			(return-from verify fact)           ; return fact
		)
	)

	; see if fact is indirectly deducible
	(dolist (relevant relevant2)
		(setf cfbuff (tryrule+ relevant))   ; get CF by firing rules recursively
		(when (>= cfbuff 0.2)               ; if CF has a value high enough to be credible
			(setcf fact cfbuff)                 ; update CF
			(return-from verify fact)           ; return fact
		)
	)

	; if not certain enough
	(setcf fact 0) ; set 0
	fact           ; return fact
))

;-----------------------------------------------------------------
; various expert system support routines

; IFP tests to see if a FACT is in the IF part of the RULE.
; note FACT is in the form of (A (VALUE ((B))))
(defun ifp (fact rule)
(let (
		(if-parts (cdar rule))
	)
	; for each if-part
	(dolist (if-part if-parts)
		(cond
			((equal (cadr if-part) '=) ; the from (a = b)
				; if slot and value is matched, return the rule
				(if (and (equal (car fact) (car if-part)) (equal (caar (cadadr fact)) (caddr if-part)))
					(return-from ifp rule)
				)
			)
			((equal (cadr if-part) '>) ; the form (a > b)
				; if slot is matched and the values satisfy the relation, return the rule
				(if (and (equal (car fact) (car if-part)) (> (caar (cadadr fact)) (caddr if-part)))
					(return-from ifp rule)
				)
			)
			(T                         ; the form (a < b)
				; if slot is matched and the values satisfy the relation, return the rule
				(if (and (equal (car fact) (car if-part)) (< (caar (cadadr fact)) (caddr if-part)))
					(return-from ifp rule)
				)
			)
		)
	)
	; if no match, return NIL
	NIL
	
	; notes about cadadr
	;(cadadr '((ifall  (major = Computer-Science)) (then   (job = programming)) (with   0.7)))
	; => (job = programming)
))

; INTHEN makes a list of all *RULES* with FACT in THEN part.
(defun inthen (fact)
	(apply #'append (mapcar
		#'(lambda (rule) (cond ((thenp fact rule) (list rule))))
		*rules*)
	)
)

; FACT-SAMESLOT returns T if slot is the same
(defun fact-sameslot (f1 f2)
	(and (equal (car f1) (car f2)) (equal (caadr f1) (caadr f2)))
)

; REMEMBER adds a NEW fact to the list of known *FACTS*.
; if the fact is already known, it returns fact with updated CF
(defun remember (new)
(let (fact-db valbuff cfbuff)
	; if new does not have CF, just assign 1.0 0.0
	(if (null (getcf new))
		(setcf new 1.0)
	)

	; check if the fact is already known
	(setf fact-db (car (member new *facts* :test #'fact-included)))
	
	(cond
		((null fact-db)    ; if unknown
			; find slot
			(setf fact-db (car (member new *facts* :test #'fact-sameslot)))
			(cond
				((null fact-db) ; if slot does not exisit either
					; add new fact to list
					(setq *facts* (cons new *facts*))
				)
				(T ; if slot exists
					; add new value to the slot
					(setf (cdadr fact-db) (cons (cadadr new) (cdadr fact-db)))
				)
			)
			; just return new
			new
		)
		(t                 ; if known, update CFs
			; get the corresponding value i.e., "(HIKING 1.0 0.0)"
			(setf valbuff (caar (member (cadadr new) (cdadr fact-db) :test #'val-equal)))
			; calculate the new CF
			(setf cfbuff (combcf (getcf new) (- (cadr valbuff) (caddr valbuff))))
			; update CF
			(setcf new cfbuff)
			(setf (cdr valbuff) (cdaar (cdadr new))) ; update for knowledge
			; return new with updated CF
			new
		)
	)
))

; TESTIFPART+ checks one IF part (used for TESTIF+)
; using VERIFY (indirect recursion) to get the fact with MB/MD.
; returns CF
; for the rule in the forms (a < b) and (a > b), it checks if there is a related fact in the current knowledge
;    because those forms are not facts deduced from rules.
(defun testifpart+ (if-part)
(let (frm)
	(cond
		; if the symbol is "="
		((equal (cadr if-part) '=)
			(setf frm (rl2frm if-part))  ; convert the form to "(a (VALUE ((b)))"
			(setf frm (verify frm))      ; a fact is deduced by VERIFY, frm is added with MB/MD
			(if (null frm)
				; if unknown, return 0
				0.0
				; otherwise, return CF
				(getcf frm)
			)
		)
		; if the symbol is ">"
		((equal (cadr if-part) '>)
			; get the fact with the same slot of if-part
			(setf frm (car (member (car if-part) *facts* :test #'slot-included)))
			(cond
				((null frm) ; if no related fact, return 0
					0
				)
				(T
					; check if frm satisfies if-part
					(if (> (caar (cadadr frm)) (caddr if-part))
						; if satisfied, return CF
						(getcf frm)
						; otherwise, return 0
						0.0
					)
				)
			)
		)
		; if the symbol is ">" or "<"
		(T (chkineq if-part) )
	)
))

; TESTIF+ checks to see whether all the IF parts of a rule match,
; using TESTIFPART+ to find known facts.
(defun testif+ (rule)
(let(
	(ifs (cdar rule))
	frm
	cf-result
)
	(cond
		; ifall
		((equal (caar rule) 'ifall)
			(setf cf-result 1.0)
			; loop through IF parts of RULE
			(dolist (if-part ifs)
				; calculate CF and store minimum one
				(setf cf-result (min cf-result (testifpart+ if-part)))
			)
			; return minimum CF
			cf-result
		)
		; ifsome
		((equal (caar rule) 'ifsome)
			(setf cf-result 0.0)
			; loop through IF parts of RULE
			(dolist (if-part ifs)
				; calculate CF and store maximum one
				(setf cf-result (max cf-result (testifpart if-part)))
			)
			; return maximum CF
			cf-result
		)
		(T
			NIL
		)
	)
))

; THENP tests to see if a FACT is in the THEN part of the RULE.
; note FACT is in the form of (A (VALUE ((B))))
(defun thenp (fact rule)
(let (then-part)
	; set then-part with a fact in then-part of the rule
	(setf then-part (cadadr rule))
	
	; if slot and value is matched, return the rule
	(if (and (equal (car fact) (car then-part)) (equal (caar (cadadr fact)) (caddr then-part)))
		rule
		NIL
	)
	
	; notes about cadadr
	;(cadadr '((ifall  (major = Computer-Science)) (then   (job = programming)) (with   0.7)))
	; => (job = programming)
))

; TRYRULE+ attempts to fire a RULE with TESTIF+ and USETHEN.
(defun tryrule+ (rule)
(let (
	cf-if
	(cf-result 0.0)
)
	; get cf from if part
	(setf cf-if (testif+ rule))
	; if CF from testif is more than 0.2, fire rule
	(when (>= cf-if 0.2)
		(setf cf-result (usethen rule cf-if))
		(setq *rulesused* (cons rule *rulesused*))
	)
	; return the result
	cf-result
))

;-----------------------------------------------------------------
; explanatory routines

; HOW answers "How did you deduce that FACT?" questions.
(defun how (fact)
	(let
		(success)

		; any fired rules with FACT in the then-part?
		(dolist (possibility *rulesused*)
			(when (thenp fact possibility)
				(setq success t)
				; if so, list the if-parts
				(format t "~a demonstrated by~%" fact)
				(dolist (possible (cdaddr possibility))
					(format t "~a~%" possible)
				)
			)
		)

		; if not, perhaps FACT was given in original knowledge base, or is not known to be true
		(cond
			(success t)
			((recall fact) (format t "~a was given.~%" fact) t)
			(t (format t "~a is not established.~%" fact) nil)
		)
	)
)

; WHY answers "Why did you deduce that FACT?" questions.
(defun why (fact)
	(let
		(success)

		; any fired rules with FACT in the if-part?
		(dolist (possibility *rulesused*)
			(when (ifp fact possibility)
				(setq success t)
				; if so, list the then-parts
				(format t "~a needed to show:~%" fact)
				(dolist (possible (cdr (cadddr possibility)))
					(format t "~a~%" possible)
				)
			)
		)

		; if not, perhaps FACT was a hypothesis to be proved, or is not known to be true
		(cond
			(success t)
			((recall fact) (format t "~a was hypothesis.~%" fact) t)
			(t (format t "~a is not established.~%" fact) nil)
		)
	)
)


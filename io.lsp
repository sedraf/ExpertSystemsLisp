(defun io (path)
	#| local variables |#
(let    (
		;the input file pointer
		(ipf (open path))
	)
	
	;do for each element in the file
	(do ((f_first T NIL) (buff T)) ((eq buff NIL))
		(setf buff (read ipf NIL))    ;read one element
		(if (not (eq buff NIL))            ;if it is not EOF, append it to the list
			(if f_first
				(setf lst (cons buff NIL)) ; in case of the first element
				(setf lst (cons buff lst)) ; the second or following elements
			)
		)
	)
	(close ipf)
	(reverse lst) ; because a newr element is added on the top of lst in the previous iteration
))

(defun students()
	(let 
		(
			(pathStudents "students")
			frame
			slot
			value
			temp2
			(temp '(1.0 0.0))
		)
		
		(setq *students* (io pathStudents))
		(dolist (student *students*)
			(setq frame (nth 0 student))
			(setq slot (nth 1 student))
			(setq temp2 (nth 2 student) )
			(setq value  (cons temp2 temp))
			(fput frame slot 'value value)
		)
		)
)

(defun fputproved (proved student)
	(let (frame slot tem2 ) 
	
		(dolist (rule proved)
				(setq frame student)
				(setq slot (nth 0 rule))
				(setq temp2 (car (car (car (cdr (nth 1 rule))))))
				(setq value (cons temp2 (cdr (car (car (cdr (nth 1 rule)))))))				
				(fput frame slot 'value value)
		)		
	)
)

(defun fclearframe(student)
	(let ()
	
	(dolist (temp (cdr (fgetframe student)))
		;(print temp)
		(setq frame student)	
		(setq slot  (car temp))
		(dolist (value (cdr (car (cdr temp))))
			(setq value    (car value))
			;(print value)
			(fremove frame slot 'value value)
		)
	)	
)
)


(defun sort-rules (n1 n2) (> (nth 1 (car (car (cdr (car (cdr n1)))))) (nth 1 (car (car (cdr (car (cdr n2)))))))) 

(defun output(name &optional (n (length *proved*)))
	(let ((i 1) then) 
		(sort *proved* #'sort-rules)
		(format t "~%Wow! ~a looks like a real winner!~%Just take a look at these wonderful career possibilities:~%~%" name)
		(if (string= (write-to-string n)  (write-to-string nil)) (setq n (length *proved*)))		
		(loop while (and (<= i n) (<= i (length *proved*))) do

			(setq rule (nth (- i 1) *proved*))
			
			(setq then (car (car (cdr (car (cdr rule))))))
			(format t "~a) ~a = ~a with CF of ~2$~%"  i  (car rule)   (car then) (nth 1 then))
			(setq i (+ i 1))
		)
		(format t "~%")
			
	)
	
	T
)





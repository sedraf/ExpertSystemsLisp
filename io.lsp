#|
                ***** IO.LSP *****

File input and output used for this program

Usage:
Code runs automatically upon loading.

	(io path )   - code for loading  file
	
		It reads each element in the file and append it to the list, and finally reverses the order so that the first element comes first.
		The entries of the list ( 1 2 3 4 5 6 7 8 9 ) correspond with the elements in the puzzle as the following:
			1 2 3
			4 5 6
			7 8 9

		parameters:
			path: path of input file

		return:
			list of elements

		usage:
		to store the list to the variable "lst"
			(setf lst (ldpuz "foo.puz"))
			(fclearframe student)	- NOT USED but usage is to delete all info about a single student
			
	(student) - grabs student data from "students" and creats a frame with the students data
	(sort-rules n1 n2) - returns ( > (cf of n1) (cf of n2))
	(output srudent &optional n) outputs n (if n is not set n is set to length of proved facts) proved facts for student	
		
|#

;io grabs data from path and returns it
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

;students grabs student data from file called "students" and sets up frames for them
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
		;All the raw student data is stored in global *students*
		(setq *students* (io pathStudents))
		;For each piece of data create a frame if frame excists will just add slot
		(dolist (student *students*)
			(setq frame (nth 0 student))
			(setq slot (nth 1 student))
			(setq temp2 (nth 2 student) )
			(setq value  (cons temp2 temp))
			(fput frame slot 'value value)
		)
		)
)


;sort rules sorts returns how n1's cf compares to n2's cf
(defun sort-rules (n1 n2) (> (nth 1 (car (car (cdr (car (cdr n1)))))) (nth 1 (car (car (cdr (car (cdr n2)))))))) 

;Outputs the data of student with name and will only output up to n rules (if n is not set n = length of proved list)
(defun output(name &optional (n (length *proved*)))
	(let ((i 1) then) 
		;Sort by cf value
		(sort *proved* #'sort-rules)
		(format t "~%Wow! ~a looks like a real winner!~%Just take a look at these wonderful career possibilities:~%~%" name)
		;If n is nil shouldnt be needed used because of random bugs
		(if (string= (write-to-string n)  (write-to-string nil)) (setq n (length *proved*)))	
	
		;Walk through n rules or lenght of proved if n is larger then length 
		(loop while (and (<= i n) (<= i (length *proved*))) do

			(setq rule (nth (- i 1) *proved*))			
			(setq then (car (car (cdr (car (cdr rule))))))
			(format t "~a) ~a = ~a with CF of ~2$~%"  i  (car rule)   (car then) (nth 1 then))
			;increment i
			(setq i (+ i 1))
		)
		(format t "~%")			
	)	
	T
)





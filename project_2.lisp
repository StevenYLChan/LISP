;A program P in FL is a list of function definitions. 
;The FL interpreter takes such a program, together with a function application, and returns the result of evaluating the application. 

;interpreter
(defun fl-interp (E P)
	(fl-interpreter E P NIL)
	)

(defun fl-interpreter (E P C)
	(cond 
  		((afunc E C)
			(car (cdr (afunc E C))))
		((atom E) 
			E)   ;this includes the case where expr is NIL
        (T
        	(let(
        		(f (car E)) 
        		(arg (cdr E))
        		)
	      	(cond
                ; handle built-in functions
				((eq f 'if) 
					(fif arg P C))
				((eq f 'and) 
					(fand arg P C))
				((eq f 'or) 
					(for arg P C))
				((eq f 'number) 
					(numberp (car (parse arg P C))))
        		((eq f 'first) 
        			(car (car (parse arg P C))))
				((eq f 'rest) 
					(cdr (car (parse arg P C))))
				((fprim f) 
					(apply f (parse arg P C)))
	       		

	        ; if f is a user-defined function,
                ;    then evaluate the arguments 
                ;         and apply f to the evaluated arguments 
                ;             (applicative order reduction) 
				((not (null (afunc f P))) 
					(fl-interpreter 
						(car (getFunc (afunc f P))) P 
						(append 
							(implement_context (getArgs (cdr (afunc f P))) (parse arg P C)) C)
					)
				)
                ; otherwise f is undefined; in this case,
                ; E is returned as if it is quoted in lisp
				(T E)
				)
	      	)
		)
	)
)
;Get Match
(defun afunc (E C)
	(cond
		((null C) 
			NIL)
		((equal E (car(car C))) 
			(car C))
		(T (afunc E (cdr C)))
	)
)

;Do IF
(defun fif (E P C) 
    (if (fl-interpreter (car E) P C) 
    	(fl-interpreter (car (cdr E)) P C)  
      	(fl-interpreter (car (cdr (cdr E))) P C)
  	) 
)

;Do AND
(defun fand (E P C)
  	(cond
   		((null E) 
   			NIL)
 		((null (fl-interpreter (car E) P C)) 
 			NIL)
 		((null (cdr E)) 
 			T)   
  		(T (fand (cdr E) P C))   
  	)
)

;Do OR
(defun for (E P C)
  	(cond
    	((null E) 
      		NIL)
      	((fl-interpreter (car E) P C) 
      		T)
      	((null (cdr E)) 
     	 	NIL)
     	(T (for (cdr E) P C))
  	)  
)
;Make sure interpreter implement based on criteria given.
(defun parse (E P C)
  	(cond
 	   ((null E) 
 			NIL)
    (T (cons (fl-interpreter (car E) P C) (parse (cdr E) P C)))  
 	)  
)

;Check if is one of primitive in list. 
(defun fprim (f)
	(let ((alist '(null atom eq cons equal + - * > < = not)))
	(inlist f alist))
)

;Used by fprim. Returns T if item is in list.
(defun inlist(i L)
	(cond
		((null L) 
			NIL)
		((equal i (car L))
			T) 
		(T (inlist i (cdr L)))
	)
)

;Get the function.
(defun getFunc (f)
  	(cond
    	((eq (car f) '=) 
    		(cdr f))
    	(T (getFunc (cdr f)))
  	)
)

;Get the arguments.
(defun getArgs (f)
    (cond
        ((eq (car f) '=) 
        	NIL)
        (T (cons (car f) (getArgs (cdr f))))
    )
)

;Create the Context. Variables to Values Mapping.
(defun implement_context (name value)
 	(cond 
    	((null name) 
    		NIL)
    	(T (cons (list (car name) (car value)) (create_context (cdr name) (cdr value))))
 	)
)
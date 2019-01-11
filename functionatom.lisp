(defun functionatom(x)
	(cond
		((null x) nil)
		((listp (car x)) 
			(ap (functionatom (car x)) (functionatom (cdr x)) ) 
		)
		(t (cons (car x) (functionatom (cdr x) ) ))

	)
)

(defun ap(list1 list2)
	(cond
		((null list1) list2)
		(t 
			(cons (car list1) (ap (cdr list1) list2))
		)
	)
	
)


(print (functionatom '( a ( v c) e )) )
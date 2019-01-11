(defun replaceword(word pattern)
	(cond
		((null pattern) nil)
		((eql word (car pattern) ) 
			(cons 'YYYY (replaceword word (cdr pattern))) 
		)
		
		(t (cons 
			(car pattern) 
			(replaceword word (cdr pattern) ) )
		)

	)
)

(print (replaceword 'hello '(hello hello world)))
;(replaceword 'hello '(hello hello world))
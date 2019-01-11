(defparameter *goalState*  '(1 2 3 4 5 6 7 8 0))
(defparameter *nExpanded* 0)
(defun solvablep (state)
	(let 
		( (count 0)
		)

		( do
            ( ( i 0 ( 1+ i ) )
            )
            ( ( >= i ( length state ) ) count )
            ( unless ( = ( nth i state ) 0 )
                ( do
                    ( ( j i ( 1+ j ) ) 
                    )
                    ( ( >= j ( length state ) ) )
                    ( when
                        ( and
                            ( > ( nth i state ) ( nth j state ) )
                            ( /= ( nth j state ) 0 )
                        )
                        ( setf count ( 1+ count ) )
                    )
                )
            )
        )

	    (cond 

			((evenp count) T)
			(t NIL)
		)

	)

)


(defun isGoal (currState)
    ( let (stateCheck flag)
        (setf stateCheck (mapcar #'eq currState *goalState*))
        (setf flag (position NIL stateCheck))
        (null flag)
    )
)

( defun generateSuccessors ( state )
    ( let (
        ( location ( position 0 state ) ) 
        ( children '() ) 
        UP DOWN LEFT RIGHT 
      )
        
        ( when ( > location 2 )
            ( setf UP ( copy-list state ) )
            ( rotatef ( nth location UP ) ( nth ( - location 3 ) UP ) )
            ( setf children ( cons UP children ) )
        )
        ( when ( < location 6 )
            ( setf DOWN ( copy-list state ) )
            ( rotatef ( nth location DOWN ) ( nth ( + location 3 ) DOWN ) )
            ( setf children ( cons DOWN children ) )
        )
        
        ( when ( > ( mod location 3 ) 0 )
            ( setf LEFT ( copy-list state ) )
            ( rotatef ( nth location LEFT ) ( nth ( - location 1 ) LEFT ) )
            ( setf children ( cons LEFT children ) )
        )
        
        ( when ( < ( mod location 3 ) 2 )
            ( setf RIGHT ( copy-list state ) )
            ( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
            ( setf children ( cons RIGHT children ) )
        )
    children
    )
)


( defun calculate_hScore (state)
    ( let
        (
            ( count 0 )
            correctPosition 
        )
        
        ( do
            (
           		( i 0 ( 1+ i ) )
            )
            ( ( >= i 9 ) count )

            ( when ( not ( eq ( nth i state ) ( nth i *goalState* ) ) )
                ( setf correctPosition ( position ( nth i state ) *goalState* ) )
                ( setf count ( + count ( abs ( - ( floor i 3 ) ( floor correctPosition 3 ) ) ) ) )
                ( setf count ( + count ( abs ( - ( mod i 3 ) ( mod correctPosition 3 ) ) ) ) )
            )

         )
    )
)
( defun generateNode ( state parent )
    ( list
        ( 1+ ( car parent ) )       
        ( calculate_hScore state ) 
        state 
    )
)


(defun leastScoreState(state)
	(setq children (generateSuccessors state))
	(mapcar #'calculate_fScore children)

)

(defun a*(state)
	(setf *nExpanded* 0)
	(setf openList (list (list '0 '0 state)))
	(setf closedList NIL)
	(setf solution (aHelper* openList closedList ))
	solution

)
(defun aHelper*(openList closedList)
	(let 
		(
			(next (findLeastScoreNode openList))
			joinedLists
			successors
			solution
		)
		(
			do()
				( (isGoal (caddr next)) (getSolution closedList))
				(setf joinedLists (addNextToClosedList next openList closedList))

				(setf openList (car joinedLists))
				(setf closedList (cadr joinedLists))
				
 				(setf successorNodeList
					( map
					    'list
					    #'( lambda ( state )
					        ( generateNode state next)
					    )
					    ( generateSuccessors ( caddr next ) )
					)
				)
 				(setf *nExpanded* (1+ *nExpanded*))
				(setf joinedLists (addSuccToOpenList successorNodeList openList closedList))
				(setf openList (car joinedLists))
				(setf closedList (cadr joinedLists))
				(setf next (findLeastScoreNode openList))

		)

	)
)

 (defun getSolution(closedList)
	(setq solution (mapcar (lambda (e) ( caddr e)) closedList))
	(reverse (cons *goalState* solution))

)

( defun findLeastScoreNode( openList &optional ( best () ) )
    ( cond
        ( ( not ( car openList ) ) best )
        ( ( not best ) ( findLeastScoreNode ( cdr openList )( car openList ) ) )
        ( ( < ( calculate_fScore ( car openList ) ) ( calculate_fScore best ) )
            ( findLeastScoreNode ( cdr openList ) ( car openList ) )
        )
        ( t ( findLeastScoreNode ( cdr openList ) best ) )
    )
)

( defun calculate_fScore ( node )
    ( + ( car node ) ( cadr node ) )
)

(defun addNextToClosedList(next openList closedList)
	( let ( joinedList )
        ( when ( member next openList :test #'equal )
            ( setf openList ( remove next openList :test #'equal ) )
            ( setf closedList ( cons next closedList ) )
            ( setf joinedList ( list openList closedList ) )
        )
        joinedList
	)
)

(defun addSuccToOpenList (succNodeList openList closedList)
    ( let
        (
            ( succNode ( car succNodeList ) )
            repeatNode
        )
        ( cond
            ( ( not succNode )
                ( list openList closedList )
            )
            ( t
                ( cond
                    ( ( setf repeatNode
                            ( searchNode ( caddr succNode ) closedList )
                        )
                
                    )
                    ( ( setf repeatNode
                            ( searchNode ( caddr succNode ) openList )
                      )
                    )
                    ( t
                        ( setf openList ( cons succNode openList ) )
                    )
                )
                ( addSuccToOpenList ( cdr succNodeList ) openList closedList )
            )
        )
    )
)
( defun searchNode ( state nodeList )
    ( car ( member
        state
        nodeList
        :test #'( lambda ( state node ) ( equal state ( caddr node ) ) )
    ))
)

(defun 8puzzle(puzzle)
	(let 
		(
			solution
		)
		(cond 
			( (not (solvablep puzzle))
					(format t "The puzzle is infeasible.~%")

			)
			(t
				(setf solution (a* puzzle ))
				solution
			)

		)



	)

)

(print (8puzzle '(0 1 3 4 2 5 7 8 6)))
(format t "The number of expanded lists are: ~S~%" *nExpanded*)
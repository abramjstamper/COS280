;; Abram Stamper & Andrew Jackson
;; Known Bugs
;; • Doesn't return anything when not goal can't be met, but program does not give false positives
;; • Does not handle chained complex rules explainations, but handles complex single rule explainations
;;   eg: Rule: ((or (not B) A) F) Goal: F explaination works
;;   eg: Rule: ((or (not B) A) F) ((F) G)) Goal: G explaination doesn't work
;; • Line 30-31 does not handle given facts
;;
;; Program is rather bug around line 34-35 in the can-prove? function 
;; 

(setf facts '(A B C))

(setf rules '(
 ((and A B) D)
 ((or D P) E)
 ((or (not B) A) F)
 (((not B)) F)
 ((and A G) Q)
 ((R) Q)
 ((F) G) ))

(setf factsX '(A B))
(setf rulesX '(
    ((G) D)
))

(defun prove? (goal facts rules)
    (format t "goal = ~A~%" goal)
    (or (can-prove? goal facts rules (get-candidate-rules goal rules))
        (member goal facts)))

(defun can-prove? (goal facts rules candidate-rules)
    ;; (format t "candidate = ~A~%" candidate-rules)
    (cond ((null candidate-rules) nil)
    ((or (prove-true? goal facts (car candidate-rules) rules)
        (can-prove? goal facts (cdr candidate-rules) rules) )))
    )

(defun get-candidate-rules (goal rules)
   ; (format t "get-candidate-rules: goal = ~A~%" goal)
    (if (null rules)
        '()
        (if (eql goal (second (car rules)))
           (cons (car rules)
                 (get-candidate-rules goal (cdr rules)))
           (get-candidate-rules goal (cdr rules)))))

(defun getOperands (rule) (cdr (car rule)))
(defun getOperator (rule) (car (car rule)))
(defun getConclusion (rule) (car (cdr rule)))

(defun resolveNot (goal facts rule rules) 
        ; deal with not
        ; handle nested conditions inside of rules
        ;; (format t "NOT TEST ~%")
        (setf resultX (first (getOperands rule)) )
        ;; (format t "resultX = ~A~%" resultX)  
        ;;     (cond (listp (car (cdr (car rule))))(
        ;;     (prove-true? goal facts (car (cdr (car rule))) rules) ))
            (if (not (member resultX facts))
                t
            )
        ; if x not in list, add to facts
        ; then call prove-true recursively in another function with the updated facts
)

(defun resolveOr (goal facts rule rules) 
        ; deal with or
        ; handle nested conditions inside of rules
        ;; (format t "OR TEST ~%")
        (setf resultX (first (getOperands rule)) )
        ;; (format t "resultX = ~A~%" resultX)  
        (setf resultY (second (getOperands rule)) )
        ;; (format t "resultY = ~A~%" resultY)      
        ;;     (cond (listp (car (cdr (car rule))))(
        ;;     (let (resultX (prove-true? goal facts (car (cdr (car rule))) rules))) ))
        ;;     (cond (listp (car (cdr (cdr rule))))(
        ;;     (let (resultY (prove-true? goal facts (car (cdr (cdr rule))) rules))) ))
            (if (or (member resultX facts)
                 (member resultY facts))
            t
            )
        
        ; if x or y are in the facts, then it's true and the result can be added to facts
        ; then call prove-true recursively in another function with the updated facts
)

(defun resolveAnd (goal facts rule rules)
        ; deal with and
        ; handle nested conditions inside of rules
        ;; (format t "AND TEST ~%")
        (setf resultX (first (getOperands rule)) )
        ;; (format t "resultX = ~A~%" resultX)  
        (setf resultY (second (getOperands rule)) )
        ;; (format t "resultY = ~A~%" resultY)  
        ;; (cond (listp (car (cdr (car rule))))(
        ;;         (let (temp car (cdr (car rule))))
        ;;         (format t "cccccccccc ~%")
        ;;     (let (resultX (prove-true? goal facts temp rules))) ))
        ;;     (cond (listp (car (cdr (cdr rule))))(
        ;;     (let (resultY (prove-true? goal facts (car (cdr (cdr rule))) rules))) ))
        (if (and (member resultX facts) (member resultY facts))
            t
            )
        ; if x and y are in the facts, then it's true and the result can be added to facts
        ; then call prove-true recursively in another function with the updated facts
)

(defun resolveAtom (goal facts rule rules)
        ;; (format t "ATOM TEST ~%")
        (setf resultX (getOperator rule))
        ;; (format t "resultX = ~A~%" resultX)
        (if (member resultX facts)
            t
        )
)

(defun prove-true? (goal facts rule rules)
    (format t "rule = ~A~%" rule)
    (cond ((null rule)
           (format t "Error: empty precondition~%")
           nil))
        ;; (format t "conditional = ~A~%" (getOperator rule))
        (if (eql `and (getOperator rule))
            (write (resolveAnd goal facts rule rules))
        )
        (if (eql `or (getOperator rule))
            (write (resolveOr goal facts rule rules))
        )
        (if (eql `not (getOperator rule))
            (write (resolveNot goal facts rule rules))
        )
        (if (eql 1 (length (car rule)))
            (write (resolveAtom goal facts rule rules))
        )
        
    )
    
    ;; (cond ((getOperator rule))
    ;; (
    ;;     (format t "Inside bottom out ~%")
    ;;     (if (member (caar rule) facts)
    ;;     (cons (cdr (car rule)) facts) )
    ;; )
    ;; )

    ;;  (t (prove? (caar rule) facts rules) )
(prove? 'E facts rules)
;; (prove? 'D factsX rulesX)
(defun getUserInput (playerString)
  (format t (concatenate 'string playerString " player turn!~%"))
  (format t "Enter a board position (0-8): ")
  (let ((n (read)))
    (if (and (numberp n) (and (<= n 8) (>= n 0)))
      (format t "You entered ~d.~%" n)
      (format t "That was not a number or active board position.") )))

(getUserInput "Computer")
(defun getUserMove (playerString)
  (format t (concatenate 'string playerString " player turn!~%"))
  (format t "Enter a board position (0-8): ")
  (let ((userIO (read)))
    (if (and (numberp userIO) (and (<= userIO 8) (>= userIO 0)))
      (format t "You entered ~d.~%" userIO)
      (format t "That was not a number or active board position.") )))

(defun createNewBoard ()
  (list 'board 0 0 0 0 0 0 0 0 0) )

(defun determineWhoGoesFirst ()
  (format t "Would you like to go first? Y or N: ")
  (let ((userIO (read)))
    (cond
    ((and (not (string= userIO "Y")) (not (string= userIO "N")))
      (format t "~%That was invalid input!")
      (quit))
    ((string= userIO "Y")
      (format t "~%Human! You are going first!")
      t
    )
    ((string= userIO "N")
      (format t "~%YAY! I am going first and am going to beat you!")
      nil
    )
    ))
  )

(defun main ()
  (format t "~%WELCOME TO ABRAM AND ANDREW'S TIC TAC TOE GAME!~%~%")
  (format t "Prepare to lose!~%~%")
  (setq humanHadFirstMove (determineWhoGoesFirst))

  (format t "~%~a" humanHadFirstMove)


)

(main)
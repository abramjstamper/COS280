(defun getHumanMove (playerString)
  (format t "Enter a board position (0-8): ")
  (let ((userIO (read)))
    (cond
      ((and (numberp userIO) (and (<= userIO 8) (>= userIO 0)))
        (format t "You entered ~d.~%" userIO) 
        userIO
      )
      ((and (numberp userIO) (and (>= userIO 8) (<= userIO 0)))
        (format t "That was not a number or active board position.")
        (format t "You failed to provide valid input. I win!")
        (quit) ))))

(defun getComputerMove ()
0
)

(defun createNewBoard ()
  (list "H" "H" "U" "U" "U" "U" "U" "U" "U") )

(defun printBoard (board)
  (format t "~%")
  (printRow (nth 0 board) (nth 1 board) (nth 2 board))
  (format t "~& -----------")
  (printRow (nth 3 board) (nth 4 board) (nth 5 board))
  (format t "~& -----------")
  (printRow (nth 6 board) (nth 7 board) (nth 8 board))
  (format t "~%~%") )

(defun printRow (first middle last)
  (format t "~&  ~A | ~A | ~A" first middle last))

(defun determineWhoGoesFirst ()
  (format t "Would you like to go first? Y or N: ")
  (let ((userIO (read)))
    (cond
    ((and (not (string= userIO "Y")) (not (string= userIO "N")))
      (format t "~%That was invalid input!")
      (quit))
    ((string= userIO "Y")
      (format t "~%Human! You are going first!~%")
      t
    )
    ((string= userIO "N")
      (format t "~%YAY! I am going first and am going to beat you!~%")
      nil
    ) )) )

(defun getPlayerString (currentPlayer)
  (if currentPlayer
    "Human"
    "Computer"))

(defun getPlayerAbbr (currentPlayer)
  (if currentPlayer
    "X"
    "O"))

(defun togglePlayer (player)
  (if player
    nil
    t ))

(defun checkRowValues (first middle third)
    (if (or (string= "U" (nth first board)) (string= "U" (nth middle board)) (string= "U" (nth third board)))
      nil
      (and (string= (nth first board) (nth middle board)) (string= (nth middle board) (nth third board)))
    )
)

(defun gameOver? (board)
    (cond 
      ((checkRowValues 0 1 2) nil)
      ((checkRowValues 3 4 5) nil)
      ((checkRowValues 6 7 8) nil)
      ((checkRowValues 0 3 6) nil)
      ((checkRowValues 1 4 7) nil)
      ((checkRowValues 2 5 8) nil)
      ((checkRowValues 0 4 8) nil)
      ((checkRowValues 2 4 6) nil)
      ((member "U" board ) nil)    
      (t t)
    )
)

(defun isCurrentMoveInvalid?Helper (move board)
  (cond 
      ((string= "X" (nth move board)) t)
      ((string= "O" (nth move board)) t)
      (t nil)
  )
)

(defun isCurrentMoveInvalid? (move board) 
  (cond
    ((and (<= move 8) (>= move 0)) nil)
    ((and (> move 8) (< move 0)) 
      (format t "Move ~d was invalid~%")
    )
    ((isCurrentMoveInvalid?Helper move board) nil)
    ((eql move -100) t)                 ; edge case to get through while loop once
  )
)

(defun gameLogic (board currentPlayer)
  (setq currentMove -100)
  (loop while (isCurrentMoveInvalid? currentMove board)
    do
      (if currentPlayer
        (setq currentMove (getHumanMove (getPlayerString currentPlayer)))
        (setq currentMove (getComputerMove) )))
  (setf (nth currentMove board) (getPlayerAbbr currentPlayer))
  board )

(defun main ()
  (format t "~%WELCOME TO ABRAM AND ANDREW'S TIC TAC TOE GAME!~%~%")
  (format t "Prepare to lose!~%~%")
  
  (setq humanMove (determineWhoGoesFirst))
  (setq board (createNewBoard))

  (loop while (gameOver? board)
    do
      (format t (concatenate 'string "Player " (getPlayerString humanMove) " turn!~%"))
      (setq board (gameLogic board humanMove))
      (printBoard board)

      (setq humanMove (togglePlayer humanMove)) )

  (format t (concatenate 'string (string-upcase (getPlayerString (not humanMove))) " WINS!"))
  (quit)
)

(main)
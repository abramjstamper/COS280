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
  (list "U" "U" "U" "U" "U" "U" "U" "U" "U") )

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

(defun gameOver? (board)

t)

(defun isCurrentMoveInvalid? (move) 
  (cond
    ((and (<= move 8) (>= move 0)) nil)
    ((and (> move 8) (< move 0)) t)
    ((eql move -100) t)                 ; edge case to get through while loop once
  )
)

(defun gameLogic (board currentPlayer)
  (setq currentMove -100)
  (loop while (isCurrentMoveInvalid? currentMove)
    do
      (if currentPlayer
        (setq currentMove (getHumanMove (getPlayerString currentPlayer)))
        (setq currentMove (getComputerMove) ))
        (format t "I made it inside") )
  (format t "I made it outside")
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
)

(main)
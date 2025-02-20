;;;; Projeto Adji-boto*
;;;; UC Inteligencia Artificial 2024/2025
;;;; Realizado por:
;;;; Raul Rohjans - 202100518


;; ************************* Helper Functions *************************
(defun get-project-path ()
  "Gets the project path"
  "/home/raul/Documents/GitHub/adji-boto-solver-alfabeta"
)

(defun read-game-entries ()
  "Reads the list of game entries from the file problemas.dat"
  (with-open-file (file (concatenate 'string (get-project-path) "/problemas.dat") 
                        :direction :input :if-does-not-exist :error)
                  (read file)
  )
)

(defun get-game-entry (num entries)
  "Return the game entry corresponding to the num index"
  (nth num entries)
)

(defun select-entry ()
  "Allows the user to select a game entry."
  (let ((entries (read-game-entries))) ;; Read entries first
    (show-available-entries) ;; Display all available entries    

    (format t "~%Choose an entry: ")
    (let ((choice (read)))
      (cond
        ((and (integerp choice) (>= choice 1) (<= choice (length entries)))
         (get-game-entry (1- choice) entries))

        (t
         (format t "Invalid input. Please enter a number between 1 and %D.~%" (length entries))
         (select-entry))
      )
    )
  )
)

(defun select-depth ()
  "Allows the user to select a maximum depth for the DFS algorithm."
  (format t "Max depth: ")
  (let ((depth (read)))
    (cond ((and (integerp depth) (> depth 0)) depth)
          (T (progn (format t "Invalid input.~%")
               (select-depth)))
    )
  )
)


;; ************************* Program Init *************************

(defun init ()
  "Starts the program by importing other files and loading main menu"

  ;; File imports
  (load (concatenate 'string (get-project-path) "/algoritmo.lisp"))
  (load (concatenate 'string (get-project-path) "/puzzle.lisp"))

  ;; Load entries
  (main-menu)
)


;; ************************* Menu Display & Handler Functions *************************

(defun main-menu ()
  "Displays the main menu and handles user input."
  (format t "~%=== Adji-boto* ===")
  (format t "~%1. Solve Game")
  (format t "~%2. Show Available Entries")
  (format t "~%0. Exit")
  (format t "~%Select an option: ")
  (let ((choice (read)))
    (case choice
      (1 (algorithm-menu))
      (2 (show-available-entries))
      (0 (format t "Exiting...~%"))
      (t (format t "Invalid option. Try again.~%") (menu))))
)

(defun algorithm-menu ()
  "Displays the algorithm choices to solve the game entries."
  (format t "~%=== Adji-boto* ===")
  (format t "~%1. MinMax with AlfaBeta")
  (format t "~%0. Exit")
  (format t "~%Select an option: ")
  (let ((choice (read)))
    (case choice
      (1 (start-minmax-alfabeta))
      (0 (format t "Exiting...~%"))
      (t (format t "Invalid option. Try again.~%") (menu))))
)

(defun start-minmax-alfabeta ()
  (let ((entry (select-entry)))
    (let ((depth (select-depth)))
      (format t "~%Starting MinMax AlfaBeta...~%")
      (let ((res (alfabeta (create-node entry) depth)))
        (cond (res res)
              (T (format t "No solution found."))
        )
      )         
    )
  )
)

(defun show-available-entries (&optional (entries (read-game-entries)) (idx 1))
  "Displays the available game entries from problemas.dat"
  (cond ((null entries) (format t "~%Done.~%")) ;; Exit when there are no entries
    ((= idx 1) ;; On the first call print headers
     (format t "~%=== Available Game Entries ===~%")
     (format t "~D: ~A~%" idx (car entries))
     (show-available-entries (cdr entries) (+ idx 1))
    )
    (t
     (format t "~D: ~A~%" idx (car entries))
     (show-available-entries (cdr entries) (+ idx 1))
    )
  )
)


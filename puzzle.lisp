;;;; Projeto Adji-boto*
;;;; UC Inteligencia Artificial 2024/2025
;;;; Realizado por:
;;;; Raul Rohjans - 202100518

;; ************************* Entry Related Functions *************************
(defun empty-entry (entry)
  "Checks if the entry is empty"
  (cond ((null entry) nil)
    ((and (= (apply #'+ (entry-line 0 entry)) 0)
          (= (apply #'+ (entry-line 1 entry)) 0))
    )
  )
)

(defun entry-piece-count (entry) 
   (cond ((null entry) nil)
         (t (+ (apply #'+ (entry-line 0 entry)) (apply #'+ (entry-line 1 entry))))
   )
)


;; ************************* Selector Functions *************************
(defun upper-entry-line (entry)
  "Returns the upper line of a given entry"
  (car entry)
)

(defun lower-entry-line (entry)
  "Returns the lower line of a given entry"
  (cadr entry)
)

(defun entry-line (lineIdx entry)
"Receives the index of the line to return and the entry"
  (cond 
      ((not (listp entry)) nil)
      ((or (< lineIdx 0)  (>= lineIdx (length entry))) nil)
      ((= lineIdx 0) (upper-entry-line entry))
      ((= lineIdx 1) (lower-entry-line entry))
  )
)

(defun entry-cell (idxLine idxCell entry)
  "Gets the indexes of line and cell of the entry"
  (cond 
      ((not (listp entry)) nil)
      ((or (< idxLine 0)  (>= idxLine (length entry))) nil)
      ((or (< idxCell 0)  (>= idxCell (length (upper-entry-line entry)))) nil)
      ((= idxLine 0)  (nth idxCell (entry-line 0 entry)))
      ((= idxLine 1)  (nth idxCell (entry-line 1 entry)))
  )
)


;; ************************* Nodes & States *************************

(defun create-node (entry &optional (parent-node nil) (depth 0) (removed-pieces 0) (heuristic 0))
  "Creates a node object to represent a piece of the game."
  (list entry depth parent-node removed-pieces heuristic)
)

(defun node-parent (node) (nth 2 node))
(defun node-depth (node) (nth 1 node))
(defun node-entry (node) (nth 0 node))
(defun node-removed-pieces (node) (nth 3 node))
(defun node-heuristic (node) (nth 4 node))

(defun node-solution (node)
  "Checks if the entry of a node is empty"
  (empty-entry (node-entry node))
)

(defun create-node-aux (entry pieces &optional (heuristic 0))
  (list entry pieces heuristic)
)

(defun node-aux-entry (node)
  (first node)
)

(defun node-aux-piece (node) 
  (second node)
)


;; ************************* Other Functions *************************
(defun existsp (node closed-nodes) 
  (some (lambda (closed-node) (equal (node-entry node) (node-entry closed-node))) closed-nodes)
)

(defun expand-node (node &optional (operators nil))
  "Expands a node generating its successors"
  (let ((successors (gen-sucessors node)))
    (if operators
        (mapcar #'(lambda (suc) (funcall operators suc)) successors)
        successors)
  )
)

(defun replace-pos (pos line piece)
  "Replaces a given piece in a given position of a line entry"
  (cond ((null line) line)
    ((= pos 0) (cons piece (cdr line)))
    (t (cons (car line) (replace-pos (1- pos) (cdr line) piece)))
  )
)

(defun replace-piece (line cell entry &optional (piece 0))
  "Replaces a piece in a specific position of the entry"
  (cond ((eq line 0) (list (replace-pos cell (car entry) piece) (cadr entry)))
    ((eq line 1) (list (car entry) (replace-pos cell (cadr entry) piece)))
    (t entry))
)

(defun replace-aux (line cell entry &optional (piece 0))
  (create-node-aux (replace-piece line cell (node-aux-entry entry) piece) (node-aux-piece entry))
)

(defun increment-pos (idx entry)
  "Increments the value in a given position of the entry"
  (cond  ((null entry) nil)
    ((= idx 0) (cons (1+ (car entry)) (cdr entry)))
    (t (cons (car entry) (increment-pos (1- idx) (cdr entry))))
  )
)

(defun replace-cell (i j entry)
  "Replaces a cell in the entry"
  (cond ((= i 0) (list (increment-pos j (upper-entry-line entry)) (lower-entry-line entry)))
    ((= i 1) (list (upper-entry-line entry) (increment-pos j (lower-entry-line entry))))
    (t nil))
)

;; ************************* Operators *************************

(defun validate-rule (x) 
  (cond 
    ((null x) nil)
    ((OR (= x 1) (= x 3) (= x 5)) T)
    (t nil)
  )
)

(defun validate-rule-aux (i j entry)
  (cond 
    ((validate-rule (entry-cell i j entry))  (create-node-aux (replace-piece i  j entry 0) (entry-cell i j entry)))
    (t (create-node-aux entry 0))
  )
)

(defun distribute-pieces (i j entry &optional (n (1+ (entry-cell i j entry))) (op #'1+) (size (length (car entry))))
  "Spreads the pieces throughout the entry"
  (cond
    ((= n 0) (if (eq i 0) (validate-rule-aux i (1+ j) entry) (validate-rule-aux i (1- j) entry)))
    ((and (= i 1) (>= j size)) (distribute-pieces 0 (1- size) entry n #'1-))
    ((and (= i 0) (< j 0)) (distribute-pieces 1 0 entry n #'1+))
    ((and (= i 0) (eq op #'1+)) (distribute-pieces i j entry n #'1-))
    (t (distribute-pieces i (funcall op j) (replace-cell i j entry) (1- n)))
  )
)

(defun gen-sucessors (node &optional (heuristic #'default-heuristic))
  "Generates the successors of a given node using the selected heuristic"
  (let ((entry (node-entry node)))
    (mapcar #'(lambda (nEntry) 
                (if (null nEntry) nil
                  (create-node (first nEntry)
                               node
                               (1+ (node-depth node))
                               (+ (node-removed-pieces node) (second nEntry))
                               (funcall heuristic (list (first nEntry) (+ (node-removed-pieces node) (second nEntry))))))) ;; Apply heu dynamically
            (mapcar #'(lambda (pos) 
                        (replace-aux (car pos) (cadr pos) (distribute-pieces (car pos) (cadr pos) entry)))
                    (rows-w-pieces entry))
    )
  )
)



(defun rows-w-pieces (entry &optional (i 0))
  "Returns a row list with the pieces of the entry"
  (cond
    ((null entry) nil)
    (t (append (mapcar #'(lambda (j) (list i j)) (col-w-pieces (car entry)))
               (rows-w-pieces (cdr entry) (1+ i)))
    )
  )
)

(defun col-w-pieces (line &optional (j 0))
  "Gets the indexes of the cols with pieces in a given line"
  (cond ((null line) nil)
    ((= (car line) 0) (col-w-pieces (cdr line) (1+ j)))
    (t (cons j (col-w-pieces (cdr line) (1+ j))))
  )
)
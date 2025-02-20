;;;; Projeto Adji-boto*
;;;; UC Inteligencia Artificial 2024/2025
;;;; Realizado por:
;;;; Raul Rohjans - 202100518

(defun default-heuristic (node)
  (cond ((null node) 0)
    (t (- (entry-piece-count (node-entry node)) (cadr node)))
  )
)

;; ************************* MinMax AlfaBeta *************************

(defun alfabeta (state depth &optional (alfa most-negative-fixnum) (beta most-positive-fixnum) (maximize t)
                    (checked-nodes 0) (alfa-prunes 0) (beta-prunes 0))
  "Implements the MinMax algorithm with Alpha-Beta pruning."
  (let ((start-time (get-internal-real-time)))
    (if (or (node-solution state) (<= depth 0))
        ;; Terminal state reached
        (let ((heur (default-heuristic state))
              (elapsed-time (/ (- (get-internal-real-time) start-time)
                               (float internal-time-units-per-second))))
          (log-terminal-state state depth heur elapsed-time)
          heur) ;; Returns heuristic value
        ;; Process children
        (let* ((top-value (if maximize most-negative-fixnum most-positive-fixnum))
               (children (expand-node state))) 
          (labels ((process-node (children top-value alfa beta checked-nodes alfa-prunes beta-prunes)
                     (let ((elapsed-time nil))
                       (if (null children)
                           ;; Log final state and return value
                           (progn
                             (setq elapsed-time (/ (- (get-internal-real-time) start-time)
                                                   (float internal-time-units-per-second)))
                             (log-final-state state top-value checked-nodes alfa-prunes beta-prunes elapsed-time)
                             top-value)
                           ;; Process next child
                           (let* ((child (car children))
                                  (checked-nodes (1+ checked-nodes))
                                  (value (if maximize
                                             (max top-value
                                                  (alfabeta child (1- depth) alfa beta nil 
                                                            checked-nodes alfa-prunes beta-prunes))
                                             (min top-value
                                                  (alfabeta child (1- depth) alfa beta t 
                                                            checked-nodes alfa-prunes beta-prunes))))
                                  (new-alfa (if maximize (max alfa value) alfa))
                                  (new-beta (if (not maximize) (min beta value) beta)))
                             ;; Handle pruning cases
                             (cond
                               ((and maximize (>= new-alfa beta))
                                (log-prune "Beta" child new-alfa beta)
                                (process-node nil value new-alfa beta checked-nodes alfa-prunes (1+ beta-prunes)))
                               ((and (not maximize) (<= new-beta alfa))
                                (log-prune "Alpha" child alfa new-beta)
                                (process-node nil value new-alfa beta checked-nodes (1+ alfa-prunes) beta-prunes))
                               (t
                                (process-node (cdr children) value new-alfa new-beta checked-nodes alfa-prunes beta-prunes))))))))
            (process-node children top-value alfa beta checked-nodes alfa-prunes beta-prunes))))))


(defun log-terminal-state (state depth heur elapsed-time)
  "Logs and prints when the algorithm reaches a terminal state."
  (let ((log-path "/home/raul/Documents/GitHub/adji-boto-solver-alfabeta/log.dat"))
    (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format log "~%========================================~%")
      (format log "TERMINAL STATE ACHIEVED~%")
      (format log "========================================~%")
      (format log "Time Elapsed: ~,6f seconds~%" elapsed-time)
      (format log "Search Depth: ~A~%" depth)
      (format log "Heuristic Value: ~A~%" heur)
      (format log "Game State:~%    ~A~%" (node-entry state))
      (format log "========================================~%~%"))
    
    ;; Print to console
    (format t "~%========================================~%")
    (format t "TERMINAL STATE ACHIEVED~%")
    (format t "========================================~%")
    (format t "Time Elapsed: ~,6f seconds~%" elapsed-time)
    (format t "Search Depth: ~A~%" depth)
    (format t "Heuristic Value: ~A~%" heur)
    (format t "Game State:~%    ~A~%" (node-entry state))
    (format t "========================================~%~%")))


(defun log-final-state (state best-value checked-nodes alfa-prunes beta-prunes elapsed-time)
  "Logs and prints the final state of the search."
  (let ((log-path "/home/raul/Documents/GitHub/adji-boto-solver-alfabeta/log.dat"))
    (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format log "~%========================================~%")
      (format log "FINAL STATE EVALUATION~%")
      (format log "========================================~%")
      (format log "Best Evaluated Value: ~A~%" best-value)
      (format log "Nodes Analyzed: ~A~%" checked-nodes)
      (format log "Alpha Prunes: ~A | Beta Prunes: ~A~%" alfa-prunes beta-prunes)
      (format log "Time Elapsed: ~,6f seconds~%" elapsed-time)
      (format log "Final Game State:~%    ~A~%" (node-entry state))
      (format log "========================================~%~%"))
    
    ;; Print to console
    (format t "~%========================================~%")
    (format t "FINAL STATE EVALUATION~%")
    (format t "========================================~%")
    (format t "Best Evaluated Value: ~A~%" best-value)
    (format t "Nodes Analyzed: ~A~%" checked-nodes)
    (format t "Alpha Prunes: ~A | Beta Prunes: ~A~%" alfa-prunes beta-prunes)
    (format t "Time Elapsed: ~,6f seconds~%" elapsed-time)
    (format t "Final Game State:~%    ~A~%" (node-entry state))
    (format t "========================================~%~%")))


(defun log-prune (type state alfa beta)
  "Logs and prints when an Alpha or Beta cut-off occurs."
  (let ((log-path "/home/raul/Documents/GitHub/adji-boto-solver-alfabeta/log.dat"))
    (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format log "~%========================================~%")
      (format log "PRUNING EVENT DETECTED~%")
      (format log "========================================~%")
      (format log "Prune Type: ~A~%" type)
      (format log "Pruned State:~%    ~A~%" (node-entry state))
      (format log "Alpha: ~A | Beta: ~A~%" alfa beta)
      (format log "========================================~%~%"))
    
    ;; Print to console
    (format t "~%========================================~%")
    (format t "PRUNING EVENT DETECTED~%")
    (format t "========================================~%")
    (format t "Prune Type: ~A~%" type)
    (format t "Pruned State:~%    ~A~%" (node-entry state))
    (format t "Alpha: ~A | Beta: ~A~%" alfa beta)
    (format t "========================================~%~%")))

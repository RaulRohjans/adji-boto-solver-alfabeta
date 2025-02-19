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

;(defun alfabeta (state depth &optional (alfa most-negative-fixnum) (beta most-positive-fixnum) (maximize t)
;                    (checked-nodes 0) (alfa-prunes 0) (beta-prunes 0))
;  "Implements the MinMax algorithm with Alpha-Beta pruning and detailed reporting."
;  (let ((start-time (get-internal-real-time))
;        (log-path "/home/raul/Documents/GitHub/adji-boto-solver-alfabeta/log.dat"))
;    (if (or (node-solution state) (<= depth 0))
;        ;; state terminal ou depth alcan�ada
;        (let ((heur (default-heuristic state))
;              (elapsed-time (/ (- (get-internal-real-time) start-time)
;                               (float internal-time-units-per-second))))
;          ;; Log para state terminal
;          (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
;            (format log "Terminal State Reached~%")
;            (format log "State: ~A~%Depth: ~A~%Heuristic: ~A~%Time: ~,6f seconds~%~%" 
;                    (node-entry state) depth heur elapsed-time))
;          ;; Imprime no ecr�
;          (format t "Terminal State Reached~%")
;          (format t "State: ~A~%Depth: ~A~%Heuristic: ~A~%Time: ~,6f seconds~%~%" 
;                  (node-entry state) depth heur elapsed-time)
;          heur) ;; Retorna o value heur�stico
;        ;; Processa filhos
;        (let* ((top-value (if maximize most-negative-fixnum most-positive-fixnum))
;               (children (expand-node state))) ;; Removida vari�vel `corte`
;          (labels ((process-node (children top-value alfa beta checked-nodes alfa-prunes beta-prunes)
;                     ;; Declara��o local expl�cita de `elapsed-time`
;                     (let ((elapsed-time nil))
;                       (if (null children)
;                           ;; Fim da expans�o
;                           (progn
;                             ;; Log final para o state atual
;                             (setq elapsed-time (/ (- (get-internal-real-time) start-time)
;                                                   (float internal-time-units-per-second)))
;                             (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
;                               (format log "Final State: ~A~%Best Value: ~A~%Nodes Analyzed: ~A~%Alpha Cuts: ~A~%Beta Cuts: ~A~%Time: ~,6f seconds~%~%" 
;                                       (node-entry state) top-value checked-nodes alfa-prunes beta-prunes elapsed-time))
;                             ;; Imprime no ecr�
;                             (format t "Final State: ~A~%Best Value: ~A~%Nodes Analyzed: ~A~%Alpha Cuts: ~A~%Beta Cuts: ~A~%Time: ~,6f seconds~%~%" 
;                                     (node-entry state) top-value checked-nodes alfa-prunes beta-prunes elapsed-time)
;                             top-value)
;                           ;; Processa o pr�ximo filho
;                           (let* ((child (car children))
;                                  (checked-nodes (1+ checked-nodes))
;                                  (value (if maximize
;                                             (max top-value
;                                                  (alfabeta child (1- depth) alfa beta nil 
;                                                            checked-nodes alfa-prunes beta-prunes))
;                                             (min top-value
;                                                  (alfabeta child (1- depth) alfa beta t 
;                                                            checked-nodes alfa-prunes beta-prunes))))
;                                  (novo-alfa (if maximize (max alfa value) alfa))
;                                  (novo-beta (if (not maximize) (min beta value) beta)))
;                             ;; Verifica se houve corte
;                             (cond
;                               ((and maximize (>= novo-alfa beta))
;                                (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
;                                  (format log "Cut-off (Beta) at State: ~A~%Alpha: ~A, Beta: ~A~%~%" 
;                                          (node-entry child) novo-alfa beta))
;                                ;; Imprime no ecr�
;                                (format t "Cut-off (Beta) at State: ~A~%Alpha: ~A, Beta: ~A~%~%" 
;                                        (node-entry child) novo-alfa beta)
;                                (process-node nil value novo-alfa beta checked-nodes alfa-prunes (1+ beta-prunes)))
;                               ((and (not maximize) (<= novo-beta alfa))
;                                (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
;                                  (format log "Cut-off (Alpha) at State: ~A~%Alpha: ~A, Beta: ~A~%~%" 
;                                          (node-entry child) alfa novo-beta))
;                                ;; Imprime no ecr�
;                                (format t "Cut-off (Alpha) at State: ~A~%Alpha: ~A, Beta: ~A~%~%" 
;                                        (node-entry child) alfa novo-beta)
;                                (process-node nil value novo-alfa beta checked-nodes (1+ alfa-prunes) beta-prunes))
;                               (t
;                                (process-node (cdr children) value novo-alfa novo-beta checked-nodes alfa-prunes beta-prunes))))))))
;            (process-node children top-value alfa beta checked-nodes alfa-prunes beta-prunes))))))

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
      (format log "Terminal State Reached~%")
      (format log "State: ~A~%Depth: ~A~%Heuristic: ~A~%Time: ~,6f seconds~%~%" 
              (node-entry state) depth heur elapsed-time))
    ;; Print to console
    (format t "Terminal State Reached~%")
    (format t "State: ~A~%Depth: ~A~%Heuristic: ~A~%Time: ~,6f seconds~%~%" 
            (node-entry state) depth heur elapsed-time)))

(defun log-final-state (state best-value checked-nodes alfa-prunes beta-prunes elapsed-time)
  "Logs and prints the final state of the search."
  (let ((log-path "/home/raul/Documents/GitHub/adji-boto-solver-alfabeta/log.dat"))
    (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format log "Final State: ~A~%Best Value: ~A~%Nodes Analyzed: ~A~%Alpha Cuts: ~A~%Beta Cuts: ~A~%Time: ~,6f seconds~%~%" 
              (node-entry state) best-value checked-nodes alfa-prunes beta-prunes elapsed-time))
    ;; Print to console
    (format t "Final State: ~A~%Best Value: ~A~%Nodes Analyzed: ~A~%Alpha Cuts: ~A~%Beta Cuts: ~A~%Time: ~,6f seconds~%~%" 
            (node-entry state) best-value checked-nodes alfa-prunes beta-prunes elapsed-time)))

(defun log-prune (type state alfa beta)
  "Logs and prints when an Alpha or Beta cut-off occurs."
  (let ((log-path "/home/raul/Documents/GitHub/adji-boto-solver-alfabeta/log.dat"))
    (with-open-file (log log-path :direction :output :if-exists :append :if-does-not-exist :create)
      (format log "Cut-off (~A) at State: ~A~%Alpha: ~A, Beta: ~A~%~%" type (node-entry state) alfa beta))
    ;; Print to console
    (format t "Cut-off (~A) at State: ~A~%Alpha: ~A, Beta: ~A~%~%" type (node-entry state) alfa beta)))

(defpackage :pf-chain-climber
  (:use :common-lisp :engine #+sbcl sb-mop #+clisp clos #+clisp mop))

(in-package :pf-chain-climber)

; Set up globals
(defparameter *pfccmonsters* '()) ;TODO: this list is incomplete!
(defparameter *playername* nil)
(defparameter *playerxp* 0)
(defparameter *playerkills* 0)
(defparameter *playerform* nil)

; Initialize Monsters
(defun init-monsters (monsters)
  (let ((crs (remove-duplicates (mapcar #'cr monsters) :test #'equal)))
    (mapcar #'(lambda (x) (cons x (list (remove-if-not #'(lambda (y) (= x (cr y)))
						       monsters))))
	    crs)))

(setf *pfccmonsters* (sort (init-monsters engine:*monsters*) #'< :key #'car))

; Get a name, set up save(?)
(defun get-player-name ()
  (let ((name (read-line)))
    (setf *playername* name)))

; Pick/make a monster for the PC to start as
(defun set-starting-form (monsters)
  (let* ((forms (cadar monsters))
	 (temp-player (make-actor :class-name (class-name 
					       (class-of 
						(nth (random (length forms)) forms))) 
				  :name *playername*)))
    (setf *playerform* (change-class temp-player
				     (make-anonymous 'engine:game-class
						     (list (class-of temp-player)
							   (find-class 'player)))))))

; Pick/make a monster for the PC to face off against first
; TODO: copy it, don't change the global list
(defun find-opponent (pc-form monsters)
  (let* ((pc-cr (cr pc-form))
	 (valid-opponents (append (cadr (assoc pc-cr monsters)) 
				  (cadr (find pc-cr monsters :test #'< :key #'car)))))
    (let ((temp-opponent (nth (random (length valid-opponents))
			      valid-opponents)))
      (change-class temp-opponent (make-anonymous 'engine:game-class
						  (list (class-of temp-opponent)
							(find-class 'npc)))))))

; Fight!
; TODO: determine who won, save history, etc.
(defun run-fight (pc monsters)
  (run-combat pc (find-opponent pc monsters)))

; Fix PC up


; In case of victory, offer PC new monster to control
; In case of PC death, go back to beginning (record score [fights won, final form])
; Repeat

(defpackage :pf-chain-climber
  (:use :common-lisp :engine #+sbcl sb-mop #+clisp clos #+clisp mop))

(in-package :pf-chain-climber)

; Set up globals
(defparameter *pfccmonsters* '())
(defparameter *playername* nil)
(defparameter *playerxp* 0)
(defparameter *playerkills* '()) ; TODO: record crits? Damage dealt/taken?
(defparameter *playerform* nil)

; Initialize Monsters
;TODO: this list is incomplete!
(defun init-monsters (monsters)
  (let ((crs (remove-duplicates (mapcar #'cr monsters) :test #'equal)))
    (mapcar #'(lambda (x) (cons x (list (remove-if-not #'(lambda (y) (= x (cr y)))
						       monsters))))
	    crs)))

; Get a name, set up save(?)
(defun get-player-name ()
  (format t "Enter your name: ")
  (let ((name (read-line)))
    (setf *playername* name)))

; Pick/make a monster for the PC to start as
(defun make-form (form &optional pc)
  (make-instance (make-anonymous 'engine:game-class 
				 (list (class-of form)
				       (find-class (if pc 'player 'npc))))
		 :name (if pc *playername* (name form))))

(defun set-starting-form (monsters)
  (let* ((forms (remove-if-not #'(lambda (x) (melee x)) (cadar monsters)))
	 (chosen (nth (random (length forms)) forms)))
    (setf *playerform* (make-form chosen t))))
;    (setf *playerform* (change-class temp-player
;				     (make-anonymous 'engine:game-class
;						     (list (class-of temp-player)
;							   (find-class 'player)))))))

; Pick/make a monster for the PC to face off against first
; TODO: copy it, don't change the global list
(defun find-opponent (pc-form monsters)
  (let* ((pc-cr (cr pc-form))
	 (valid-opponents (append (cadr (assoc pc-cr monsters)) 
				  (cadr (find pc-cr monsters :test #'< :key #'car)))))
    (nth (random (length valid-opponents))
	 valid-opponents)))
;    (let ((temp-opponent (nth (random (length valid-opponents))
;			      valid-opponents)))
;      (make-instance (make-anonymous 
;		      'engine:game-class
;		      (list (class-of temp-opponent)
;			    (find-class 'npc)))
;		     :name (name temp-opponent)))))

; Fight!
; TODO: determine who won, save history, etc.
(defun run-fight (pc badguys)
  (multiple-value-bind (opponents events) 
      (if (listp badguys) 
	  (apply #'run-combat (cons pc badguys))
	  (run-combat pc badguys))
    (update-stats (remove pc opponents))
    (values opponents events)))

(defun player-wins? (pc)
  (if (or (dying? pc) (dead? pc) (unconscious? pc))
      ; PC lost; 
      ; TODO: save their high score
      (format t "You lose!~%")
      t))

(defun update-stats (opponents)
  (mapcar #'(lambda (x) (let ((creature (car x)))
			  (if (or (dying? creature) (dead? creature) (stable? creature))
			      (progn (appendf *playerkills* (list creature))
				     (incf *playerxp* (xp creature))))))
	  opponents))

; In case of victory, offer PC new monster to control
(defun offer-forms (pc forms)
  (let ((form-choice (combat-choice pc 
				    (append (if (listp forms) forms (list forms)) 
					    '("Keep current form")) 
				    :prompt "Take a new form?")))
    (format t "You chose ~A.~%" form-choice)
    (when (not (stringp form-choice))
      form-choice)))

; Fix PC up if PC kept form
(defun restore-creature (creature)
  (engine:clear-conditions creature)
  (engine:restore-hp creature))

; In case of PC death, go back to beginning (record score [fights won, final form])

; Repeat
(defun setup-game ()
  (setf *playerkills* '())
  (setf *playerxp* 0)
  (when (not *playername*) (get-player-name))
  (setf *pfccmonsters* (sort (init-monsters (copy-list engine:*monsters*)) #'< :key #'car))
  (set-starting-form *pfccmonsters*))

(defun run-round ()
  (let* ((opponent-forms (find-opponent *playerform* *pfccmonsters*))
	 (opponents (if (listp opponent-forms)
			(mapcar #'make-form opponent-forms)
			(make-form opponent-forms))))
    (multiple-value-bind (participants events)
	(run-fight *playerform* opponents)
      (when (player-wins? *playerform*)
	  (let ((newform (offer-forms *playerform* 
				      opponent-forms)))
	    (when newform
	      (setf *playerform* (make-form newform t))))
	  (mapcar #'(lambda (x) (restore-creature (car x))) participants)
	  (format t "Participants: ~A~%" participants)
	  *playerform*))))

(defun play ()
  (setup-game)
  (loop for continue? = t
        for alive? = t
        do (setf alive? (run-round))
           (when alive? (format t "Continue? ")
	         (setf continue? (read)))
        until (or (not alive?) (not continue?) (string= continue? "n"))))

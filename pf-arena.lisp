(defpackage :pf-arena
  (:use :common-lisp :engine #+sbcl sb-mop #+clisp clos #+clisp mop)
  (:export pfcc-player difficulty kills xp-earned restore-creature
	   *pfccmonsters* *difficultymap* play pick-starting-form init-monsters 
	   change-form make-form update-form valid-name play))

(in-package :pf-arena)

;;; Set up player & default settings
(defgameobject pfcc-player ()
  ((difficulty :initarg :difficulty
	       :initform :normal
	       :accessor difficulty)
   (kills :initarg :kills
	  :initform nil
	  :accessor kills)
   (xp-earned :initarg :xp-earned
	      :initform 0
	      :accessor xp-earned)))

(defgameobject console-player (pfcc-player)
  ())

(defmethod engine:output (message)
  "The default output method for a console player"
  (declare (ignore history))
  (format t "~A" message))

(defmethod engine:prompt-input ((it console-player) &key prompt &allow-other-keys)
  (when prompt (output prompt))
  (read-line))

(defmethod engine:select ((it console-player) choices &key display prompt)
  (cond ((null choices) "")
	((and (= (length choices) 1) (typep (car choices) 'engine:actor)) (car choices))
	(t 
	 (do (n)
	     ((and n (integerp n) (<= 0 n (1- (length choices))))
	      (nth n choices))
	   (when prompt (output (format nil "~&~a~%" prompt)))
	   (loop for n from 1
	      for c in choices
	      do (let ((printer (find-method #'pretty-print 
					     '()
					     (list (class-of c))
					     nil)))
		   (output (format nil 
				   "  ~d) ~a~%" 
				   n 
				   (cond (display (funcall display c))
					 (printer (pretty-print c))
					 (t c))))))
	   (force-output)
	   (setf n (parse-integer (read-line *standard-input* nil)
				  :junk-allowed t))
	   (if (not (integerp n))
	       (output "Invalid selection. ")
	       (decf n))))))
  
(defparameter *difficultymap* '((:easy . :max) (:normal . :half) (:hard . :rolled)))
(defparameter *pc-class* 'console-player)
(defparameter *pfccmonsters* '())
;; TODO: record crits? Damage dealt/taken?

;; TODO: this monster list is incomplete!
(defun init-monsters (monsters)
  "Initialize the monsters list for a game"
  (let ((crs (remove-duplicates (mapcar #'engine:cr monsters) :test #'equal)))
    (mapcar #'(lambda (x) (cons x (list (remove-if-not #'(lambda (y) (= x (engine:cr y)))
						       monsters))))
	    crs)))

(defun valid-name (name)
  (and (< 0 (length name) 12)
       (stringp name)
       (every #'(lambda (x) (or (alphanumericp x) (char= #\Space x))) name)
       (notevery #'(lambda (x) (char= #\Space x)) name)))

;;; Get a name, set up save(?)
;; TODO: should get-player-name be a generic?
(defun get-player-name (player)
  "Get the player's name"
  (let ((name (prompt-input player 
			    :prompt "Enter your name: "
			    :validation #'valid-name)))
    (setf (name player) name)))

;;; Pick/make a monster for the PC to start as
(defun make-form (form)
  "Make an instance of a monster form based on a prototypal form"
  (when form
    (make-instance (engine:make-anonymous 'engine:game-class 
					  (list (class-of form)
						(find-class 'engine:npc)))
		   :name (engine:name form))))

(defgeneric update-form 
    "Update the player with a new form"
  (currentform newform))
(defmethod update-form ((it console-player) newform)
  (change-form it newform 'console-player))

(defun change-form (oldform newform base)
  "Change the classes of a player form, keeping the base player class"
  (change-class oldform 
		(engine:make-anonymous 'engine:game-class
				       (list (find-class base)
					     (find-class 'engine:player)
					     (class-of newform)))
		:hp-method (cdr (assoc (difficulty oldform) *difficultymap*))))

(defun pick-starting-form (monsters)
  "Choose a starting form for the player"
  (let ((forms (remove-if-not #'(lambda (x) (melee x)) (cadar monsters))))
    (nth (random (length forms)) forms)))

(defun set-starting-form (player monsters)
  "Set the player's starting form"
  (update-form player (pick-starting-form monsters)))

;; Pick/make a monster for the PC to face
;; TODO: copy it, don't change the global list
(defun find-opponent (pc-form monsters)
  "Choose an opponent (monster) for the PC to face"
  (let* ((pc-cr (engine:cr pc-form))
	 (valid-opponents (append (cadr (assoc pc-cr monsters)) 
				  (cadr (find pc-cr monsters :test #'< :key #'car)))))
;    (warn "PC CR: ~d, opponents: ~A" pc-cr valid-opponents)
    (when valid-opponents 
      (nth (random (length valid-opponents))
	   valid-opponents))))

;;; Fight!
;; TODO: save history, etc.
(defun run-fight (pc badguys)
  "Run a fight between a player (pc) and one or more opponents"
  (multiple-value-bind (opponents events) 
      (if (listp badguys) 
	  (apply #'run-combat (cons pc badguys))
	  (run-combat pc badguys))
    (update-stats pc badguys events)
    (values opponents events)))

(defun player-wins? (pc history)
  "Determine from the history whether or not the player won a fight"
  (if (engine:conditions-active? pc '(engine:condition-dying 
				      engine:condition-dead 
				      engine:condition-unconscious) 
				 history :test :some)
      ; PC lost; 
      ; TODO: save their high score
      (output "You lose!")
      t))

(defun update-stats (player opponents history)
  "Update the player's kills and experience"
  (mapcar #'(lambda (creature) 
	      (when (not (engine:conditions-active? player '(engine:condition-dying 
							     engine:condition-dead 
							     engine:condition-unconscious) 
						    history :test :some))
		(progn (setf (kills player) (append (list creature) (kills player)))
		       (incf (xp-earned player) (xp creature)))))
	  (if (listp opponents)
	      opponents
	      (list opponents))))

;; In case of victory, offer PC new monster to control
(defun offer-forms (pc forms)
  "Offer the form just defeated to the player"
  (let* ((existing-form (format nil "Keep current form (~A)" (form-name pc)))
	 (form-choice (select pc 
			      (append (if (listp forms) forms (list forms)) 
				      (list existing-form "Restart")) 
			      :display #'(lambda (x) (if (find-method #'form-name
								      '()
								      (list (class-of x))
								      nil)
							 (string-capitalize (form-name x))
							 x))
			      :prompt "Take a new form?")))
    (cond ((equal form-choice existing-form) nil)
	  ((equal form-choice "Restart") "Restart")
	  (t form-choice))))

(defun setup-game (player)
  "Set up a new game"
  (when (or (not (name player))
	    (string= "" (name player)))
    (get-player-name player))
  (setf (difficulty player) (select player (mapcar #'car *difficultymap*) :prompt "Difficulty:"))
  (setf *pfccmonsters* (sort (init-monsters (copy-list engine:*monsters*)) #'< :key #'car))
  (set-starting-form player *pfccmonsters*))


(defun setup-console-game (&optional player)
  "Set up a game for play via the console"
  (let ((player (or player (make-instance 'console-player))))
    (setup-game player)))

(defun run-round (player)
  "Run one round of the fight"
  (let* ((opponent-forms (find-opponent player *pfccmonsters*))
	 (opponents (if (listp opponent-forms)
			(mapcar #'make-form opponent-forms)
			(make-form opponent-forms))))
    (when opponents 
      (multiple-value-bind (participants events)
	  (run-fight player opponents)
	(when (player-wins? player events)
	  (let ((newform (offer-forms player 
				      opponent-forms)))
	    (when (and newform (not (stringp newform)))
	      (update-form player newform))
	    (when (not (stringp newform))
	      player)))))))

(defun play (player)
  "Play the game (shouldn't be called directly)"
  (when player
    (loop for continue? = t
       for alive? = t
       do (setf alive? (run-round player))
       until (not alive?))))

;; TODO: In case of PC death, go back to beginning (record score [fights won, final form])
(defun play-console ()
  "Play the game from the console"
  (let ((player (setup-console-game)))
    (play player)))

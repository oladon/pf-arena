;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;(ql:update-dist "quicklisp")
;(mapcar #'ql:quickload
;        '("alexandria" "parser-combinators" "split-sequence"
;          "cl-ppcre" "fset" "cl-charms" "swank" "unix-options"))

(defpackage #:chain-climber-asd
  (:use :cl :asdf))

(in-package :chain-climber-asd)

(defsystem chain-climber
  :name "chain-climber"
  :version "0.0.0"
  :maintainer "Oladon"
  :author "Oladon"
  :description "A text-based game."
  :long-description "A text-based game where you fight your way up the food chain."
  :components ((:file "chain-climber")))


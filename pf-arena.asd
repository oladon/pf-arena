;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(ql:update-dist "quicklisp")
(ql:quickload 'engine)

(defpackage #:pf-arena-asd
  (:use :cl :asdf))

(in-package :pf-arena-asd)

(defsystem pf-arena
  :name "pf-arena"
  :version "0.0.1"
  :maintainer "Oladon"
  :author "Oladon"
  :description "A text-based game."
  :long-description "A text-based game where you fight your way up the food chain."
  :components ((:file "pf-arena")))

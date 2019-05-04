;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:dodgeball-asd
  (:use :cl :asdf))

(in-package :dodgeball-asd)

(asdf:defsystem "dodgeball"
  :name "dodgeball"
  :version "0.0.1"
  :author "Maris Orbidans"
  :licence "Public Domain"
  :serial t
  :components ((:module "src"
		:serial t
		:components ((:file "dodgeball"))))
  :in-order-to ((test-op (test-op "dodgeball/tests"))))

(asdf:defsystem "dodgeball/tests"
  :licence "Public Domain"
  :depends-on (:dodgeball
	       :alexandria
	       :check-it
	       :fiasco)
  :serial t
  :components ((:module "tests"
		:components ((:file "dodgeball-tests"))))
  :perform (test-op (o c) (uiop:symbol-call 'fiasco 'all-tests)))

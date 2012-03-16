;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :tiny-clos
  (:use)
  (:export))

(defpackage :tiny-clos.internal
  (:use :tiny-clos :cl :fiveam :srfi-5)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :map :lambda :loop :member :assoc
           :list* :every
           :remove
           :union
           :remove-duplicates
           :class-of
           :make-method
           :allocate-instance
           :add-method
           :sequence))

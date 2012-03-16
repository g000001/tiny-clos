;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :tiny-clos
  (:use)
  (:export
   :make-class
   :make-generic
   :make-method
   :add-method
   :make
   :initialize
   :allocate-instance
   :compute-getter-and-setter
   :compute-cpl
   :compute-slots
   :compute-apply-generic
   :compute-methods
   :compute-method-more-specific?
   :compute-apply-methods
   :applicable?
   :more-specific?
   :make-primitive-class
   :slot-ref
   :slot-set!
   :class-of
   :get-field
   :set-field!
   :lookup-slot-info
   :class-direct-slots
   :class-direct-supers
   :class-slots
   :class-cpl
   :generic-methods
   :method-specializers
   :method-procedure
   :<primitive-class>
   :<class>
   :<top>
   :<object>
   :<procedure-class>
   :<entity-class>
   :<generic>
   :<method>
   :<boolean>
   :<symbol>
   :<char>
   :<vector>
   :<pair>
   :<number>
   :<string>
   :<procedure> ))

(defpackage :tiny-clos.internal
  (:use :tiny-clos :cl :fiveam :srfi-5)
  (:shadowing-import-from :srfi-5 :let)
  (:shadowing-import-from :srfi-23 :error)
  (:shadow :map :lambda :loop :member :assoc
           :list* :every
           :remove
           :union
           :remove-duplicates
           :sequence)
  (:shadowing-import-from :tiny-clos
                          :class-of
                          :make-method
                          :allocate-instance
                          :add-method))

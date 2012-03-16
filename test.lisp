(cl:in-package :tiny-clos.internal)

(def-suite tiny-clos)

(in-suite tiny-clos)

; **********************************************************************
; Copyright (c) 1992 Xerox Corporation.
; All Rights Reserved.
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; **********************************************************************
;
; Some simple examples of using Tiny CLOS and its MOP.
;
; Much of this stuff corresponds to stuff in AMOP (The Art of the
; Metaobject Protocol).
;

;***
;
; This is a useful sort of helper function.  Note how it uses the
; introspective part of the MOP.  The first few pages of chapter
; two of the AMOP discuss this.
;
; Note that this introspective MOP doesn't support back-links from
; the classes to methods and generic functions.  Is that worth adding?
;
;
(defun initialize-slots (object initargs)
  (let ((not-there (list 'shes-not-there)))
    (for-each (lambda (slot)
                (let ((name (car slot)))
                  (let ((value  (getl initargs name not-there)))
                    (if (eq? value not-there)
                        'do-nothing
                        (slot-set! object name value)))))
              (class-slots (class-of object)))))

;***
;
; A simple class, just an instance of <class>.  Note that we are using
; make and <class> rather than make-class to make it.  See Section 2.4
; of AMOP for more on this.
;
;

(defparameter <pos>
  (make <class>                          ;[make-class
        'direct-supers (list <object>)   ;  (list <object>)
        'direct-slots  (list 'x 'y)))    ;  (list 'x 'y)]

(add-method #'initialize
            (make-method (list <pos>)
                         (lambda (call-next-method pos initargs)
                           (funcall call-next-method)
                           (initialize-slots pos initargs))))

(defparameter p1 (make <pos> 'x 1 'y 2))
(defparameter p2 (make <pos> 'x 3 'y 5))

(test simple-class
  (is (= 1 (slot-ref p1 'x)))
  (is (= 2 (slot-ref p1 'y)))
  (is (= 3 (slot-ref p2 'x)))
  (is (= 5 (slot-ref p2 'y))))


;***
;
; Another way of writing that class definition, that achives better
; `encapsulation' by using slot names that are unique keys, rather
; than symbols.
;
;

(defparameter <pos> :unbound)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function pos-x (make-generic))
  (define-function pos-y (make-generic))
  (define-function move  (make-generic)))

(let ((x (vector 'x))
      (y (vector 'y)))

  (set! <pos> (make <class>
		    'direct-supers (list <object>)
		    'direct-slots  (list x y)))

  (add-method #'pos-x
      (make-method (list <pos>)
	(lambda (call-next-method pos)
          (declare (ignore call-next-method))
          (slot-ref pos x))))
  (add-method #'pos-y
      (make-method (list <pos>)
	(lambda (call-next-method pos)
          (declare (ignore call-next-method))
          (slot-ref pos y))))

  (add-method #'move
      (make-method (list <pos>)
        (lambda (call-next-method pos new-x new-y)
          (declare (ignore call-next-method))
	  (slot-set! pos x new-x)
	  (slot-set! pos y new-y))))

  (add-method #'initialize
      (make-method (list <pos>)
	(lambda (call-next-method pos initargs)
          (declare (ignore call-next-method))
	  (move pos (getl initargs 'x 0) (getl initargs 'y 0)))))
  )

(defparameter p3 (make <pos> 'x 1 'y 2))
(defparameter p4 (make <pos> 'x 3 'y 5))

(test better-encapsulation
  (is (= 1 (pos-x p3)))
  (is (= 2 (pos-y p3)))
  (is (= 3 (pos-x p4)))
  (is (= 5 (pos-y p4)))
  (move p3 100 100)
  (move p4 100 100)
  (is (= 100 (pos-x p3)))
  (is (= 100 (pos-y p3)))
  (is (= 100 (pos-x p4)))
  (is (= 100 (pos-y p4)))
  (initialize p3 '(x 0 y 0))
  (initialize p4 '(x 0 y 0))
  (is (= 0 (pos-x p3)))
  (is (= 0 (pos-y p3)))
  (is (= 0 (pos-x p4)))
  (is (= 0 (pos-y p4))))

;***
;
; Class allocated slots.
;
; In Scheme, this extension isn't worth a whole lot, but what the hell.
;
;

(defvar <class-slots-class>
  (make-class (list <class>)
              (list)))

(add-method #'compute-getter-and-setter
    (make-method (list <class-slots-class>)
      (lambda (call-next-method class slot allocator)
        (declare (ignore class allocator))
	(if (null? (memq ':class-allocation slot))
	    (funcall call-next-method)
	    (let ((cell '()))
	      (list (lambda (o)
                      (declare (ignore o))
                      cell)
		    (lambda (o new)
                      (declare (ignore o))
                      (set! cell new) new)))))))

;
; Here's a silly program that uses class allocated slots.
;
;
(defvar <ship>
  (make <class-slots-class>
        'direct-supers (list <object>)
        'direct-slots  (list 'name
                             '(all-ships :class-allocation))))

(add-method #'initialize
    (make-method (list <ship>)
      (lambda (call-next-method ship initargs)
	(funcall call-next-method)
	(initialize-slots ship initargs)
	(slot-set! ship
		   'all-ships
		   (cons ship (slot-ref ship 'all-ships))))))

(define-function siblings (make-generic))
(add-method #'siblings
    (make-method (list <ship>)
      (lambda (call-next-method ship)
        (declare (ignore call-next-method))
	(remove ship (slot-ref ship 'all-ships)))))

(defparameter s1 (make <ship> 'name 's1))
(defparameter s2 (make <ship> 'name 's2))
(defparameter s3 (make <ship> 'name 's3))

(test class-allocated-slots
  (is (null
       (set-difference '(s1 s2 s3)
                       (reduce #'intersection
                               (list (mapcar (lambda (s) (slot-ref s 'name))
                                             (cons s1 (siblings s1)) )
                                     (mapcar (lambda (s) (slot-ref s 'name))
                                             (cons s2 (siblings s2)) )
                                     (mapcar (lambda (s) (slot-ref s 'name))
                                             (cons s3 (siblings s3)) )))))))

;***
;
; Here's a class of class that allocates some slots dynamically.
;
; It has a layered protocol (dynamic-slot?) that decides whether a given
; slot should be dynamically allocated.  This makes it easy to define a
; subclass that allocates all its slots dynamically.
;
;
(defvar <dynamic-class>
    (make-class (list <class>)
		(list 'alist-g-n-s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function dynamic-slot? (make-generic)))

(add-method #'dynamic-slot?
    (make-method (list <dynamic-class>)
      (lambda (call-next-method class slot)
        (declare (ignore call-next-method class))
	(memq :dynamic-allocation (cdr slot)) )))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function alist-getter-and-setter
    (lambda (dynamic-class allocator)
      (let ((old (slot-ref dynamic-class 'alist-g-n-s)))
	(if (null? old)
	    (let ((new (funcall allocator (lambda () '()))))
	      (slot-set! dynamic-class 'alist-g-n-s new)
	      new )
	    old )))))


(add-method #'compute-getter-and-setter
    (make-method (list <dynamic-class>)
      (lambda (call-next-method class slot allocator)
	(if (null? (dynamic-slot? class slot))
	    (funcall call-next-method)
	    (let* ((name (car slot))
		   (g-n-s (alist-getter-and-setter class allocator))
		   (alist-getter (car g-n-s))
		   (alist-setter (cadr g-n-s)))
	      (list (lambda (o)
		      (let ((entry (assq name  (funcall alist-getter o))))
			(if (null? entry)
			    '()
			    (cdr entry))))
		    (lambda (o new)
		      (let* ((alist (funcall alist-getter o))
			     (entry (assq name alist)))
			(if (null? entry)
			    (funcall alist-setter o
			                  (cons (cons name new) alist))
			    (set-cdr! entry new))
			new))))))))


(defvar <all-dynamic-class>
    (make-class (list <dynamic-class>)
		(list)))

(add-method #'dynamic-slot?
    (make-method (list <all-dynamic-class>)
      (lambda (call-next-method class slot)
        (declare (ignore call-next-method class slot))
        'T)))

(test dynamic
  (is-true (dynamic-slot? (make <all-dynamic-class>) :ignore)))

;
; A silly program that uses this.
;
;
(defvar <person>
  (make <all-dynamic-class>
        'direct-supers (list <object>)
        'direct-slots  (list 'name 'age 'address)))

(add-method #'initialize
    (make-method (list <person>)
      (lambda (call-next-method person initargs)
        (declare (ignore call-next-method))
	(initialize-slots person initargs))))


(defparameter person1 (make <person> 'name 'sally))
(defparameter person2 (make <person> 'name 'betty))
(defparameter person3 (make <person> 'name 'sue))

(test person
  (is (eq (slot-ref person1 'name)
          'sally))
  (is (eq (slot-ref person2 'name)
          'betty))
  (is (eq (slot-ref person3 'name)
          'sue)))


;***
;
; A ``database'' class that stores slots externally.
;
;

#|(defvar <db-class>
  (make-class (list <class>)
	      (list 'id-g-n-s)))|#

#|(define-function id-getter-and-setter
    (lambda (db-class allocator)
      (let ((old (slot-ref db-class 'id-g-n-s)))
	(if (null? old)
	    (let ((new (funcall allocator db-allocate-id)))
	      (slot-set! class 'id-g-n-s new)
	      new)
	    old))))|#

#|(add-method compute-getter-and-setter
    (make-method (list <db-class>)
      (lambda (call-next-method class slot allocator)
	(let* ((id-g-n-s (id-getter-and-setter class allocator))
	       (id-getter (car id-g-n-s))
	       (id-setter (cadr id-g-n-s))
	       (slot-name (car slot)))
	  (list (lambda (o)
		  (db-lookup (id-getter o) slot-name))
		(lambda (o new)
		  (db-store  (id-getter o) slot-name new)))))))|#


;***
;
; A kind of generic that supports around methods.
;
;
(defvar <around-generic>
  (make <entity-class>
        'direct-supers (list <generic>)))

(defvar <around-method>
  (make <class>
        'direct-supers (list <method>)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-function make-around-generic
    (lambda () (make <around-generic>)) )

  (define-function make-around-method
    (lambda (specializers procedure)
      (make <around-method>
            'specializers specializers
            'procedure procedure)))

  (define-function  around-method?   (make-generic)) )


(add-method #'around-method?
    (make-method (list <method>)
      (lambda (call-next-method x)
        (declare (ignore call-next-method x))
        'NIL)))

(add-method #'around-method?
    (make-method (list <around-method>)
      (lambda (call-next-method x)
        (declare (ignore call-next-method x))
        'T)))


(add-method #'compute-methods
    (make-method (list <around-generic>)
      (lambda (call-next-method generic)
        (declare (ignore generic))
	(let ((normal-compute-methods (funcall call-next-method)))
	  (lambda (args)
	    (let ((normal-methods (funcall normal-compute-methods args)))
	      (append
	        (filter-in #'around-method?
			   normal-methods)
		(filter-in (lambda (m) (not (around-method? m)))
			   normal-methods))))))))


;
; And a simple example of using it.
;
;
(defvar <baz> (make-class (list <object>) (list)))
(defvar <bar> (make-class (list <baz>)    (list)))
(defvar <foo> (make-class (list <bar>)    (list)))


(define-function test-around
    (lambda (generic)
      (add-method generic
	  (make-method        (list <foo>)
	                      (lambda (cnm x)
                                (declare (ignore x))
                                (cons 'foo (funcall cnm)))))

      (add-method generic
	  (make-around-method (list <bar>)
			      (lambda (cnm x)
                                (declare (ignore x))
                                (cons 'bar (funcall cnm)))))

      (add-method generic
	  (make-method        (list <baz>)
	                      (lambda (cnm x)
                                (declare (ignore cnm x))
                                '(baz))))

      (funcall generic (make <foo>))))


(test test-around
  (is-true (equal? (test-around (make-generic))
                   '(foo bar baz)))
  (is-true (equal? (test-around (make-around-generic))
                   '(bar foo baz))))

;;; eof

(cl:in-package :tiny-clos.internal)
;; (in-readtable :tiny-clos)

(setq *print-circle* t)

; Mode: Scheme
;
;
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
; EDIT HISTORY:
;
;      10/**/92  Gregor  Originally Written
; 1.0  11/10/92  Gregor  Changed names of generic invocation generics.
;                        Changed compute-getters-and-setters protocol.
;                        Made comments match the code.
;                        Changed maximum line width to 72.
; 1.1  11/24/92  Gregor  Fixed bug in compute-method-more-specific?,
;                        wrt the use of for-each.
;                        Both methods on allocate instance failed to
;                        initialize fields properly.
;                        The specializers and procedure initargs are
;                        now required when creating a method, that is,
;                        they no longer default.  No working program
;                        should notice this change.
; 1.2  12/02/92  Gregor  Fix minor things that improve portability:
;                          DEFINE needs 2 args in R4Rs
;                          Conditionalize printer hooks.
;                          () doesn't evaluate to ()
;
;
(defvar tiny-clos-version "1.2")

#|
'(;Stuff to make emacs more reasonable.

  (put 'letrec 'lisp-indent-hook 1)

  (put 'make-method  'lisp-indent-hook 1)
  (put 'add-method   'lisp-indent-hook 'defun)

 )
|#
;
; A very simple CLOS-like language, embedded in Scheme, with a simple
; MOP.  The features of the default base language are:
;
;   * Classes, with instance slots, but no slot options.
;   * Multiple-inheritance.
;   * Generic functions with multi-methods and class specializers only.
;   * Primary methods and call-next-method; no other method combination.
;   * Uses Scheme's lexical scoping facilities as the class and generic
;     function naming mechanism.  Another way of saying this is that
;     class, generic function and methods are first-class (meta)objects.
;
; While the MOP is simple, it is essentially equal in power to both MOPs
; in AMOP.  This implementation is not at all optimized, but the MOP is
; designed so that it can be optimized.  In fact, this MOP allows better
; optimization of slot access extenstions than those in AMOP.
;
;
;
; In addition to calling a generic, the entry points to the default base
; language are:
;
;   (MAKE-CLASS list-of-superclasses list-of-slot-names)
;   (MAKE-GENERIC)
;   (MAKE-METHOD list-of-specializers procedure)
;   (ADD-METHOD generic method)
;
;   (MAKE class . initargs)
;   (INITIALIZE instance initargs)            ;Add methods to this,
;                                             ;don't call it directly.
;
;   (SLOT-REF  object slot-name)
;   (SLOT-SET! object slot-name new-value)
;
;
; So, for example, one might do:
;
;   (define <position> (make-class (list <object>) (list 'x 'y)))
;   (add-method initialize
;       (make-method (list <position>)
;         (lambda (call-next-method pos initargs)
;           (for-each (lambda (initarg-name slot-name)
;                       (slot-set! pos
;                                  slot-name
;                                  (getl initargs initarg-name 0)))
;                     '(x y)
;                     '(x y)))))
;
;   (set! p1 (make <position> 'x 1 'y 3))
;
;
;
; NOTE!  Do not use EQUAL? to compare objects!  Use EQ? or some hand
;        written procedure.  Objects have a pointer to their class,
;        and classes are circular structures, and ...
;
;
;
; The introspective part of the MOP looks like the following.  Note that
; these are ordinary procedures, not generics.
;
;   CLASS-OF
;
;   CLASS-DIRECT-SUPERS
;   CLASS-DIRECT-SLOTS
;   CLASS-CPL
;   CLASS-SLOTS
;
;   GENERIC-METHODS
;
;   METHOD-SPECIALIZERS
;   METHOD-PROCEDURE
;
;
; The intercessory protocol looks like (generics in uppercase):
;
;   make
;     ALLOCATE-INSTANCE
;     INITIALIZE                   (really a base-level generic)
;
;   class initialization
;     COMPUTE-CPL
;     COMPUTE-SLOTS
;     COMPUTE-GETTER-AND-SETTER
;
;   add-method                     (Notice this is not a generic!)
;     COMPUTE-APPLY-GENERIC
;       COMPUTE-METHODS
;         COMPUTE-METHOD-MORE-SPECIFIC?
;       COMPUTE-APPLY-METHODS
;

;
; OK, now let's get going.  But, as usual, before we can do anything
; interesting, we have to muck around for a bit first.  First, we need
; to load the support library.
;
;


;
; Then, we need to build what, in a more real implementation, would be
; the interface to the memory subsystem: instances and entities.  The
; former are used for instances of instances of <class>; the latter
; are used for instances of instances of <entity-class>.  In this MOP,
; none of this is visible to base- or MOP-level programmers.
;
; (One might consider rewriting the definition of instances using
; define-record or something.  It doesn't turn out to make it much
; simpler, at least not to me.  It also breaks the nice parallelism
; with entities.)
;

;
; instances
;
;
(define-function %allocate-instance   #'values)
(define-function %instance? #'values)
(define-function %instance-class #'values)
(define-function %set-instance-class! #'values) ;This is used only once
                                               ;as part of bootstrapping
                                               ;the braid.
(define-function %instance-ref #'values)
(define-function %instance-set! #'values)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((instance-tag (list '%instance-tag)))

  #|(case what-scheme-implementation
    ((mit)
     (unparser/set-tagged-vector-method!      ;Make objects print a bit
      instance-tag                            ;more reasonably.  Scheme
      (unparser/standard-method 'object)))    ;is pretty feeble in this
    ((chez)))|#                                 ;regard.

  (define-function %allocate-instance
	(lambda (class nfields)
	  (let ((instance (make-vector (+ nfields 2) NIL)))
	    (vector-set! instance 0 instance-tag)
	    (vector-set! instance 1 class)
	    instance)))

  (define-function %instance?
        (lambda (x)
          (and (vector? x)
	       (>= (vector-length x) 2)
	       (eq? (vector-ref x 0) instance-tag))))

  (define-function %instance-class
	(lambda (instance)
	  (vector-ref instance 1)))

  (define-function %set-instance-class!
	(lambda (instance new-value)
	  (vector-set! instance 1 new-value)))

  (define-function %instance-ref
        (lambda (instance index)
	  (vector-ref instance (+ index 2))))

  (define-function %instance-set!
        (lambda (instance index new-value)
	  (vector-set! instance (+ index 2) new-value)))
  ))


;
; This implementation of entities is almost laughably bad.   Maybe in
; fact it is laughably bad.  But, it works, and keep in mind that this
; level of the stuff should just be assumbed as existing, rather than
; having to be understood by students and the like.
;
(define-function %allocate-entity   #'values)
(define-function %entity?           #'values)
(define-function %set-entity-proc!  #'values)
(define-function %entity-class      #'values)
(define-function %entity-ref        #'values)
(define-function %entity-set!       #'values)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (letrec ((entities ())
           (get-vector
            (lambda (closure)
              (let ((cell (assq closure entities)))
                (if (null? cell) NIL (cdr cell)) )))
           (default-proc
               (lambda args
                 (declare (ignore args))
                 (error "Called entity without first setting proc.") )))

    (define-function %allocate-entity
      (lambda (class nfields)
        (letrec ((*vector (make-vector (+ nfields 2) NIL))
                 (*closure (lambda args
                             (apply (vector-ref *vector 0) args) )))
          (vector-set! *vector 0 default-proc)
          (vector-set! *vector 1 class)
          (set! entities (cons (cons *closure *vector) entities))
          *closure )))

    (define-function %entity?
      (lambda (x) (not (null? (get-vector x)))) )

    (define-function %entity-class
      (lambda (closure)
        (let ((vector (get-vector closure)))
          (vector-ref vector 1) )))

    (define-function %set-entity-proc!
      (lambda (closure proc)
        (let ((vector (get-vector closure)))
          (vector-set! vector 0 proc) )))

    (define-function %entity-ref
      (lambda (closure index)
        (let ((vector (get-vector closure)))
          (vector-ref vector (+ index 2)) )))

    (define-function %entity-set!
      (lambda (closure index new-value)
        (let ((vector (get-vector closure)))
          (vector-set! vector (+ index 2) new-value) )))
  ))


;
; These next three, plus %allocate-instance and %allocate-entity, are
; the normal interface, from the rest of the code, to the low-level
; memory system.  One thing to take note of is that the protocol does
; not allow the user to add low-level instance representations.  I
; have never seen a way to make that work.
;
; Note that this implementation of class-of assumes the name of a the
; primitive classes that are set up later.
;
(defvar <boolean>)
(defvar <symbol>)
(defvar <char>)
(defvar <vector>)
(defvar <pair>)
(defvar <number>)
(defvar <string>)
(defvar <procedure>)

(define-function class-of
    (lambda (x)
      (cond ((%instance? x)  (%instance-class x))
	    ((%entity? x)    (%entity-class x))

	    ((boolean? x)    <boolean>)
	    ((symbol? x)     <symbol>)
	    ((char? x)       <char>)
	    ((vector? x)     <vector>)
	    ((pair? x)       <pair>)
	    ((number? x)     <number>)
	    ((string? x)     <string>)
	    ((procedure? x)  <procedure>))))

(define-function get-field
    (lambda (object field)
      (cond ((%instance? object) (%instance-ref object field))
	    ((%entity?   object) (%entity-ref   object field))
	    (:else
	     (error "Can only get-field of instances and entities.")))))

(define-function set-field!
    (lambda (object field new-value)
      (cond ((%instance? object) (%instance-set! object field new-value))
	    ((%entity?   object) (%entity-set!   object field new-value))
	    (:else
	     (error "Can only set-field! of instances and entities.")))))




;
; Now we can get down to business.  First, we initialize the braid.
;
; For Bootstrapping, we define an early version of MAKE.  It will be
; changed to the real version later on.  String search for ``set! make''.
;
(defvar <class>)
(defvar <entity-class>)
(defvar <generic>)
(defvar <method>)

(define-function make
    (lambda (class . initargs)
      (cond ((or (eq? class <class>)
		 (eq? class <entity-class>))
	     (let* ((new (%allocate-instance
			  class
			  (list-length the-slots-of-a-class)))
		    (dsupers (getl initargs 'direct-supers '()))
		    (dslots  (map #'list
				  (getl initargs 'direct-slots  '())))
		    (cpl     (let loop ((sups dsupers)
					(so-far (list new)))
				  (if (null? sups)
				      (reverse so-far)
				      (loop (class-direct-supers
					     (car sups))
					    (cons (car sups)
						  so-far)))))
		    (slots (apply #'append
				  (cons dslots
					(map #'class-direct-slots
					     (cdr cpl)))))
		    (nfields 0)
		    (field-initializers '())
		    (allocator
		      (lambda (init)
			(let ((f nfields))
			  (set! nfields (+ nfields 1))
			  (set! field-initializers
				(cons init field-initializers))
			  (list (lambda (o)   (get-field  o f))
				(lambda (o n) (set-field! o f n))))))
		    (getters-n-setters
		      (map (lambda (s)
			     (cons (car s)
				   (funcall allocator (lambda () '()))))
			   slots)))

	       (slot-set! new 'direct-supers      dsupers)
	       (slot-set! new 'direct-slots       dslots)
	       (slot-set! new 'cpl                cpl)
	       (slot-set! new 'slots              slots)
	       (slot-set! new 'nfields            nfields)
	       (slot-set! new 'field-initializers (reverse
						   field-initializers))
	       (slot-set! new 'getters-n-setters  getters-n-setters)
	       new))
	    ((eq? class <generic>)
	     (let ((new (%allocate-entity class
					  (list-length (class-slots class)))))
	       (slot-set! new 'methods ())
	       new))
	    ((eq? class <method>)
	     (let ((new (%allocate-instance
			 class
			 (list-length (class-slots class)))))
	       (slot-set! new
			  'specializers
			  (getl initargs 'specializers))
	       (slot-set! new
			  'procedure
			  (getl initargs 'procedure))
	       new)))))


;
; These are the real versions of slot-ref and slot-set!.  Because of the
; way the new slot access protocol works, with no generic call in line,
; they can be defined up front like this.  Cool eh?
;
;
(define-function slot-ref
    (lambda (object slot-name)
      (let* ((info   (lookup-slot-info (class-of object) slot-name))
	     (getter (list-ref info 0)))
	(funcall getter object))))

(define-function slot-set!
    (lambda (object slot-name new-value)
      (let* ((info   (lookup-slot-info (class-of object) slot-name))
	     (setter (list-ref info 1)))
	(funcall setter object new-value))))

(define-function lookup-slot-info
    (lambda (class slot-name)
      (let* ((getters-n-setters
	       (if (eq? class <class>)           ;* This grounds out
		   getters-n-setters-for-class   ;* the slot-ref tower.
		   (slot-ref class 'getters-n-setters)))
	     (entry (assq slot-name getters-n-setters)))
	(if (null? entry)
	    (error "No slot" slot-name "in instances of" class)
	    (cdr entry)))))



;
; Given that the early version of MAKE is allowed to call accessors on
; class metaobjects, the definitions for them come here, before the
; actual class definitions, which are coming up right afterwards.
;
;
(define-function class-direct-slots
    (lambda (class) (slot-ref class 'direct-slots)))
(define-function class-direct-supers
    (lambda (class) (slot-ref class 'direct-supers)))
(define-function class-slots
    (lambda (class) (slot-ref class 'slots)))
(define-function class-cpl
    (lambda (class) (slot-ref class 'cpl)))

(define-function generic-methods
    (lambda (generic) (slot-ref generic 'methods)))

(define-function method-specializers
    (lambda (method) (slot-ref method 'specializers)))
(define-function method-procedure
    (lambda (method) (slot-ref method 'procedure)))


;
; The next 7 clusters define the 6 initial classes.  It takes 7 to 6
; because the first and fourth both contribute to <class>.
;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar the-slots-of-a-class     ;
    '(direct-supers              ;(class ...)
      direct-slots               ;((name . options) ...)
      cpl                        ;(class ...)
      slots                      ;((name . options) ...)
      nfields                    ;an integer
      field-initializers         ;(proc ...)
      getters-n-setters ))        ;((slot-name getter setter) ...)
                                 ;
  (defvar getters-n-setters-for-class      ;see lookup-slot-info
    ;
    ; I know this seems like a silly way to write this.  The
    ; problem is that the obvious way to write it seems to
    ; tickle a bug in MIT Scheme!
    ;
    (let ((make-em (lambda (s f)
		     (list s
			   (lambda (o)   (%instance-ref  o f))
			   (lambda (o n) (%instance-set! o f n))))))
      (map (lambda (s)
	     (funcall make-em s (position-of s the-slots-of-a-class)))
	   the-slots-of-a-class))))
(defparameter <class> (%allocate-instance NIL (list-length the-slots-of-a-class)))
(progn
  (%set-instance-class! <class> <class>)
  nil)

(defvar <top>          (make <class>
			     'direct-supers (list)
			     'direct-slots  (list)))

(defvar <object>       (make <class>
			     'direct-supers (list <top>)
			     'direct-slots  (list)))

;
; This cluster, together with the first cluster above that defines
; <class> and sets its class, have the effect of:
;
;   (define <class>
;     (make <class>
;           'direct-supers (list <object>)
;           'direct-slots  (list 'direct-supers ...)))
;
(progn
  (%instance-set! <class> 0 (list <object>))                  ;d supers
  (%instance-set! <class> 1 (map #'list the-slots-of-a-class))  ;d slots
  (%instance-set! <class> 2 (list <class> <object> <top>))    ;cpl
  (%instance-set! <class> 3 (map #'list the-slots-of-a-class))  ;slots
  (%instance-set! <class> 4 (list-length the-slots-of-a-class))    ;nfields
  (%instance-set! <class> 5 (map (lambda (s)                  ;field-ini..
                                   (declare (ignore s))
                                   (lambda () '()))
                                 the-slots-of-a-class))
  (%instance-set! <class> 6 '())                              ;not needed
  nil
  )

(defparameter <procedure-class> (make <class>
                                      'direct-supers (list <class>)
                                      'direct-slots  (list)))

(defparameter <entity-class>    (make <class>
			        'direct-supers (list <procedure-class>)
			        'direct-slots  (list)))

(defparameter <generic>         (make <entity-class>
			        'direct-supers (list <object>)
			        'direct-slots  (list 'methods)))

(defparameter <method>          (make <class>
			        'direct-supers (list <object>)
			        'direct-slots  (list 'specializers
						     'procedure)))



;
; These are the convenient syntax we expose to the base-level user.
;
;
(define-function make-class
    (lambda (direct-supers direct-slots)
      (make <class>
	    'direct-supers direct-supers
	    'direct-slots  direct-slots)))

(define-function make-generic
    (lambda ()
      (make <generic>)))

(define-function make-method
    (lambda (specializers procedure)
      (make <method>
	    'specializers specializers
	    'procedure    procedure)))



;
; The initialization protocol
;

(define-function initialize (make-generic))


;
; The instance structure protocol.
;
(define-function allocate-instance (make-generic))

(define-function compute-getter-and-setter (make-generic))


;
; The class initialization protocol.
;
(define-function compute-cpl (make-generic))
(define-function compute-slots (make-generic))

;
; The generic invocation protocol.
;
(define-function compute-apply-generic         (make-generic))
(define-function compute-methods               (make-generic))
(define-function compute-method-more-specific? (make-generic))
(define-function compute-apply-methods         (make-generic))




;
; The next thing to do is bootstrap generic functions.
;
(defparameter generic-invocation-generics (list #'compute-apply-generic
                                                #'compute-methods
                                                #'compute-method-more-specific?
                                                #'compute-apply-methods))

(define-function add-method
    (lambda (generic method)
      (slot-set! generic
		 'methods
		 (cons method
		       (filter-in
			(lambda (m)
			  (not (every #'eq?
				      (method-specializers m)
				      (method-specializers method))))
			(slot-ref generic 'methods))))
      (%set-entity-proc! generic (compute-apply-generic generic))))

;
; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls
; the other generics in the generic invocation protocol.  Two, related,
; problems come up.  A chicken and egg problem and a infinite regress
; problem.
;
; In order to add our first method to COMPUTE-APPLY-GENERIC, we need
; something sitting there, so it can be called.  The first definition
; below does that.
;
; Then, the second definition solves both the infinite regress and the
; not having enough of the protocol around to build itself problem the
; same way: it special cases invocation of generics in the invocation
; protocol.
;
;
(%set-entity-proc! #'compute-apply-generic
     (lambda (generic)             ;The ONE time this is called
				   ;it doesn't get cnm.
       (lambda args
	 (apply (method-procedure (car (generic-methods generic)))
		(cons NIL args))))) ;But, the ONE time it is run,
				   ;it needs to pass a dummy
				   ;value for cnm!
(define-function last-pair #'cl:last)

(add-method #'compute-apply-generic
    (make-method (list <generic>)
      (lambda (call-next-method generic)
        (declare (ignore call-next-method))
	(lambda args
	  (if (and (memq generic generic-invocation-generics)     ;* G  c
		   (memq (car args) generic-invocation-generics)) ;* r  a
	      (apply (method-procedure                            ;* o  s
		      (car (last-pair (generic-methods generic))));* u  e
		     (cons NIL args))                              ;* n
	                                                          ;* d
	      (funcall (compute-apply-methods generic)
                       (funcall (compute-methods generic) args)
                       args))))))


(add-method #'compute-methods
    (make-method (list <generic>)
      (lambda (call-next-method generic)
        (declare (ignore call-next-method))
	(lambda (args)
	  (let ((applicable
		 (filter-in (lambda (method)
			      ;
			      ; Note that every only goes as far as the
			      ; shortest list!
			      ;
			      (every #'applicable?
				     (method-specializers method)
				     args))
			    (generic-methods generic))))
	    (gsort applicable
		   (lambda (m1 m2)
		     (funcall (compute-method-more-specific? generic)
		      m1
		      m2
		      args))))))))


(add-method #'compute-method-more-specific?
    (make-method (list <generic>)
      (lambda (call-next-method generic)
        (declare (ignore call-next-method generic))
	(lambda (m1 m2 args)
          (block nil
            (let loop ((specls1 (method-specializers m1))
		     (specls2 (method-specializers m2))
		     (args args))
	      (cond ((null? specls1) (return T))     ;*Maybe these two
                    ((null? specls2) (return NIL))     ;*should barf?
                    ((null? args)
                     (error "Fewer arguments than specializers."))
                    (:else
                     (let ((c1  (car specls1))
                           (c2  (car specls2))
                           (arg (car args)))
                       (if (eq? c1 c2)
                           (loop (cdr specls1)
                                 (cdr specls2)
                                 (cdr args))
                           (more-specific? c1 c2 arg)))))))))))


(add-method #'compute-apply-methods
    (make-method (list <generic>)
      (lambda (call-next-method generic)
        (declare (ignore call-next-method generic))
	(lambda (methods args)
	  (letrec ((one-step (lambda (tail)
			       (lambda ()
				 (apply (method-procedure (car tail))
					(cons (one-step (cdr tail))
					      args))))))
	    (funcall (one-step methods)))))))

(define-function applicable?
    (lambda (c arg)
      (memq c (class-cpl (class-of arg)))))

(define-function more-specific?
    (lambda (c1 c2 arg)
      (memq c2 (memq c1 (class-cpl (class-of arg))))))




(add-method #'initialize
    (make-method (list <object>)
      (lambda (call-next-method object initargs)
        (declare (ignore call-next-method initargs))
        object)))

(add-method #'initialize
    (make-method (list <class>)
      (lambda (call-next-method class initargs)
	(funcall call-next-method)
	(slot-set! class
		   'direct-supers
		   (getl initargs 'direct-supers '()))
	(slot-set! class
		   'direct-slots
		   (map (lambda (s)
			  (if (pair? s) s (list s)))
			(getl initargs 'direct-slots  '())))
	(slot-set! class 'cpl   (compute-cpl   class))
	(slot-set! class 'slots (compute-slots class))
	(let* ((nfields 0)
	       (field-initializers '())
	       (allocator
		(lambda (init)
		  (let ((f nfields))
		    (set! nfields (+ nfields 1))
		    (set! field-initializers
			  (cons init field-initializers))
		    (list (lambda (o)   (get-field  o f))
			  (lambda (o n) (set-field! o f n))))))
	       (getters-n-setters
		(map (lambda (slot)
		       (cons (car slot)
			     (compute-getter-and-setter class
							slot
							allocator)))
		     (slot-ref class 'slots))))
	  (slot-set! class 'nfields nfields)
	  (slot-set! class 'field-initializers field-initializers)
	  (slot-set! class 'getters-n-setters getters-n-setters)))))

(add-method #'initialize
    (make-method (list <generic>)
      (lambda (call-next-method generic initargs)
        (declare (ignore initargs))
	(funcall call-next-method)
	(slot-set! generic 'methods '())
	(%set-entity-proc! generic
			   (lambda args
                             (declare (ignore args))
                             (error "Has no methods."))))))

(add-method #'initialize
    (make-method (list <method>)
      (lambda (call-next-method method initargs)
	(funcall call-next-method)
	(slot-set! method 'specializers (getl initargs 'specializers))
	(slot-set! method 'procedure    (getl initargs 'procedure)))))



(add-method #'allocate-instance
    (make-method (list <class>)
      (lambda (call-next-method class)
        (declare (ignore call-next-method))
	(let* ((field-initializers (slot-ref class 'field-initializers))
	       (new (%allocate-instance
		      class
		      (list-length field-initializers))))
	  (let loop ((n 0)
		     (inits field-initializers))
	    (if (pair? inits)
		(begin
                  (%instance-set! new n (funcall (car inits)))
		 (loop (+ n 1)
		       (cdr inits)))
		new))))))

(add-method #'allocate-instance
    (make-method (list <entity-class>)
      (lambda (call-next-method class)
        (declare (ignore call-next-method))
	(let* ((field-initializers (slot-ref class 'field-initializers))
	       (new (%allocate-entity
		      class
		      (list-length field-initializers))))
	  (let loop ((n 0)
		     (inits field-initializers))
	    (if (pair? inits)
		(begin
		 (%entity-set! new n (funcall (car inits)))
		 (loop (+ n 1)
		       (cdr inits)))
		new))))))


(add-method #'compute-cpl
    (make-method (list <class>)
      (lambda (call-next-method class)
        (declare (ignore call-next-method))
	(compute-std-cpl class #'class-direct-supers))))


(add-method #'compute-slots
    (make-method (list <class>)
      (lambda (call-next-method class)
	(let collect ((to-process (apply #'append
					 (map #'class-direct-slots
					      (class-cpl class))))
		      (result '()))
	  (if (null? to-process)
	      (reverse result)
	      (let* ((current (car to-process))
		     (name (car current))
		     (others '())
		     (remaining-to-process
		      (collect-if (lambda (o)
				    (if (eq? (car o) name)
					(progn;sequence
					 (set! others (cons o others))
					 NIL)
					T))
				  (cdr to-process))))
		(collect remaining-to-process
			 (cons (append current
				       (apply #'append (map #'cdr others)))
			       result))))))))


(add-method #'compute-getter-and-setter
    (make-method (list <class>)
      (lambda (call-next-method class slot allocator)
        (declare (ignore call-next-method class slot))
	(funcall allocator (lambda () '())))))


;
; Now everything works, both generic functions and classes, so we can
; turn on the real MAKE.
;
;
(define-function make
      (lambda (class . initargs)
	(let ((instance (allocate-instance class)))
	  (initialize instance initargs)
	  instance)))
;
; Now define what CLOS calls `built in' classes.
;
;
(defvar <primitive-class>
    (make <class>
	  'direct-supers (list <class>)
	  'direct-slots  (list)))

(define-function make-primitive-class
    (lambda class
      (make (if (null? class) <primitive-class> (car class))
	    'direct-supers (list <top>)
	    'direct-slots  (list))))

(defvar <boolean>   (make-primitive-class))
(defvar <symbol>    (make-primitive-class))
(defvar <char>      (make-primitive-class))
(defvar <vector>    (make-primitive-class))
(defvar <pair>      (make-primitive-class))
(defvar <number>    (make-primitive-class))
(defvar <string>    (make-primitive-class))
(defvar <procedure> (make-primitive-class <procedure-class>))


;
; All done.
;
;

'tiny-clos-up-and-running

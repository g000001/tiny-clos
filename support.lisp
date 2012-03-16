(cl:in-package :tiny-clos.internal)
; Mode: Scheme
;
;
; *************************************************************************
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
; *************************************************************************
;
;
; Scheme is such a wonderful language, you can't program in it!
;
; This is a library of stuff I find useful.  I'll bet there's dozens
; of these out there.
;

;
; In order to make this code more easily portable, we have to be
; explicit about its implementation dependencies.  To do this, we
; have the following variable.  Please adjust it before trying to
; run this code.  See also the macro, scheme-implementation-case,
; which follows shortly.
;
; Note that some of these dependencies (i.e. gsort) are purely for
; convenience (i.e. saving me from writing sort from scratch).
; Others are more pressing, like define-macro.
;
;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar what-scheme-implementation
    'sbcl
                                        ;'chez
    ))

#|(case what-scheme-implementation
  ((mit)
   (syntax-table/define                    ;Kind of like DEFMACRO
     user-initial-syntax-table             ;  lifted from Cornell.
     'DEFINE-MACRO
     (macro (formals . body)
	    (if (not (pair? formals))
		(error "DEFINE-MACRO: First argument must be formals.") )
	    `(SYNTAX-TABLE-DEFINE
	       USER-INITIAL-SYNTAX-TABLE
	       (QUOTE ,(car formals))
	       ,(append (list 'MACRO (cdr formals)) body) ))))
  ((chez)
   ??? ))|#


(define-function (gsort list pred)
  (cl:sort (copy-list list) pred))

(define-function simple-printer (lambda () 'barf))

(defvar ??? 'unspecified-result)

(define-function list*
  (lambda args
    (letrec ((chase
	      (lambda (args)
		(cond ((null? args) '())
		      ((null? (cdr args)) (car args))
		      (:else (cons (car args) (chase (cdr args))))))))
      (chase args))))

(define-function apply*
  (lambda (proc . args)
    (apply proc (apply #'list* args))))


(define-function position-of
    (lambda (x lst)
      (if (eq? x (car lst)) 0 (+ 1 (position-of x (cdr lst))))))

(define-function map-append
    (lambda (proc . lists)
      (apply #'append (apply #'map (cons proc lists)))))

(define-function every
    (lambda (test . lists)
      (let scan ((tails lists))
	(if (member T (map #'null? tails))             ;(any null? lists)
	    T
	    (and (apply test (map #'car tails))
		 (scan (map #'cdr tails)))))))

(define-function remove
    (lambda (x list)
      (cond ((null? list) '())
	    ((eq? (car list) x) (cdr list))
	    (:else (cons (car list) (remove x (cdr list)))))))

(define-function getl
    (lambda (initargs name . not-found)
      (letrec ((scan (lambda (tail)
		       (cond ((null? tail)
			      (if (pair? not-found)
				  (car not-found)
				  (error "GETL couldn't find" name)))
			     ((eq? (car tail) name) (cadr tail))
			     (:else (scan (cddr tail)))))))
	(scan initargs))))

(define-function union
    (lambda lists
      (letrec ((clean (lambda (list result)
			(cond ((null? list) result)
			      ((memq (car list) result)
			       (clean (cdr list) result))
			      (:else
			       (clean (cdr list) (cons (car list) result)))))))
	(clean (apply #'append lists) '()))))

(define-function filter-in
    (lambda (f l)
      (cond ((null? l) '())
	    ((funcall f (car l)) (cons (car l) (filter-in f (cdr l))))
	    (:else (filter-in f (cdr l))))))

(define-function collect-if
    (lambda (test? list)
      (cond ((null? list) '())
	    ((funcall test? (car list))
             (cons (car list) (collect-if test? (cdr list))))
	    (:else (collect-if test? (cdr list))))))

;(define remove-unless
;    (lambda (test list)
;      (if (null? list)
;	  ()
;	  (let ((rest (remove-unless test (cdr list))))
;	    (if (test (car list))
;		(cons (car list) rest)
;		rest)))))

(define-function remove-duplicates
    (lambda (list)
      (let loop ((result-so-far '())
		 (remaining list))
	   (if (null? remaining)
	       result-so-far
	       (if (null? (memq (car remaining) result-so-far))
		   (loop (cons (car remaining) result-so-far)
			 (cdr remaining))
		   (loop result-so-far
			 (cdr remaining)))))))




;
; A simple topological sort.
;
; It's in this file so that both TinyClos and Objects can use it.
;
; This is a fairly modified version of code I originally got from Anurag
; Mendhekar <anurag@moose.cs.indiana.edu>.
;
;

(define-function compute-std-cpl
    (lambda (c get-direct-supers)
      (top-sort (funcall (build-transitive-closure get-direct-supers) c)
		(funcall (build-constraints get-direct-supers) c)
		(std-tie-breaker get-direct-supers) )))

(define-function top-sort
  (lambda (elements constraints tie-breaker)
    (let loop ((elements    elements)
               (constraints constraints)
               (result      '()) )
         (if (null? elements)
             result
             (let ((can-go-in-now
                    (filter-in
                     (lambda (x)
                       (every (lambda (constraint)
                                (or (not (eq? (cadr constraint) x))
                                    (memq (car constraint) result) ))
                              constraints ))
                     elements )))
               (if (null? can-go-in-now)
                   (error 'top-sort "Invalid constraints")
                   (let ((choice (if (null? (cdr can-go-in-now))
                                     (car can-go-in-now)
                                     (funcall tie-breaker result
                                              can-go-in-now ))))
                     (loop
                       (filter-in (lambda (x) (not (eq? x choice)))
                              elements )
                                        ;(filter-in (lambda (x) (not (eq? (cadr x) choice)))
                                        ;           constraints)
                       constraints
                       (append result (list choice)) ))))))))

(define-function std-tie-breaker
    (lambda (get-supers)
      (lambda (partial-cpl min-elts)
	(let loop ((pcpl (reverse partial-cpl)))
	     (let ((current-elt (car pcpl)))
	       (let ((ds-of-ce (funcall get-supers current-elt)))
		 (let ((common (filter-in (lambda (x)
					    (memq x ds-of-ce))
					  min-elts)))
		   (if (null? common)
		       (if (null? (cdr pcpl))
			   (error 'std-tie-breaker "Nothing valid")
			   (loop (cdr pcpl)))
		       (car common)))))))))


(define-function build-transitive-closure
    (lambda (get-follow-ons)
      (lambda (x)
	(let track ((result '())
		    (pending (list x)))
	     (if (null? pending)
		 result
		 (let ((next (car pending)))
		   (if (memq next result)
		       (track result (cdr pending))
		       (track (cons next result)
			      (append (funcall get-follow-ons next)
				      (cdr pending))))))))))

(define-function build-constraints
  (lambda (get-follow-ons)
    (lambda (x)
      (let loop ((elements (funcall
                            (build-transitive-closure get-follow-ons) x))
		 (this-one '())
		 (result '()))
	   (if (or (null? this-one) (null? (cdr this-one)))
	       (if (null? elements)
		   result
		   (loop (cdr elements)
			 (cons (car elements)
			       (funcall get-follow-ons (car elements)))
			 result))
	       (loop elements
		     (cdr this-one)
		     (cons (list (car this-one) (cadr this-one))
			   result)))))))

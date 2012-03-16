;;;; tiny-clos.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :tiny-clos
  :serial t
  :depends-on (:fiveam :srfi-5 :srfi-23)
  :components ((:file "package")
               (:file "util")
               (:file "support")
               (:file "tiny-clos")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :tiny-clos))))
  (load-system :tiny-clos)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :tiny-clos.internal :tiny-clos))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

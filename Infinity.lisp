;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DESCRIPTION
;;; INFINITY LIBRARY FOR OM AND OM#
;;; BY PAULO HENRIQUE RAPOSO - 2023

(in-package :om)

(mapc 'compile&load (list
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
                         (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "infinity-functions" :type "lisp")
                    )
)

(fill-library '( ("INFINITY-SERIES" Nil Nil (infinity::infinity-series infinity::infinity-canons) Nil)


))

(print 
"
INFINITY LIBRARY FOR OM
BY PAULO HENRIQUE RAPOSO - 2023

")




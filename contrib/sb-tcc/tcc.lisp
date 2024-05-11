;; This file makes a Common Lisp interface for Capstone.
;; We extract the API from https://github.com/aquynh/capstone/blob/master/include/capstone.h
;; We follow the file order and names as much as possible

(defpackage "SB-TCC"
  (:use :common-lisp)
  (:use :sb-alien)
  (:export
   ;; Constants
   #:tcc-output-memory
   #:tcc-output-exe
   #:tcc-output-obj
   #:tcc-output-preprocess
   #:tcc-output-format-elf
   #:tcc-output-format-binary
   #:tcc-output-format-coff
   ;; Macros
   ;; Types
   #:tcc-state
   ;; Slots
   ;; TCC functions
   #:tcc-new
   #:tcc-delete
   #:tcc-enable-debug
   #:tcc-set-error-func
   #:tcc-set-warning
   #:tcc-add-include-path
   #:tcc-add-sysinclude-path
   #:tcc-define-symbol
   #:tcc-undefine-symbol
   #:tcc-add-file
   #:tcc-compile-string
   #:tcc-set-output-type
   #:tcc-add-library-path
   #:tcc-add-library
   #:tcc-add-symbol
   #:tcc-output-file
   #:tcc-run
   #:tcc-relocate
   #:tcc-get-symbol
   #:tcc-set-lib-path
   ;; Helper functions
   ))

   (in-package "SB-TCC")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p *package*) t))

(defun try-load-shared-object (pathname)
  (handler-case
      (load-shared-object pathname :dont-save t)
    (error ()
      nil)))

(defun load-tcc ()
  (if (some #'try-load-shared-object
            #-(or win32 darwin) '("libtcc.so")
            #+darwin '("libtcc.dylib" "/usr/local/lib/libtcc.dylib")
            #+win32 '("libtcc.dll"))
      (pushnew :sb-tcc *features*)
      (warn "libtcc not loaded.")))

(load-tcc)

(define-alien-type tcc-state
    (* (struct tcc-state)))

(define-alien-routine tcc-new tcc-state)

(define-alien-routine tcc-delete void
  (s tcc-state))

(define-alien-routine tcc-enable-debug void
  (s tcc-state))

(define-alien-routine tcc-set-error-func void
  (s tcc-state) (opaque (* t)) (error-func (* (function void (* t) c-string))))

(define-alien-routine tcc-set-warning int
  (s tcc-state) (warning-name c-string) (value int))

;; preprocessor
(define-alien-routine tcc-add-include-path int
  (s tcc-state) (pathname c-string))

(define-alien-routine tcc-add-sysinclude-path int
  (s tcc-state) (pathname c-string))

(define-alien-routine tcc-define-symbol void
  (s tcc-state) (sym c-string) (value c-string))

(define-alien-routine tcc-undefine-symbol void
  (s tcc-state) (sym c-string))

;; compiling
(define-alien-routine tcc-add-file int
  (s tcc-state) (filename c-string))

(define-alien-routine tcc-compile-string int
  (s tcc-state) (buf c-string))

;; linking
(defconstant tcc-output-memory 0)
(defconstant tcc-output-exe 1)
(defconstant tcc-output-obj 3)
(defconstant tcc-output-preprocess 4)

(define-alien-routine tcc-set-output-type int
  (s tcc-state) (output-type int))

(defconstant tcc-output-format-elf 0)
(defconstant tcc-output-format-binary 1)
(defconstant tcc-output-format-coff 2)

(define-alien-routine tcc-add-library-path int
  (s tcc-state) (pathname c-string))

(define-alien-routine tcc-add-library int
  (s tcc-state) (libraryname c-string))

(define-alien-routine tcc-add-symbol int
  (s tcc-state) (name c-string) (val (* t)))

(define-alien-routine tcc-output-file int
  (s tcc-state) (filename c-string))

(define-alien-routine tcc-run int
  (s tcc-state) (argc int) (argv (* c-string)))

(define-alien-routine tcc-relocate int
  (s1 tcc-state) (ptr (* t)))

(define-alien-routine tcc-get-symbol (* t)
  (s tcc-state) (name c-string))

(define-alien-routine tcc-set-lib-path void
  (s tcc-state) (path c-string))

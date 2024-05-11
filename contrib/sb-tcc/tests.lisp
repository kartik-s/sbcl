(defpackage "SB-TCC-TESTS"
  (:use :common-lisp :sb-tcc :sb-alien))

(in-package "SB-TCC-TESTS")

(defun compile-and-run (program-string)
  (let ((state (tcc-new)))
    (tcc-set-output-type state tcc-output-memory)
    (tcc-compile-string state program-string)
    (tcc-run state 0 nil)))

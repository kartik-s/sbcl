#|
To run this benchmark after building from source:

1. gcc -Wall -shared -fPIC -o fcb.so fcb.c
2. ../run-sbcl.sh --script fcb.lisp

Here are some sample runs with the default parameters:

#-fcb-lazy-thread-cleanup
------------------------
$ ../run-sbcl.sh --script fcb.lisp
control (inline, no call)
elapsed time: 0.176754 seconds

regular C call (C -> C)
elapsed time: 0.191120 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.725714 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 6.000000 seconds


#+fcb-lazy-thread-cleanup
------------------------
$ ../run-sbcl.sh --script fcb-windows.lisp
control (inline, no call)
elapsed time: 0.117288 seconds

regular C call (C -> C)
elapsed time: 0.125753 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.319400 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 1.043674 seconds
|#
(require :sb-sprof)

(sb-alien:load-shared-object #+win32 "fcb.dll"
                             #+darwin "fcb.dylib"
                             #-(or win32 darwin) "fcb.so")

(define-alien-callable square (unsigned 64) ((x (unsigned 64)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (unsigned-byte 64) x))
  (* x x))

(defparameter *n-calls* 10000000)
(defparameter *arg-mod* 100)
(defparameter *sum-mod* 1000000000)

(with-alien ((run-benchmark (function void (* t) (unsigned 64) (unsigned 64) (unsigned 64) boolean)
                            :extern "run_benchmark")
             (c-square (function (unsigned 64) (unsigned 64)) :extern "square"))
  ;; control: inline
  (format t "control (inline, no call)~%")
  (alien-funcall run-benchmark
                 nil
                 *n-calls* *arg-mod* *sum-mod*
                 nil)
  (terpri)

  ;; regular C call
  (format t "regular C call (C -> C)~%")
  (alien-funcall run-benchmark
                 (alien-sap c-square)
                 *n-calls* *arg-mod* *sum-mod*
                 nil)
  (terpri)

  ;; alien callback
  (format t "alien callback (C on Lisp thread -> Lisp)~%")
  (alien-funcall run-benchmark
                 (alien-sap (alien-callable-function 'square))
                 *n-calls* *arg-mod* *sum-mod*
                 nil)
  (terpri)

  ;; foreign callback
  (format t "foreign callback (C on foreign thread -> Lisp)~%")
  (alien-funcall run-benchmark
                 (alien-sap (alien-callable-function 'square))
                 *n-calls* *arg-mod* *sum-mod*
                 t))

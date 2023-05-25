#|
To run this benchmark on Windows after building from source:

1. gcc -Wall -shared -fPIC -o fcb-windows.dll fcb-windows.c
2. ../run-sbcl.sh --script fcb-windows.lisp

Here are some sample runs with the default parameters:

#-foreign-callback-fiber
------------------------
$ ../run-sbcl.sh --script fcb-windows.lisp
control (inline, no call)
elapsed time: 0.119883 seconds

regular C call (C -> C)
elapsed time: 0.124107 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.317698 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 393.213626 seconds


#+foreign-callback-fiber
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
(sb-alien:load-shared-object "fcb-windows.dll")

(define-alien-callable square (unsigned 64) ((x (unsigned 64)))
  (declare (optimize (speed 3) (safety 0)))
  (declare (type (unsigned-byte 64) x))
  (* x x))

(defparameter *n-calls* 10000000)
(defparameter *arg-mod* 100)
(defparameter *sum-mod* 1000000000)

(with-alien ((benchmark-control (function void (unsigned 64) (unsigned 64) (unsigned 64))
                                :extern "benchmark_control")
             (benchmark-calls-from-same-thread (function void system-area-pointer (unsigned 64) (unsigned 64) (unsigned 64))
                                               :extern "benchmark_calls_from_same_thread")
             (benchmark-calls-from-new-thread (function void system-area-pointer (unsigned 64) (unsigned 64) (unsigned 64))
                                              :extern "benchmark_calls_from_new_thread")
             (c-square (function (unsigned 64) (unsigned 64)) :extern "square"))
  ;; control: inline
  (format t "control (inline, no call)~%")
  (alien-funcall benchmark-control *n-calls* *arg-mod* *sum-mod*)
  (terpri)

  ;; regular C call
  (format t "regular C call (C -> C)~%")
  (alien-funcall benchmark-calls-from-same-thread
                 (alien-sap c-square)
                 *n-calls* *arg-mod* *sum-mod*)
  (terpri)

  ;; alien callback
  (format t "alien callback (C on Lisp thread -> Lisp)~%")
  (alien-funcall benchmark-calls-from-same-thread
                 (alien-sap (alien-callable-function 'square))
                 *n-calls* *arg-mod* *sum-mod*)
  (terpri)

  ;; foreign callback
  (format t "foreign callback (C on foreign thread -> Lisp)~%")
  (alien-funcall benchmark-calls-from-new-thread
                 (alien-sap (alien-callable-function 'square))
                 *n-calls* *arg-mod* *sum-mod*))

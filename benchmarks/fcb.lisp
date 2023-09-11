#|
To run this benchmark after building from source (on Linux, for example):

1. gcc -Wall -shared -fPIC -o fcb.so fcb.c
2. LD_LIBRARY_PATH=. sh ../run-sbcl.sh --script fcb.lisp

Here are some sample runs with the default parameters:

x86_64 Linux
============
------------------------------------------
#+linux #+x86-64 #-fcb-lazy-thread-cleanup
------------------------------------------
$ LD_LIBRARY_PATH=. sh ../run-sbcl.sh --script fcb.lisp
[10,000,000 calls]
control (inline, no call)
elapsed time: 0.145976 seconds

regular C call (C -> C)
elapsed time: 0.157714 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.279785 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 142.108533 seconds

------------------------------------------
#+linux #+x86-64 #+fcb-lazy-thread-cleanup
------------------------------------------
$ LD_LIBRARY_PATH=. sh ../run-sbcl.sh --script fcb.lisp
[10,000,000 calls]
control (inline, no call)
elapsed time: 0.145932 seconds

regular C call (C -> C)
elapsed time: 0.157795 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.281898 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 8.629588 seconds


ARM64 macOS
===========
------------------------------------------
#+darwin #+arm64 #-fcb-lazy-thread-cleanup
------------------------------------------
$ sh ../run-sbcl.sh --dynamic-space-size 4096 --script fcb.lisp
[10,000,000 calls]
control (inline, no call)
elapsed time: 0.039138 seconds

regular C call (C -> C)
elapsed time: 0.058451 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.137326 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 359.856311 seconds

------------------------------------------
#+darwin #+arm64 #-fcb-lazy-thread-cleanup
------------------------------------------
$ sh ../run-sbcl.sh --script fcb.lisp
[10,000,000 calls]
control (inline, no call)
elapsed time: 0.039052 seconds

regular C call (C -> C)
elapsed time: 0.058007 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.173779 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 36.750681 seconds


x86_64 Windows
==============
------------------------------------------
#+win32 #+x86-64 #-fcb-lazy-thread-cleanup
------------------------------------------
$ sh ../run-sbcl.sh --script fcb.lisp
[10,000,000 calls]
control (inline, no call)
elapsed time: 0.202304 seconds

regular C call (C -> C)
elapsed time: 0.208525 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.523955 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 237.134741 seconds

------------------------------------------
#+win32 #+x86-64 #+fcb-lazy-thread-cleanup
------------------------------------------
$ sh ../run-sbcl.sh --script fcb.lisp
[10,000,000 calls]
control (inline, no call)
elapsed time: 0.178765 seconds

regular C call (C -> C)
elapsed time: 0.179948 seconds

alien callback (C on Lisp thread -> Lisp)
elapsed time: 0.685713 seconds

foreign callback (C on foreign thread -> Lisp)
elapsed time: 5.386313 seconds
|#
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
  (format t "[~:D calls]~%" *n-calls*)

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

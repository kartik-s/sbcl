(sb-alien:load-shared-object "fcb-windows.dll")

(define-alien-callable square (unsigned 64) ((x (unsigned 64)))
  (* x x))

(defparameter *n-calls* 100000)
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

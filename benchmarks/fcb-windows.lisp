(sb-alien:load-shared-object "fcb-windows.dll")

(define-alien-callable square int ((x int))
  (* x x))

(defparameter *n-calls* 1000)

(with-alien ((benchmark-calls-from-same-thread (function void system-area-pointer int)
                                               :extern "benchmark_calls_from_same_thread")
             (benchmark-calls-from-new-thread (function void system-area-pointer int)
                                              :extern "benchmark_calls_from_new_thread")
             (c-square (function int int) :extern "square"))
  ;; control: regular C call
  (alien-funcall benchmark-calls-from-same-thread
                 (alien-sap c-square) *n-calls*)
  ;; alien callback
  (alien-funcall benchmark-calls-from-same-thread
                 (alien-sap (alien-callable-function 'square)) *n-calls*)
  ;; foreign callback
  (alien-funcall benchmark-calls-from-new-thread
                 (alien-sap (alien-callable-function 'square)) *n-calls*))

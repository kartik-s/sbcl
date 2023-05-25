(sb-alien:load-shared-object "fcb-bench.dll")

(define-alien-callable square int ((x int))
  (* x x))

(defparameter *n-calls* 1000)

(with-alien ((benchmark-regular (function void system-area-pointer int)
                                :extern "benchmark_regular")
             (benchmark-callback-fiber (function void system-area-pointer int)
                                       :extern "benchmark_callback_fiber"))
  (let ((square-fn-ptr (alien-sap (alien-callable-function 'square))))
    (alien-funcall benchmark-regular square-fn-ptr *n-calls*)
    (alien-funcall benchmark-callback-fiber square-fn-ptr *n-calls*)))

(define-alien-callable ("_square" square) int ((x int)) (alien-funcall (extern-alien "print_backtrace" (function void))) (* x x))

(define-alien-callable is-seh-handler-thunk int ((addr (unsigned 64))) (if (and (<= SB-VM:WIN64-SEH-DATA-ADDR addr) (< addr (+ SB-VM:WIN64-SEH-DATA-ADDR 32))) 1 0))

(define-alien-callable is-lisp-pc int ((addr (unsigned 64))) (if (and (<= SB-VM:TEXT-SPACE-START addr) (< addr (+ SB-VM:TEXT-SPACE-START SB-VM:TEXT-SPACE-SIZE))) 1 0))

(save-lisp-and-die "test.core" :callable-exports '(("_square" square) is-seh-handler-thunk is-lisp-pc))

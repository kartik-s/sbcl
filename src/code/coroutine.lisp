;;; **********************************************************************
;;;
;;; Stack-group  support for SBCL x86.
;;;

(defpackage "SB-COROUTINE"
  (:documentation "Coroutines!!!!!!")
  (:use "CL"))

(in-package "SB-COROUTINE")

#+nil
(defpackage* "SB-FASL"
    (:documentation "private: stuff related to FASL load/dump logic (and GENESIS)")
  (:use "CL" "SB-ALIEN" "SB-ASSEM" "SB-BIGNUM" "SB-C"
        "SB-EXT" "SB-INT" "SB-KERNEL" "SB-SYS")
  (:import-from "SB-VM" "+FIXUP-KINDS+")
  (:import-from "SB-IMPL" "FIND-OR-MAYBE-MAKE-DEFERRED-PACKAGE" "WITH-LOADER-PACKAGE-NAMES")
  (:export "*ASSEMBLER-ROUTINES*"
           "GET-ASM-ROUTINE"
           "+BACKEND-FASL-FILE-IMPLEMENTATION+"
           "*FASL-FILE-TYPE*"
           "CLOSE-FASL-OUTPUT"
           "DUMP-ASSEMBLER-ROUTINES"
           "DUMP-FOP" "DUMP-OBJECT"
           "FASL-CONSTANT-ALREADY-DUMPED-P"
           "+FASL-FILE-VERSION+"
           "FASL-DUMP-COMPONENT"
           "FASL-DUMP-LOAD-TIME-VALUE-LAMBDA"
           "FASL-DUMP-PARTIAL-SOURCE-INFO"
           "FASL-DUMP-SOURCE-INFO" "FASL-DUMP-TOPLEVEL-LAMBDA-CALL"
           "FASL-NOTE-HANDLE-FOR-CONSTANT"
           "FASL-OUTPUT" "FASL-OUTPUT-P"
           "FASL-OUTPUT-ENTRY-TABLE" "FASL-OUTPUT-STREAM"
           "FASL-VALIDATE-STRUCTURE"
           "FASL-NOTE-INSTANCE-SAVES-SLOTS"
           "*!LOAD-TIME-VALUES*"
           "OPEN-FASL-OUTPUT"
           "*!COLD-TOPLEVELS*"
           "COLD-CONS" "COLD-INTERN" "COLD-PUSH"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Handle the binding stack.

;;; Undo all the bindings in the bind stack, restoring the global
;;; values.
(defun unbind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (sb-kernel:binding-stack-pointer-sap))
         (binding-stack
           (sb-sys:int-sap (sb-alien:extern-alien "binding_stack" sb-alien:unsigned)))
         (size (sb-sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding size))
        ((zerop binding))
      (declare (type (unsigned-byte 29) binding))
      (decf binding 8)
      (let* ((value
               (sb-kernel:make-lisp-obj
                (sb-sys:sap-int (sb-sys:sap-ref-sap binding-stack binding))))
             (symbol
               (sb-kernel:make-lisp-obj
                (sb-sys:sap-int (sb-sys:sap-ref-sap binding-stack (+ binding 4))))))
        (cond ((symbolp symbol)
               (let ((symbol-value (sb-c::%primitive sb-c:fast-symbol-value symbol)))
                 #+nil
                 (format t "Undoing: ~s ~s <-> ~s~%" symbol value symbol-value)
                 (sb-kernel:%set-symbol-value symbol value)
                 (setf (sb-sys:sap-ref-sap binding-stack binding)
                       (sb-sys:int-sap (sb-kernel:get-lisp-obj-address
                                        symbol-value)))))
              (t
               #+nil
               (format t "Ignoring undoing: ~s ~s~%" symbol value)))))))

;;; Re-apply the bindings in a binding stack after an
;;; unbind-binding-stack.
(defun rebind-binding-stack ()
  (declare (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (sb-kernel:binding-stack-pointer-sap))
         (binding-stack
           (sb-sys:int-sap (sb-alien:extern-alien "binding_stack" sb-alien:unsigned)))
         (size (sb-sys:sap- binding-stack-pointer binding-stack)))
    (declare (type (unsigned-byte 29) size))
    (do ((binding 0 (+ 8 binding)))
        ((= binding size))
      (declare (type (unsigned-byte 29) binding))
      (let* ((value
               (sb-kernel:make-lisp-obj
                (sb-sys:sap-int (sb-sys:sap-ref-sap binding-stack binding))))
             (symbol
               (sb-kernel:make-lisp-obj
                (sb-sys:sap-int (sb-sys:sap-ref-sap binding-stack (+ binding 4))))))
        (cond ((symbolp symbol)
               (let ((symbol-value (sb-c::%primitive sb-c:fast-symbol-value symbol)))
                 #+nil
                 (format t "Rebinding: ~s ~s <-> ~s~%"
                         symbol value symbol-value)
                 (sb-kernel:%set-symbol-value symbol value)
                 (setf (sb-sys:sap-ref-sap binding-stack binding)
                       (sb-sys:int-sap (sb-kernel:get-lisp-obj-address
                                        symbol-value)))))
              (t
               #+nil
               (format t "Ignoring rebinding: ~s ~s~%" symbol value)))))))

(defun save-binding-stack (binding-save-stack)
  (declare (type (simple-array t (*)) binding-save-stack)
           (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-pointer (sb-kernel:binding-stack-pointer-sap))
         (binding-stack
           (sb-sys:int-sap (sb-alien:extern-alien "binding_stack" sb-alien:unsigned)))
         (size (sb-sys:sap- binding-stack-pointer binding-stack))
         (vector-size (truncate size 4)))
    (declare (type (unsigned-byte 29) size))
    ;; Grow binding-save-stack if necessary.
    (when (< (length binding-save-stack) vector-size)
      (setq binding-save-stack
            (adjust-array binding-save-stack vector-size :element-type t)))
    ;; Save the stack.
    (do ((binding 0 (+ 4 binding))
         (index 0 (1+ index)))
        ((= binding size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (aref binding-save-stack index)
            (sb-kernel:make-lisp-obj
             (sb-sys:sap-int (sb-sys:sap-ref-sap binding-stack binding)))))
    (values binding-save-stack vector-size)))

(defun restore-binding-stack (new-binding-stack size)
  (declare (type (simple-array t (*)) new-binding-stack)
           (type (unsigned-byte 29) size)
           (optimize (speed 3) (safety 0)))
  (let* ((binding-stack-size (* size 4))
         (binding-stack (sb-alien:extern-alien "binding_stack" sb-alien:unsigned)))
    (declare (type (unsigned-byte 32) binding-stack-size binding-stack))
    (setf (sb-kernel:binding-stack-pointer-sap)
          (sb-sys:int-sap (+ binding-stack binding-stack-size)))
    (do ((binding 0 (+ 4 binding))
         (index 0 (1+ index)))
        ((= binding binding-stack-size))
      (declare (type (unsigned-byte 29) binding index))
      (setf (sb-sys:sap-ref-sap (sb-sys:int-sap binding-stack) binding)
            (sb-sys:int-sap (sb-kernel:get-lisp-obj-address
                          (aref new-binding-stack index))))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Alien-stack

;;; The Top of the Alien-stack.
(declaim (type (unsigned-byte 32) *alien-stack-top*))
(defvar *alien-stack-top* 0)

;;; Save the alien-stack.
(defun save-alien-stack (save-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
           (optimize (speed 3) (safety 0)))
  (let* ((alien-stack (sb-kernel:get-lisp-obj-address sb-vm::*alien-stack-pointer*))
         (size (- *alien-stack-top* alien-stack))
         (vector-size (ceiling size 4)))
    (declare (type (unsigned-byte 32) alien-stack)
             (type (unsigned-byte 29) size))
    #+nil
    (format t "alien-stack ~x; size ~x~%" alien-stack size)
    ;; Grow save-stack if necessary.
    (when (< (length save-stack) vector-size)
      (setq save-stack
            (adjust-array save-stack vector-size
                          :element-type '(unsigned-byte 32))))
    ;; Save the stack.
    (do ((index 0 (1+ index)))
        ((>= index vector-size))
      (declare (type (unsigned-byte 29) index))
      (setf (aref save-stack index)
            (sb-sys:sap-ref-32 (sb-sys:int-sap *alien-stack-top*)
                            (* 4 (- (1+ index))))))
    (values save-stack vector-size alien-stack)))

(defun restore-alien-stack (save-stack size alien-stack)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-stack)
           (type (unsigned-byte 29) size)
           (type (unsigned-byte 32) alien-stack)
           (optimize (speed 3) (safety 0)))
  (setf sb-vm::*alien-stack-pointer* (sb-kernel:make-lisp-obj alien-stack))
  (do ((index 0 (1+ index)))
      ((>= index size))
    (declare (type (unsigned-byte 29) index))
    (setf (sb-sys:sap-ref-32 (sb-sys:int-sap *alien-stack-top*) (* 4 (- (1+ index))))
          (aref save-stack index)))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interrupt contexts.

;;; Save the interrupt contexts.
(defun save-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
           (optimize (speed 3) (safety 0)))
  (let* ((size sb-kernel:*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    ;; Grow save-stack if necessary.
    (when (< (length save-vector) size)
      (setq save-vector
            (adjust-array save-vector size :element-type '(unsigned-byte 32))))
    (sb-alien:with-alien
        ((lisp-interrupt-contexts (array sb-alien:unsigned) :extern))
      (dotimes (index size)
        (setf (aref save-vector index)
              (sb-alien:deref lisp-interrupt-contexts index))))
    save-vector))

;;; Restore the interrupt contexts.
(defun restore-interrupt-contexts (save-vector)
  (declare (type (simple-array (unsigned-byte 32) (*)) save-vector)
           (optimize (speed 3) (safety 0)))
  (let* ((size sb-kernel:*free-interrupt-context-index*))
    (declare (type (unsigned-byte 29) size))
    (sb-alien:with-alien
        ((lisp-interrupt-contexts (array sb-alien:unsigned) :extern))
      (dotimes (index size)
        (setf (sb-alien:deref lisp-interrupt-contexts index)
              (aref save-vector index)))))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;

;;; The control stacks need special handling on the x86 as they
;;; contain conservative roots. When placed in the *control-stacks*
;;; vector they will be scavenged for conservative roots by the
;;; garbage collector.
(declaim (type (simple-array (or null (simple-array (unsigned-byte 32) (*)))
                             (*)) *control-stacks*))
(defvar *control-stacks*
  (make-array 0 :element-type '(or null (unsigned-byte 32))
                :initial-element nil))

;;; Stack-group structure.
(defstruct (stack-group
            (:constructor %make-stack-group)
            (:print-function
             (lambda (stack-group stream depth)
               (declare (type stack-group stack-group)
                        (stream stream)
                        (ignore depth))
               (print-unreadable-object (stack-group stream :identity t)
                 (format stream "Stack-group ~a, ~a"
                         (stack-group-name stack-group)
                         (stack-group-state stack-group))))))
  ;; Must have a name.
  (name "Anonymous" :type simple-base-string)
  ;; State: :active or :inactive.
  (state :inactive :type (member :active :inactive))
  ;; The control stack; an index into *control-stacks*.
  (control-stack-id nil :type (or sb-kernel::index null))
  ;; Binding stack.
  (binding-stack nil :type (or (simple-array t (*)) null))
  ;; Twice the number of bindings.
  (binding-stack-size 0 :type (unsigned-byte 29))
  ;; Current catch block, on the control stack.
  (current-catch-block 0 :type fixnum)
  ;; Unwind protect block, on the control stack.
  (current-unwind-protect-block 0 :type fixnum)
  ;; Alien stack
  (alien-stack nil :type (or (simple-array (unsigned-byte 32) (*)) null))
  (alien-stack-size 0 :type (unsigned-byte 29))
  (alien-stack-pointer 0 :type (unsigned-byte 32))
  ;; Eval-stack
  (eval-stack nil :type (or (simple-array t (*)) null))
  (eval-stack-top 0 :type fixnum)
  ;;
  ;; Interrupt contexts
  (interrupt-contexts nil :type (or (simple-array (unsigned-byte 32) (*))
                                    null))
  ;; Resumer
  (resumer nil :type (or stack-group null)))

;;; The current stack group.
(declaim (type (or stack-group null) *current-stack-group*))
(defvar *current-stack-group* nil)

(declaim (type (or stack-group null) *initial-stack-group*))
(defvar *initial-stack-group* nil)

;;; Process defstruct is up here because stack group functions refer
;;; to process slots in assertions, but are also compiled at high
;;; optimization... so if the process structure changes, all hell
;;; could break loose.

(defstruct (process
            (:constructor %make-process)
            (:predicate processp)
            (:print-function
             (lambda (process stream depth)
               (declare (type process process) (stream stream) (ignore depth))
               (print-unreadable-object (process stream :identity t)
                 (format stream "Process ~a" (process-name process))))))
  (name "Anonymous" :type simple-base-string)
  (state :killed :type (member :killed :active :inactive))
  (%whostate nil :type (or null simple-base-string))
  (initial-function nil :type (or null function))
  (initial-args nil :type list)
  (wait-function nil :type (or null function))
  (wait-function-args nil :type list)
  (%run-reasons nil :type list)
  (%arrest-reasons nil :type list)
  ;; The real time after which the wait will timeout.
  (wait-timeout nil :type (or null double-float))
  (wait-return-value nil :type t)
  (interrupts '() :type list)
  (stack-group nil :type (or null stack-group))
  ;;
  ;; The real and run times when the current process was last
  ;; scheduled or yielded.
  (scheduled-real-time (get-real-time) :type double-float)
  (scheduled-run-time (get-run-time) :type double-float)
  ;;
  ;; Accrued real and run times in seconds.
  (%real-time 0d0 :type double-float)
  (%run-time 0d0 :type double-float)
  (property-list nil :type list)
  (%return-values nil :type list)
  (initial-bindings nil :type list))


;;; Init-Stack-Groups -- Interface
;;;
;;; Setup the initial stack group.
;;;
(defun init-stack-groups ()
  ;; Grab the top of the alien-stack; it's currently stored at the top
  ;; of the control stack.
  (setf *alien-stack-top*
        (sb-sys:sap-ref-32
         (sb-sys:int-sap (sb-alien:extern-alien "control_stack_end" sb-alien:unsigned))
         -4))
  ;; Initialise the *control-stacks* vector.
  (setq *control-stacks*
        (make-array 10 :element-type '(or null (unsigned-byte 32))
                       :initial-element nil))
  ;; Setup a control-stack for the initial stack-group.
  (setf (aref *control-stacks* 0)
        (make-array 0 :element-type '(unsigned-byte 32)
                      :initial-element 0))
  ;; Make and return the initial stack group.
  (setf *current-stack-group*
        (%make-stack-group
         :name "Initial"
         :state :active
         :control-stack-id 0
         :binding-stack #()
         :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
         :interrupt-contexts (make-array 0 :element-type '(unsigned-byte 32))
         :eval-stack #()))
  (setf *initial-stack-group* *current-stack-group*))

;;; Inactivate-Stack-Group -- Internal
;;;
;;; Inactivate the stack group, cleaning its slot and freeing the
;;; control stack.
;;;
(defun inactivate-stack-group (stack-group)
  (declare (type stack-group stack-group))
  (setf (stack-group-state stack-group) :inactive)
  (let ((cs-id (stack-group-control-stack-id stack-group)))
    (when (and cs-id (aref *control-stacks* cs-id))
      (setf (aref *control-stacks* cs-id) nil)))
  (setf (stack-group-control-stack-id stack-group) nil)
  (setf (stack-group-binding-stack stack-group) nil)
  (setf (stack-group-binding-stack-size stack-group) 0)
  (setf (stack-group-current-catch-block stack-group) 0)
  (setf (stack-group-current-unwind-protect-block stack-group) 0)
  (setf (stack-group-alien-stack stack-group) nil)
  (setf (stack-group-alien-stack-size stack-group) 0)
  (setf (stack-group-alien-stack-pointer stack-group) 0)
  (setf (stack-group-eval-stack stack-group) nil)
  (setf (stack-group-eval-stack-top stack-group) 0)
  (setf (stack-group-resumer stack-group) nil))

;;; Scrub-Stack-Group-Stacks -- Internal
;;;
;;; Scrub the binding stack of the give stack-group.
;;;
(defun scrub-stack-group-stacks (stack-group)
  (declare (type stack-group stack-group)
           (optimize (speed 3) (safety 0)))
  ;; Binding stack.
  (let ((binding-save-stack (stack-group-binding-stack stack-group)))
    (when binding-save-stack
      (let ((size
              ;; The stored binding stack for the current stack group
              ;; can be completely scrubbed.
              (if (eq stack-group *current-stack-group*)
                  0
                  (stack-group-binding-stack-size stack-group)))
            (len (length binding-save-stack)))
        ;; Scrub the remainder of the binding stack.
        (do ((index size (+ index 1)))
            ((>= index len))
          (declare (type (unsigned-byte 29) index))
          (setf (aref binding-save-stack index) 0))))))

;;; Initial-binding-stack  --  Internal
;;;
;;; Generate the initial bindings for a newly created stack-group.
;;; This function may be redefined to return a vector with other bindings
;;; but *interrupts-enabled* and *gc-inhibit* must be the last two.
;;;
(defun initial-binding-stack ()
  (vector
   (find-package "COMMON-LISP-USER") '*package*
   ;; Other bindings may be added here.
   nil 'sb-unix::*interrupts-enabled*
   t 'sb-kernel::*gc-inhibit*))

;;; Make-Stack-Group -- Interface
;;;
;;; Fork a new stack-group from the *current-stack-group*. Execution
;;; continues with the *current-stack-group* returning the new stack
;;; group. Control may be transfer to the child by stack-group-resume
;;; and it executes the initial-function.
;;;
(defun sigmask (&rest signals)
  "Returns a mask given a set of signals."
  (apply #'logior
         (mapcar #'(lambda (signal)
                     (ash 1 (1- signal)))
                 signals)))

(defmacro pthread-sigmask (how new old)
  `(let ((how ,how) (new ,new) (old ,old))
     (alien-funcall (extern-alien
                     #+sb-thread ,(or #+unix "pthread_sigmask" #-unix "sb_pthread_sigmask")
                     #-sb-thread ,(or #+netbsd "sb_sigprocmask" #-netbsd "sigprocmask")
                     (function void int system-area-pointer system-area-pointer))
                    how
                    (cond ((system-area-pointer-p new) new)
                          (new (vector-sap new))
                          (t (int-sap 0)))
                    (cond ((system-area-pointer-p old) old)
                          (old (vector-sap old))
                          (t (int-sap 0))))))

(defun make-stack-group (name initial-function &optional
                                                 (resumer *current-stack-group*)
                                                 (inherit t))
  (declare (type simple-base-string name)
           (type function initial-function)
           (type stack-group resumer))
  (flet ((allocate-control-stack ()
           (let* (;; Allocate a new control-stack ID.
                  (control-stack-id (position nil *control-stacks*))
                  ;; Find the required stack size.
                  (control-stack-end
                    (sb-alien:extern-alien "control_stack_end" sb-alien:unsigned))
                  (control-stack-pointer (sb-kernel:control-stack-pointer-sap))
                  (control-stack-size
                    (- control-stack-end (sb-sys:sap-int control-stack-pointer)))
                  ;; Saved control stack needs three extra words. The
                  ;; stack pointer will be stored in the first
                  ;; element, and the frame pointer and return address
                  ;; push onto the bottom of the stack.
                  (control-stack
                    (make-array (+ (ceiling control-stack-size 4) 3)
                                :element-type '(unsigned-byte 32)
                                :initial-element 0)))
             (declare (type (unsigned-byte 29) control-stack-size))
             (unless control-stack-id
               ;; Need to extend the *control-stacks* vector.
               (setq control-stack-id (length *control-stacks*))
               (setq *control-stacks*
                     (adjust-array *control-stacks*
                                   (* 2 (length *control-stacks*))
                                   :element-type '(or null (unsigned-byte 32))
                                   :initial-element nil)))
             (setf (aref *control-stacks* control-stack-id) control-stack)
             (values control-stack control-stack-id)))
         ;; Allocate a stack group inheriting stacks and bindings from
         ;; the current stack group.
         (allocate-child-stack-group (control-stack-id)
           ;; Save the interrupt-contexts while the size is still
           ;; bound.
           (let ((interrupt-contexts
                   (save-interrupt-contexts
                    (make-array 0 :element-type '(unsigned-byte 32)))))
             ;; Save the binding stack.  Note that
             ;; *interrupts-enabled* could be briefly set during the
             ;; unbinding and re-binding process so signals are
             ;; blocked.
             (let ((old-sigs (sb-unix::pthread-sigmask
                              sb-unix::SIG_BLOCK
                              (sigmask sb-unix:sigint sb-unix:sigalrm)
                              nil)))
               (declare (type (unsigned-byte 32) old-sigs))
               (unbind-binding-stack)
               (multiple-value-bind (binding-stack binding-stack-size)
                   (save-binding-stack #())
                 (rebind-binding-stack)
                 (sb-unix::pthread-sigmask sb-unix::SIG_SETMASK old-sigs nil)
                 ;; Save the Alien stack
                 (multiple-value-bind (alien-stack alien-stack-size
                                       alien-stack-pointer)
                     (save-alien-stack
                      (make-array 0 :element-type '(unsigned-byte 32)))
                   ;; Allocate a stack-group structure.
                   (%make-stack-group
                    :name name
                    :state :active
                    :control-stack-id control-stack-id
                    ;; Misc stacks.
                    :current-catch-block sb-vm::*current-catch-block*
                    :current-unwind-protect-block
                    sb-vm::*current-unwind-protect-block*
                    ;; Alien stack.
                    :alien-stack alien-stack
                    :alien-stack-size alien-stack-size
                    :alien-stack-pointer alien-stack-pointer
                    ;; Interrupt contexts
                    :interrupt-contexts interrupt-contexts
                    ;; Binding stack.
                    :binding-stack binding-stack
                    :binding-stack-size binding-stack-size
                    ;; Resumer
                    :resumer resumer))))))
         ;; Allocate a new stack group with fresh stacks and bindings.
         (allocate-new-stack-group (control-stack-id)
           (let ((binding-stack (initial-binding-stack)))
             ;; Allocate a stack-group structure.
             (%make-stack-group
              :name name
              :state :active
              :control-stack-id control-stack-id
              ;; Eval stack. Needs at least one element be because
              ;; push doubles the size when full.
              :eval-stack (make-array 32)
              :eval-stack-top 0
              ;; Misc stacks.
              :current-catch-block 0
              :current-unwind-protect-block 0
              ;; Alien stack.
              :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
              :alien-stack-size 0
              :alien-stack-pointer *alien-stack-top*
              ;; Interrupt contexts
              :interrupt-contexts (make-array 0 :element-type
                                              '(unsigned-byte 32))
              ;; Binding stack - some initial bindings.
              :binding-stack binding-stack
              :binding-stack-size (length binding-stack)
              ;; Resumer
              :resumer resumer))))
    (let ((child-stack-group nil))
      (let ((sb-unix::*interrupts-enabled* nil)
            (sb-kernel::*gc-inhibit* t))
        (multiple-value-bind (control-stack control-stack-id)
            (allocate-control-stack)
          (setq child-stack-group
                (if inherit
                    (allocate-child-stack-group control-stack-id)
                    (allocate-new-stack-group control-stack-id)))
          ;; Fork the control-stack
          (if (control-stack-fork control-stack inherit)
              ;; Current-stack-group returns the child-stack-group.
              child-stack-group
              ;; Child starts.
              (unwind-protect
                   (progn
                     (setq *current-stack-group* child-stack-group)
                     (assert (eq *current-stack-group*
                                 (process-stack-group *current-process*)))
                     ;; Enable interrupts and GC.
                     (setf sb-unix::*interrupts-enabled* t)
                     (setf sb-kernel::*gc-inhibit* nil)
                     (when sb-unix::*interrupt-pending*
                       (sb-unix::do-pending-interrupt))
                     (when sb-kernel::*need-to-collect-garbage*
                       (sb-kernel::maybe-gc))
                     (funcall initial-function))
                (let ((resumer (stack-group-resumer child-stack-group)))
                  ;; Disable interrupts and GC.
                  (setf sb-unix::*interrupts-enabled* nil)
                  (setf sb-kernel::*gc-inhibit* t)
                  (inactivate-stack-group child-stack-group)
                  ;; Verify the resumer.
                  (unless (and resumer
                               (eq (stack-group-state resumer) :active))
                    (format t "*Resuming stack-group ~s instead of ~s~%"
                            *initial-stack-group* resumer)
                    (setq resumer *initial-stack-group*))
                  ;; Restore the resumer state.
                  (setq *current-stack-group* resumer)
                  ;; The binding stack.  Note that
                  ;; *interrutps-enabled* could be briefly set during
                  ;; the unbinding and re-binding process so signals
                  ;; are blocked.
                  (let ((old-sigs (sb-unix::pthread-sigmask
                                   sb-unix::SIG_BLOCK
                                   (sigmask sb-unix:sigint sb-unix:sigalrm)
                                   nil)))
                    (declare (type (unsigned-byte 32) old-sigs))
                    (unbind-binding-stack)
                    (restore-binding-stack
                     (stack-group-binding-stack resumer)
                     (stack-group-binding-stack-size resumer))
                    (rebind-binding-stack)
                    (sb-unix::pthread-sigmask sb-unix::SIG_SETMASK old-sigs nil))
                  ;; Misc stacks.
                  (setf sb-kernel::*current-catch-block*
                        (stack-group-current-catch-block resumer))
                  (setf sb-kernel::*current-unwind-protect-block*
                        (stack-group-current-unwind-protect-block resumer))
                  ;; The Alien stack
                  (restore-alien-stack
                   (stack-group-alien-stack resumer)
                   (stack-group-alien-stack-size resumer)
                   (stack-group-alien-stack-pointer resumer))
                  ;; Interrupt-contexts.
                  (restore-interrupt-contexts
                   (stack-group-interrupt-contexts resumer))
                  ;;
                  (let ((new-control-stack
                          (aref *control-stacks*
                                (stack-group-control-stack-id resumer))))
                    (declare (type (simple-array (unsigned-byte 32) (*))
                                   new-control-stack))
                    (control-stack-return new-control-stack)))))))
      (when (and sb-unix::*interrupts-enabled* sb-unix::*interrupt-pending*)
        (sb-unix::do-pending-interrupt))
      (when (and sb-kernel::*need-to-collect-garbage* (not sb-kernel::*gc-inhibit*))
        (sb-kernel::maybe-gc))
      child-stack-group)))


;;; Stack-Group-Resume -- Interface
;;;
;;; Transfer control to the given stack-group, resuming its execution,
;;; and saving the *current-stack-group*.
;;;
(defun stack-group-resume (new-stack-group)
  (declare (type stack-group new-stack-group)
           (optimize (speed 3)))
  (assert (and (eq (stack-group-state new-stack-group) :active)
               (not (eq new-stack-group *current-stack-group*))))
  (assert (eq new-stack-group (process-stack-group *current-process*)))
  (let ((sb-unix::*interrupts-enabled* nil)
        (sb-kernel::*gc-inhibit* t))
    (let* (;; Save the current stack-group on its stack.
           (stack-group *current-stack-group*)
           ;; Find the required stack size.
           (control-stack-end
             (sb-alien:extern-alien "control_stack_end" sb-alien:unsigned))
           (control-stack-pointer (sb-kernel:control-stack-pointer-sap))
           (control-stack-size (- control-stack-end
                                  (sb-sys:sap-int control-stack-pointer)))
           ;; Stack-save array needs three extra elements. The stack
           ;; pointer will be stored in the first, and the frame
           ;; pointer and return address push onto the bottom of the
           ;; stack.
           (save-stack-size (+ (ceiling control-stack-size 4) 3))
           ;; The save-stack vector.
           (control-stack (aref *control-stacks*
                                (stack-group-control-stack-id stack-group))))
      (declare (type (unsigned-byte 29) control-stack-size save-stack-size)
               (type (simple-array (unsigned-byte 32) (*)) control-stack))
      ;; Increase the save-stack size if necessary.
      (when (> save-stack-size (length control-stack))
        (setf control-stack (adjust-array control-stack save-stack-size
                                          :element-type '(unsigned-byte 32)
                                          :initial-element 0))
        (setf (aref *control-stacks*
                    (stack-group-control-stack-id stack-group))
              control-stack))

      ;; Misc stacks.
      (setf (stack-group-current-catch-block stack-group)
            sb-vm::*current-catch-block*)
      (setf (stack-group-current-unwind-protect-block stack-group)
            sb-vm::*current-unwind-protect-block*)
      (setf sb-vm::*current-catch-block*
            (stack-group-current-catch-block new-stack-group))
      (setf sb-vm::*current-unwind-protect-block*
            (stack-group-current-unwind-protect-block new-stack-group))

      ;; Save the interrupt-contexts.
      (setf (stack-group-interrupt-contexts stack-group)
            (save-interrupt-contexts
             (stack-group-interrupt-contexts stack-group)))

      ;; The binding stack.  Note that *interrutps-enabled* could be
      ;; briefly set during the unbinding and re-binding process so
      ;; signals are blocked.
      (let ((old-sigs (sb-unix::pthread-sigmask
                       sb-unix::SIG_BLOCK
                       (sigmask sb-unix:sigint sb-unix:sigalrm)
                       nil)))
        (declare (type (unsigned-byte 32) old-sigs))
        (unbind-binding-stack)
        (multiple-value-bind (stack size)
            (save-binding-stack (stack-group-binding-stack stack-group))
          (setf (stack-group-binding-stack stack-group) stack)
          (setf (stack-group-binding-stack-size stack-group) size))
        (restore-binding-stack (stack-group-binding-stack new-stack-group)
                               (stack-group-binding-stack-size
                                new-stack-group))
        (rebind-binding-stack)
        (sb-unix::pthread-sigmask sb-unix::SIG_SETMASK old-sigs nil))

      ;; Restore the interrupt-contexts.
      (restore-interrupt-contexts
       (stack-group-interrupt-contexts new-stack-group))

      ;; The Alien stack
      (multiple-value-bind (save-stack size alien-stack)
          (save-alien-stack (stack-group-alien-stack stack-group))
        (setf (stack-group-alien-stack stack-group) save-stack)
        (setf (stack-group-alien-stack-size stack-group) size)
        (setf (stack-group-alien-stack-pointer stack-group) alien-stack))
      (restore-alien-stack (stack-group-alien-stack new-stack-group)
                           (stack-group-alien-stack-size new-stack-group)
                           (stack-group-alien-stack-pointer new-stack-group))
      ;;
      (let ((new-control-stack
              (aref *control-stacks*
                    (stack-group-control-stack-id new-stack-group))))
        (declare (type (simple-array (unsigned-byte 32) (*))
                       new-control-stack))
        (control-stack-resume control-stack new-control-stack))
      ;; Thread returns.
      (setq *current-stack-group* stack-group)))
  (assert (eq *current-stack-group* (process-stack-group *current-process*)))
  (when (and sb-unix::*interrupts-enabled* sb-unix::*interrupt-pending*)
    (sb-unix::do-pending-interrupt))
  (when (and sb-kernel::*need-to-collect-garbage* (not sb-kernel::*gc-inhibit*))
    (sb-kernel::maybe-gc))
  (values))

;;;; VOPs

(defknown control-stack-fork ((simple-array (unsigned-byte 32) (*)) t)
  (member t nil))

(define-vop (control-stack-fork)
  (:policy :fast-safe)
  (:translate control-stack-fork)
  (:args (save-stack :scs (descriptor-reg) :to :result)
         (inherit :scs (descriptor-reg)))
  (:arg-types simple-array-unsigned-byte-32 *)
  (:results (child :scs (descriptor-reg)))
  (:result-types t)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) index)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) stack)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) temp)
  (:save-p t)
  (:generator 25
    (inst cmp inherit nil-value)
    (inst jmp :e FRESH-STACK)

    ;; Child inherits the stack of the parent.

    ;; Setup the return context.
    (inst push (make-fixup nil :code-object return))
    (inst push ebp-tn)
    ;; Save the stack.
    (inst xor index index)
    ;; First the stack-pointer.
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
                       :disp (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                                sb-vm:other-pointer-type))
          esp-tn)
    (inst inc index)
    (load-foreign-data-symbol stack "control_stack_end")
    (inst mov stack (make-ea :dword :base stack))
    (inst jmp-short LOOP)

    FRESH-STACK
    ;; Child has a fresh control stack.

    ;; Setup the return context.
    (inst push (make-fixup nil :code-object return))
    (load-foreign-data-symbol stack "control_stack_end")
    (inst mov stack (make-ea :dword :base stack))
    ;; New FP is the Top of the stack.
    (inst push stack)
    ;; Save the stack.
    (inst xor index index)
    ;; First save the adjusted stack-pointer.
    (inst sub stack ebp-tn)
    (inst add stack esp-tn)
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
                       :disp (- (* vm:vector-data-offset vm:word-bytes)
                                vm:other-pointer-type))
          stack)
    ;; Save the current frame, replacing the OCFP and RA by 0.
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
                       :disp (- (* (+ vm:vector-data-offset 1) vm:word-bytes)
                                vm:other-pointer-type))
          0)
    ;; Save 0 for the OCFP.
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
                       :disp (- (* (+ vm:vector-data-offset 2) vm:word-bytes)
                                vm:other-pointer-type))
          0)
    (inst add index 3)
    ;; Copy the remainder of the frame, skiping the OCFP and RA which
    ;; are saved above.
    (inst lea stack (make-ea :byte :base ebp-tn :disp -8))

    LOOP
    (inst cmp stack esp-tn)
    (inst jmp :le stack-save-done)
    (inst sub stack 4)
    (inst mov temp (make-ea :dword :base stack))
    (inst mov (make-ea :dword :base save-stack :index index :scale 4
                       :disp (- (* vm:vector-data-offset vm:word-bytes)
                                vm:other-pointer-type))
          temp)
    (inst inc index)
    (inst jmp-short LOOP)

    RETURN
    ;; Stack already clean if it reaches here. Parent returns NIL.
    (inst mov child nil-value)
    (inst jmp-short DONE)

    STACK-SAVE-DONE
    ;; Cleanup the stack
    (inst add esp-tn 8)
    ;; Child returns T.
    (load-symbol child t)
    DONE))

(defknown control-stack-resume ((simple-array (unsigned-byte 32) (*))
                                (simple-array (unsigned-byte 32) (*)))
    (values))

(define-vop (control-stack-resume)
  (:policy :fast-safe)
  (:translate control-stack-resume)
  (:args (save-stack :scs (descriptor-reg) :to :result)
         (new-stack :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-32 simple-array-unsigned-byte-32)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) index)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) stack)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) temp)
  (:save-p t)
  (:generator 25
              ;; Setup the return context.
              (inst push (make-fixup nil :code-object RETURN))
              (inst push ebp-tn)
              ;; Save the stack.
              (inst xor index index)
              ;; First the stack-pointer.
              (inst mov (make-ea :dword :base save-stack :index index :scale 4
                                        :disp (- (* vm:vector-data-offset vm:word-bytes)
                                                 vm:other-pointer-type))
                    esp-tn)
              (inst inc index)
              (load-foreign-data-symbol stack "control_stack_end")
              (inst mov stack (make-ea :dword :base stack))
              LOOP
              (inst cmp stack esp-tn)
              (inst jmp :le STACK-SAVE-DONE)
              (inst sub stack 4)
              (inst mov temp (make-ea :dword :base stack))
              (inst mov (make-ea :dword :base save-stack :index index :scale 4
                                        :disp (- (* vm:vector-data-offset vm:word-bytes)
                                                 vm:other-pointer-type))
                    temp)
              (inst inc index)
              (inst jmp-short LOOP)

              STACK-SAVE-DONE
              ;; Cleanup the stack
              (inst add esp-tn 8)

              ;; Restore the new-stack.
              (inst xor index index)
              ;; First the stack-pointer.
              (inst mov esp-tn
                    (make-ea :dword :base new-stack :index index :scale 4
                                    :disp (- (* vm:vector-data-offset vm:word-bytes)
                                             vm:other-pointer-type)))
              (inst inc index)
              (load-foreign-data-symbol stack "control_stack_end")
              (inst mov stack (make-ea :dword :base stack))
              LOOP2
              (inst cmp stack esp-tn)
              (inst jmp :le STACK-RESTORE-DONE)
              (inst sub stack 4)
              (inst mov temp (make-ea :dword :base new-stack :index index :scale 4
                                             :disp (- (* vm:vector-data-offset vm:word-bytes)
                                                      vm:other-pointer-type)))
              (inst mov (make-ea :dword :base stack) temp)
              (inst inc index)
              (inst jmp-short LOOP2)
              STACK-RESTORE-DONE
              ;; Pop the frame pointer, and resume at the return address.
              (inst pop ebp-tn)
              (inst ret)

              ;; Original thread resumes, stack has been cleaned up.
              RETURN))


(defknown control-stack-return ((simple-array (unsigned-byte 32) (*)))
    (values))

(define-vop (control-stack-return)
  (:policy :fast-safe)
  (:translate control-stack-return)
  (:args (new-stack :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-32)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) index)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) stack)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to (:eval 1)) temp)
  (:save-p t)
  (:generator 25
              ;; Restore the new-stack.
              (inst xor index index)
              ;; First the stack-pointer.
              (inst mov esp-tn
                    (make-ea :dword :base new-stack :index index :scale 4
                                    :disp (- (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                                             sb-vm:other-pointer-type)))
              (inst inc index)
              (load-foreign-data-symbol stack "control_stack_end")
              (inst mov stack (make-ea :dword :base stack))
              LOOP
              (inst cmp stack esp-tn)
              (inst jmp :le STACK-RESTORE-DONE)
              (inst sub stack 4)
              (inst mov temp (make-ea :dword :base new-stack :index index :scale 4
                                             :disp (- (* vm:vector-data-offset vm:word-bytes)
                                                      vm:other-pointer-type)))
              (inst mov (make-ea :dword :base stack) temp)
              (inst inc index)
              (inst jmp-short LOOP)
              STACK-RESTORE-DONE
              ;; Pop the frame pointer, and resume at the return address.
              (inst pop ebp-tn)
              (inst ret)))

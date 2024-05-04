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

;;; coroutine structure.
(defstruct (coroutine
            (:constructor %make-coroutine)
            (:print-function
             (lambda (coroutine stream depth)
               (declare (type coroutine coroutine)
                        (stream stream)
                        (ignore depth))
               (print-unreadable-object (coroutine stream :identity t)
                 (format stream "Coroutine ~a, ~a"
                         (coroutine-name coroutine)
                         (coroutine-state coroutine))))))
  ;; Must have a name.
  (name "Anonymous" :type simple-base-string)
  ;; State: :active or :inactive.
  (state :inactive :type (member :active :inactive))
  ;; The control stack; an index into *control-stacks*.
  (control-stack-id nil :type (or sb-kernel::index null))
  ;; Current catch block, on the control stack.
  (current-catch-block 0 :type fixnum)
  ;; Unwind protect block, on the control stack.
  (current-unwind-protect-block 0 :type fixnum)
  ;; Alien stack
  (alien-stack nil :type (or (simple-array (unsigned-byte 32) (*)) null))
  (alien-stack-size 0 :type (unsigned-byte 29))
  (alien-stack-pointer 0 :type (unsigned-byte 32))
  ;; Resumer
  (resumer nil :type (or coroutine null)))

;;; The current stack group.
(declaim (type (or coroutine null) *current-coroutine*))
(defvar *current-coroutine* nil)

(declaim (type (or coroutine null) *initial-coroutine*))
(defvar *initial-coroutine* nil)

(defun convert-thread-to-coroutine ()
  (setf *initial-coroutine*))

(defun make-coroutine (name initial-function)
  (declare (type simple-base-string name)
           (type function initial-function)
           (type coroutine resumer))
  (flet ((allocate-control-stack ()
           (let* (;; Allocate a new control-stack ID.
                  (control-stack-id (position nil *control-stacks*))
                  ;; Find the required stack size.
                  (control-stack-size
                    (- (sb-sys:sap-int sb-vm:*control-stack-end*)
                       (sb-sys:sap-int sb-vm:*control-stack-start*)))
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
         ;; Allocate a new stack group with fresh stacks and bindings.
         (allocate-new-coroutine (control-stack-id)
           ;; Allocate a coroutine structure.
           (%make-coroutine
            :name name
            :state :active
            :control-stack-id control-stack-id
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
            ;; Resumer
            :resumer resumer)))
    (let ((child-coroutine nil))
      (let ((sb-unix::*interrupts-enabled* nil)
            (sb-kernel::*gc-inhibit* t))
        (multiple-value-bind (control-stack control-stack-id)
            (allocate-control-stack)
          (setq child-coroutine (allocate-new-coroutine control-stack-id))
          ;; Fork the control-stack
          (if (control-stack-fork control-stack inherit)
              ;; Current-coroutine returns the child-coroutine.
              child-coroutine
              ;; Child starts.
              (unwind-protect
                   (progn
                     (setq *current-coroutine* child-coroutine)
                     (assert (eq *current-coroutine*
                                 (process-coroutine *current-process*)))
                     ;; Enable interrupts and GC.
                     (setf sb-unix::*interrupts-enabled* t)
                     (setf sb-kernel::*gc-inhibit* nil)
                     (when sb-unix::*interrupt-pending*
                       (sb-unix::do-pending-interrupt))
                     (when sb-kernel::*need-to-collect-garbage*
                       (sb-kernel::maybe-gc))
                     (funcall initial-function))
                (let ((resumer (coroutine-resumer child-coroutine)))
                  ;; Disable interrupts and GC.
                  (setf sb-unix::*interrupts-enabled* nil)
                  (setf sb-kernel::*gc-inhibit* t)
                  (inactivate-coroutine child-coroutine)
                  ;; Verify the resumer.
                  (unless (and resumer
                               (eq (coroutine-state resumer) :active))
                    (format t "*Resuming coroutine ~s instead of ~s~%"
                            *initial-coroutine* resumer)
                    (setq resumer *initial-coroutine*))
                  ;; Restore the resumer state.
                  (setq *current-coroutine* resumer)
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
                     (coroutine-binding-stack resumer)
                     (coroutine-binding-stack-size resumer))
                    (rebind-binding-stack)
                    (sb-unix::pthread-sigmask sb-unix::SIG_SETMASK old-sigs nil))
                  ;; Misc stacks.
                  (setf sb-kernel::*current-catch-block*
                        (coroutine-current-catch-block resumer))
                  (setf sb-kernel::*current-unwind-protect-block*
                        (coroutine-current-unwind-protect-block resumer))
                  ;; The Alien stack
                  (restore-alien-stack
                   (coroutine-alien-stack resumer)
                   (coroutine-alien-stack-size resumer)
                   (coroutine-alien-stack-pointer resumer))
                  ;; Interrupt-contexts.
                  (restore-interrupt-contexts
                   (coroutine-interrupt-contexts resumer))
                  ;;
                  (let ((new-control-stack
                          (aref *control-stacks*
                                (coroutine-control-stack-id resumer))))
                    (declare (type (simple-array (unsigned-byte 32) (*))
                                   new-control-stack))
                    (control-stack-return new-control-stack)))))))
      (when (and sb-unix::*interrupts-enabled* sb-unix::*interrupt-pending*)
        (sb-unix::do-pending-interrupt))
      (when (and sb-kernel::*need-to-collect-garbage* (not sb-kernel::*gc-inhibit*))
        (sb-kernel::maybe-gc))
      child-coroutine)))

(defun coroutine-resume (new-coroutine)
  (declare (type coroutine new-coroutine)
           (optimize (speed 3)))
  (assert (and (eq (coroutine-state new-coroutine) :active)
               (not (eq new-coroutine *current-coroutine*))))
  (assert (eq new-coroutine (process-coroutine *current-process*)))
  (let ((sb-unix::*interrupts-enabled* nil)
        (sb-kernel::*gc-inhibit* t))
    (let* (;; Save the current coroutine on its stack.
           (coroutine *current-coroutine*)
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
                                (coroutine-control-stack-id coroutine))))
      (declare (type (unsigned-byte 29) control-stack-size save-stack-size)
               (type (simple-array (unsigned-byte 32) (*)) control-stack))
      ;; Increase the save-stack size if necessary.
      (when (> save-stack-size (length control-stack))
        (setf control-stack (adjust-array control-stack save-stack-size
                                          :element-type '(unsigned-byte 32)
                                          :initial-element 0))
        (setf (aref *control-stacks*
                    (coroutine-control-stack-id coroutine))
              control-stack))

      ;; Misc stacks.
      (setf (coroutine-current-catch-block coroutine)
            sb-vm::*current-catch-block*)
      (setf (coroutine-current-unwind-protect-block coroutine)
            sb-vm::*current-unwind-protect-block*)
      (setf sb-vm::*current-catch-block*
            (coroutine-current-catch-block new-coroutine))
      (setf sb-vm::*current-unwind-protect-block*
            (coroutine-current-unwind-protect-block new-coroutine))

      ;; Save the interrupt-contexts.
      (setf (coroutine-interrupt-contexts coroutine)
            (save-interrupt-contexts
             (coroutine-interrupt-contexts coroutine)))

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
            (save-binding-stack (coroutine-binding-stack coroutine))
          (setf (coroutine-binding-stack coroutine) stack)
          (setf (coroutine-binding-stack-size coroutine) size))
        (restore-binding-stack (coroutine-binding-stack new-coroutine)
                               (coroutine-binding-stack-size
                                new-coroutine))
        (rebind-binding-stack)
        (sb-unix::pthread-sigmask sb-unix::SIG_SETMASK old-sigs nil))

      ;; Restore the interrupt-contexts.
      (restore-interrupt-contexts
       (coroutine-interrupt-contexts new-coroutine))

      ;; The Alien stack
      (multiple-value-bind (save-stack size alien-stack)
          (save-alien-stack (coroutine-alien-stack coroutine))
        (setf (coroutine-alien-stack coroutine) save-stack)
        (setf (coroutine-alien-stack-size coroutine) size)
        (setf (coroutine-alien-stack-pointer coroutine) alien-stack))
      (restore-alien-stack (coroutine-alien-stack new-coroutine)
                           (coroutine-alien-stack-size new-coroutine)
                           (coroutine-alien-stack-pointer new-coroutine))
      ;;
      (let ((new-control-stack
              (aref *control-stacks*
                    (coroutine-control-stack-id new-coroutine))))
        (declare (type (simple-array (unsigned-byte 32) (*))
                       new-control-stack))
        (control-stack-resume control-stack new-control-stack))
      ;; Thread returns.
      (setq *current-coroutine* coroutine)))
  (assert (eq *current-coroutine* (process-coroutine *current-process*)))
  (when (and sb-unix::*interrupts-enabled* sb-unix::*interrupt-pending*)
    (sb-unix::do-pending-interrupt))
  (when (and sb-kernel::*need-to-collect-garbage* (not sb-kernel::*gc-inhibit*))
    (sb-kernel::maybe-gc))
  (values))

;;;; VOPs

(sb-c:defknown control-stack-fork ((simple-array (unsigned-byte 32) (*)) t)
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

(sb-c:defknown control-stack-resume ((simple-array (unsigned-byte 32) (*))
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


(sb-c:defknown control-stack-return ((simple-array (unsigned-byte 32) (*)))
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

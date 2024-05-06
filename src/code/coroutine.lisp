(in-package "SB-VM")

(defknown control-stack-fork ((simple-array (unsigned-byte 64) (*)))
  (member t nil))

(define-vop (control-stack-fork)
  (:policy :fast-safe)
  (:translate control-stack-fork)
  (:args (save-stack :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-64)
  (:results (child :scs (descriptor-reg)))
  (:result-types t)
  (:temporary (:sc unsigned-reg) index)
  (:temporary (:sc unsigned-reg) stack)
  (:temporary (:sc unsigned-reg) temp)
  (:temporary (:sc unsigned-reg) temp2)
  (:save-p t)
  (:generator 25
    ;; Setup the return context.
    (inst adr temp return)
    (inst str temp (@ csp-tn))
    (inst add csp-tn csp-tn sb-vm:n-word-bytes)

    (loadw stack thread-tn thread-control-stack-start-slot)

    ;; New FP is the Top of the stack.
    (inst str stack (@ csp-tn))
    (inst add csp-tn csp-tn sb-vm:n-word-bytes)
    ;; Save the stack.
    (move index zr-tn)
    ;; First save the adjusted stack-pointer.
    (inst add stack stack cfp-tn)
    (inst sub stack stack csp-tn)
    (inst add temp save-stack (lsl index word-shift))
    (storew stack temp sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    ;; Save the current frame, replacing the OCFP and RA by 0.
    (storew zr-tn temp (+ 1 sb-vm:vector-data-offset) sb-vm:other-pointer-lowtag)
    ;; Save 0 for the OCFP.
    (storew zr-tn temp (+ 2 sb-vm:vector-data-offset) sb-vm:other-pointer-lowtag)
    (inst add index index 3)
    ;; Copy the remainder of the frame, skiping the OCFP and RA which
    ;; are saved above.

    (format t "~a" stack)

    (inst add stack cfp-tn (* 2 sb-vm:n-word-bytes))

    LOOP
    (inst cmp stack csp-tn)
    (inst b :ge stack-save-done)
    (inst add stack stack sb-vm:n-word-bytes)
    (inst ldr temp (@ stack))
    (inst add temp2 save-stack (lsl index word-shift))
    (storew temp temp2 sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst add index index 1)
    (inst b LOOP)
    
    RETURN
    ;; Stack already clean if it reaches here. Parent returns NIL.
    (move child null-tn)
    (inst b DONE)
    
    STACK-SAVE-DONE
    ;; Cleanup the stack
    (inst sub csp-tn csp-tn (* 2 sb-vm:n-word-bytes))
    ;; Child returns T.
    (load-symbol child t)
    DONE))

(defknown control-stack-swap ((simple-array (unsigned-byte 64) (*))
                              (simple-array (unsigned-byte 64) (*)))
    (values))

(define-vop (control-stack-swap)
  (:policy :fast-safe)
  (:translate control-stack-swap)
  (:args (resumer-stack :scs (descriptor-reg) :to :result)
         (resumee-stack :scs (descriptor-reg) :to :result))
  (:temporary (:sc unsigned-reg) jump-addr)
  (:arg-types simple-array-unsigned-byte-64 simple-array-unsigned-byte-64)
  (:save-p t)
  (:generator 25
              (inst str csp-tn (@ resumer-stack (* 0 sb-vm:n-word-bytes)))
              (inst ldr csp-tn (@ resumee-stack (* 0 sb-vm:n-word-bytes)))
              (inst ldr jump-addr (@ csp-tn (* -1 sb-vm:n-word-bytes)))
              (inst ret jump-addr)))

(defknown control-stack-resume ((simple-array (unsigned-byte 64) (*))
				(simple-array (unsigned-byte 64) (*)))
  (values))

(define-vop (control-stack-resume)
  (:policy :fast-safe)
  (:translate control-stack-resume)
  (:args (save-stack :scs (descriptor-reg) :to :result)
	 (new-stack :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-64 simple-array-unsigned-byte-64)
  (:temporary (:sc unsigned-reg) index)
  (:temporary (:sc unsigned-reg) stack)
  (:temporary (:sc unsigned-reg) temp)
  (:save-p t)
  (:generator 25
    ;; Setup the return context.
    (inst adr temp return)
    (inst str temp (@ csp-tn))
    (inst add csp-tn csp-tn sb-vm:n-word-bytes)

    (inst str cfp-tn (@ csp-tn))
    (inst add csp-tn csp-tn sb-vm:n-word-bytes)
    ;; Save the stack.
    (move index zr-tn)
    ;; First the stack-pointer.
    (inst add temp save-stack (lsl index word-shift))
    (storew csp-tn temp sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst add index index 1)
    (loadw stack thread-tn thread-control-stack-start-slot)

    LOOP
    (inst cmp stack csp-tn)
    (inst b :ge STACK-SAVE-DONE)
    (inst add stack stack sb-vm:n-word-bytes)
    (inst ldr temp (@ stack))
    (inst add temp2 save-stack (lsl index word-shift))
    (storew temp temp2 sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst add index index 1)
    (inst b LOOP)

    STACK-SAVE-DONE
    ;; Cleanup the stack
    (inst sub csp-tn csp-tn (* 2 sb-vm:n-word-bytes))

    ;; Restore the new-stack.
    (move index zr-tn)
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

(defpackage "SB-COROUTINE"
  (:documentation "Coroutines!!!!!!")
  (:use "CL"))

(in-package "SB-COROUTINE")

(declaim (type (simple-array (or null (simple-array (unsigned-byte 64) (*)))
                             (*)) *control-stacks*))
(defvar *control-stacks*
  (make-array 10 :element-type '(or null (unsigned-byte 64))
                 :initial-element nil))

(defstruct (coroutine
            (:constructor %make-coroutine)
            (:print-function
             (lambda (coroutine stream depth)
               (declare (type coroutine coroutine)
                        (stream stream)
                        (ignore depth))
               (print-unreadable-object (coroutine stream :identity t)
                 (format stream "Coroutine ~a, ~a"
                         (sb-kernel:get-lisp-obj-address coroutine)
                         (coroutine-state coroutine))))))
  (state :inactive :type (member :active :inactive))
  (control-stack-id nil :type (or sb-kernel::index null))
  (current-catch-block 0 :type fixnum)
  (current-unwind-protect-block 0 :type fixnum)
  (alien-stack nil :type (or (simple-array (unsigned-byte 64) (*)) null))
  (alien-stack-pointer 0 :type (unsigned-byte 32))
  (resumer nil :type (or coroutine null)))

(declaim (type (or coroutine null) *current-coroutine*))
(defvar *current-coroutine* nil)

(defun convert-thread-to-coroutine ()
  (setf *current-coroutine*
        (%make-coroutine :state :active
                         :control-stack-id nil
                         :current-catch-block sb-vm::*current-catch-block*
                         :current-unwind-protect-block sb-vm::*current-unwind-protect-block*
                         :alien-stack nil
                         :alien-stack-pointer sb-vm::*alien-stack-pointer*
                         :resumer nil)))

(defun make-coroutine (initial-function)
  (declare (type function initial-function))
  (flet ((allocate-control-stack ()
           (let* (;; Allocate a new control-stack ID.
                  (control-stack-id (position nil *control-stacks*))
                  ;; Find the required stack size.
                  (control-stack-size
                    (- sb-vm:*control-stack-end* sb-vm:*control-stack-start*))
                  ;; Saved control stack needs three extra words. The
                  ;; stack pointer will be stored in the first
                  ;; element, and the frame pointer and return address
                  ;; push onto the bottom of the stack.
                  (control-stack
                    (make-array (+ (ceiling control-stack-size 4) 3)
                                :element-type '(unsigned-byte 64)
                                :initial-element 0)))
             (declare (type (unsigned-byte 29) control-stack-size))
             (unless control-stack-id
               ;; Need to extend the *control-stacks* vector.
               (setf control-stack-id (length *control-stacks*))
               (setf *control-stacks*
                     (adjust-array *control-stacks*
                                   (* 2 (length *control-stacks*))
                                   :element-type '(or null (unsigned-byte 64))
                                   :initial-element nil)))
             (setf (aref *control-stacks* control-stack-id) control-stack)
             (values control-stack control-stack-id)))
         ;; Allocate a new stack group with fresh stacks and bindings.
         (allocate-new-coroutine (control-stack-id)
           ;; Allocate a coroutine structure.
           (%make-coroutine
            :state :active
            :control-stack-id control-stack-id
            :current-catch-block 0
            :current-unwind-protect-block 0
            :alien-stack (make-array 0 :element-type '(unsigned-byte 64))
            :alien-stack-pointer 0
            :resumer nil)))
    (let ((child-coroutine nil))
      (sb-sys:without-interrupts
        (sb-sys:without-gcing
          (multiple-value-bind (control-stack control-stack-id)
              (allocate-control-stack)
            (setq child-coroutine (allocate-new-coroutine control-stack-id))
            (print (sb-vm::control-stack-fork control-stack)))))
      child-coroutine)))

(defun coroutine-resume (resumee)
  (declare (type coroutine resumee)
           (optimize (speed 3)))
  (assert (and (eq (coroutine-state resumee) :active)
               (not (eq resumee *current-coroutine*))))
  (sb-sys:without-gcing
    (let* (;; Save the current coroutine on its stack.
           (resumer *current-coroutine*)
           ;; The save-stack vector.
           (resumer-control-stack
             (let ((stack-id (coroutine-control-stack-id resumer)))
               (if (null stack-id)
                   (make-array 1024 :element-type '(unsigned-byte 64))
                   (aref *control-stacks* stack-id)))))
      (declare (type (simple-array (unsigned-byte 64) (*)) resumer-control-stack))
      ;; Misc stacks.
      (setf (coroutine-current-catch-block resumer)
            sb-vm::*current-catch-block*)
      (setf (coroutine-current-unwind-protect-block resumer)
            sb-vm::*current-unwind-protect-block*)
      (setf (coroutine-alien-stack-pointer resumer)
            sb-vm::*alien-stack-pointer*)
      (setf sb-vm::*current-catch-block*
            (coroutine-current-catch-block resumee))
      (setf sb-vm::*current-unwind-protect-block*
            (coroutine-current-unwind-protect-block resumee))
      (setf sb-vm::*alien-stack-pointer*
            (coroutine-alien-stack-pointer resumee))

      (let ((resumee-control-stack
              (aref *control-stacks*
                    (coroutine-control-stack-id resumee))))
        (declare (type (simple-array (unsigned-byte 64) (*))
                       resumee-control-stack))
        (format t "about to swap control stacks~%")
        (sb-vm::control-stack-swap resumer-control-stack resumee-control-stack))
      ;; Thread returns.
      (setq *current-coroutine* resumer)))
  (values))

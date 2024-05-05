(defpackage "SB-COROUTINE"
  (:documentation "Coroutines!!!!!!")
  (:use "CL"))

(in-package "SB-COROUTINE")

(declaim (type (simple-array (or null (simple-array (unsigned-byte 32) (*)))
                             (*)) *control-stacks*))
(defvar *control-stacks*
  (make-array 0 :element-type '(or null (unsigned-byte 32))
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
  (alien-stack nil :type (or (simple-array (unsigned-byte 32) (*)) null))
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
                                :element-type '(unsigned-byte 32)
                                :initial-element 0)))
             (declare (type (unsigned-byte 29) control-stack-size))
             (unless control-stack-id
               ;; Need to extend the *control-stacks* vector.
               (setf control-stack-id (length *control-stacks*))
               (setf *control-stacks*
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
            :state :active
            :control-stack-id control-stack-id
            :current-catch-block 0
            :current-unwind-protect-block 0
            :alien-stack (make-array 0 :element-type '(unsigned-byte 32))
            :alien-stack-pointer *alien-stack-top*
            :resumer nil)))
    (let ((child-coroutine nil))
      (sb-sys:without-interrupts
        (sb-sys:without-gcing
          (multiple-value-bind (control-stack control-stack-id)
              (allocate-control-stack)
            (setq child-coroutine (allocate-new-coroutine control-stack-id))
            (let ((trampoline
                    (lambda ()
                      (unwind-protect
                           (funcall initial-function)
                        (format t "uh oh the stack unwound!!!!! uh oh")))))
              (sb-sys:with-pinned-objects (trampoline)
                (setf (aref control-stack 1)
                      (sb-sys:sap-int (sb-vm:simple-fun-entry-sap trampoline))))))))
      child-coroutine)))

(defun coroutine-resume (resumee)
  (declare (type coroutine resumee)
           (optimize (speed 3)))
  (assert (and (eq (coroutine-state resumee) :active)
               (not (eq resumee *current-coroutine*))))
  (sb-sys:without-interrupts
    (sb-sys:without-gcing
      (let* (;; Save the current coroutine on its stack.
             (resumer *current-coroutine*)
             ;; The save-stack vector.
             (resumer-control-stack (aref *control-stacks*
                                          (coroutine-control-stack-id resumer))))
        (declare (type (simple-array (unsigned-byte 32) (*)) resumer-control-stack))
        ;; Misc stacks.
        (setf (coroutine-current-catch-block resumer)
              sb-vm::*current-catch-block*)
        (setf (coroutine-current-unwind-protect-block resumer)
              sb-vm::*current-unwind-protect-block*)
        (setf (coroutine-alien-stack-pointer resumer)
              sb-vm::*current-alien-stack-pointer*)
        (setf sb-vm::*current-catch-block*
              (coroutine-current-catch-block resumee))
        (setf sb-vm::*current-unwind-protect-block*
              (coroutine-current-unwind-protect-block resumee))
        (setf sb-vm::*alien-stack-pointer*
              (coroutine-alien-stack-pointer resumee))

        (let ((resumee-control-stack
                (aref *control-stacks*
                      (coroutine-control-stack-id resumee))))
          (declare (type (simple-array (unsigned-byte 32) (*))
                         resumee-control-stack))
          (control-stack-swap resumer-control-stack resumee-control-stack))
        ;; Thread returns.
        (setq *current-coroutine* resumer))))
  (values))

;;;; VOPs

(sb-c:defknown control-stack-swap ((simple-array (unsigned-byte 32) (*))
                                   (simple-array (unsigned-byte 32) (*)))
    (values))

(define-vop (control-stack-swap)
  (:policy :fast-safe)
  (:translate control-stack-swap)
  (:args (resumer-stack :scs (descriptor-reg) :to :result)
         (resumee-stack :scs (descriptor-reg) :to :result))
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

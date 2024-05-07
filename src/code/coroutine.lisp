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
  (:temporary (:sc unsigned-reg)
              temp)
  (:temporary (:sc unsigned-reg) temp2)
  (:save-p :force-to-stack)
  (:generator 25
    ;; Setup the return context.
    (inst adr temp RETURN)
    (inst add csp-tn csp-tn sb-vm:n-word-bytes)
    (inst str temp (@ csp-tn))

    (loadw stack thread-tn thread-control-stack-start-slot)

    ;; New FP is the Top of the stack.
    (inst add csp-tn csp-tn sb-vm:n-word-bytes)
    (inst str stack (@ csp-tn))
    #+nil (inst add csp-tn csp-tn sb-vm:n-word-bytes)
    ;; Save the stack.
    (move index zr-tn)
    ;; First save the adjusted stack-pointer.
    (inst add stack stack csp-tn)
    (inst sub stack stack cfp-tn)
    (inst add temp save-stack (lsl index word-shift))
    (storew stack temp sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)

    ;; Save the current frame, replacing the OCFP and RA by 0.
    (storew zr-tn temp (+ 1 sb-vm:vector-data-offset) sb-vm:other-pointer-lowtag)
    ;; Save 0 for the OCFP.
    (storew zr-tn temp (+ 2 sb-vm:vector-data-offset) sb-vm:other-pointer-lowtag)
    (inst add index index 3)
    ;; Copy the remainder of the frame, skiping the OCFP and RA which
    ;; are saved above.

    (inst add stack cfp-tn (* 2 sb-vm:n-word-bytes))

    LOOP
    (inst cmp stack csp-tn)
    (inst b :ge STACK-SAVE-DONE)
    (inst add stack stack sb-vm:n-word-bytes)
    (inst ldr temp (@ stack))
    (inst add temp2 save-stack (lsl index word-shift))
    (storew temp temp2 sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst add index index 1)
    (inst b LOOP)
    
    RETURN
    ;; Stack already clean if it reaches here. Child returns NIL.
    (move child null-tn)
    (inst b DONE)
    
    STACK-SAVE-DONE
    ;; Cleanup the stack
    (inst sub csp-tn csp-tn (* 2 sb-vm:n-word-bytes))
    ;; Parent returns T.
    (load-symbol child t)
    DONE))

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
  (:temporary (:sc unsigned-reg) temp2)
  (:save-p :force-to-stack)
  (:generator 25
    ;; Setup the return context.
    (inst adr temp RETURN)
    (inst add csp-tn csp-tn sb-vm:n-word-bytes)
    (inst str temp (@ csp-tn))

    (inst add csp-tn csp-tn sb-vm:n-word-bytes)
    (inst str cfp-tn (@ csp-tn))
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
    (inst add temp new-stack (lsl index word-shift))
    (loadw csp-tn temp sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst add index index 1)
    (loadw stack thread-tn thread-control-stack-start-slot)
    LOOP2
    (inst cmp stack csp-tn)
    (inst b :ge STACK-RESTORE-DONE)
    (inst add stack stack sb-vm:n-word-bytes)
    (inst add temp new-stack (lsl index word-shift))
    (loadw temp temp sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst str temp (@ stack))
    (inst add index index 1)
    (inst b LOOP2)

    STACK-RESTORE-DONE
    ;; Pop the frame pointer, and resume at the return address.
    (inst ldr cfp-tn (@ csp-tn))
    (inst sub csp-tn csp-tn sb-vm:n-word-bytes)
    (inst ldr lr-tn (@ csp-tn))
    (inst sub csp-tn csp-tn sb-vm:n-word-bytes)
    (inst ret lr-tn)
    
    ;; Original thread resumes, stack has been cleaned up.
    RETURN))

(defknown control-stack-return ((simple-array (unsigned-byte 64) (*)))
  (values))

(define-vop (control-stack-return)
  (:policy :fast-safe)
  (:translate control-stack-return)
  (:args (new-stack :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-64)
  (:temporary (:sc unsigned-reg) index)
  (:temporary (:sc unsigned-reg) stack)
  (:temporary (:sc unsigned-reg) temp)
  (:save-p :force-to-stack)
  (:generator 25
    ;; Restore the new-stack.
    #+nil
    (format t "new-stack: ~a, index: ~a, stack: ~a, temp: ~a~%" new-stack index stack temp)
    (move index zr-tn)
    ;; First the stack-pointer.
    (inst add temp new-stack (lsl index word-shift))
    (loadw csp-tn temp sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst add index index 1)
    (loadw stack thread-tn thread-control-stack-start-slot)

    LOOP
    (inst cmp stack csp-tn)
    (inst b :ge STACK-RESTORE-DONE)
    (inst add stack stack sb-vm:n-word-bytes)
    (inst add temp new-stack (lsl index word-shift))
    (loadw temp temp sb-vm:vector-data-offset sb-vm:other-pointer-lowtag)
    (inst str temp (@ stack))
    (inst add index index 1)
    (inst b LOOP)

    STACK-RESTORE-DONE
    ;; Pop the frame pointer, and resume at the return address.
    (inst ldr cfp-tn (@ csp-tn))
    (inst sub csp-tn csp-tn sb-vm:n-word-bytes)
    (inst ldr lr-tn (@ csp-tn))
    (inst sub csp-tn csp-tn sb-vm:n-word-bytes)
    (inst ret lr-tn)))

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

(declaim (type (or coroutine null) *initial-coroutine*))
(defvar *initial-coroutine* nil)

(defun allocate-control-stack ()
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

(defun convert-thread-to-coroutine ()
  (multiple-value-bind (control-stack control-stack-id)
      (allocate-control-stack)
    (declare (ignore control-stack))
    (setf *current-coroutine*
          (%make-coroutine :state :active
                           :control-stack-id control-stack-id
                           :current-catch-block sb-vm::*current-catch-block*
                           :current-unwind-protect-block sb-vm::*current-unwind-protect-block*
                           :alien-stack nil
                           :alien-stack-pointer sb-vm::*alien-stack-pointer*
                           :resumer nil))
    (setf *initial-coroutine* *current-coroutine*)))

(defun make-coroutine (initial-function)
  (declare (type function initial-function))
  (flet (;; Allocate a new stack group with fresh stacks and bindings.
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
      (let ((sb-unix::*interrupts-enabled* nil)
            (sb-kernel::*gc-inhibit* t)))
      (multiple-value-bind (control-stack control-stack-id)
          (allocate-control-stack)
        (setq child-coroutine (allocate-new-coroutine control-stack-id))
        (if (sb-vm::control-stack-fork control-stack)
            child-coroutine
            (unwind-protect
		 (progn
		   (setq *current-coroutine* child-coroutine)
		   ;; Enable interrupts and GC.
		   (setf sb-unix::*interrupts-enabled* t)
		   (setf sb-kernel::*gc-inhibit* nil)

		   (funcall initial-function))
	      (let ((resumer (coroutine-resumer *current-coroutine*)))
		;; Disable interrupts and GC.
		(setf sb-unix::*interrupts-enabled* nil)
		(setf sb-kernel::*gc-inhibit* t)
                ;; Inactivate the coroutine.
                (setf (coroutine-state *current-coroutine*) :inactive)
                ;; Verify the resumer.
		(unless (and resumer
			     (eq (coroutine-state resumer) :active))
		  #+nil(format t "*Resuming coroutine ~s instead of ~s~%"
			  *initial-coroutine* resumer)
		  (setq resumer *initial-coroutine*))
		;; Restore the resumer state.
		(setq *current-coroutine* resumer)
		;; Misc stacks.
		(setf sb-vm::*current-catch-block*
		      (coroutine-current-catch-block resumer))
		(setf sb-vm::*current-unwind-protect-block*
		      (coroutine-current-unwind-protect-block resumer))
		(let ((new-control-stack
			(aref *control-stacks*
			      (coroutine-control-stack-id resumer))))
		  (declare (type (simple-array (unsigned-byte 64) (*))
				 new-control-stack))
		  (sb-vm::control-stack-return new-control-stack)))))))))

(defun coroutine-resume (new-coroutine)
  (declare (type coroutine new-coroutine)
	   (optimize (speed 3)))
  (assert (and (eq (coroutine-state new-coroutine) :active)
	       (not (eq new-coroutine *current-coroutine*))))
  (let ((sb-unix::*interrupts-enabled* nil)
	(sb-kernel::*gc-inhibit* t))
    (let* (;; Save the current coroutine on its stack.
	   (coroutine *current-coroutine*)
	   ;; Find the required stack size.
	   (control-stack-size (- sb-vm::*control-stack-end*
				  sb-vm::*control-stack-start*))
	   ;; Stack-save array needs three extra elements. The stack
	   ;; pointer will be stored in the first, and the frame
	   ;; pointer and return address push onto the bottom of the
	   ;; stack.
	   (save-stack-size (+ (ceiling control-stack-size sb-vm:n-word-bytes) 3))
	   ;; The save-stack vector.
	   (control-stack (aref *control-stacks*
				(coroutine-control-stack-id coroutine))))
      (declare (type (unsigned-byte 29) control-stack-size save-stack-size)
	       (type (simple-array (unsigned-byte 64) (*)) control-stack))
      ;; Increase the save-stack size if necessary.
      (when (> save-stack-size (length control-stack))
	(setf control-stack (adjust-array control-stack save-stack-size
					  :element-type '(unsigned-byte 64)
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

      ;; 
      (let ((new-control-stack
	     (aref *control-stacks*
		   (coroutine-control-stack-id new-coroutine))))
	(declare (type (simple-array (unsigned-byte 64) (*))
		       new-control-stack))
        (setf (coroutine-resumer new-coroutine) coroutine)
	(sb-vm::control-stack-resume control-stack new-control-stack))
      ;; Thread returns.
      (setq *current-coroutine* coroutine)))
  (values))

;;;; alg-test-support.lisp -- org.eclipse.elk.core.alg.TestPhases and
;;;; TestProcessors.  Both are enums whose factories create processors that
;;;; append their own name to a StringBuffer; a Lisp adjustable string stands
;;;; in for the StringBuffer.

(in-package #:elk)

(defun %sb-new () (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
(defun %sb-append (buf s)
  (loop for c across s do (vector-push-extend c buf))
  buf)

(defjenum test-processors (processor-1 processor-2 processor-3)
  :supers (i-layout-processor-factory))

(defjmethod create ((this test-processors))
  (new-test-processor-impl/1 this))

(defjclass test-processor-impl (i-layout-processor)
  ((factor nil)))

(defjctor test-processor-impl (factor)
  (setf (.factor this) factor))

(defjmethod process ((this test-processor-impl) graph monitor)
  (declare (ignore monitor))
  (%sb-append graph (substitute #\_ #\- (name (.factor this)))))

(defjenum test-phases (phase-1 phase-2)
  :supers (i-layout-phase-factory))

(defjmethod create ((this test-phases))
  (new-test-phase-impl/1 this))

(defjclass test-phase-impl (i-layout-phase)
  ((phase nil)))

(defjctor test-phase-impl (phase)
  (setf (.phase this) phase))

(defjmethod process ((this test-phase-impl) graph monitor)
  (declare (ignore monitor))
  (%sb-append graph (substitute #\_ #\- (name (.phase this)))))

(defjmethod get-layout-processor-configuration ((this test-phase-impl) graph)
  (declare (ignore graph))
  (let ((config (layout-processor-configuration.create)))
    (add-before config (.phase this)
                (aref (vector test-processors.processor-1 test-processors.processor-2 test-processors.processor-3) (ordinal (.phase this))))
    config))

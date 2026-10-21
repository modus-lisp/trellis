;;;; algorithm-assembler-test.lisp -- port of
;;;; org.eclipse.elk.core.alg.AlgorithmAssemblerTest.

(in-package #:elk)

(defun %aa-phase-count (algorithm)
  (count-if (lambda (o) (typep o 'i-layout-phase)) algorithm))

(defcore-class algorithm-assembler-test
  ("testEnableCaching"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (set-phase assembler test-phases.phase-2 test-phases.phase-2)
     (let ((algorithm (build assembler nil)))
       (assert-equals 2 (%aa-phase-count algorithm))
       (let ((algorithm2 (build assembler nil)))
         (assert-equals 2 (%aa-phase-count algorithm2))
         (loop for i from 0 below (length algorithm)
               do (assert-true (eq (aref algorithm i) (aref algorithm2 i))))))))
  ("testDisableCaching"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (with-caching assembler nil)
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (set-phase assembler test-phases.phase-2 test-phases.phase-2)
     (let ((algorithm (build assembler nil)))
       (assert-equals 2 (%aa-phase-count algorithm))
       (let ((algorithm2 (build assembler nil)))
         (assert-equals 2 (%aa-phase-count algorithm2))
         (loop for i from 0 below (length algorithm)
               do (assert-true (not (eq (aref algorithm i) (aref algorithm2 i)))))))))
  ("testFailOnMissingPhase"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (assert-throws error (build assembler nil))))
  ("testDontFailOnMissingPhase"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (with-fail-on-missing-phase assembler nil)
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (assert-true (> (length (build assembler nil)) 0))))
  ("testClearCache"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (with-caching assembler t)
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (set-phase assembler test-phases.phase-2 test-phases.phase-2)
     (let ((algorithm (build assembler nil)))
       (assert-equals 2 (%aa-phase-count algorithm))
       (clear-cache assembler)
       (let ((algorithm2 (build assembler nil)))
         (assert-equals 2 (%aa-phase-count algorithm2))
         (loop for i from 0 below (length algorithm)
               do (assert-true (not (eq (aref algorithm i) (aref algorithm2 i)))))))))
  ("testResetWithFailOnMissingPhase"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (with-fail-on-missing-phase assembler t)
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (set-phase assembler test-phases.phase-2 test-phases.phase-2)
     (reset assembler)
     (assert-throws error (build assembler nil))))
  ("testResetWithoutFailOnMissingPhase"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (with-fail-on-missing-phase assembler nil)
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (set-phase assembler test-phases.phase-2 test-phases.phase-2)
     (reset assembler)
     (assert-equals 0 (length (build assembler nil)))))
  ("testAddProcessorConfiguration"
   (let ((assembler (algorithm-assembler.create 'test-phases)))
     (add-processor-configuration
      assembler
      (add-before (layout-processor-configuration.create) test-phases.phase-1
                  test-processors.processor-3))
     (set-phase assembler test-phases.phase-1 test-phases.phase-1)
     (set-phase assembler test-phases.phase-2 test-phases.phase-2)
     (let ((algorithm (build assembler nil))
           (expected-strings #("PROCESSOR_1" "PROCESSOR_3" "PHASE_1" "PROCESSOR_2" "PHASE_2")))
       (loop for i from 0 below (length algorithm)
             do (let ((buffer (%sb-new)))
                  (process (aref algorithm i) buffer nil)
                  (assert-equals (aref expected-strings i) (coerce buffer 'simple-string))))))))

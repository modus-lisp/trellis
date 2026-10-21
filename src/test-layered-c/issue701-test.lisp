;;;; issue701-test.lisp -- port of org.eclipse.elk.alg.layered.issues.Issue701Test.

(in-package #:elk)

(defconstant +i701-coordinate-fuzzyness+ 0.5d0)

(defun %i701-assemble-labels (node)
  "assembleLabels: the node's labels, its ports' labels, and those of its children, recursively."
  (let ((labels (new-jlist)))
    (jdo (label (get-labels node)) (jl-add labels label))
    (jdo (port (get-ports node))
      (jdo (label (get-labels port)) (jl-add labels label)))
    (jdo (child (get-children node))
      (jdo (label (%i701-assemble-labels child)) (jl-add labels label)))
    labels))

(defun %i701-assert-node-width-according-to-port-width (node port opposite-port east-or-west-side)
  "Ensure that the width of the node is as expected according to the port width."
  (let* ((port-label-width (get-width (j-get (get-labels port) 0)))
         (opposite-port-label-width (if (null opposite-port) 0
                                        (get-width (j-get (get-labels opposite-port) 0))))
         (spacing-between-port (individual-spacings.get-individual-or-inherited
                                node core-options.spacing-port-port)))
    (if (not east-or-west-side)
        (tlc-assert-equals-delta "Wrong node width according to port(s) label width."
                                 (+ spacing-between-port
                                    (math.max port-label-width opposite-port-label-width)
                                    spacing-between-port)
                                 (get-width node) 0)
        (let* ((spacing-label-port (get-property port core-options.spacing-label-port-horizontal))
               (spacing-label-opposite-port
                 (if (null opposite-port) 0
                     (get-property opposite-port core-options.spacing-label-port-horizontal)))
               (port-border-offset (get-property port core-options.port-border-offset))
               (opposite-port-border-offset
                 (if (null opposite-port) 0
                     (get-property opposite-port core-options.port-border-offset)))
               (node-label (j-get (get-labels node) 0))
               (node-label-padding (get-property node core-options.node-labels-padding))
               (expected-width (+ (get-left node-label-padding)
                                  (* 2 (math.max (+ spacing-label-port port-label-width)
                                                 (+ spacing-label-opposite-port opposite-port-label-width)))
                                  (get-width node-label)
                                  (get-right node-label-padding)
                                  (- port-border-offset)
                                  (- opposite-port-border-offset))))
          (tlc-assert-equals-delta "Wrong node width according to port(s) label width."
                                   expected-width (get-width node) 0)))))

(defun %i701-assert-node-height-according-to-port-height (node port opposite-port east-or-west-side)
  "Ensure the node height is as expected for the port height, and that the node label location is as expected."
  (let* ((port-label-height (.height (elk-util.get-labels-bounds
                                      (elk-graph-adapters.adapt-single-port port))))
         (opposite-port-label-height
           (if (null opposite-port) 0
               (.height (elk-util.get-labels-bounds (elk-graph-adapters.adapt-single-port opposite-port)))))
         (opposite-port-height (if (null opposite-port) 0 (get-height opposite-port)))
         (node-padding (get-property node core-options.padding))
         (node-label (j-get (get-labels node) 0))
         (node-label-padding (get-property node core-options.node-labels-padding))
         (spacing-between-port (individual-spacings.get-individual-or-inherited
                                node core-options.spacing-port-port)))
    (if east-or-west-side
        (let ((port-height-plus-padding
                (math.max (+ spacing-between-port port-label-height spacing-between-port)
                          (+ (math.max spacing-between-port (get-top node-padding))
                             (get-height port)
                             (math.max spacing-between-port (get-bottom node-padding)))))
              (opposite-port-height-plus-padding
                (math.max (+ spacing-between-port opposite-port-label-height spacing-between-port)
                          (+ (math.max spacing-between-port (get-top node-padding))
                             opposite-port-height
                             (math.max spacing-between-port (get-bottom node-padding))))))
          (tlc-assert-equals-delta "Wrong node height according to port label height."
                                   (math.max port-height-plus-padding opposite-port-height-plus-padding)
                                   (get-height node) 0)
          (tlc-assert-equals-delta "Wrong node label location."
                                   (+ (.y (elk-util.absolute-position node)) (get-top node-label-padding))
                                   (.y (elk-util.absolute-position node-label)) 0))
        (let* ((spacing-label-port-vertical (get-property port core-options.spacing-label-port-vertical))
               (spacing-label-opposite-port-vertical
                 (if (null opposite-port) 0
                     (get-property opposite-port core-options.spacing-label-port-vertical)))
               (port-border-offset (get-property port core-options.port-border-offset))
               (opposite-port-border-offset
                 (if (null opposite-port) 0
                     (get-property opposite-port core-options.port-border-offset)))
               (expected-height (+ (- port-border-offset)
                                   (- opposite-port-border-offset)
                                   (* 2 (math.max (+ spacing-label-port-vertical port-label-height)
                                                  (+ spacing-label-opposite-port-vertical
                                                     opposite-port-label-height)))
                                   (get-top node-label-padding)
                                   (* 2 (get-height node-label))
                                   (get-bottom node-label-padding)))
               (port-side (get-property port layered-options.port-side)))
          (tlc-assert-equals-delta "Wrong node height according to port label height."
                                   expected-height (get-height node) 0)
          (cond ((eq port-side port-side.north)
                 (tlc-assert-equals-delta "Wrong node label location."
                                          (+ (.y (elk-util.absolute-position (j-get (get-labels port) 0)))
                                             port-label-height (get-top node-label-padding))
                                          (.y (elk-util.absolute-position node-label)) 0))
                ((eq port-side port-side.south)
                 (tlc-assert-equals-delta "Wrong node label location."
                                          (+ (.y (elk-util.absolute-position node))
                                             port-label-height
                                             (get-property port core-options.spacing-label-port-vertical)
                                             (get-top node-label-padding))
                                          (.y (elk-util.absolute-position node-label)) 0)))))))

(deflayouttest issue701-test
  (:graph-resources (list (model-resource-path "tickets/layered/701_portLabels.elkt")))
  ;; Check behavior for port labels that is forced to be inside the node according to
  ;; PortLabelPlacement.INSIDE property.
  (:test test-inside-labels () (graph)
    (let ((container-for-inside-labels
            (graph-test-utils.get-child graph '("ContainerWithLabelsInsideAndOutside"
                                                "Container_OneBorderNode_LabelInside"))))
      ;; Ensure there is no label overlaps
      (when (graph-test-utils.have-overlaps (%i701-assemble-labels container-for-inside-labels))
        (fail "Overlaps between labels detected in \"Container_OneBorderNode_LabelInside\"!"))
      ;; Port on north
      (let* ((node1 (graph-test-utils.get-child container-for-inside-labels '("MyNode1")))
             (port-of-node1 (graph-test-utils.get-port node1 "P1")))
        (%i701-assert-node-width-according-to-port-width node1 port-of-node1 nil nil)
        (%i701-assert-node-height-according-to-port-height node1 port-of-node1 nil nil))
      ;; Port on south
      (let* ((node3 (graph-test-utils.get-child container-for-inside-labels '("MyNode3")))
             (port-of-node3 (graph-test-utils.get-port node3 "P1")))
        (%i701-assert-node-width-according-to-port-width node3 port-of-node3 nil nil)
        (%i701-assert-node-height-according-to-port-height node3 port-of-node3 nil nil))
      ;; Port on east
      (let* ((node2 (graph-test-utils.get-child container-for-inside-labels '("MyNode2")))
             (port-of-node2 (graph-test-utils.get-port node2 "P1")))
        (%i701-assert-node-height-according-to-port-height node2 port-of-node2 nil t)
        (%i701-assert-node-width-according-to-port-width node2 port-of-node2 nil t))
      ;; Port on west
      (let* ((node4 (graph-test-utils.get-child container-for-inside-labels '("MyNode4")))
             (port-of-node4 (graph-test-utils.get-port node4 "P1")))
        (%i701-assert-node-width-according-to-port-width node4 port-of-node4 nil t)
        (%i701-assert-node-height-according-to-port-height node4 port-of-node4 nil t))))
  ;; Check behavior for port labels that is forced to be inside the node according to
  ;; PortLabelPlacement.OUTSIDE property.  (The Java test has only the overlap check; the
  ;; rest is a TODO there.)
  (:test test-outside-labels () (graph)
    (let ((container-for-outside-labels
            (graph-test-utils.get-child graph '("ContainerWithLabelsInsideAndOutside"
                                                "Container_OneBorderNode_LabelOutside"))))
      (when (graph-test-utils.have-overlaps (%i701-assemble-labels container-for-outside-labels))
        (fail "Overlaps between labels detected in \"Container_OneBorderNode_LabelOutside\"!"))))
  (:test test-mix-inside-and-outside-labels () (graph)
    (let ((container (graph-test-utils.get-child graph '("ContainerWithLabelsInsideAndOutside"
                                                         "Container_SeveralBorderNodes_LabelInsideAndOutside"))))
      ;; Check deactivated in Java: it fails for "MyCont2" (see issue 638).
      (graph-test-utils.have-overlaps (%i701-assemble-labels container))
      ;; Ports on north and on south sides
      (let* ((node1 (graph-test-utils.get-child container '("MyNode1")))
             (node1-port1 (graph-test-utils.get-port node1 "P1"))
             (node1-port2 (graph-test-utils.get-port node1 "P2")))
        (%i701-assert-node-width-according-to-port-width node1 node1-port1 node1-port2 nil)
        (%i701-assert-node-height-according-to-port-height node1 node1-port1 node1-port2 nil))
      ;; Ports on east and on west sides
      (let* ((node2 (graph-test-utils.get-child container '("MyNode2")))
             (node2-port1 (graph-test-utils.get-port node2 "P1"))
             (node2-port2 (graph-test-utils.get-port node2 "P2")))
        (%i701-assert-node-width-according-to-port-width node2 node2-port1 node2-port2 t)
        (%i701-assert-node-height-according-to-port-height node2 node2-port1 node2-port2 t))
      ;; MyCont2 is ignored in Java (behaviour seems KO, see issue 638).
      ;; Ports on east and on west sides with a node inside the container and linked to the ports,
      ;; check width of the container
      (let* ((cont3 (graph-test-utils.get-child container '("MyCont3")))
             (cont3-port1 (graph-test-utils.get-port cont3 "P1"))
             (cont3-port1-label (j-get (get-labels cont3-port1) 0))
             (cont3-port2 (graph-test-utils.get-port cont3 "P2"))
             (cont3-port2-label (j-get (get-labels cont3-port2) 0))
             (spacing-label-cont3-port1 (get-property cont3-port1 core-options.spacing-label-port-horizontal))
             (spacing-label-cont3-port2 (get-property cont3-port2 core-options.spacing-label-port-horizontal))
             (cont3-port1-border-offset (get-property cont3-port1 core-options.port-border-offset))
             (cont3-port2-border-offset (get-property cont3-port2 core-options.port-border-offset))
             (cont3-padding (get-property cont3 core-options.padding))
             (cont3-node2 (graph-test-utils.get-child cont3 '("MyNode2")))
             ;; TODO in Java: "It seems to be a bug here. There is only 4 pixels instead of 12".
             (expected-cont3-width (- (get-left cont3-padding) 8)))
        (incf expected-cont3-width (get-width cont3-node2))
        (incf expected-cont3-width (- cont3-port2-border-offset))
        (incf expected-cont3-width spacing-label-cont3-port2)
        (incf expected-cont3-width (get-width cont3-port2-label))
        (incf expected-cont3-width (get-width cont3-port1-label))
        (incf expected-cont3-width spacing-label-cont3-port1)
        (incf expected-cont3-width (- cont3-port1-border-offset))
        (incf expected-cont3-width (get-right cont3-padding))
        (tlc-assert-equals-delta "Wrong node width according to ports label width."
                                 expected-cont3-width (get-width cont3) 0))
      ;; Port on east side on a node in container, port on west side of the container, and with edge between
      ;; ports, check width of the container
      (let* ((cont4 (graph-test-utils.get-child container '("MyCont4")))
             (cont4-node1 (graph-test-utils.get-child cont4 '("MyNode1")))
             (cont4-node1-port1 (graph-test-utils.get-port cont4-node1 "P1"))
             (cont4-node1-port1-label (j-get (get-labels cont4-node1-port1) 0))
             (cont4-port1 (graph-test-utils.get-port cont4 "P1"))
             (cont4-port1-label (j-get (get-labels cont4-port1) 0))
             (spacing-label-cont4-node1-port1 (get-property cont4-node1-port1 core-options.spacing-label-port-horizontal))
             (spacing-label-cont4-port1 (get-property cont4-port1 core-options.spacing-label-port-horizontal))
             (cont4-node1-port1-border-offset (get-property cont4-node1-port1 core-options.port-border-offset))
             (cont4-port1-border-offset (get-property cont4-port1 core-options.port-border-offset))
             (cont4-padding (get-property cont4 core-options.padding))
             ;; ElkUtil.calcPortOffset is supposed to do the same computing, but does not (see Java TODO).
             (expected-cont4-width (get-left cont4-padding)))
        (incf expected-cont4-width (get-width cont4-node1))
        (incf expected-cont4-width (+ (get-width cont4-node1-port1) cont4-node1-port1-border-offset))
        (incf expected-cont4-width spacing-label-cont4-node1-port1)
        (incf expected-cont4-width (get-width cont4-node1-port1-label))
        (incf expected-cont4-width (get-width cont4-port1-label))
        (incf expected-cont4-width spacing-label-cont4-port1)
        (incf expected-cont4-width (- cont4-port1-border-offset))
        (incf expected-cont4-width (get-right cont4-padding))
        (tlc-assert-equals-delta "Wrong node width according to ports label width."
                                 expected-cont4-width (get-width cont4) 0))))
  ;; Check behavior for port labels that is fixed, empty PortLabelPlacement property, but partially located
  ;; inside the node.
  (:test test-fixed-inside-labels () (graph)
    (let ((container-for-inside-labels
            (graph-test-utils.get-child graph '("ContainerWithFixedLabels"
                                                "Container_OneBorderNode_LabelFixedPartiallyInside"))))
      (when (graph-test-utils.have-overlaps (%i701-assemble-labels container-for-inside-labels))
        (fail "Overlaps between labels detected in \"Container_OneBorderNode_LabelFixedPartiallyInside\"!"))
      (let* ((node1 (graph-test-utils.get-child container-for-inside-labels '("MyNode1")))
             (port-of-node1 (graph-test-utils.get-port node1 "P1")))
        (%i701-assert-node-width-according-to-port-width node1 port-of-node1 nil nil)
        (%i701-assert-node-height-according-to-port-height node1 port-of-node1 nil nil))
      (let* ((node3 (graph-test-utils.get-child container-for-inside-labels '("MyNode3")))
             (port-of-node3 (graph-test-utils.get-port node3 "P1")))
        (%i701-assert-node-width-according-to-port-width node3 port-of-node3 nil nil)
        (%i701-assert-node-height-according-to-port-height node3 port-of-node3 nil nil))
      (let* ((node2 (graph-test-utils.get-child container-for-inside-labels '("MyNode2")))
             (port-of-node2 (graph-test-utils.get-port node2 "P1")))
        (%i701-assert-node-height-according-to-port-height node2 port-of-node2 nil t)
        (%i701-assert-node-width-according-to-port-width node2 port-of-node2 nil t))
      ;; Java: the port of MyNode2Bis is looked up on node2 (a copy-paste in the Java source), kept as is.
      (graph-test-utils.get-child container-for-inside-labels '("MyNode2Bis"))
      (graph-test-utils.get-port (graph-test-utils.get-child container-for-inside-labels '("MyNode2")) "P1")
      (let* ((node4 (graph-test-utils.get-child container-for-inside-labels '("MyNode4")))
             (port-of-node4 (graph-test-utils.get-port node4 "P1")))
        (%i701-assert-node-width-according-to-port-width node4 port-of-node4 nil t)
        (%i701-assert-node-height-according-to-port-height node4 port-of-node4 nil t))
      (graph-test-utils.get-child container-for-inside-labels '("MyNode4Bis"))
      (graph-test-utils.get-port (graph-test-utils.get-child container-for-inside-labels '("MyNode4Bis")) "P1")))
  ;; Check behavior for port labels that is fixed, empty PortLabelPlacement property, but located outside the node.
  (:test test-fixed-outside-labels () (graph)
    (let ((container-for-outside-labels
            (graph-test-utils.get-child graph '("ContainerWithFixedLabels"
                                                "Container_OneBorderNode_LabelFixedOutside"))))
      (when (graph-test-utils.have-overlaps (%i701-assemble-labels container-for-outside-labels))
        (fail "Overlaps between labels detected in \"Container_OneBorderNode_LabelFixedOutside\"!"))
      (let* ((node1 (graph-test-utils.get-child container-for-outside-labels '("MyNode1")))
             (port-of-node1 (graph-test-utils.get-port node1 "P1")))
        (%i701-assert-node-width-according-to-port-width node1 port-of-node1 nil nil))
      (let* ((node3 (graph-test-utils.get-child container-for-outside-labels '("MyNode3")))
             (port-of-node3 (graph-test-utils.get-port node3 "P1")))
        (%i701-assert-node-width-according-to-port-width node3 port-of-node3 nil nil))
      (let* ((node2 (graph-test-utils.get-child container-for-outside-labels '("MyNode2")))
             (port-of-node2 (graph-test-utils.get-port node2 "P1")))
        (%i701-assert-node-height-according-to-port-height node2 port-of-node2 nil t))
      (let* ((node4 (graph-test-utils.get-child container-for-outside-labels '("MyNode4")))
             (port-of-node4 (graph-test-utils.get-port node4 "P1")))
        (%i701-assert-node-height-according-to-port-height node4 port-of-node4 nil t))))
  ;; Port labels fixed, empty PortLabelPlacement property, located inside or outside the node, more complex.
  (:test test-fixed-mix-inside-and-outside-labels () (graph)
    (let ((container (graph-test-utils.get-child graph '("ContainerWithFixedLabels"
                                                         "Container_SeveralBorderNodes_LabelFixedPartiallyInside"))))
      (when (graph-test-utils.have-overlaps (%i701-assemble-labels container))
        (fail "Overlaps between labels detected in \"Container_SeveralBorderNodes_LabelFixedPartiallyInside\"!"))
      (let* ((node1 (graph-test-utils.get-child container '("MyNode1")))
             (node1-port1 (graph-test-utils.get-port node1 "P1"))
             (node1-port2 (graph-test-utils.get-port node1 "P2")))
        (%i701-assert-node-width-according-to-port-width node1 node1-port1 node1-port2 nil))
      (let* ((node2 (graph-test-utils.get-child container '("MyNode2")))
             (node2-port1 (graph-test-utils.get-port node2 "P1"))
             (node2-port2 (graph-test-utils.get-port node2 "P2")))
        (%i701-assert-node-height-according-to-port-height node2 node2-port1 node2-port2 t))
      (graph-test-utils.get-child container '("MyNode2Bis"))
      (let* ((cont3 (graph-test-utils.get-child container '("MyCont3")))
             (cont3-port1 (graph-test-utils.get-port cont3 "P1"))
             (cont3-port1-label (j-get (get-labels cont3-port1) 0))
             (cont3-port2 (graph-test-utils.get-port cont3 "P2"))
             (cont3-port2-label (j-get (get-labels cont3-port2) 0))
             (spacing-label-cont3-port1 (get-property cont3-port1 core-options.spacing-label-port-horizontal))
             (spacing-label-cont3-port2 (get-property cont3-port2 core-options.spacing-label-port-horizontal))
             (cont3-port1-border-offset (get-property cont3-port1 core-options.port-border-offset))
             (cont3-port2-border-offset (get-property cont3-port2 core-options.port-border-offset))
             (cont3-padding (get-property cont3 core-options.padding))
             (expected-cont3-width (- (get-left cont3-padding) 8)))
        (declare (ignorable cont3-port1-label cont3-port2-label spacing-label-cont3-port1
                            spacing-label-cont3-port2 cont3-port1-border-offset cont3-port2-border-offset))
        ;; Java computes expectedCont3Width here and then overwrites it with the literal 248.
        (setf expected-cont3-width 248)
        (tlc-assert-equals-delta "Wrong node width according to ports label width."
                                 expected-cont3-width (get-width cont3) 0))
      (let* ((cont4 (graph-test-utils.get-child container '("MyCont4")))
             (cont4-node1 (graph-test-utils.get-child cont4 '("MyNode1")))
             (cont4-node1-port1 (graph-test-utils.get-port cont4-node1 "P1"))
             (cont4-port1 (graph-test-utils.get-port cont4 "P1"))
             (cont4-padding (get-property cont4 core-options.padding))
             (expected-cont4-width 312.5d0))
        (declare (ignorable cont4-node1-port1 cont4-port1 cont4-padding))
        ;; Java computes expectedCont4Width here and then overwrites it with the literal 312.5.
        (tlc-assert-equals-delta "Wrong node width according to ports label width."
                                 expected-cont4-width (get-width cont4) 0)))))

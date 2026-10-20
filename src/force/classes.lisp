;;;; force classes -- org.eclipse.elk.alg.force{,.graph,.model,.options,.stress}

(in-package #:elk)

;;; org.eclipse.elk.alg.force.graph

;; FParticle (abstract)
(defjclass f-particle (map-property-holder)
  ((displacement (new-k-vector))
   (position (new-k-vector))
   (size (new-k-vector))))

(defjclass f-node (f-particle)
  ((id 0)
   (label nil)
   (parent nil)
   (children nil)))

(defjclass f-edge (map-property-holder)
  ((bendpoints (new-jlist))
   (labels (new-jlist))
   (source nil)
   (target nil)))

(defjclass f-bendpoint (f-particle)
  ((edge nil)))

(defjclass f-label (f-particle)
  ((edge nil)
   (text nil)))

(defjclass f-graph (map-property-holder)
  ((nodes (new-jlist))
   (edges (new-jlist))
   (labels (new-jlist))
   (bend-points (new-jlist))
   (adjacency nil)))

;;; org.eclipse.elk.alg.force.model

(defjclass abstract-force-model ()
  ((random nil)
   (graph nil)
   (disp-bound 0.0)))

(defjclass eades-model (abstract-force-model)
  ((max-iterations (get-default force-options.iterations))
   (spring-length (get-default force-options.spacing-node-node))
   (repulsion-factor (get-default force-options.repulsion))))

(defjclass fruchterman-reingold-model (abstract-force-model)
  ((temperature (get-default force-options.temperature))
   (threshold 0.0)
   (k 0.0)))

;;; org.eclipse.elk.alg.force

(defjinterface force-i-graph-importer)
(defjgeneric import-graph 1)
(defjgeneric apply-layout 1)

(defjclass force-elk-graph-importer (force-i-graph-importer) ())

(defjclass force-components-processor () ())

(defjclass force-layout-provider (abstract-layout-provider)
  ((force-model nil)
   (components-processor (new-force-components-processor))))

;;; org.eclipse.elk.alg.force.stress

(defjclass stress-majorization ()
  ((graph nil)
   (apsp nil)
   (w nil)
   (desired-edge-length 0.0)
   (dim nil)
   (epsilon 0.0)
   (iteration-limit 0)
   ;; LinkedListMultimap<FNode, FEdge>: node -> jlist of edges, insertion ordered
   (connected-edges (new-jmap))))

(defjclass stress-layout-provider (abstract-layout-provider)
  ((components-processor (new-force-components-processor))
   (stress-majorization (new-stress-majorization))))

;;; org.eclipse.elk.alg.force.options.InternalProperties

(defproperty force-internal-properties.origin "origin")
(defproperty force-internal-properties.random "random")
(defproperty force-internal-properties.bb-upleft "boundingBox.upLeft")
(defproperty force-internal-properties.bb-lowright "boundingBox.lowRight")

;;;; package.lisp -- the public interface.

(defpackage #:trellis
  (:use #:cl)
  (:export
   ;; graph model
   #:node #:node-p #:node-id #:node-x #:node-y #:node-width #:node-height
   #:node-children #:node-ports #:node-edges #:node-labels #:node-parent
   #:port #:port-p #:port-id #:port-x #:port-y #:port-width #:port-height
   #:port-labels #:port-node
   #:edge #:edge-p #:edge-id #:edge-sources #:edge-targets #:edge-sections
   #:edge-labels #:edge-container
   #:label #:label-p #:label-id #:label-text #:label-x #:label-y #:label-width
   #:label-height #:label-owner
   #:section #:section-start #:section-end #:section-bends #:section-points
   #:section-curve #:section-path #:node-ties
   ;; building graphs
   #:make-graph #:add-node #:add-port #:add-edge #:add-label
   #:option #:set-option #:find-element #:map-nodes #:all-edges
   ;; layout
   #:layout #:algorithms #:options-of #:describe-option
   ;; ELK JSON
   #:read-elk-json #:write-elk-json #:layout-elk-json
   ;; output and checking
   #:graph->svg #:check-layout #:layout-metrics
   #:absolute-position))

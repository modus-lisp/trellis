(defpackage #:diagram
  (:use #:cl)
  (:export
   ;; JSON
   #:parse-json #:write-json #:jget #:jset #:jobject
   ;; graph model
   #:node #:node-id #:node-x #:node-y #:node-width #:node-height
   #:node-children #:node-ports #:node-labels #:node-edges #:node-parent
   #:port #:port-p #:node-p #:port-id #:port-x #:port-y #:port-width #:port-height #:port-owner
   #:label #:label-text #:label-x #:label-y #:label-width #:label-height
   #:edge #:edge-id #:edge-sources #:edge-targets #:edge-sections #:edge-container
   #:section #:section-start #:section-bends #:section-end
   #:json->graph #:graph->json #:plist->graph #:find-element
   ;; layout
   #:layout #:layout-json
   ;; rendering
   #:graph->svg))

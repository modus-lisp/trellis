# layered-p3 (p3order, p3order/counting)

Files (load order): classes.lisp, counting.lisp, p3order.lisp, layer-sweep.lisp.

Coverage: every top-level p3order class and every p3order/counting class is ported,
including LayerSweepCrossingMinimizer, InteractiveCrossingMinimizer and NoCrossingMinimizer
(layer-sweep.lisp).

Notes:
- Arity of defjmethod/defjgeneric counts Java parameters, not `this`.
- LayerSweepCrossingMinimizer private helpers that are Java-private predicates are plain
  `%lsc-` defuns; the INTERMEDIATE_PROCESSING_CONFIGURATION constants are rebuilt per call
  (createFrom copies them, so the result is the same).
- The configurations use `layered-intermediate-processor-strategy.*` (the generated enum name);
  src/layered-p4 uses `intermediate-processor-strategy.*` which is not defined in the enums file.
- Java `return` inside the counting methods is an explicit `(block ...-body)`.
- The Java commented-out code inside LayerSweepCrossingMinimizer is not ported.
- Undefined names that belong to other modules at check time: model-order-node/port-comparator
  constructors, sort-by-input-model-processor, greedy-switch-heuristic, between-layer-edge
  counter, cm-group-model-order-calculator, port-list-sorter.
- Not run: the ELK tests (no runtime harness here); only tools/check-module.lisp was used.
  tools/check-module.lisp itself fails on a read error in another module (layered core-util
  layout-providers.lisp) at the time of writing, so it was run through a copy that skips
  unreadable files (/tmp/p3tool/check.lisp).

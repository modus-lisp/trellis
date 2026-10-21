# layered-p4 notes

Files: classes.lisp, p4nodes-bk.lisp, p4nodes.lisp (load order = files.txt).

Coverage: every Java class in p4nodes/ and p4nodes/bk/ has a defjclass or
defjinterface in classes.lisp, and the methods were checked against the Java
method lists (BKNodePlacer, NetworkSimplexPlacer, LinearSegmentsNodePlacer,
InteractiveNodePlacer, SimpleNodePlacer, BK*, ThresholdStrategy,
NeighborhoodInformation).

OPEN: the module does not finish compiling. Evaluating or compile-file of
%LSP-SORT-LINEAR-SEGMENTS (p4nodes.lisp) exhausts the SBCL heap (2 GB and 8 GB
both; a runtime+gen-only harness reproduces it). Disabling constraint
propagation (sb-c::*constraint-propagate* nil) and compilation-speed/speed 0
did not help. The jdo / jl- nesting in that function is the suspect.

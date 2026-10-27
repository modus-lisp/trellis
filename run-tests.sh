#!/bin/sh
# run-tests.sh — trellis's unit tests, then the ELK corpus conformance suite, on SBCL.
#
#   ./run-tests.sh            unit tests and the whole corpus (about a minute)
#   ./run-tests.sh unit       unit tests only
#
# json-simple is looked for beside this directory (../json-simple), as in a
# modus-lisp workspace.  The corpus run rewrites test/corpus-report.md and
# test/corpus-results.jsonl, and fails if any case errors or times out.
cd "$(dirname "$0")"
exec sbcl --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --eval '(push (truename "../json-simple/") asdf:*central-registry*)' \
  --eval '(asdf:load-system "trellis/test")' \
  --eval "(uiop:quit
            (if (and (trellis-test:run-unit-tests)
                     (or (equal (uiop:command-line-arguments) '(\"unit\"))
                         (notany (lambda (r) (member (getf r :status) '(:error :timeout)))
                                 (trellis-test:run-corpus :verbose nil))))
                0 1))" \
  "$@"

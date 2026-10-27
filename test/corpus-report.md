# trellis on the ELK corpus

1000 cases.  `ok` = laid out and passed every harness check; `warn` = laid out but a check failed; `error` = signalled an error; `timeout` = over 30 s.

## Outcomes

| | trellis | ELK |
|---|---|---|
| ok | 985 | 515 |
| warn | 15 | 381 |
| error | 0 | 96 |
| timeout | 0 | 8 |

## By algorithm

| algorithm | cases | ok | warn | error | timeout | ELK ok |
|---|---|---|---|---|---|---|
| box | 26 | 26 | 0 | 0 | 0 | 0 |
| disco | 44 | 44 | 0 | 0 | 0 | 4 |
| fixed | 25 | 12 | 13 | 0 | 0 | 2 |
| force | 71 | 71 | 0 | 0 | 0 | 50 |
| layered | 386 | 384 | 2 | 0 | 0 | 299 |
| mrtree | 74 | 74 | 0 | 0 | 0 | 51 |
| radial | 58 | 58 | 0 | 0 | 0 | 16 |
| random | 29 | 29 | 0 | 0 | 0 | 2 |
| rectpacking | 71 | 71 | 0 | 0 | 0 | 1 |
| spore-compaction | 30 | 30 | 0 | 0 | 0 | 8 |
| spore-overlap | 42 | 42 | 0 | 0 | 0 | 29 |
| stress | 80 | 80 | 0 | 0 | 0 | 37 |
| topdownpacking | 29 | 29 | 0 | 0 | 0 | 2 |
| vertiflex | 35 | 35 | 0 | 0 | 0 | 14 |

## Checks failed

| check | trellis cases | ELK cases |
|---|---|---|
| non-finite | 0 | 0 |
| overlaps | 15 | 175 |
| detached | 0 | 91 |
| unrouted | 0 | 196 |
| outside | 0 | 8 |

## Quality against ELK

Over cases where ELK's layout passed every check, and over the edges ELK routed.  Ratio = trellis / ELK; below 1 is better for trellis.

| algorithm | cases | crossings (median ratio, better/worse) | bends | edge length | area | edges through nodes (trellis/ELK total) |
|---|---|---|---|---|---|---|
| disco | 4 | 1.00 (0/0) | 1.00 (0/0) | 1.00 (0/0) | 0.62 (1/3) | 0/0 |
| fixed | 2 | 1.00 (0/0) | 1.00 (0/0) | 1.00 (0/0) | 0.00 (1/1) | 0/0 |
| force | 50 | 1.00 (25/3) | 1.00 (6/0) | 0.79 (30/16) | 0.88 (27/23) | 323/631 |
| layered | 299 | 1.00 (60/50) | 1.00 (69/59) | 1.00 (115/139) | 1.00 (125/127) | 0/144 |
| mrtree | 52 | 1.00 (23/1) | 0.00 (52/0) | 0.87 (33/17) | 1.15 (7/40) | 11051/4636 |
| radial | 16 | 1.00 (1/2) | 1.00 (0/0) | 1.17 (6/9) | 0.84 (8/8) | 31/25 |
| random | 2 | 1.00 (0/0) | 1.00 (0/0) | 1.00 (0/0) | 0.00 (0/2) | 0/0 |
| rectpacking | 1 | 1.00 (0/0) | 1.00 (0/0) | 1.00 (0/0) | 0.00 (0/1) | 0/0 |
| spore-compaction | 8 | 1.00 (3/1) | 1.00 (0/0) | 0.78 (6/2) | 0.66 (6/2) | 115/70 |
| spore-overlap | 29 | 1.00 (7/2) | 1.00 (0/1) | 1.00 (11/2) | 1.00 (9/6) | 2928/2517 |
| stress | 37 | 1.00 (2/8) | 1.00 (0/0) | 1.26 (2/32) | 1.23 (10/27) | 96/137 |
| topdownpacking | 2 | 1.00 (0/0) | 1.00 (0/0) | 1.00 (0/0) | 0.09 (1/1) | 0/0 |
| vertiflex | 14 | 1.00 (1/1) | 1.00 (0/0) | 0.79 (12/2) | 1.13 (5/9) | 6/2 |

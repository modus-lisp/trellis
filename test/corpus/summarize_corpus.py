# Merges the per-shard manifests into manifest.jsonl and writes summary.json + SUMMARY.md.
import json, re, sys
from collections import Counter, defaultdict
from pathlib import Path

d = Path(sys.argv[1])
rows = []
# Start from an already merged manifest (if any) so reruns can be folded in; later files win per id.
for p in ([d / "manifest.jsonl"] if (d / "manifest.jsonl").exists() else []) + sorted(d.glob("manifest-*.jsonl")):
    rows += [json.loads(l) for l in p.read_text().splitlines() if l.strip()]
rows = sorted({r["id"]: r for r in rows}.values(), key=lambda r: r["id"])
for r in rows:
    if r["status"] not in ("ok", "warn"):
        (d / "cases" / f"{r['id']}.out.json").unlink(missing_ok=True)
(d / "manifest.jsonl").write_text("".join(json.dumps(r, separators=(",", ":")) + "\n" for r in rows))
for p in d.glob("manifest-*.jsonl"):
    p.unlink()

def short(a):
    return a.rsplit(".", 1)[-1] if a else "?"

def signature(err):
    """First line of the stack trace, with numbers and node names blurred so equal failures group together."""
    line = err.splitlines()[0]
    line = re.sub(r"^([\w.$]+\.)(\w+(Exception|Error))", r"\2", line)
    line = re.sub(r"\d+", "N", line)
    return line[:160]

def frame(err):
    """Innermost ELK stack frame, which is usually the place to start looking."""
    for l in err.splitlines()[1:]:
        if "org.eclipse.elk" in l:
            return l.strip().removeprefix("at ")
    return ""

def issue_kind(i):
    return re.sub(r"^\d+ ", "", i)

status = Counter(r["status"] for r in rows)
by_alg = defaultdict(Counter)
for r in rows:
    by_alg[short(r["algorithm"])][r["status"]] += 1
fam = defaultdict(Counter)
for r in rows:
    fam[r["family"]][r["status"]] += 1

errors = defaultdict(list)
for r in rows:
    if r["error"] and r["status"] != "timeout":
        errors[(signature(r["error"]), frame(r["error"]))].append(r)
issues = Counter(issue_kind(i) for r in rows for i in r["issues"])
edge_tags = Counter(t.split(":")[0] for r in rows for t in r["edgeCases"])
timeouts = [r for r in rows if r["status"] == "timeout"]
ok_rows = [r for r in rows if r["status"] in ("ok", "warn")]
slow = sorted(ok_rows, key=lambda r: -r["millis"])[:15]
total_nodes = sum(r["nodes"] for r in rows)
total_edges = sum(r["edges"] for r in rows)
opt_usage = Counter(k for r in rows for k in list(r["rootOptions"]) + list(r["elementOptions"]))

summary = {
    "cases": len(rows), "status": status, "byAlgorithm": by_alg, "byFamily": fam,
    "errorGroups": [{"signature": s, "frame": f, "count": len(v), "cases": [r["id"] for r in v]}
                    for (s, f), v in sorted(errors.items(), key=lambda kv: -len(kv[1]))],
    "issues": issues, "edgeCaseTags": edge_tags, "timeouts": [r["id"] for r in timeouts],
    "distinctOptionsExercised": len(opt_usage), "totalNodes": total_nodes, "totalEdges": total_edges,
    "totalLayoutMillis": sum(r["millis"] for r in rows),
}
(d / "summary.json").write_text(json.dumps(summary, indent=2))

STATUSES = ["ok", "warn", "error", "timeout", "generate-error", "export-input-error", "check-error", "export-output-error"]
present = [s for s in STATUSES if status.get(s)]

def table(head, body):
    out = ["| " + " | ".join(head) + " |", "|" + "|".join("---" for _ in head) + "|"]
    out += ["| " + " | ".join(str(c) for c in row) + " |" for row in body]
    return "\n".join(out)

md = [f"# ELK random corpus — summary\n",
      f"{len(rows)} cases · {total_nodes:,} nodes · {total_edges:,} edges · "
      f"{summary['totalLayoutMillis']/1000:.1f} s total layout time · {len(opt_usage)} distinct layout options exercised\n",
      "## Outcomes\n",
      table(["status", "cases"], [[s, status[s]] for s in present]), "",
      "`ok` = laid out and passed every check; `warn` = laid out but a sanity check flagged something "
      "(see Issues); `error` = ELK threw; `timeout` = no result within 30 s. Other `*-error` statuses would mean the "
      "harness itself failed at that stage.\n",
      "## By algorithm\n",
      table(["algorithm"] + present + ["total"],
            [[a] + [c.get(s, 0) for s in present] + [sum(c.values())] for a, c in sorted(by_alg.items(), key=lambda kv: -sum(kv[1].values()))]), "",
      "## By graph family\n",
      table(["family"] + present + ["total"],
            [[f] + [c.get(s, 0) for s in present] + [sum(c.values())] for f, c in sorted(fam.items())]), "",
      "## Exceptions, grouped\n",
      table(["count", "exception", "innermost ELK frame", "example cases"],
            [[len(v), s.replace("|", "\\|"), f"`{f}`" if f else "", ", ".join(r["id"] for r in v[:6]) + (" …" if len(v) > 6 else "")]
             for (s, f), v in sorted(errors.items(), key=lambda kv: -len(kv[1]))]), "",
      "## Timeouts\n",
      table(["case", "algorithm", "family", "nodes", "edges", "root options"],
            [[r["id"], short(r["algorithm"]), r["family"], r["nodes"], r["edges"], ", ".join(f"{short(k)}={v}" for k, v in r["rootOptions"].items()) or "—"] for r in timeouts]) if timeouts else "None.", "",
      "## Sanity-check issues (cases flagged)\n",
      table(["issue", "cases"], issues.most_common()), "",
      "Some of these are expected by design: packing and fixed/random layouts don't route edges, and the fixed "
      "layout keeps whatever overlaps the input had.\n",
      "## Edge-case injections\n",
      table(["injection", "cases"], edge_tags.most_common()), "",
      "## Slowest successful layouts\n",
      table(["case", "algorithm", "family", "nodes", "edges", "ms"],
            [[r["id"], short(r["algorithm"]), r["family"], r["nodes"], r["edges"], r["millis"]] for r in slow]), ""]
(d / "SUMMARY.md").write_text("\n".join(md))
print(f"{len(rows)} rows; status {dict(status)}; {len(errors)} error groups; {len(timeouts)} timeouts")

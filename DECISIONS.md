# DECISIONS.md — the running ledger (SPEC §2, §12)

Format: `date — decision — why — revisitable?`

- 2026-07-02 — The constitution file is `SPEC.md`; `CLAUDE.md` is a thin pointer to it — the repo was scaffolded with `SPEC.md` at the root; SPEC §3.3 names `CLAUDE.md` as the constitution — yes (mechanical rename).
- 2026-07-02 — Packages: `SPUTTER.IMPL` holds the whole stage-0 compiler + runtime support; `SPUTTER` is the user namespace (no `:use`; mangled identifiers and prelude only) — user `fn` names must never collide with compiler internals; emitted forms reference impl symbols as objects, so nothing needs exporting across the line — yes.
- 2026-07-02 — Stage-0 scalar representations: `true` = CL `T`, `false` = `+sput-false+` singleton, `nil` = CL `NIL`; atoms, heads, and identifier names = keywords (identifier names case-sensitive, e.g. `:|total|`) — quoting a literal must yield its runtime value (I1), and CL `NIL` cannot play both `false` and `nil` (§13.2) — representation yes, semantics spec-pinned.
- 2026-07-02 — `node-equal` (and later `==` on nodes) ignores `meta` — provenance is not identity; §5.7 round-trips are "modulo meta" — yes.
- 2026-07-02 — `bin/sput` is a `/bin/sh` shim that execs `sbcl --script src/boot.lisp`; `boot.lisp` loads `~/quicklisp/setup.lisp` when present, else falls back to bare ASDF — `--script` skips init files and the frozen deps live in Quicklisp — yes (M7 saved image supersedes).
- 2026-07-02 — Two files added beyond the §3.3 layout: `src/boot.lisp` (script entry, outside the ASDF system) and `Makefile` (`make test` is the CI entry) — the launcher needs a load file and the M0 DoD wants rove green "in CI-able form" — yes.
- 2026-07-02 — Golden harness convention: `tests/golden/NAME.sput` + `NAME.expected.{expand,dump,fmt,run,show}`; `SPUTTER_GOLDEN=update` regenerates every *existing* expected file (touch an empty one to add coverage) — smallest mechanism with an explicit opt-in per mode — yes.

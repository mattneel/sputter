# Sputter — agent guide

The constitution is **[SPEC.md](SPEC.md)**. Read it first. When code and SPEC.md
disagree, SPEC.md wins. When SPEC.md is silent, follow its §12 decision protocol
and record the call in [DECISIONS.md](DECISIONS.md).

Practical entry points:

- `make test` — rove suites + golden corpus (the CI entry).
- `SPUTTER_GOLDEN=update make test` — regenerate existing golden `.expected.*` files.
- `bin/sput` — the CLI (`run | expand | fmt | repl | test`).
- Milestones: work M0 → M8 in order (SPEC.md §11). A milestone ends shippable:
  tests green, `sput` runnable, one commit tagged `mN`.

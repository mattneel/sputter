# Sputter

A Lisp with C-family surface syntax, hosted on SBCL.

Elixir's architecture wearing Zig's clothes: real curly-brace grammar on the
surface, a uniform code-as-data tree underneath, hygienic macros written *in
surface syntax*, and Common Lisp as the rented compiler and runtime — the way
Elixir rents the BEAM.

The name: *sputtering* is thin-film deposition — atoms knocked off a target
condense as a coating on a substrate. Sputter is a C-family film deposited on
a Lisp substrate.

```zig
fn area(w: f64, h: f64) f64 {
    w * h
}

fn describe(user) {
    let sign = if user.karma < 0 { -1 } else { 1 };

    let total = user.orders
        |> filter(is_paid)
        |> map(fn(o) { o.amount })
        |> sum();

    switch fetch(user.id) {
        .ok(profile) => greet(profile, total),
        .err(.not_found) => "who?",
        .err(reason) => fallback(reason),
    }
}
```

## Install

Requirements: [SBCL](https://sbcl.org) and [Quicklisp](https://www.quicklisp.org)
(for the frozen dependency set: `alexandria`, `trivia`, `rove`).

```sh
git clone <this repo> sputter && cd sputter
make test          # rove suites + golden corpus
bin/sput help
```

`bin/sput` uses `bin/sput.image` when present, and otherwise falls back to
`sbcl --script src/boot.lisp`. Build the saved image with `sput build-image`.

## CLI

```
sput run file.sput...         compile + execute in a fresh image state
sput expand file.sput         fully expand; print the module as surface syntax
sput expand --dump file.sput  same, as data literals
sput fmt file.sput            parse + print canonically (--check for CI)
sput repl                     REPL (line editing: rlwrap sput repl)
sput test file.sput...        run `test "name" { ... }` blocks
sput build-image [path]       save a preloaded SBCL core (default: bin/sput.image)
```

`sput expand` is the flagship: its output is a valid `.sput` module — macros
fully expanded, printed back as surface syntax. No s-expression ever crosses
the user-facing line (the *Waterline*).

## Quick tour

Run the normative tour from SPEC.md §10:

```sh
bin/sput run examples/tour.sput
bin/sput fmt --check examples/tour.sput
```

Write tests in Sputter itself:

```zig
test "lists and checks" {
    let xs = [1, 2] |> push(3);
    check(len(xs) == 3);
    check(str(.ok(1)) == ".ok(1)");
}
```

```sh
bin/sput test tests/golden/test_prelude.sput
```

For faster startup, build and use a saved SBCL image:

```sh
bin/sput build-image          # writes bin/sput.image
bin/sput help                 # automatically uses it
```

## Stage-1 beachhead

`src-sput/print.sput` is compiled by stage 0 and exposes `stage1_print_node`.
The test suite diffs it against the CL printer over every parseable form in the
golden corpus, proving the first self-hosted source can participate in the
toolchain without replacing the trusted stage-0 path yet.

## Status

Working through the v0.1 milestones (SPEC.md §11):

- [x] M0 — skeleton: node model, test harness, CLI shim
- [x] M1 — parse + print the macro-free core
- [x] M2 — lower + emit + run via SBCL
- [x] M3 — data (atoms, tagged, lists, records) + `switch` + pipelines
- [x] M4 — nodes as first-class values (`quote`, `dump`, `print`)
- [x] M5 — procedural macros (`macro fn`), hygiene
- [x] M6 — by-example macros (`macro name { pattern => template }`)
- [x] M7 — prelude, `sput test`, polish
- [x] M8 — stage-1 beachhead: the printer, self-hosted in Sputter

## Repo map

- `SPEC.md` — the constitution. When code and spec disagree, the spec wins.
- `DECISIONS.md` — the running ledger of calls made where the spec is silent.
- `src/` — stage-0 compiler in Common Lisp: lexer → parser → expander →
  lowerer (to the **Plasma** core IR) → emitter (Plasma → CL) → SBCL.
- `src-sput/` — stage-1 Sputter sources compiled by stage 0; currently the
  M8 printer beachhead.
- `tests/unit/` — rove suites. `tests/golden/` — golden corpus
  (`SPUTTER_GOLDEN=update make test` regenerates).
- `examples/` — the normative programs from SPEC.md §10.

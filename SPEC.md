# Sputter v0.1: Language & Implementation Spec

You are implementing **Sputter**: a Lisp with C-family surface syntax, hosted on SBCL.

Elixir's architecture wearing Zig's clothes. Real curly-brace grammar on the surface; a
uniform code-as-data tree underneath; hygienic macros written *in surface syntax*
(patterns and templates never look like s-expressions); Common Lisp as the rented
compiler and runtime, the way Elixir rents the BEAM.

The name: *sputtering* is thin-film deposition — atoms knocked off a target condense as
a coating on a substrate. Sputter is a C-family film deposited on a Lisp substrate.

- Sources: `.sput` files. Binary: `sput`. Core IR: **Plasma**.
- Stage 0 implementation language: **Common Lisp (SBCL-only is fine)**.
- Stage 1 (later): self-host — the compiler rewritten in Sputter, still lowering to CL.
- Stage 2 (much later, do not design for it beyond keeping Plasma backend-agnostic):
  native backend.

This file is the constitution. When code and this file disagree, this file wins.
When this file is silent, follow §12 (Decision protocol).

---

## 1. Non-negotiable invariants

These override everything else in the repo, including convenience and performance.

**I1 — Homoiconicity via uniform nodes, not parens.** Every surface form maps totally
and canonically to a plain-data node `(head, meta, args)`. `quote` yields that node.
Macros are functions from nodes to nodes. The node type is ordinary data with ordinary
accessors — there is no privileged AST API beyond it.

**I2 — The Waterline.** Common Lisp and s-expressions are implementation substrate and
exist only *below* the core-lowering line. **No user-facing channel ever emits
s-expressions**: not the printer, not `sput expand`, not error messages, not REPL
echoes, not panics, not docs. `print(node)` emits Sputter surface syntax. `dump(node)`
and `show(value)` emit Sputter data literals. There is no sexp printer anywhere in the
public toolchain. A raw SBCL condition or backtrace reaching the user is a bug
(exception: the `--host-backtrace` debug flag, §8).

**I3 — Surface syntax is the notation for code everywhere it appears**: source, quote
bodies, macro patterns, macro templates, expansion output. Templates must parse.
There is no list-splicing back door.

**I4 — Macro notation has no sigils.** Pattern holes are declared by ascription
(`cond: expr`); bare identifiers in macro patterns match literally. Templates splice
in-scope node-typed names by bare name. Sequences use `...name` in both patterns and
templates. `$`, `~`, backtick, and unquote forms do not exist.

**I5 — `->` does not exist in the language.** `=>` is the single pattern→result pivot
(term-level `switch` arms and macro arms alike). Function return types are juxtaposed,
Zig-style: `fn f(x: i64) i64 { ... }`.

**I6 — Fixed operator set, fixed precedence.** No user-defined operators, no fixity
declarations, ever. Macros see operators as head-position nodes.

**I7 — Hygiene by default.** Template-introduced bindings never capture user code.
`inject(name)` is the deliberate opt-out. `raw(name)` emits an identifier literally
when it collides with a binder name.

**I8 — Splicing is structural.** Precedence cannot leak through a splice; templates
never need defensive parens. Parens and separators are the printer's problem: the
printer re-inserts grouping exactly where the tree demands it and nowhere else.

**I9 — Every language feature lands with tests** (§12): unit tests plus at least one
golden test through `sput expand` / `sput fmt` / `sput run`.

---

## 2. How to work in this repo (agent protocol)

- **Milestones are the plan.** Work M0 → M8 in order (§11). Each milestone ends
  shippable: tests green, `sput` runnable, a commit. Do not start Mn+1 with Mn red.
- **Small commits**, imperative mood, scoped: `parser: comparison ops are non-chainable`.
- **DECISIONS.md is the ledger.** When this spec is silent and you must choose, make
  the call per §12, record one line in `DECISIONS.md` (`date — decision — why —
  revisitable?`), and keep moving. Do not stall on forks. Do not ask the human
  mid-milestone unless an invariant is at stake.
- **Tiger Style, adapted to CL.** Assert density is a feature: `check-type` on every
  pipeline-stage entry; explicit `assert`s for invariants, including *negative space*
  (e.g., the emitter asserts no macro-call heads remain; the printer asserts it never
  receives a Plasma-only head it can't render). Small functions. No clever reader
  macros in the implementation. No `eval` outside the documented staging points.
- **Dependencies are near-frozen.** Quicklisp deps allowed: `trivia` (pattern
  matching), `alexandria`. Anything else goes in DECISIONS.md with justification.
  Do not build line editing — document `rlwrap sput repl`.
- **SBCL-only is acceptable.** Use `sb-ext` where it pays. Keep host-specific calls
  behind `src/host.lisp` so they're greppable, not for portability theater.
- The human's role is spec review and veto, not code review of every commit. Surface
  spec-level questions in DECISIONS.md and at milestone boundaries.

---

## 3. Architecture

### 3.1 Bootstrap plan (the Elixir model)

Stage 0 is written in Common Lisp and lives *in the SBCL image*, exactly as Elixir's
first compiler was written in Erlang and lived on the BEAM:

- **Stage 0 (this repo, now):** CL implements lexer → parser → expander → lowerer →
  emitter. Emitted CL forms are `compile`d/`eval`ed in-image. Macro bodies (`macro fn`)
  are themselves compiled through this same pipeline at definition time and `funcall`ed
  at expand time — the live image is the staging evaluator; comptime costs nothing.
- **Stage 1 (after v0.1):** rewrite the compiler in Sputter, module by module, still
  targeting CL on SBCL. First self-host target: the printer (§11, M8).
- **Stage 2 (not now):** native backend as a backend swap behind Plasma. The only
  obligation today: keep Plasma small and free of CL-isms (§6).

### 3.2 Pipeline

```
file.sput
  → LEX      tokens, each carrying file/line/col
  → PARSE    enforestation-lite (§5.8.6): tokens → Node tree, macro-aware
  → EXPAND   hygienic macro expansion to fixpoint; macro fns run in-image
  → LOWER    expanded Nodes → Plasma forms (closed head set, §6)
  → EMIT     Plasma → CL forms + span table (§7, §8)
  → HOST     SBCL compile/load into the image
```

Parsing and expansion interleave *only* at macro invocation boundaries (§5.8.6);
everything else is a conventional Pratt/precedence parser.

### 3.3 Repo layout

```
sputter/
  CLAUDE.md            this file
  DECISIONS.md         running ledger (create at M0)
  sputter.asd          ASDF system
  bin/sput             launcher (sbcl --script or saved image; M0 = script)
  src/
    package.lisp
    host.lisp          SBCL-specific calls, quarantined
    node.lisp          node & meta structs, walkers, constructors
    lex.lisp
    parse.lisp         grammar + enforestation-lite
    expand.lisp        macro env, hygiene, fixpoint driver
    plasma.lisp        core IR definition + validators
    emit.lisp          Plasma → CL, span table
    print.lisp         print / dump / show (§5.7)
    rt.lisp            runtime support: truthy, sput-equal, field, tagged, panic
    prelude.lisp       stage-0 stdlib registration (later: prelude.sput)
    cli.lisp           sput run|expand|fmt|repl|test
  tests/
    unit/              rove suites per module
    golden/            *.sput + *.expected.{expand,fmt,run,show}
  examples/            the §10 programs, runnable
```

Test framework: **rove**. Golden harness: run the CLI entry points in-process, diff
against `.expected` files, with a `SPUTTER_GOLDEN=update` regeneration mode.

---

## 4. The node model

The AST. One shape for everything, Elixir-style, in Zig clothes.

### 4.1 Definition

A **node** has exactly three fields:

- `head` — an atom naming the form (`.add`, `.call`, `.if`, `.ident`, ...).
- `meta` — a record: `.{ .file, .line, .col, .scopes, .synthetic }`. `scopes` is the
  hygiene mark set (§5.8.5); `synthetic` marks printer-owned nodes with no source span.
- `args` — a list whose elements are nodes or scalars.

**Scalars self-quote**: integers, floats, strings, booleans, atoms appear directly in
`args` without wrapping. An identifier *is* a node: head `.ident`, args `[.name]`.

### 4.2 Stage-0 representation (CL)

```lisp
(defstruct (node (:constructor %make-node)) head meta args)
(defstruct meta file line col scopes synthetic)
```

Structs, not lists — this makes the Waterline structurally hard to violate.
`print-object` on `node` prints an opaque `#<sput-node ADD demo.sput:12>` so host-level
debugging output can never be mistaken for code. Heads are CL keywords internally.

### 4.3 Head inventory produced by the parser

| Category | Heads |
|---|---|
| Leaves | `.ident`, literals are scalars (no node) |
| Operators | `.add .sub .mul .div .rem .concat .eq .ne .lt .le .gt .ge .and .or .not .neg .pipe` |
| Postfix | `.call` (args: callee, then arguments), `.index`, `.field` |
| Data | `.list_lit` (elements; spread element = `.spread` node), `.record_lit` (args: `.field_init` nodes), `.atom_lit`? — no: bare atoms are scalars; `.tagged_lit` (args: atom, then values) |
| Control | `.if` (cond, then-block, else-block-or-nil), `.switch` (scrutinee, `.arm` nodes), `.arm` (pattern, body), `.block` (statements; last-expr rule §5.4), `.while`, `.for_in`, `.return`, `.unreachable` |
| Bindings | `.let`, `.var`, `.assign`, `.op_assign` (op atom, target, value) |
| Defs | `.fn` (name-or-nil, `.param` nodes, ret-type-or-nil, body), `.param` (name, type-or-nil), `.macro_def`, `.macro_fn_def` |
| Macro-space | `.macro_call` (name, raw token-group payload §5.8.6), `.quote` (kind atom, body), `.splice_seq` (`...name` in templates), `.hole` (name, kind — pattern positions only), `.raw`, `.inject`, `.insert` |
| Types | `.type_ident`, `.type_list`? — v0.1 types are bare `.type_ident` only |

The expander eliminates every macro-space head; the lowerer eliminates everything else
into Plasma heads (§6). Each stage **asserts** its input contains no heads from later
stages and its output contains no heads it was responsible for eliminating.

### 4.4 Node values at runtime

From M4 on, nodes are first-class runtime values (that's the point). `quote { ... }`
evaluates to a node; `head(n)`, `args(n)`, `meta(n)`, `node(head, args)` (meta
synthesized), `prewalk(n, f)`, `postwalk(n, f)` are ordinary stdlib functions; `switch`
patterns destructure nodes like any tagged data.

---

## 5. The language

### 5.1 Lexical structure

- **Comments:** `//` to end of line. (Doc comments `///` reserved, ignored in v0.1.)
- **Identifiers:** `[A-Za-z_][A-Za-z0-9_]*`, snake_case by convention. Mangling to CL:
  snake_case → hyphenated uppercase symbol in package `SPUTTER` (`my_func` →
  `SPUTTER::MY-FUNC`).
- **Keywords (reserved):** `fn macro let var if else switch while for in quote and or
  return true false nil unreachable`. (`raw`, `inject`, `insert`, `test` are known
  forms, not reserved words.)
- **Atoms:** `.` + identifier in *prefix* position: `.ok`, `.not_found`. Lower to CL
  keywords (`:ok`). `.` + identifier in *postfix* position is field access (§5.3).
  `.{` opens a record literal. Disambiguation is purely positional.
- **Numbers:** decimal integers (underscores allowed: `1_000_000`), hex `0x...`,
  floats `1.5`, `2.0e10`. Integers get CL bignums for free.
- **Strings:** `"..."`, escapes `\n \t \\ \" \xNN`. Concatenation is `++`.
- **Punctuation:** `{ } ( ) [ ] , ; : => = . ... |>` plus the operator set (§5.3).
- **Statement separator:** `;` is required between statements. No automatic semicolon
  insertion. The Rust rule (§5.4) gives the trailing `;` semantic weight.

### 5.2 Programs and files

A `.sput` file is a sequence of top-level forms: `fn`, `macro`, `macro fn`, `let`,
`var`, `test`, and bare statements (executed at load time, in order). **Single global
namespace in v0.1** (one CL package, `SPUTTER`). Define-before-use, top to bottom,
including macros — a macro must be defined earlier in the file (or an earlier file on
the command line) than its first use, exactly like CL and Zig comptime. No module
system in v0.1 (§13).

### 5.3 Expressions and operators

Fixed precedence, tightest to loosest. Zig's decisions unless noted.

| Level | Operators | Notes |
|---|---|---|
| 1 | `x(args)` `x[i]` `x.field` | postfix; left-assoc |
| 2 | `!x` `-x` | unary prefix |
| 3 | `* / %` | left-assoc |
| 4 | `+ - ++` | left-assoc; `++` concatenates strings and lists |
| 5 | `== != < <= > >=` | **non-chainable** (Zig rule): `a < b < c` is a parse error |
| 6 | `and` | short-circuit |
| 7 | `or` | short-circuit |
| 8 | `\|>` | left-assoc pipeline |
| — | `= += -= *= /= ++=` | **statements only**, never expressions (§5.4) |

- Operators parse to head-position nodes: `total + tax * 2` →
  `.add(ident total, .mul(ident tax, 2))`. Macros therefore never see fixity.
- **Pipeline desugar (Elixir insert-first):** `x |> f(a, b)` → `f(x, a, b)`;
  `x |> f` → `f(x)`. Desugared by the lowerer, not the parser — macros see `.pipe`.
- `if` and `switch` take **paren-free** conditions/scrutinees; braces are mandatory:
  `if user.karma < 0 { -1 } else { 1 }`. `if` without `else` has value `nil`.
- Calls: `f(a, b)`. Callee may be any postfix chain (`obj.handler(x)` calls the value
  stored in the field — there are no methods).
- Lambdas: `fn(x) { x + 1 }` — anonymous `fn`, same node shape with nil name. Arrow
  shorthand deliberately absent in v0.1 (§13) so `=>` keeps exactly one job.

### 5.4 Blocks, bindings, statements

- `{ s1; s2; expr }` is an expression. **Rust semicolon rule:** a trailing expression
  *without* `;` is the block's value; if the last statement ends in `;`, the value is
  `nil`. `}`-terminated forms (`if`, `switch`, `while`, `for`, nested blocks, `fn`
  defs) need no trailing `;` as statements.
- `let name = expr;` — immutable binding. Immutability is **erased discipline**
  (the TS `readonly` analogy): enforced by the lowerer as a compile error on
  reassignment, no runtime cost. `let name: Type = expr;` ascribes.
- `var name = expr;` — mutable. `name = expr;` and compound assignment (`+=` etc.)
  are statements, legal only on `var` targets and record-free (no `x.f = v` in v0.1,
  records are immutable, §5.5).
- `while cond { ... }` — value `nil`. `for x in xs { ... }` — iterates a list, value
  `nil`. No `break`/`continue` in v0.1 (§13); use `return`, restructure, or recurse.
- `return expr;` / `return;` — early exit from the enclosing `fn`. Lowers to CL
  `return-from` for free.
- `fn name(a: T, b) RetT { body }` — types optional anywhere (gradual). Value of the
  body block is the return value. `unreachable` — signals a panic if reached.

**Grammar note (the `->`-ban tax):** juxtaposed return types require Zig's
TypeExpr/Expr split. Return position and ascription position parse **TypeExpr**, which
in v0.1 is just a type identifier and *never* takes a `{`-suffix — so `fn f() T {`
unambiguously starts the body. Keep the split explicit in the parser even while
TypeExpr is trivial; it's load-bearing later.

### 5.5 Data and runtime representation

| Sputter | Syntax | CL representation |
|---|---|---|
| Integer / Float | `42`, `1.5` | integer / double-float |
| String | `"hi"` | simple string |
| Boolean | `true`, `false` | `T` / `+sput-false+` singleton struct (see below) |
| Nil | `nil` | `+sput-nil+` singleton — distinct from the empty list; `NIL` is only ever `[]` |
| Atom | `.ok` | keyword `:OK` |
| Tagged | `.ok(v1, v2)` | `%tagged` struct: tag keyword + values (simple-vector) |
| List | `[1, 2, 3]`, spread `[a, ...rest]` | CL list |
| Record | `.{ .name = "amy", .age = 3 }` | hash table, keyword keys, immutable by fiat |
| Function | `fn(x) { ... }` | CL function |
| Node | `quote { ... }` | `node` struct (§4.2) |

- **Truthiness:** `false` and `nil` are falsy; everything else truthy — including
  `[]` (the Elixir model, in full: `nil` and `false` are distinct singletons, and
  the empty list is a list, not nil — `[] != nil`, `[]` is truthy). CL `NIL` plays
  no role beyond the empty list. Every conditional lowers through `(truthy x)`, an
  inlined rt function testing the two falsy singletons. Cost ≈ zero under SBCL
  inlining. (Human veto 2026-07-02: the earlier stage-0 pun `[] ≡ nil` is out;
  see §13.18.)
- **Equality:** `==` is structural `sput-equal`: numbers by `=` (so `1 == 1.0`,
  Elixir-style, §13), atoms/booleans by identity, strings `string=`, lists/records/
  tagged/nodes recursively. `!=` is its negation.
- **Field access** `x.field` → `(sput-field x :field)`: records via gethash (missing
  key → panic), tagged values expose `.tag`, nodes expose `.head/.meta/.args`.
- Records are **immutable** in v0.1; build new ones. (Update syntax: §13.)

### 5.6 Term-level pattern matching: `switch`

```zig
switch fetch(user.id) {
    .ok(profile) => greet(profile),
    .err(.not_found) => "who?",
    .err(reason) => fallback(reason),
    .{ .role = .admin, .name = name } => hello_admin(name),
    [first, ...rest] => first,
    42 => "the answer",
    else => "dunno",
}
```

- Arms are `pattern => expr` or `pattern => { block }`. **Rust comma rule:** comma
  required after expression arms, optional after `}`-terminated arms.
- **Term patterns bind bare identifiers** — the exact inverse of macro patterns, where
  bare identifiers match literally (§5.8.2). Literals and atoms match by `==`.
  `.tag(p1, p2)` destructures tagged values (arity-checked). `.{ .field = p }` matches
  records with at least those fields. `[p1, p2]` exact-length list; `[p, ...rest]`
  binds the tail. `_` wildcard. `else` is sugar for `_`.
- First matching arm wins; no fallthrough. **No match → panic** with the scrutinee
  `show`n. Guards (`pattern if cond =>`) deferred (§13).
- v0 lowering rides `trivia:match` (§7); the semantics above are normative, the
  mechanism is not.

### 5.7 `quote`, `print`, `dump`, `show`

- `quote { ... }` — parses the body with the real parser, evaluates to a node. Default
  nonterminal is `expr`; `quote(stmt) { ... }`, `quote(stmts) { ... }`, `quote(block)`,
  `quote(type)`, `quote(arm)` select others (Rust fragment specifiers, generalized).
  Quote bodies must parse — ill-formed fragments are impossible by construction (I3).
- `print(node) -> str` — canonical surface pretty-printer. Contract:
  `parse(print(n)) ≡ n` modulo meta; **minimal parens** — grouping appears exactly
  where precedence demands (I8); canonical layout (4-space indent, one arm per line,
  trailing commas on multiline arm lists). `sput fmt` is `parse` ∘ `print`.
- `dump(node) -> str` — the node as a Sputter **data literal**, in the language's own
  record/list syntax, e.g.:

```zig
dump(quote { total + tax * 2 })
// .{ .head = .add, .meta = .{ .line = 12, .scopes = [] }, .args = [
//     .{ .head = .ident, .meta = ..., .args = [.total] },
//     .{ .head = .mul, .meta = ..., .args = [
//         .{ .head = .ident, .meta = ..., .args = [.tax] },
//         2,
//     ]},
// ]}
```

- `show(value) -> str` — runtime values as Sputter literals: `.ok(3)`,
  `.{ .name = "amy" }`, `[1, 2, 3]`, `true`, `nil`, `"hi"`, `<fn area/2>`,
  `<node add demo.sput:12>`. The REPL echoes through `show`. `dump` on a node ≡
  `show` of its record representation. These three printers are the *only* renderers
  of code and values in the toolchain (I2).

### 5.8 The macro system

Two tiers. The by-example tier is sugar over the procedural tier.

#### 5.8.1 By-example: `macro name { arms }`

```zig
macro cond {
    { cond { } } =>
        { unreachable },

    { cond { c: expr => body: expr, ...rest: arm } } =>
        { if c { body } else { cond { ...rest } } },
}

macro unless {
    { unless cond: expr { ...body: stmt } } =>
        { if (!(cond)) { ...body } },

    { unless cond: expr { ...body: stmt } else { ...alt: stmt } } =>
        { if (!(cond)) { ...body } else { ...alt } },
}
```

(The `!(cond)` parens above are the author's choice, not a requirement — see I8; the
`check` example in §10 shows the paren-free form.)

- Arms are `{ pattern } => { template }`, comma rule as in `switch`.
- Left sides are **implicit quotes** (patterns), right sides implicit templates.
- Arms are tried **in order**; for invocation-extent parsing, longest-match applies
  (§5.8.6). No arm matches → compile error naming the macro, the source span, and the
  arms tried.
- Expansion is outermost-first and runs to **fixpoint** — templates may re-invoke the
  same macro (see `cond`), with a depth limit (default 512) and a cycle error message.

#### 5.8.2 Patterns

- Written in surface syntax; parsed with the real parser plus hole extensions.
- **Ascription declares a hole:** `cond: expr` binds `cond` to one node of kind
  `expr`. **Bare identifiers match literally** — that's how `unless`'s second arm
  claims the `else` keyword. (This is syntax-rules flipped: literals are the default,
  variables are declared. Right choice for a keyword-heavy C grammar.)
- **Sequence holes:** `...body: stmt` binds a list of nodes. v0.1 restriction, to keep
  matching decidable without backtracking: a sequence hole must be delimited by a
  literal token or a group boundary on both sides (all §10 examples qualify). Lift
  later if needed; error clearly when violated.
- **Kind inventory (v0.1):** `expr`, `stmt`, `block`, `ident`, `atom`, `literal`,
  `type`, `arm`. Kinds constrain what a hole may match — the structure token-stream
  macro systems forfeit.

#### 5.8.3 Templates

- Implicit quotes. Any **in-scope node-typed name splices by bare name**; everything
  else is literal syntax. `...body` splices a sequence. Separators and grouping are
  the grammar/printer's problem — there is no `$(...),*`-style separator syntax, and
  no template ever needs defensive parens (I8).
- **Escape hatches**, all rare:
  - `raw(name)` — emit the identifier `name` literally even though `name` is bound as
    a hole in this arm.
  - `insert(expr)` — computed splice: evaluate `expr` (expand-time) to a node and
    splice it. This is the explicit antiquote; nested-quote depth games are punted to
    it deliberately.
  - `inject(name)` — splice identifier `name` *unhygienically*, resolving at the call
    site (anaphora). Use sparingly; every use is a documented API decision.
- **Bind-first idiom** for computed identifiers (also the Tiger Style answer — name
  your intermediates):

```zig
macro fn getter(field: ident) stmt {
    let name: ident = concat_ident("get_", field);
    quote(stmt) { fn name(r) { r.insert(field) } }
    // `name` and `field` splice by name; insert() shown for a computed field access
}
```

#### 5.8.4 Procedural: `macro fn`

```zig
macro fn check(cond: expr) expr {
    let text = print(cond);          // runs at expand time
    quote {
        if !cond {
            panic("check failed: " ++ text)
        }
    }
}
```

- A `macro fn` is a **comptime function from nodes to nodes**. Parameter kinds and the
  return kind use the §5.8.2 inventory — kinds are the types of macro land. The
  returned node is kind-checked; scalar returns lift to literal nodes (as do scalars
  spliced into templates: `text` above becomes a string literal).
- The body is ordinary Sputter, compiled through the full pipeline **at definition
  time** and installed in the image; the expander `funcall`s it. This is the rented
  live image doing staging for free (§3.1).
- Comptime stdlib available in macro bodies: `print`, `dump`, `head`, `args`, `meta`,
  `node`, `prewalk`, `postwalk`, `concat_ident`, `gensym_ident`, plus the whole
  runtime stdlib.
- **Desugaring:** `macro name { arms }` compiles to one `macro fn` whose body
  `switch`es on its input node — one language, one mechanism. `raw`/`insert`/`inject`
  work identically in both tiers.

#### 5.8.5 Hygiene

Observable contract (normative):

1. Bindings introduced by a template (a `let`/`var`/`fn`-param whose name is template
   literal text, not a splice) can never capture or be captured by user code.
2. Free identifiers in a template resolve in the **macro's definition environment**.
3. `inject` opts out of (1)/(2) for one identifier.

Implementation (v0.1 allowance): rename-based hygiene — template-literal binders get
fresh names per expansion, recorded as scope marks in `meta.scopes`. Contract (2) may
be approximated by definition-order + single-namespace resolution in v0.1 (note in
DECISIONS.md). The long-term design is Flatt's sets-of-scopes; `meta.scopes` is
already shaped for it. Required tests: template `let tmp` vs user `tmp` (no capture,
both directions); `inject` anaphora works; `raw` collision escape works.

#### 5.8.6 Invocation extent (enforestation-lite)

The hard problem of C-syntax macros: does `unless c { } else { }` own the `else`?
v0.1 rule, sufficient for the whole §10 corpus:

- The parser knows which identifiers are macros (define-before-use, §5.2). On a macro
  identifier in statement or expression head position, it collects the invocation as
  raw token groups: balanced `(...)`, `[...]`, `{...}` groups and the loose tokens
  between them, up to each arm's declared shape. **Arms are tried longest-first**; an
  arm's own pattern defines exactly which trailing tokens (e.g. a literal `else`
  followed by a `{...}` group) belong to the invocation. First (longest) arm whose
  shape fits claims the extent; its holes are then sub-parsed by kind.
- `macro fn` invocations use call syntax: `check(reserved <= capacity)` — extent is
  the balanced parens; each argument sub-parses as its declared kind.
- This is a deliberate simplification of Honu-style enforestation (Flatt; Rhombus is
  the production descendant). Full operator-aware enforestation is a post-v0.1
  upgrade; the token-group interface is designed so the upgrade is internal to
  `parse.lisp`.

---

## 6. Plasma — the core IR

Plasma is the closed set of forms that exists **after** expansion and desugaring.
It is the Core Erlang of Sputter: small, boring, backend-agnostic. The lowerer maps
surface nodes to Plasma; the emitter maps Plasma to CL. Nothing above the lowerer
knows CL exists; nothing below it knows macros existed.

Plasma heads (complete list — additions require a DECISIONS.md entry):

```
p.lit  p.ref  p.call  p.host_call  p.fn  p.if  p.let  p.assign  p.block
p.match  p.list  p.record  p.tagged  p.field  p.index  p.while  p.return
p.and  p.or  p.panic
```

Notes:

- Operators are **not** Plasma: they lower to `p.call` of rt builtins or, where the
  emitter's known-function table allows, direct CL ops (§7). `|>` and `for..in` are
  desugared before Plasma. `unreachable` → `p.panic`. `not/neg` → calls.
- `p.match` keeps structured patterns (the trivia ride is an emitter detail, §7).
- `p.host_call` carries a resolved CL symbol — the *only* Plasma form that names the
  host, produced solely by the `cl.` bridge (§7) and rt-builtin lowering. A native
  backend replaces its emitter case and the rt library; nothing else.
- `plasma.lisp` ships `validate-plasma`: every emitter entry asserts its input is
  well-formed Plasma and contains no surface or macro-space heads (negative space).

---

## 7. Lowering to Common Lisp

| Sputter / Plasma | CL emission |
|---|---|
| `fn name(a: f64) f64 { ... }` | `(defun name (a) (declare (type double-float a)) (the double-float <body>))` — `defun` establishes the block `return` needs |
| `p.return` | `(return-from <fn-name> v)` |
| `let` / `var` / `p.let` | `let*`; `let`-immutability enforced in the **lowerer** (compile error), not by CL |
| `p.if` | `(if (truthy c) a b)` |
| `p.and` / `p.or` | CL `and`/`or` threaded through `truthy`, preserving short-circuit + value semantics |
| `p.match` | `trivia:match` + translation table for §5.6 patterns; no-match branch panics with `show`n scrutinee |
| `p.list` / spread | `list` / `append` |
| `p.record` | rt `make-record` (hash table) |
| `p.tagged` / `.ok` | rt `make-tagged` / keyword |
| `p.field` / `p.index` | rt `sput-field` / `sput-index` (bounds-checked, panics) |
| `p.while` | `loop while (truthy c) do ...` |
| `==` | rt `sput-equal` |
| `++` | rt `sput-concat` (string/string, list/list; type mismatch panics) |
| arithmetic/comparison | direct CL (`+ - * / rem = /= < <= > >=`) via the known-function table; SBCL specializes where declarations allow. `%` is truncated `rem` (C/Zig, matching the `.rem` head) — human veto 2026-07-02 |
| `p.panic` | `(error 'sputter-panic :message ... :span ...)` |
| identifiers | mangled per §5.1 into package `SPUTTER` |

- **Lisp-1 on Lisp-2:** Sputter is Lisp-1. Known globals (functions defined by `fn`,
  rt builtins) compile to direct calls; everything else — locals, params, field
  values — compiles to `funcall`. The lowerer tracks which is which.
- **Type ascriptions** lower to `declare`/`the` per the table below and are otherwise
  semantics-free in v0.1 (no Sputter typechecker; SBCL's derived-type warnings are
  surfaced as Sputter warnings through the §8 renderer). Gradual typing arrives as
  gradual performance.

| Sputter type | CL type |
|---|---|
| `i32` / `i64` | `(signed-byte 32)` / `(signed-byte 64)` |
| `u32` / `u64` | `(unsigned-byte 32)` / `(unsigned-byte 64)` |
| `int` | `integer` |
| `f32` / `f64` | `single-float` / `double-float` |
| `bool` / `str` / `atom` | `boolean`* / `string` / `keyword` |
| `list` / `record` / `node` / `any` | `list` / `hash-table` / `node` / `t` |

\* `bool` declaration accepts the false singleton via a satisfies-type in rt; keep it
loose rather than clever.

- **Host bridge:** `cl.some_function(args)` calls `COMMON-LISP:SOME-FUNCTION`
  (mangling per §5.1), emitted as `p.host_call`. **Functions only — host macros never
  cross the bridge** (they'd want sexp templates; Elixir doesn't expose the Erlang
  preprocessor either, and nobody mourns). Unknown symbol → compile error.
- **TCO:** SBCL performs tail-call elimination under default optimize settings but the
  standard doesn't guarantee it. Sputter therefore *has loops* and documents
  self-recursion depth as implementation-defined. One `(declaim (optimize ...))` in
  `host.lisp`, documented.

---

## 8. Errors and the Waterline (rendering contract)

- All conditions escaping user code are caught at the CLI/REPL boundary and rendered
  by Sputter: `error: <message>` plus, when spans are known, `at <fn> (file:line:col)`
  frames. Parse/expand/lower errors always carry precise spans (tokens have them);
  runtime frames use the span table below, best-effort.
- **Span table:** the emitter records `generated CL function → source span` (and, where
  cheap, form-level positions). Runtime backtrace mapping is best-effort in v0.1;
  *compile-time* diagnostics pointing at exact braces are mandatory from M1.
- `--host-backtrace` (env `SPUTTER_HOST_BACKTRACE=1`) prints the raw SBCL condition and
  backtrace *after* the Sputter rendering — implementor tool, the sole sanctioned I2
  exception.
- Waterline regression test: golden files assert that `sput expand`, `sput fmt`, error
  output, and REPL echoes contain Sputter syntax / prose only. A crude
  `looks-like-sexp-p` heuristic on all user-channel output is cheap and worth it.
- Future (post-v0.1, do not build now): CL conditions/restarts surfaced as
  `handle`/`retry` syntax — the rented runtime's killer feature. Nothing in v0.1 may
  preclude it; `sputter-panic` subclassing `error` suffices.

---

## 9. CLI — `sput`

```
sput run file.sput...         compile + execute in a fresh image state
sput expand file.sput         fully expand; print the module as SURFACE SYNTAX
sput expand --dump file.sput  same, as data literals (dump form)
sput fmt file.sput            parse + print canonically (--check for CI)
sput repl                     read (balanced-brace multiline) → pipeline → show
sput test file.sput...        run `test "name" { ... }` blocks, report pass/fail
```

- **`sput expand` is the flagship** — the executable proof of I2/I3. Its output is a
  valid `.sput` module: `sput run x.sput` ≡ running its own expansion (macro
  definitions print as comments noting they were consumed). Expansion is idempotent:
  `expand ∘ expand = expand` — golden-tested.
- REPL prompt `sput> `; echoes via `show`; errors per §8; state persists across
  entries within a session. Line editing via `rlwrap` (documented, not built).
- `test` is a *macro in the prelude* (dogfood): registers a thunk; `sput test` runs
  them. Inside tests, `check(...)` (§10) is the assertion.
- `bin/sput` is an `sbcl --script` shim in M0; a saved image (`sb-ext:save-lisp-and-die`)
  is an M7 nicety behind `sput build-image`.

---

## 10. Normative examples (seed the golden corpus with exactly these)

### 10.1 Term-level tour (`examples/tour.sput`)

```zig
fn area(w: f64, h: f64) f64 {
    w * h
}

fn describe(user) {
    let status = .ok;
    let found = .ok(user.id);
    let account = .{ .name = "amy", .roles = [.admin, .ops] };

    var retries = 0;
    retries += 1;

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

Golden assertions: parses; `fmt` is a fixpoint; `area` emits with `double-float`
declares (≈ `(defun area (w h) (declare (type double-float w h))
(the double-float (* w h)))`); pipeline desugars insert-first.

### 10.2 `cond` and `unless` (by-example tier)

As written in §5.8.1. Golden assertions: `sput expand` on

```zig
let grade = cond {
    score >= 90 => .a,
    score >= 60 => .c,
    true        => .f,
};
```

yields nested `if`s in surface syntax; single-step expansion shows the re-invocation
(`if score >= 90 { .a } else { cond { ... } }`) and fixpoint terminates; `unless`'s
second arm claims its `else` (extent test, §5.8.6); empty `cond {}` expands to
`unreachable`.

### 10.3 `check` (procedural tier — Tiger Style demo)

```zig
macro fn check(cond: expr) expr {
    let text = print(cond);
    quote {
        if !cond {
            panic("check failed: " ++ text)
        }
    }
}

check(reserved <= capacity);
// expands to:
// if !(reserved <= capacity) { panic("check failed: " ++ "reserved <= capacity") }
```

Golden assertions: the printed expansion contains `!(reserved <= capacity)` — parens
inserted by the **printer** because the tree demands them, though the template wrote
`!cond` bare (I8); `print(cond)` ran at expand time (the string literal is baked in);
hygiene tests from §5.8.5 pass alongside.

### 10.4 Dump round-trip

`dump(quote { total + tax * 2 })` matches §5.7's shape; reading that data literal
back and `print`ing it yields `total + tax * 2`.

---

## 11. Milestones

Each milestone = tests green + golden corpus green + one commit tagged `mN`.
Definition of done listed per milestone; do not gold-plate past it.

- **M0 — Skeleton.** ASDF system, packages, `node`/`meta` structs + walkers +
  opaque printing, rove wired, golden harness with update mode, `bin/sput` shim
  prints usage, DECISIONS.md created. DoD: `sput` runs; `rove` green in CI-able form.
- **M1 — Parse + print the M-core** (no macros, no switch): literals, idents,
  operators with the §5.3 table (non-chainable comparisons enforced), calls, postfix
  chains, blocks with the Rust `;` rule, `if/else`, `let/var/assign`, `fn` with the
  TypeExpr split, `while`, `return`, `unreachable`. Surface printer with
  minimal-paren proof (parse∘print property test over the corpus). `sput fmt`,
  `sput expand` (identity). DoD: tour file §10.1 minus switch/pipeline round-trips.
- **M2 — Lower + emit + run the M1 subset.** Plasma for the M1 heads, emitter,
  known-function table, mangling, rt (`truthy`, panic), value `show`, `sput run`,
  `sput repl`, §8 boundary rendering with compile-time spans. DoD: `area` runs;
  deliberate errors render Sputter-side; waterline heuristic test in place.
- **M3 — Data + match + sugar.** Atoms, tagged, lists (+spread), records, field/index,
  `==`/`sput-equal`, `++`, `false`-singleton truthiness, `switch` via trivia with §5.6
  patterns and panic-on-no-match, `|>` desugar, lambdas, `for..in`. DoD: full §10.1
  runs end to end.
- **M4 — Nodes as values.** `quote {}` / `quote(kind) {}`, `dump`, `print`, node
  accessors/constructors/walkers in stdlib, nodes matchable in `switch`. DoD: §10.4
  golden passes; REPL can quote/inspect/print.
- **M5 — Procedural macros.** `macro fn`: definition-time compile+install, expand-time
  funcall, kind checks, scalar lifting, rename-based hygiene + `raw`/`insert`/`inject`,
  fixpoint driver with depth limit, `sput expand` shows real expansions. DoD: §10.3
  including the printer-paren assertion and hygiene tests.
- **M6 — By-example tier.** `macro name { arms }` desugar to `macro fn`, pattern holes
  by ascription, literal-ident matching, sequence holes with the delimiter
  restriction, longest-arm extent rule. DoD: §10.2 in full (`cond` fixpoint, `unless`
  `else`-claiming, empty-`cond`, no-arm-match diagnostics).
- **M7 — Dogfood + polish.** Prelude grows (`map/filter/reduce/sum/len/push?/str
  utils`) — in CL for now; `test` macro + `sput test`; `check` moved into prelude;
  `sput fmt --check`; `build-image`; stress corpus (every spec section exercised);
  README with install + tour; DECISIONS.md reconciled against §13.
- **M8 — Stage-1 beachhead.** Port the **printer** to Sputter (`src-sput/print.sput`),
  compiled by stage 0, its output diffed against the CL printer over the whole golden
  corpus. This proves the self-hosting loop without betting the toolchain on it.

---

## 12. Decision protocol

When the spec is silent: (1) does an invariant (§1) decide it? then it's decided;
(2) else, what would Zig do? prefer that; (3) else, what does Elixir's macro system
do? prefer that; (4) else, smallest mechanism that doesn't foreclose the ledger items
in §13. Record the call in DECISIONS.md and continue. Never resolve ambiguity by
adding user-visible sigils, user-defined operators, or anything that emits sexps.

---

## 13. Decided defaults (the veto ledger)

Calls already made, chosen to unblock v0.1; each is revisitable and none may be
silently changed. The human veto-scans this list.

1. **Lambda shorthand deferred** — only `fn(x) { ... }`; no `(x) => e` (keeps `=>` to
   one job), no `|x| e` yet.
2. **`false ≠ nil`**, both falsy (Elixir model), via a false-singleton + inlined
   `truthy` shim — chosen over conflating `false` with CL `NIL`.
3. **`==` compares numerics across int/float** (`1 == 1.0` is `true`, Elixir-style).
4. **`|>` binds loosest** (below `or`, above `=`), left-assoc, insert-first desugar.
5. **Comparisons non-chainable** (Zig): `a < b < c` is a parse error.
6. **Assignment is a statement**, never an expression. Compound assigns on `var` only.
7. **No `break`/`continue`** in v0.1; `while`/`for..in` return `nil`.
8. **Records immutable**; no `x.f = v`; no update syntax yet (candidate:
   `.{ ...r, .f = v }`).
9. **Single namespace, define-before-use** (incl. macros); module system is a
   post-v0.1 design, not to be improvised by the agent.
10. **Strings are CL strings**; `++` concat; rich string lib deferred.
11. **Sequence holes need literal/group delimiters** on both sides (§5.8.2).
12. **Guards in `switch` deferred**; `else` arm = `_`.
13. **`;` required**, no newline significance; Rust trailing-`;` semantics.
14. **Comments are `//` only**; `///` reserved.
15. **`cl.` bridge is functions-only**, `COMMON-LISP` package only for now.
16. **Semantic types v0.1 = declarations only** (no Sputter typechecker); unknown type
    idents warn and emit `t`.
17. **Naming:** core IR is **Plasma** (fallback label "Sputter Core" if the whimsy
    wears thin); snake_case identifiers; `concat_ident` (not `concatIdent`).
18. **`nil` is a distinct singleton; `[] != nil`** (human veto 2026-07-02).
    `[]` is truthy and equals only `[]`; `show([])` prints `[]`; `%` stays
    truncated `rem` (same veto). CL `NIL` in node args is reserved for
    structural absence (a missing else/type/name), never a value.

## 14. Non-goals for v0.1

Performance tuning beyond free SBCL wins; module/package system; Sputter-level type
checking; native backend; concurrency; conditions/restarts surface syntax (§8 future);
user-defined operators (never, per I6); doc tooling; editor tooling beyond `sput fmt`.

---

*End of spec. Commit this file, then start at M0.*

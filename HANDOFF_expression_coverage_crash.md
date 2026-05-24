# HANDOFF — NVC crash when collecting EXPRESSION coverage inside PACKAGE bodies

**Date:** 2026-05-24
**NVC:** fork branch `claude/nvc-coverage-uvvm-Lcp7F` = upstream + cherry-pick of
PR nickg/nvc#1456 (`+package` coverage-spec directive, commit `31ae842`
"Collect code coverage for package declarations").
**Goal of this document:** give a dedicated session everything needed to fix the
crash so that **expression coverage can stay ENABLED** on package subprogram
bodies. Right now the downstream UVVM flow works around it by dropping
`expression` from `--cover` (see §6); we want to remove that workaround.

> Context: PR #1456 makes NVC instrument subprogram bodies inside packages
> (opt-in via `+package`). statement / branch / toggle coverage of packages
> work and are reliable. **Expression coverage of package bodies crashes the
> compiler** on some real designs and, even where it does not crash, appears to
> mis-attribute per-expression items. This handoff is ONLY about that.

---

## 1. Symptom

With `--cover=...,expression,...` AND a `+package` opt-in that pulls a package
subprogram body into coverage, NVC aborts during the **lazy lowering** of that
subprogram body (it is lowered on first call, at run time, via
`unit_registry_get`). Two different crash sites have been observed for the SAME
trigger — the exact site depends on lazy-lowering order/state, which already
hints that the cover-item / cover-counter state for lazily-lowered package
subprograms is inconsistent rather than a single off-by-one:

**Crash A — expression coverage, invalid vcode reg:**
```
../src/vcode.c:4604        emit_not
../src/lower.c:2432        lower_expr_coverge        <-- emit_not(rhs) with rhs == VCODE_INVALID_REG
../src/lower.c:6505        lower_sequence            (assignment RHS in a case alternative)
../src/lower.c:7210        lower_case_scalar
../src/lower.c:7502        lower_case
../src/lower.c:10546       lower_proc_body
../src/lower.c:13128       unit_registry_get         (lazy lowering of the package subprogram)
... jit_interp / model_run (i.e. happens at RUN time, not at -e time)
```

**Crash B — branch coverage counters, SIGSEGV in var-upref:**
```
*** Caught signal 11 (SEGV_MAPERR) [address=0xffffffff0000016d] ***
../src/vcode.c:4879        emit_var_upref
../src/lower.c:1651        lower_cover_counters      <-- emit_var_upref(hops, var) with a bad hops/var
../src/lower.c:1671        lower_branch_coverage
../src/lower.c:7180        lower_case_scalar
../src/lower.c:7502        lower_case
../src/lower.c:10546       lower_proc_body
../src/lower.c:13128       unit_registry_get
```

Both originate in coverage instrumentation of a **CASE statement inside a
package subprogram body**, reached through the lazy `unit_registry_get →
lower_proc_body` path. Crash A is in expression coverage; crash B is in the
branch-coverage *counter* fetch — but B only appears once `expression` is also
enabled (a `statement,branch,toggle,fsm-state` run of the very same design is
clean — see §6), so the expression pass is what destabilises it.

---

## 2. Root-cause analysis (what is established, what is hypothesis)

### 2.1 Crash A — confirmed mechanism
`lower_logical()` (src/lower.c:2491) lowers a logical/relational `T_FCALL` and
emits expression coverage. For a UNARY `not` (`S_SCALAR_NOT`, line 2587-2588) it
calls:
```c
return lower_expr_coverge(lu, first, emit_not(r0), r0, r1, 0);   // r1 == VCODE_INVALID_REG
```
because `p1` is NULL for a unary op so `r1` stays `VCODE_INVALID_REG` (line 2572).

Inside `lower_expr_coverge()` (line 2420):
```c
if (first->flags & COVER_FLAGS_LHS_RHS_BINS) {   // 00|01|10|11, see cov-api.h:187
   lhs_n = emit_not(lhs);
   rhs_n = emit_not(rhs);    // <-- line 2432-2433: emit_not(VCODE_INVALID_REG) -> crash A
}
```
`COVER_FLAGS_LHS_RHS_BINS` is the *binary* bin set. A unary `not` item should
carry `COV_FLAG_TRUE | COV_FLAG_FALSE` (cov-api.h:161-162), NOT the binary bins.
So **the cover item `first` retrieved for this unary `not` is the WRONG item**
— it belongs to a *binary* expression. `first` comes from
`cover_get_item(gs->cscope, COV_ITEM_EXPRESSION, (*nth)++)` (line 2504): the
`nth`-th expression item of the scope. ⇒ **The item-CREATION order and the
item-CONSUMPTION order disagree for package subprogram bodies**, so the `nth`
pointer lands on the wrong item.

- Items are CREATED by the PR's new walk: `vhdl_cover_package()`
  (src/vhdl/vhdl-cover.c, near line 378) → `vhdl_cover_decls(pack, …)` and again
  for the body, called from `lower_cover_setup_package()` in src/lower.c.
- Items are CONSUMED in lowering order by `lower_logical()` / the statement
  lowering, via the per-context `nth` counter.
- For architectures/processes these two walks agree (expression coverage works
  there). For PACKAGE subprogram bodies they apparently do not.

### 2.2 Crash B — likely related scope/hops bug
`lower_cover_counters()` (src/lower.c:1641) finds the `#counters` variable by
walking `hops` enclosing lower-units (`lower_search_vcode_obj(W_COUNTERS, lu,
&hops)`) and, when `hops > 0`, does `emit_load_indirect(emit_var_upref(hops,
var))`. The `#counters` var is created by the PR in `lower_cover_setup_package()`
(it does `emit_get_counters` + `emit_store` into the package lower-unit). The
SIGSEGV in `emit_var_upref` means the `hops`/context chain computed for a
**lazily-lowered package subprogram** does not match the actual vcode context
nesting at run time. So the package cover **counter** plumbing is also fragile on
the lazy `unit_registry_get → lower_proc_body` path, not just the expr items.

### 2.3 Most probable single underlying cause
Package subprogram bodies are lowered LAZILY at run time (`unit_registry_get`,
line 13128), in a context that differs from the eager elaboration-time lowering
of the design tree. The PR sets up the package cover scope + `#counters` in
`lower_cover_setup_package` during `lower_package`/`lower_pack_body`, but the
per-subprogram lowering that happens later (lazily) may not see the same cscope
/ counter context, so (a) expression `nth` indexing is misaligned vs creation
and (b) the counters var-upref depth is wrong. Verify this hypothesis first.

---

## 3. Guaranteed reproduction (UVVM, ~3 min once nvc is built)

The reliable repro is `tc_30_master_multiword` in the sibling repo
`uvvm_no_dal_assessment` (BFM = a package with a multi-word `case` body). From
that repo, after `bash bitvis_vip_spi_ext/script/compile_nvc.sh --clean` once to
build the libraries:
```
cd bitvis_vip_spi_ext
export NVC_GLOBAL="--std=2008 -L sim"
WORK=bitvis_vip_spi_ext ; TB=tc_30_master_multiword_tb
TOP=BITVIS_VIP_SPI_EXT.TC_30_MASTER_MULTIWORD_TB
printf '+hierarchy %s\n+hierarchy %s.*\n+package *_bfm_pkg\n' "$TOP" "$TOP" > /tmp/sp30.spec
# CRASHES (expression enabled):
nvc $NVC_GLOBAL --work="${WORK}:sim/${WORK}" \
  -e --cover=statement,branch,expression,toggle,fsm-state \
  --cover-spec=/tmp/sp30.spec --cover-file=/tmp/cd.covdb "$TB" -r
# CLEAN (expression dropped) — this is the current workaround:
nvc $NVC_GLOBAL --work="${WORK}:sim/${WORK}" \
  -e --cover=statement,branch,toggle,fsm-state \
  --cover-spec=/tmp/sp30.spec --cover-file=/tmp/cd.covdb "$TB" -r
```
The offending package body is `src/spi_ext_bfm_pkg.vhd` (the multi-word transfer
procedure, body around line 678; it has nested `case action_between_words is`
plus several `not config.CPHA` / `not config.CPOL` expressions).

### 3.1 Minimal repro — NOT yet found (negative results recorded)
Two hand-written minimal cases did **not** crash (a package proc with a couple
of `and`/`or`/`not` plus a `case` with `not` in an alternative; and a nested
`case` variant). The desync depends on the exact count/order of expression items
relative to the case structure. **Recommended:** start from `tc_30` and
*minimise downward* (delete BFM procedures / case arms until it stops crashing)
rather than build up. A standalone minimal repro is the first deliverable of the
dedicated session and should become a `test/regress/coverNN` case.

---

## 4. Key source pointers (this branch)

| What | Where |
|---|---|
| Expr coverage emit (crash A) | `src/lower.c:2420` `lower_expr_coverge`; crash at 2432 `emit_not`; unary path 2587-2588 |
| Logical-expr lowering / item fetch | `src/lower.c:2491` `lower_logical`; `cover_get_item(..., COV_ITEM_EXPRESSION, (*nth)++)` line 2504 |
| Branch coverage / counters (crash B) | `src/lower.c:1654` `lower_branch_coverage`; `src/lower.c:1641` `lower_cover_counters` (crash 1651) |
| Cover-flag definitions | `src/cov/cov-api.h:161-189` (`COV_FLAG_TRUE/FALSE`, `COVER_FLAGS_LHS_RHS_BINS`) |
| PR package cover setup (#counters + cscope) | `src/lower.c` `lower_cover_setup_package` (called from `lower_package`/`lower_pack_body`) |
| Cover-item CREATION walk for packages | `src/vhdl/vhdl-cover.c` `vhdl_cover_package` → `vhdl_cover_decls` |
| Lazy lowering of package subprograms | `src/lower.c:13128` `unit_registry_get` → `lower_proc_body` (10546) |
| Cover-item creation (TRUE/FALSE vs 00/01/..) | `src/cov/cov-data.c:436-569` |

---

## 5. Investigation plan (dedicated session)

1. **Confirm the item desync (crash A).** Instrument or gdb `lower_logical` for a
   package body: log `kind`, `*nth`, and `first->flags` for each expression.
   Compare against the creation walk in `vhdl_cover_decls`. Find where the two
   orders diverge for a package subprogram body containing a `case`. (Hypothesis:
   the creation walk visits case choices/alternatives in a different order, or
   double-counts, vs the lowering walk.)
2. **Confirm the counters scope bug (crash B).** Check `hops`/context for the
   `#counters` upref when a package subprogram is lowered lazily via
   `unit_registry_get`. Compare with an architecture-local subprogram (which
   works). Likely the lazy path does not nest the subprogram unit under the
   package cover context the way eager lowering does.
3. **Fix the alignment (the real fix), not just the crash.** A defensive guard in
   `lower_expr_coverge` (skip when `lhs`/`rhs` are `VCODE_INVALID_REG`, e.g.
   `return result;` before line 2432) stops crash A but MASKS the desync —
   expression numbers for packages would then be mis-attributed and unreliable.
   Prefer making the creation and lowering walks traverse package bodies in the
   same order (and fixing the counter context) so package expression coverage is
   correct, then the guard becomes belt-and-braces.
4. **Re-enable and verify.** Re-run §3 with `expression` in `--cover`; it must not
   crash AND must produce plausible, monotonic expression numbers for the BFM.
   Sanity-check that a unary `not` reports TRUE/FALSE bins, a binary op reports
   00/01/10/11.
5. **Regression test.** Add a `test/regress/coverNN` (package + body with a
   subprogram whose case body contains unary `not` and binary logicals) with a
   gold report, mirroring `cover30` (the PR's package-coverage test). This is the
   minimal repro from §3.1 turned into a permanent test.
6. **Upstream.** This is almost certainly in scope for PR #1456 itself or a
   follow-up; cross-check the upstream expression-coverage crash reports
   (nickg/nvc#1194 SEGFAULT with expression coverage, #1191 SEGFAULT with prot.
   types in generic packages + coverage). Coordinate with PR #1456's author
   (malles030) / reviewer (Blebowski) rather than diverging the fork further.

---

## 6. Current downstream workaround (to be removed once fixed)

In `uvvm_no_dal_assessment`:
- `bitvis_vip_spi_ext/script/compile_nvc.sh` and
  `tools/templates/compile_nvc.sh.tmpl` elaborate with
  `--cover=statement,branch,toggle,fsm-state` (expression OMITTED), and run with
  the `+package *_bfm_pkg` spec. `tools/scripts/parse_coverage.sh` reports
  expression as `na` (which passes the aux gate vacuously).
- Once expression coverage of packages is fixed here, add `expression` back to
  the `--cover` list in both files and re-baseline the campaign. The BFM has
  ~381 expression coverpoints currently unmeasured.

Note (§6 honesty): with `statement,branch,toggle,fsm-state` the full 40-run
spi_ext campaign is clean (40/40, no crash) and the BFM IS measured for
statement/branch/toggle. The crash is specific to enabling `expression`.

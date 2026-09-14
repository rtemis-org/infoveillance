# Contributing to infoveillance

## Reporting Issues

Search [existing issues](https://github.com/rtemis-org/infoveillance/issues) first. A
useful report includes:

1. **infoveillance version**: `utils::packageVersion("infoveillance")`
2. **R version**: `R.version.string`
3. **Operating system**
4. **A minimal reproducible example**, and what you expected instead
5. **Complete error messages**, with the stack trace

## Pull Requests

Discuss major changes in an issue first, so that design questions are settled
before anyone writes code.

### Licensing of contributions

infoveillance is released under the [BSD 3-Clause License](../LICENSE.md). By
submitting a pull request, patch, or any other contribution, you agree to all
of the following.

1. **Inbound equals outbound.** Your contribution is licensed under the BSD
   3-Clause License, the same terms that cover the rest of the package. You
   retain copyright in your own work.

2. **You grant the right to relicense.** You grant E.D. Gennatas a perpetual,
   worldwide, non-exclusive, royalty-free, irrevocable license to reproduce,
   modify, distribute and sublicense your contribution, including the right to
   distribute it under any OSI-approved license the project later adopts.

3. **You have the right to grant it.** Either you wrote the contribution
   yourself, or you have permission from its copyright holder to submit it
   under these terms. Do not submit code copied or adapted from a source under
   a copyleft license (GPL, AGPL, LGPL) or under any license whose terms
   conflict with BSD 3-Clause. If any part of your contribution originates
   elsewhere, say so in the pull request and name the source and its license.

### Sign your commits

Every commit must carry a `Signed-off-by` line certifying the
[Developer Certificate of Origin](https://developercertificate.org/):

```sh
git commit -s -m "Your commit message"
```

Amend an unsigned commit with `git commit --amend -s`, or a range with
`git rebase --signoff <base>`.

### Dependencies

A new dependency needs a justification in the pull request covering what it
replaces, its license, and its own dependency tree. Anything under a copyleft
license will not be accepted in `Imports`.

### Before you open a pull request

Development tasks go through the `justfile` rather than direct `Rscript` or
`R CMD` calls; `just --list` shows every recipe. Recipes chain, so `just
install` already runs `just document`, which already runs `just format`.

- `just install` -- format, document, and install
- `just test` -- run the test suite
- `just lint`, `just check-rd`, `just spell`
- `just check-cran` -- run before claiming CRAN compliance

### Code conventions

**Classes and types.** The backend is S7 throughout. Declare a hand-written
optional property as `NULL | <class>`, never `<class> | NULL`: S7 takes a
union's prototype from its **first** member. `NULL` is the only unset value;
test for it with `is.null()`, not `length(x) == 0L`.

**Validation.** Type-check and validate as early as possible, with corrective
error messages.

**Logging.** `msg()`, `info()`, and `abort()` from rtemis.core. Any function
that can print to the console takes a `verbosity` argument controlling how
much.

**Style.**

- Type-stable code; never rely on implicit coercion
- Integer literals carry an `L` suffix: `n = 10L`
- Optional arguments default to `NULL`, with the real default set in the body
- Two blank lines between definitions
- US English: `behavior`, `normalize`, `analyze`, `license`
- ASCII only, everywhere. CRAN rejects non-ASCII characters; use hexadecimal
  Unicode escapes where a literal one is unavoidable
- Comments describe only the current state of the code. No history and no
  argument for why the project works the way it does. Git records what
  changed. Do document non-obvious mechanism that a future editor has to
  preserve

**Documentation.** roxygen2 on everything, with examples. Internal functions
get `@keywords internal` and `@noRd`. Document a `@param` as
`Class: Description ending with period.` Do not restate default values in the
description -- they already appear in the `Usage` section.

**Tests.** Include tests for new functionality.

## Questions?

- **Bug reports and features**:
  [GitHub Issues](https://github.com/rtemis-org/infoveillance/issues)
- **Security issues**: contact the maintainer directly (see `DESCRIPTION`)

---

Thank you for contributing to infoveillance.

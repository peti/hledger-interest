# Revision history for hledger-interest

## 2.0.0 (2026-11-11)

### Incompatible changes

- The built-in `--bgb288`, `--ing-diba`, and `--db24` modes have been
  removed. hledger-interest no longer contains bank-specific or statutory
  rate tables; supply the rates with `--schedule` or `--schedule-file`
  together with an explicit day-count flag. Unlike the former modes,
  schedules earn no interest before their first entry and keep the last
  rate indefinitely. The historical tables survive as regression tests,
  which also show how to reproduce the former modes' output.
- The interest account is matched literally by its full name. Punctuation
  is no longer interpreted as a regular expression, subaccounts are no
  longer included, and an account without matching transactions is an
  error.
- Rate selection is independent of posting frequency. Every rate option,
  including `--constant`, now posts interest at year ends by default;
  `--constant` and `--annual` are equivalent.
- Interest is no longer computed backwards: balance changes are processed
  in effective-date order, and overlapping periods are rejected instead of
  being counted twice.

### Changes that affect computed interest

- `--30-360`, `--30E-360`, and `--30E-360isda` miscounted periods that
  start on a day adjusted by the convention: a year of monthly payments on
  the 30th accrued 365, 367, or 358 days instead of 360. Periods are now
  counted from the last accounted day, so consecutive periods add up to
  the convention's count over the whole span. `--act` is unaffected
  ([#29](https://github.com/peti/hledger-interest/issues/29)).
- Posting-level dates are honored. A posting's own date, or its secondary
  date, determines when it changes the balance, taking precedence over the
  transaction's dates
  ([#31](https://github.com/peti/hledger-interest/issues/31)).
- Secondary transaction dates are respected when ordering transactions.
- Transaction costs (`@` and `@@`) no longer leak into the principal,
  the descriptions, or the generated postings.
- `--act` splits calculations at calendar-year boundaries in every posting
  mode, so each year uses its correct 365/366-day denominator. These
  internal splits do not compound interest unless it is actually posted.
- No rate is looked up while the balance is zero, e.g. before the first
  loan.

### New features

- `--until=YYYY-MM-DD` computes and posts interest through an inclusive
  cutoff date and ignores later transactions, adapting Bas van Dijk's
  contribution in [#19](https://github.com/peti/hledger-interest/pull/19).
  `--today` is now an alias for `--until=today`; the last cutoff option
  wins.
- `--schedule-file=PATH` reads dated annualized rates from a text file.
  Rates may be given as fractions or percentages, and the file accepts
  hledger date and comment syntax. Syntax errors report the file, line,
  and column.
- `--schedule` specifies annualized rates and their effective dates;
  `--annual-schedule` remains as an alias.
- `--post-on-rate-changes` posts interest at transactions and rate-period
  ends, but not at year ends. `--post-on-transactions` takes precedence
  when both flags are given.

### Other changes

- Invalid rate arguments and a missing account argument are reported
  before the journal is read, and computation errors are shown without
  exception noise.
- The `--verbose` and `--quiet` help texts describe the output accurately:
  verbose mode prints the matching input transactions, not the entire
  journal.
- The README documents the rounding and precision policy: interest accrues
  unrounded, and only its display is rounded.
- A new `calculations` test suite checks interest amounts and posting
  dates against hand-derived values, and the golden-file `regression`
  suite covers all day-count conventions with all posting modes.
- Require hledger-lib >= 1.32.

## 1.7.0 (2026-11-11)

### Changes that affect computed interest

Several bugs have been fixed that made hledger-interest compute wrong
amounts. Re-running it on existing journals may produce different
results in the following cases:

- `--bgb288` applied 5.37% to every date after 2011-06-30, because a
  stray entry in the table of base rates shadowed all later rates. The
  table has been fixed, checked against the rates published by the
  Deutsche Bundesbank, and extended up to 2026-12-31. Dates after the
  end of the table now fail with an explicit message requesting a
  rate-table update.
- `--annual-schedule` used the rate in effect at the start of a period
  until the next transaction or year end, ignoring any rate changes in
  between, and a new rate took effect only on the day after its start
  date. Rates now take effect exactly on their start date
  ([#20](https://github.com/peti/hledger-interest/issues/20),
  [#23](https://github.com/peti/hledger-interest/pull/23)). Dates
  before the first entry of the schedule earn no interest instead of
  crashing.
- A transaction on the first day of a rate period, e.g. on January 1st
  with `--annual`, earned one day of interest too many
  ([#21](https://github.com/peti/hledger-interest/issues/21)).
- `--30E-360isda` counted the days of a period that starts on the 31st
  of a month wrongly, e.g. 30 instead of 15 days from 2023-01-31 to
  2023-02-15 ([#28](https://github.com/peti/hledger-interest/issues/28)).
- Interest on balances without decimal places is now shown with at
  least two decimal places instead of being rounded to whole units.

### New features

- The new `--post-on-transactions` flag posts interest only on transaction
  dates (and on the final day with `--today`). Year and rate boundaries
  can still split the calculation, but do not themselves trigger posting
  or compounding
  ([#16](https://github.com/peti/hledger-interest/issues/16),
  [#26](https://github.com/peti/hledger-interest/issues/26)).
- Without `--file`, hledger-interest now reads `$LEDGER_FILE` or
  `~/.hledger.journal` like hledger does, instead of silently reading
  nothing ([#24](https://github.com/peti/hledger-interest/issues/24)).
  Errors while reading the journal, such as a missing file, are
  reported without the "Uncaught exception" noise.

### Other changes

- Descriptions of interest on multi-currency balances no longer contain
  line breaks ([#11](https://github.com/peti/hledger-interest/issues/11),
  [#13](https://github.com/peti/hledger-interest/pull/13)).
- The README explains how interest is computed with each day counting
  convention, and that `--act` corresponds to Actual/Actual ISDA
  ([#25](https://github.com/peti/hledger-interest/issues/25)).
- Support hledger-lib up to version 1.52.x and GHC 9.6 to 9.14.
- Drop the obsolete dependency on Cabal and the `Setup.hs` file.

## 1.6.7 and earlier

See the [git history](https://github.com/peti/hledger-interest/commits/master).

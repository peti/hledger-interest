# Revision history for hledger-interest

## Unreleased

- Process transactions in effective-date order, respecting secondary dates,
  and reject backwards accrual rather than counting overlapping periods.
- Match the full interest account name literally throughout. Punctuation
  is no longer interpreted as a regular expression, subaccounts are excluded,
  and an account with no matching transactions produces an error.
- Remove the built-in BGB288, ING-DiBa, and DB24 modes and their flags.
  Their historical data now live only in the test suite, which supplies
  them through `--schedule` and explicit day-count flags. Unlike the former
  modes, schedules earn zero interest before the first entry and keep the
  last rate indefinitely. The DB24 example uses `--post-on-rate-changes`
  to reproduce its original output.
- Add `--schedule`, retaining `--annual-schedule` as an alias. Rate selection
  is independent of posting frequency: all rates now post automatically at
  year ends by default, including `--constant` (now equivalent to `--annual`).
- Add `--post-on-rate-changes` to post only at transactions and rate-period
  ends. `--post-on-transactions` suppresses both automatic posting boundaries
  and takes precedence when combined with the new flag.
- Split actual-day calculations at calendar-year boundaries for correct
  365/366-day denominators in every posting mode, without compounding at
  internal calculation boundaries. Fixed 360-day conventions can calculate
  across years without a split when year-end posting is suppressed.
- Skip rate lookup while the balance is zero, including before the first loan.
- Report invalid rate arguments and missing account arguments before
  reading the journal, and display computation errors without exception noise.
- Preserve Unicode output in golden tests, require existing baselines,
  and cover all four day-count conventions with all three posting modes
  for scheduled rates, as well as regression cases for these fixes.
- Require hledger-lib >= 1.32 and use its precision helper directly.

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

# Revision history for hledger-interest

## 1.7.0 (2026-11-11)

### Changes that affect computed interest

Several bugs have been fixed that made hledger-interest compute wrong
amounts. Re-running it on existing journals may produce different
results in the following cases:

- `--bgb288` applied 5.37% to every date after 2011-06-30, because a
  stray entry in the table of base rates shadowed all later rates. The
  table has been fixed, checked against the rates published by the
  Deutsche Bundesbank, and extended up to 2026-12-31. Dates after the
  end of the table now produce a clear error message instead of a
  crash.
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

- The new `--post-on-transactions` flag posts interest only when the
  balance changes (and on the final day with `--today`), rather than at
  the end of every year and rate period. Interest is still computed
  separately for every year and rate, but no longer compounds at the end
  of each year
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

hledger-interest
================

[![hackage release](https://img.shields.io/hackage/v/hledger-interest.svg?label=hackage)](http://hackage.haskell.org/package/hledger-interest)
[![stackage LTS package](http://stackage.org/package/hledger-interest/badge/lts)](http://stackage.org/lts/package/hledger-interest)
[![stackage Nightly package](http://stackage.org/package/hledger-interest/badge/nightly)](http://stackage.org/nightly/package/hledger-interest)
[![CI Status](https://github.com/peti/hledger-interest/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/peti/hledger-interest/actions/workflows/haskell-ci.yml)

hledger-interest is a small command-line utility based on [Simon
Michael's hledger library](http://hledger.org/). Its purpose is to
compute interest for a given ledger account. Using command line flags,
the program can be configured to use various [day counting
conventions](http://en.wikipedia.org/wiki/Day_count_convention), such as
"act/act", "30/360", "30E/360", and "30/360isda". It supports
constant rates, fixed annual rates, and schedules of annual rates supplied
on the command line.

An overview over the available run-time options can be displayed by
running "`hledger-interest --help`":

    Usage: hledger-interest [OPTION...] ACCOUNT
    Default: post at transactions, rate-period ends, and year ends.
      -h          --help                      print this message and exit
      -V          --version                   show version number and exit
      -v          --verbose                   echo input ledger to stdout (default)
      -q          --quiet                     don't echo input ledger to stdout
                  --today                     compute interest up until today
                  --post-on-transactions      post only at transactions (and with --today);
                                              skip rate-period and automatic year-end postings;
                                              overrides --post-on-rate-changes
                  --post-on-rate-changes      post at transactions and rate-period ends;
                                              skip automatic year-end postings;
                                              --post-on-transactions takes precedence;
                                              --today also posts on the final day
      -f FILE     --file=FILE                 input ledger file (pass '-' for stdin)
                                              default: $LEDGER_FILE or ~/.hledger.journal
      -s ACCOUNT  --source=ACCOUNT            interest source account
      -t ACCOUNT  --target=ACCOUNT            interest target account
      -I          --ignore-assertions         ignore any failing balance assertions
                  --act                       use 'act/act' day counting convention
                                              (Actual/Actual ISDA; see README)
                  --30-360                    use '30/360' day counting convention
                  --30E-360                   use '30E/360' day counting convention
                  --30E-360isda               use '30E/360isda' day counting convention
                  --constant=RATE             fixed annual interest rate
                  --annual=RATE               alias for --constant
                  --schedule=SCHEDULE         schedule of annual interest rates.
                                              syntax: '[(Date1,Rate1),(Date2,Rate2),...]'
                  --annual-schedule=SCHEDULE  alias for --schedule

When run, hledger-interest reads the [ledger
file](http://hledger.org/MANUAL.html#file-format) designated by the
`--file` flag and filters all transactions that change the account
specified on the command line. All other accounts will be ignored. Every
account name must be specified in full and is matched literally, including
any punctuation; selecting a parent account does not include its subaccounts.
An account with no matching transactions produces an error. Transactions
are processed in effective-date order, using the secondary date when one
is present and the primary date otherwise. Every
time a transaction modifies the given account's balance -- thereby
changing the amount of money that earns interest --, hledger-interest
transfers the interest that accrued so far. Interest will be debited
from the account designed by the `--source` flag and credited to the
account designed by the `--target` flag.

## Day counting conventions

For every period, hledger-interest computes

    interest = balance * rate * days / days-in-year

where `days` is the length of the period, both ends included, and
`days-in-year` is the length of the calendar year in which the period
ends. Both are counted with the selected convention:

- `--act` counts actual calendar days. Calculation splits at calendar-year
  boundaries, using 365 or 366 days for each year. This is the
  [Actual/Actual ISDA](https://en.wikipedia.org/wiki/Day_count_convention#Actual/Actual_ISDA)
  method for every rate option. Internal calculation splits do not cause
  compounding unless interest is actually posted.
- `--30-360`, `--30E-360`, and `--30E-360isda` count every month as 30
  days and every year as 360 days. They differ in how they treat the
  31st and the end of February; see the [Wikipedia
  article](https://en.wikipedia.org/wiki/Day_count_convention#30/360_methods)
  for details. `--30E-360isda` does not apply the February exception
  for the maturity date, because hledger-interest has no notion of
  maturity.

Rate selection and posting frequency are independent. By default, interest
is posted on transaction dates, at the end of each rate period, and on
December 31st. Crediting interest to the interest-bearing account compounds
it from the posting date.

| Posting option | Transactions | Rate-period ends | Year ends |
|---|---|---|---|
| Default | Yes | Yes | Yes |
| `--post-on-rate-changes` | Yes | Yes | No |
| `--post-on-transactions` | Yes | No | No |

`--post-on-rate-changes` posts at transactions and rate-period ends, without
an additional automatic year-end posting. A transaction
on December 31st or a new rate taking effect on January 1st still causes a
posting on December 31st. `--post-on-transactions` takes precedence if both
flags are supplied, regardless of their order. All modes also post on the
final day when `--today` is used.

With `--act`, calculations still split internally at year boundaries to
apply the correct 365/366-day denominator, even when year-end posting is
suppressed. Fixed 360-day conventions need no internal year split unless
a year-end posting is requested.

With `--post-on-transactions`, a posting may combine several rates, so its
description omits the rate, for example:

    2022-01-10 interest for -9841.10 USD over 31 days
        Liabilities:Loan       -53.65 USD
        Expenses:Interest       53.65 USD

## Regression tests

Run the golden tests against the Cabal-built executable with:

```sh
cabal test regression --test-show-details=direct
```

The cases in `tests/golden-tests.hs` run fixed journals from `tests/fixtures/`
and compare stdout bytes exactly with `tests/golden/*.golden`, preserving
Unicode. Successful commands must exit with empty stderr; error cases
check the exit status, empty stdout, and the expected diagnostic. Coverage includes all
four day-count conventions with constant, annual, and scheduled rates.
Scheduled rates have golden coverage for all three posting modes across
all four conventions. Other cases cover
historical rate schedules; leap days and month/year ends;
rate changes, same-day payments, compounding versus separate interest
accounts, deferred posting, zero/negative rates, and verbose output.
Additional cases cover secondary dates, literal account names, Unicode,
the start of each historical schedule, dates outside the former coverage
limits, and invalid arguments. The historical BGB288, DB24, and ING-DiBa data live only
in `tests/HistoricalRates.hs`; tests pass them through `--schedule`
with explicit day-count flags. These are fixed regression snapshots.
The DB24 case uses `--post-on-rate-changes` and reproduces the original
built-in mode's output exactly. Tests also cover rate aliases and posting
flag precedence across all four day-count conventions.
Tests deliberately avoid `--today` so results do not depend on the clock.

To add a case, add its argument list to `tests/golden-tests.hs` and, if needed,
add a fixture journal. Missing baselines fail the suite, even with
`--accept`. For a new golden case, first create an empty baseline explicitly
(for example, `touch tests/golden/new-case.golden`). Then record or
intentionally update expected output:

```sh
cabal test regression --test-options=--accept
git diff -- tests/golden
```

Review new and changed golden files before committing them. They capture
current behavior, including formatting supplied by `hledger-lib`, rather
than independently proving the calculations correct. Dependency upgrades
can therefore require reviewed baseline updates. The fixtures and golden
files are included in source distributions, and CI runs the suite.

## Examples

Suppose that you've loaned 1000 Euro from your bank at an annual
interest rate of 5%, and that you would like to see how interest
develops over time. Then you would create a ledger file, say
`loan.ledger`, that looks something like this:

    2010/09/26 Loan
        Assets:Bank                     EUR 1000.00
        Liabilities:Loan

Now, `ledger-interest` is run to determine the interest up until today:

    $ hledger-interest -f loan.ledger --act --annual=0.05 --today -s Expenses:Interest -t Liabilities:Loan:Interest Liabilities:Loan
    2010/09/26 Loan
        Assets:Bank                     EUR 1000.00
        Liabilities:Loan

    2010/12/31 5.00% interest for EUR -1000.00 over 96 days
        Liabilities:Loan:Interest       EUR -13.15
        Expenses:Interest

    2011/08/22 5.00% interest for EUR -1000.00 over 234 days
        Liabilities:Loan:Interest       EUR -32.05
        Expenses:Interest

Note a separate credit account for the interest was chosen:
`Liabilities:Loan:Interest`. Consequently, interest accrued in one
interest period does *not* earn interest in the following periods. If
interest is credited to the main account instead, that behavior changes:

    $ hledger-interest -f loan.ledger --act --annual=0.05 --today -s Expenses:Interest -t Liabilities:Loan Liabilities:Loan
    2010/09/26 Loan
        Assets:Bank                     EUR 1000.00
        Liabilities:Loan

    2010/12/31 5.00% interest for EUR -1000.00 over 96 days
        Liabilities:Loan                EUR -13.15
        Expenses:Interest

    2011/08/22 5.00% interest for EUR -1013.15 over 234 days
        Liabilities:Loan                EUR -32.48
        Expenses:Interest

Of course, loans are supposed to be paid back, and these payments change
the amount of interest accrued. Suppose that `loan.ledger` would be
extended by the following transactions:

    2010/12/11 Payment
        Assets:Bank                     EUR -150.00
        Liabilities:Loan

    2011/03/07 Payment
        Assets:Bank                     EUR -300.00
        Liabilities:Loan

    2011/08/21 Payment
        Assets:Bank                     EUR -150.00
        Liabilities:Loan

Then interest would develop as follows:

    $ hledger-interest -f loan.ledger --act --annual=0.05 -s Expenses:Interest -t Liabilities:Loan Liabilities:Loan
    2010/09/26 Loan
        Assets:Bank                     EUR 1000.00
        Liabilities:Loan

    2010/12/11 5.00% interest for EUR -1000.00 over 76 days
        Liabilities:Loan                EUR -10.41
        Expenses:Interest

    2010/12/11 Payment
        Assets:Bank                     EUR -150.00
        Liabilities:Loan

    2010/12/31 5.00% interest for EUR -860.41 over 20 days
        Liabilities:Loan                EUR -2.36
        Expenses:Interest

    2011/03/07 5.00% interest for EUR -862.77 over 66 days
        Liabilities:Loan                EUR -7.80
        Expenses:Interest

    2011/03/07 Payment
        Assets:Bank                     EUR -300.00
        Liabilities:Loan

    2011/08/21 5.00% interest for EUR -570.57 over 167 days
        Liabilities:Loan                EUR -13.05
        Expenses:Interest

    2011/08/21 Payment
        Assets:Bank                     EUR -150.00
        Liabilities:Loan

## Variable annual rates

Use `--schedule` to supply rates and their effective dates, together
with an explicit day-count convention. For example:

```sh
hledger-interest -f loan.ledger --act \
  '--schedule=[(2023-01-01,0.05),(2023-07-01,0.07)]' \
  --source=Expenses:Interest --target=Liabilities:Loan Liabilities:Loan
```

This applies 5% from January 1, 2023, and 7% from July 1, 2023. Dates
before the first entry earn zero interest, and the last rate continues
indefinitely. By default, interest is posted at year ends, rate changes,
and transaction dates. Use `--post-on-rate-changes` or
`--post-on-transactions` to control posting as explained above.
`--annual-schedule` remains an alias for `--schedule`. Likewise,
`--annual` is an alias for `--constant`; both specify a fixed annual rate. The executable contains no bank-specific or statutory
rate tables; supply the appropriate schedule yourself.

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
"act/act", "30/360", "30E/360", and "30E/360isda". It supports
fixed annualized rates and rates that change on specified dates. Rates
are expressed as fractions: `0.05` means 5% per year.

Display the available command-line options with `hledger-interest --help`:

    Usage: hledger-interest [OPTION...] ACCOUNT
    Default: post at transactions, rate-period ends, and year ends.
      -h          --help                      print this message and exit
      -V          --version                   show version number and exit
      -v          --verbose                   echo input ledger to stdout (default)
      -q          --quiet                     don't echo input ledger to stdout
                  --today                     alias for --until=today
                  --until=DATE                compute interest through DATE (YYYY-MM-DD or today);
                                              exclude transactions with later effective dates
                  --post-on-transactions      post only at transactions (and at --until/--today);
                                              skip rate-period and automatic year-end postings;
                                              overrides --post-on-rate-changes
                  --post-on-rate-changes      post at transactions and rate-period ends;
                                              skip automatic year-end postings;
                                              --post-on-transactions takes precedence;
                                              --until/--today also post on the final day
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
                  --schedule=SCHEDULE         annualized rates and their effective dates;
                                              rates are fractions: 0.05 means 5% per year
                                              syntax: '[(Date1,Rate1),(Date2,Rate2),...]'
                  --schedule-file=PATH        read a schedule from a text file;
                                              one date and rate per line, starting in column 1;
                                              rates: 0.05 or 5%; hledger comments allowed
                  --annual-schedule=SCHEDULE  alias for --schedule

When run, hledger-interest reads the [ledger
file](http://hledger.org/MANUAL.html#file-format) designated by the
`--file` flag and selects transactions containing postings to the account
specified on the command line. The selected account name must be specified
in full and is matched literally, including punctuation; selecting a parent
account does not include its subaccounts. Other postings in a matching
transaction are preserved when the input journal is echoed.
An account with no matching transactions produces an error. Transactions
are processed in effective-date order, using the secondary date when one
is present and the primary date otherwise. Interest accrued through each
transaction date is posted before that transaction changes the balance.
Generated interest is booked to the account designated by `--target`, with
the opposite amount booked to the account designated by `--source`.
The amounts' signs depend on the balance and rate.

## Day counting conventions

For every period, hledger-interest computes

    interest = balance * rate * days / days-in-year

where `days` is the length of the period, counted from the last day
already accounted for (for example, the loan date) to the posting date,
and `days-in-year` is the length of the calendar year in which the period
ends. Both are counted with the selected convention. For every convention,
the day counts of consecutive periods add up to the count over the whole
span:

- `--act` counts actual calendar days. Calculation splits at calendar-year
  boundaries, using 365 or 366 days for each year. This is the
  [Actual/Actual ISDA](https://en.wikipedia.org/wiki/Day_count_convention#Actual/Actual_ISDA)
  method for every rate option. Internal calculation splits do not cause
  compounding unless interest is actually posted.
- `--30-360` uses 30-day month offsets and a 360-day year, without
  adjusting day numbers at month ends.
- `--30E-360` also uses a 360-day year, but replaces the 31st with the
  30th when counting days. February retains its actual last day.
- `--30E-360isda` additionally treats the last day of February as the
  30th. It does not apply the February exception for the maturity date,
  because hledger-interest has no notion of maturity.

Rate selection and posting frequency are independent. By default, interest
is posted on transaction dates, at the end of each rate period, and on
December 31st. Posting interest to the interest-bearing account compounds
it from the following day.

| Posting option | Transactions | Rate-period ends | Year ends |
|---|---|---|---|
| Default | Yes | Yes | Yes |
| `--post-on-rate-changes` | Yes | Yes | No |
| `--post-on-transactions` | Yes | No | No |

`--post-on-rate-changes` posts at transactions and rate-period ends, without
an additional automatic year-end posting. A transaction on December 31st
or a new rate taking effect on January 1st still causes a
posting on December 31st. `--post-on-transactions` takes precedence if both
flags are supplied, regardless of their order. All modes also post on the
final day when `--until` or `--today` is used.

With `--act`, calculations still split internally at year boundaries to
apply the correct 365/366-day denominator, even when year-end posting is
suppressed. Fixed 360-day conventions need no internal year split unless
a year-end posting is requested.

With `--post-on-transactions`, a posting may combine several rates, so its
description omits the rate, for example:

    2022-01-10 interest for -9841.10 USD over 31 days
        Liabilities:Loan       -53.65 USD
        Expenses:Interest       53.65 USD

## Rounding and precision

Interest is calculated using decimal arithmetic at its available precision.
Amounts are not explicitly rounded to cents when they accrue or are posted.
If interest is posted to the interest-bearing account, the full computed
quantity becomes part of the principal for subsequent calculations.
Deferred postings sum the unrounded accrued amounts before displaying them.

Display precision is separate from calculation precision. Generated postings
show at least two decimal places, retaining higher display precision from
the input. Formatting and display rounding follow `hledger-lib`'s rendering
rules; they do not change the quantity used internally. For example, an
interest amount of EUR 0.004 can be displayed as EUR 0.00 while EUR 0.004
is still added to the internal principal when compounding.

Consequently, later displayed balances and interest can differ slightly
from calculations made using only the printed amounts. Reading the output
journal back in also uses those printed amounts, not the undisplayed
fractions. The tool does not implement a policy of rounding each interest
posting to the currency's smallest unit before compounding.

## Calculation cutoff

Use `--until=YYYY-MM-DD` to compute interest through a specific date,
including that day. For example, `--until=2024-01-10` posts any remaining
interest on January 10th, even if there is no transaction on that date.
Dates may omit leading zeros in the month and day.

`--today` is an alias for `--until=today`, using the current local date.
Both options use one cutoff setting: if they are repeated or combined,
the last one wins. Without either option, computation stops at the last
matching transaction.

Transactions with effective dates after the cutoff are excluded from
both computation and echoed output. A secondary date takes precedence
over the primary date, so a transaction with a later primary date can
still be included if its secondary date is on or before the cutoff.
Transactions on the cutoff date are processed normally. A cutoff before
the first matching transaction produces no output.

## Examples

Suppose you borrowed EUR 1000 from your bank at 5% per year and made three
repayments. Store the following journal in `loan.ledger`:

    2010-09-26 Loan
        Assets:Bank          EUR 1000.00
        Liabilities:Loan

    2010-12-11 Payment
        Assets:Bank          EUR -150.00
        Liabilities:Loan

    2011-03-07 Payment
        Assets:Bank          EUR -300.00
        Liabilities:Loan

    2011-08-21 Payment
        Assets:Bank          EUR -150.00
        Liabilities:Loan

To post interest to a separate account, run:

    $ hledger-interest -f loan.ledger --quiet --act --annual=0.05 -s Expenses:Interest -t Liabilities:Loan:Interest Liabilities:Loan
    2010-12-11 5% interest for EUR -1000.00 over 76 days
        Liabilities:Loan:Interest      EUR -10.41
        Expenses:Interest               EUR 10.41

    2010-12-31 5% interest for EUR -850.00 over 20 days
        Liabilities:Loan:Interest       EUR -2.33
        Expenses:Interest                EUR 2.33

    2011-03-07 5% interest for EUR -850.00 over 66 days
        Liabilities:Loan:Interest       EUR -7.68
        Expenses:Interest                EUR 7.68

    2011-08-21 5% interest for EUR -550.00 over 167 days
        Liabilities:Loan:Interest      EUR -12.58
        Expenses:Interest               EUR 12.58

The target account, `Liabilities:Loan:Interest`, differs from the selected
account, `Liabilities:Loan`, so the posted interest does not itself earn
interest. Selecting the parent account does not include its subaccounts.
The `--quiet` flag prints only generated interest postings.

To compound interest, use the loan account itself as the target:

    $ hledger-interest -f loan.ledger --quiet --act --annual=0.05 -s Expenses:Interest -t Liabilities:Loan Liabilities:Loan
    2010-12-11 5% interest for EUR -1000.00 over 76 days
        Liabilities:Loan       EUR -10.41
        Expenses:Interest       EUR 10.41

    2010-12-31 5% interest for EUR -860.41 over 20 days
        Liabilities:Loan        EUR -2.36
        Expenses:Interest        EUR 2.36

    2011-03-07 5% interest for EUR -862.77 over 66 days
        Liabilities:Loan        EUR -7.80
        Expenses:Interest        EUR 7.80

    2011-08-21 5% interest for EUR -570.57 over 167 days
        Liabilities:Loan       EUR -13.05
        Expenses:Interest       EUR 13.05

Interest posted on December 11th and at year end now becomes part of the
principal for subsequent calculations. Add `--today` to either command to
extend the calculation beyond the final repayment through the current date;
that additional output depends on when the command runs.
Use `--until=2011-08-22` instead for a fixed, reproducible cutoff.

## Variable annualized rates

Use `--schedule` to supply annualized rates and their effective dates,
together with an explicit day-count convention. For example:

```sh
hledger-interest -f loan.ledger --act \
  '--schedule=[(2010-09-26,0.05),(2011-01-01,0.07)]' \
  --source=Expenses:Interest --target=Liabilities:Loan Liabilities:Loan
```

For the journal above, this applies 5% per year from September 26, 2010,
and 7% per year from January 1, 2011. Rates can change on any date. Dates
before the first entry earn zero interest, and the last rate continues
indefinitely. By default, interest is posted at year ends, at the end of
each rate period (the day before the next rate takes effect), and on
transaction dates. Use `--post-on-rate-changes` or
`--post-on-transactions` to control posting as explained above.
`--annual-schedule` remains an alias for `--schedule`. Likewise,
`--annual` is an alias for `--constant`; both specify a fixed annualized
rate. The executable contains no bank-specific or statutory rate tables;
supply the appropriate schedule yourself.

### Schedule files

Use `--schedule-file=PATH` to read the rates from a plain text file instead
of specifying a list on the command line. For example, `rates.txt` could
contain:

```text
# Annualized rates and their effective dates
2010-09-26 5%
2011/1/1  0.07  ; equivalent to 7%

comment
This block is ignored.
end comment
```

Run it with the loan journal above:

```sh
hledger-interest -f loan.ledger --act --schedule-file=rates.txt \
  --source=Expenses:Interest --target=Liabilities:Loan Liabilities:Loan
```

Each entry consists of a hledger date and an annualized rate, separated by
spaces or tabs. A fraction such as `0.15` and a percentage such as `15%`
are equivalent. Signed rates are supported, and whitespace before `%` or
at the end of a line is ignored. Empty and whitespace-only lines are ignored.
An entry must begin in column 1: `   2026-01-01 0.05` is a syntax error.

Dates accept `YYYY-MM-DD`, `YYYY/MM/DD`, or `YYYY.MM.DD`, with optional
leading zeros. As in hledger journals, dates without a year use the
current local year. Specify the year explicitly for reproducible schedules.

Comments follow hledger syntax: lines beginning with `#`, `;`, or `*`
(optionally indented) are ignored. A semicolon starts an inline comment;
`#` and `*` do not. A `comment` line starts a block that ends at an
`end comment` line or the end of the file. Both block delimiters start
in column 1. Windows line endings and a missing final newline are accepted.

File schedules use the same sorting, posting, and rate rules as `--schedule`.
An empty or comments-only file specifies zero interest. When rate options
are repeated or combined, the last one wins; overridden files are not read.
Read and syntax errors identify the schedule file, with line and column
numbers for syntax errors.

## Regression tests

Run the independent calculation assertions and CLI golden tests with:

```sh
cabal test all --test-show-details=direct
```

The `calculations` suite in `tests/calculation-tests.hs` checks the computation
engine directly against hand-derived rational values, independently of
rendered output. It covers day counts, repayment timing, leap-year
denominators, compounding, deferred posting, rate changes, signs, and the
rounding policy above. A comparison tolerance of `10^-30` accommodates finite
decimal representations of recurring fractions while remaining far below
the errors caused by rounding these examples to cents. These assertions
have no golden files to update.

The `regression` suite in `tests/golden-tests.hs` runs fixed journals from
`tests/fixtures/` and compares stdout bytes exactly with
`tests/golden/*.golden`, preserving Unicode. Successful commands must exit
with empty stderr; error cases
check the exit status, empty stdout, and the expected diagnostic. Coverage
includes all four day-count conventions with constant, annual, and scheduled
rates. Scheduled rates have golden coverage for all three posting modes
across all four conventions. Other cases cover historical rate schedules,
leap days, month and year ends, rate changes, same-day payments, compounding
versus separate interest accounts, deferred posting, zero and negative
rates, and verbose output. Additional cases cover secondary dates,
literal account names, Unicode,
the start of each historical schedule, dates outside the former coverage
limits, and invalid arguments. Historical BGB288 rates live in
`tests/fixtures/bgb288.rates` and are read with `--schedule-file`. DB24 and
ING-DiBa schedules are defined in `tests/HistoricalRates.hs` and passed
through `--schedule`. All use explicit day-count flags and are fixed
regression snapshots.
The DB24 case uses `--post-on-rate-changes` and reproduces the original
built-in mode's output exactly. Tests also cover rate aliases and posting
flag precedence across all four day-count conventions.
Golden cases use explicit years and avoid `--today` so their expected
output does not depend on the clock. File-based schedules are checked
against the same goldens for all day-count conventions and posting modes.
Cutoff goldens exercise `--until` across the same matrix, with additional
checks for inclusive dates, secondary dates, aliases, and option precedence.
Balance cases cover multiple currencies, full repayment followed by a new
loan, overpayments, and multiple postings to the selected account under all
three posting policies. Input cases check stdin, multiple journals in either
order, and valid, invalid, and explicitly ignored balance assertions.
Independent calculation tests also verify those balance transitions against
hand-derived amounts, including repayment of compounded interest.

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

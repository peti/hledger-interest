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
"act/act", "30/360", "30E/360", and "30/360isda". Furthermore, it
supports several of different interest schemes, i.e. annual interest
with a fixed rate and the scheme mandated by the German law [§ 288 BGB
Verzugszinsen](http://de.wikipedia.org/wiki/Verzugszinssatz). Extending
support for other schemes is fairly easy, but currently requires hacking
the source code.

An overview over the available run-time options can be displayed by
running "`hledger-interest --help`":

    Usage: hledger-interest [OPTION...] ACCOUNT
      -v          --verbose                   echo input ledger to stdout (default)
      -q          --quiet                     don't echo input ledger to stdout
                  --today                     compute interest up until today
                  --post-on-transactions      post interest only when the balance changes
                                              (and with --today), not at the end of
                                              every year or rate period
      -f FILE     --file=FILE                 input ledger file (pass '-' for stdin)
                                              default: $LEDGER_FILE or ~/.hledger.journal
      -s ACCOUNT  --source=ACCOUNT            interest source account
      -t ACCOUNT  --target=ACCOUNT            interest target account
                  --act                       use 'act/act' day counting convention
                                              (Actual/Actual ISDA; see README)
                  --30-360                    use '30/360' day counting convention
                  --30E-360                   use '30E/360' day counting convention
                  --30E-360isda               use '30E/360isda' day counting convention
                  --constant=RATE             constant interest rate
                  --annual=RATE               annual interest rate
                  --annual-schedule=SCHEDULE  schedule of annual interest rates.
                                              syntax: '[(Date1,Rate1),(Date2,Rate2),...]'
                  --bgb288                    compute interest according to German BGB288

When run, hledger-interest reads the [ledger
file](http://hledger.org/MANUAL.html#file-format) designated by the
`--file` flag and filters all transactions that change the account
specified on the command line. All other accounts will be ignored. Every
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

- `--act` counts actual calendar days, so a year has 365 or 366 days.
  `--annual`, `--annual-schedule`, and `--bgb288` never let a period
  cross a year boundary, which makes this the [Actual/Actual
  ISDA](https://en.wikipedia.org/wiki/Day_count_convention#Actual/Actual_ISDA)
  method. It is also the reason why these rates post interest on
  December 31st of every year (see below). With `--constant`, a period can span
  several years and is divided by the length of the year it ends in, so
  the result is only an approximation of Actual/Actual ISDA in that case.
- `--30-360`, `--30E-360`, and `--30E-360isda` count every month as 30
  days and every year as 360 days. They differ in how they treat the
  31st and the end of February; see the [Wikipedia
  article](https://en.wikipedia.org/wiki/Day_count_convention#30/360_methods)
  for details. `--30E-360isda` does not apply the February exception
  for the maturity date, because hledger-interest has no notion of
  maturity.

By default, hledger-interest posts the interest at the end of every
period: on every transaction date, but also on December 31st and on
every date at which the rate changes. If the interest is credited to the
interest-bearing account itself, it compounds from then on. Many loans
and savings accounts compound less often, though, e.g. only when a
payment is made. With `--post-on-transactions`, interest is still
computed separately for every year and rate, but posted only on
transaction dates (and on the final day with `--today`). Since such a
posting may cover several rates, its description does not mention a
rate, e.g. for a rate change from 5% to 7% on 2021-12-20:

    2022-01-10 interest for -9841.10 USD over 31 days
        Liabilities:Loan       -53.65 USD
        Expenses:Interest       53.65 USD

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
the amount of interest accrued. Suppose that `load.ledger` would be
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

Last but not least, there is a special case known as "Verzugszinsen" in
German law, which applies when someone is supposed to pay a bill, but
fails to do so on time. For every day past the deadline, interest
accrues according to terms specified in [§ 247
BGB](http://www.gesetze-im-internet.de/bgb/__247.html). The command line
flag `--bgb288` enables this scheme in `hledger-interest`.

Let's assume that customer ACME is supposed to pay 35 Euro by
2010/09/15, but the money actually arrives almost half a year late:

    2010/09/15 Services rendered to Customer ACME
        ACME                            1 hour @ EUR 35.00
        Receivable:ACME

    2011/03/17 ACME
        ACME                            EUR 35.00
        Receivable:ACME

According to German law, you are entitled to the following interest:

    $ hledger-interest -f acme.ledger --quiet --bgb288 -s Income:Interest -t Receivable:ACME:Interest Receivable:ACME
    2010/12/31 5.12% interest for EUR 35.00 over 107 days
        Receivable:ACME:Interest        EUR 0.53
        Income:Interest

    2011/03/17 5.12% interest for EUR 35.00 over 76 days
        Receivable:ACME:Interest        EUR 0.37
        Income:Interest

So, if you're smart, then you'll book the payment so that the accrued
interest is paid *first*:

    2011/03/17 ACME
        ACME                            EUR 35.00
        Receivable:ACME:Interest        EUR -0.90
        Receivable:ACME

This gives the following transaction history for the ACME account:

    $ hledger-interest -f acme.ledger --bgb288 -s Income:Interest -t Receivable:ACME:Interest Receivable:ACME |
      hledger -f - reg Receivable:ACME
    2010/09/15 Services rendered .. Receivable:ACME           EUR 35.00    EUR 35.00
    2010/12/31 5.12% interest for.. Re:ACME:Interest           EUR 0.53    EUR 35.53
    2011/03/17 5.12% interest for.. Re:ACME:Interest           EUR 0.37    EUR 35.90
    2011/03/17 ACME                 Re:ACME:Interest          EUR -0.90    EUR 35.00
                                    Receivable:ACME          EUR -34.10     EUR 0.90

hledger-interest
================

[![hackage release](https://img.shields.io/hackage/v/hledger-interest.svg?label=hackage)](http://hackage.haskell.org/package/hledger-interest)
[![stackage LTS package](http://stackage.org/package/hledger-interest/badge/lts)](http://stackage.org/lts/package/hledger-interest)
[![stackage Nightly package](http://stackage.org/package/hledger-interest/badge/nightly)](http://stackage.org/nightly/package/hledger-interest)
[![CI Status](https://github.com/peti/hledger-interest/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/peti/hledger-interest/actions/workflows/haskell-ci.yml)

hledger-interest is a small command-line utility based on [Simon
Michael's hlegder library](http://hledger.org/). Its purpose is to
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
running "`hleder-interest --help`":

    Usage: hledger-interest [OPTION...] ACCOUNT
      -v          --verbose                   echo input ledger to stdout (default)
      -q          --quiet                     don't echo input ledger to stdout
                  --today                     compute interest up until today
      -f FILE     --file=FILE                 input ledger file (pass '-' for stdin)
      -s ACCOUNT  --source=ACCOUNT            interest source account
      -t ACCOUNT  --target=ACCOUNT            interest target account
                  --act                       use 'act' day counting convention
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

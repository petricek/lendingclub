#!/usr/bin/env bash

set -e
set -u

wget --no-check-certificate 'https://www.lendingclub.com/fileDownload.action?file=LoanStats.csv&type=gen' -O >(tail -n+2 | perl -pe 's/,$//;' > LoanStats.csv)
wget --no-check-certificate 'http://www.lendingclub.com/fileDownload.action?file=InFunding2StatsNew.csv&type=gen' -O >(tail -n+2 | perl -pe 's/,$//;' > InFunding2StatsNew.csv)
R --vanilla -f combined.r 


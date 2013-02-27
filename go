#!/usr/bin/env bash

set -e
usage="$0 train|predict"
set -u


if echo $* | grep train
then
wget --no-check-certificate 'https://www.lendingclub.com/fileDownload.action?file=LoanStats.csv&type=gen' -O >(tail -n+2 | perl -pe 's/,$//;' > LoanStats.csv)
R --vanilla -f lendingclub-train.r 
fi

if echo $* | grep predict
then
wget --no-check-certificate 'http://www.lendingclub.com/fileDownload.action?file=InFunding2StatsNew.csv&type=gen' -O >(tail -n+2 | perl -pe 's/,$//;' > InFunding2StatsNew.csv)

R --vanilla -f lendingclub-predict.r 
fi

